# #4 A shapeless detour

[Previous page](03-constraining-value-types.md)

Before we go further, I'll go through some key things to understand about the shapeless library. This won't be a thorough deep dive, but if you'd like further information I'd highly recommend [Dave Gurnell](https://twitter.com/davegurnell)'s [The Type Astronaut’s Guide to Shapeless](https://books.underscore.io/shapeless-guide/shapeless-guide.html). We'll use features described in that book all the way through chapter seven.

### `HList`
`HList`s are linked lists at the type level. The typical implementation of a linked list in Scala is
```scala
sealed trait List[+T]
final case class ::[+T](head: T, tail: List[T]) extends List[T]
case object Nil extends List[Nothing]
```
In comparison, shapeless' `HList` looks like
```scala
sealed trait HList
final case class ::[+H, +T <: HList](head: H, tail: T) extends HList
case object HNil extends HList
```
There's not much different, but it's a very powerful construction. We can write code such as
```scala
val x = 1 :: true :: "hi" :: HNil
```
and the compiler will maintain complete type information about each element of the list, as opposed to Scala's regular `List` which would be forced into a `List[Any]`. We need to be careful though, as if we wrote
```scala
val x: HList = 1 :: true :: "hi" :: HNil
```
then we'd immediately lose the type information held about the elements, and we're effectively back to a `List[Any]`. As such, we need to be careful not to lose track of the specific subtype of `HList` we're dealing with in our code.

### `Coproduct`
`Coproduct`s on the other hand are similar in some ways to the `Either` type, and in some ways to the `List` type. Where `Either` can be implemented as
```scala
sealed trait Either[+A, +B]
final case class Right[T](t: T) extends Either[Nothing, T]
final case class Left[T](t: T) extends Either[T, Nothing]
```
the `Coproduct` implementation looks like
```scala
sealed trait Coproduct
sealed trait CNil extends Coproduct // No actual implementations
sealed trait :+:[+H, +T <: Coproduct] extends Coproduct
final case class Inl[+H, +T <: Coproduct](head: H) extends :+:[H, T]
final case class Inr[+H, +T <: Coproduct](tail: T) extends :+:[H, T]
```
An example of how this might look in practice is
```scala
val x: Int :+: Boolean :+: String :+: CNil = Inr(Inl(true))
```
or to write out the type another way, `:+:[Int, :+:[Boolean, :+:[String, CNil]]]`.

The reasoning for this implementation is that in order to do useful things with `Coproduct`s, we need the ability to recurse through their possible members. The encoding above makes this very easy.

### `Generic`
Shapeless provides a type class, `Generic`, which provides methods for switching between two isomorphic types:
```scala
trait Generic[T] {
  type Repr

  def to(t : T) : Repr
  def from(r : Repr) : T
}
```
`HList`s and `Coproduct`s can be seen as alternative representations of case classes and traits, respectively. As such, shapeless provides implicit instances for case classes and _sealed_[^1] traits via macros. Therefore, if we can think of a way to do useful things with generic `HList`s and `Coproduct`s, then we suddenly gain the ability to do those things with all of our ADTs.

### Recursive type class derivation
Let's say we have a simple type class:
```scala
trait Show[T] {
  def show(t: T): String
}

object Show {
  implicit val showString: Show[String] = identity
  implicit val showInt: Show[Int] = _.toString
  implicit val showBoolean: Show[Boolean] = _.toString
}
```
There's an omnipresent technique in shapeless which can be used to provide `Show` instances for all `HList`s and `Coproduct`s which themselves are built from types which have `Show` instances. First, we define an instance of the type class for the `Nil` case. Then we define a way to generate an instance of the type class which combines an individual element, and an arbitrary `HList`/`Coproduct` tail. For example:
```scala
implicit val showHNil: Show[HNil] = _ => ""

implicit def showHCons[H, T <: HList](
  implicit showH: Show[H], 
  showT: Show[T]
): Show[H :: T] = 
  { case h :: t => s"${showH.show(h)} ${showT.show(t)}" }
```
To see how this works, let's say we're trying to summon an instance of `Show` for `String :: Boolean :: HNil`.
```scala
val x = implicitly[Show[String :: Boolean :: HNil]]
// the compiler will transform this into
val x = 
  showHCons(
    showString,
    showHCons(
      showBoolean,
      showHNil
    )
  )
```
This is an example of how we can define functionality that works on `HList`s. Paired with `Generic`, this is an incredibly useful trick for deriving functionality that acts on arbitrary ADTs.

### The `Aux` pattern
The `Show` type class above is rather simple. It collapses the entire `HList` down into a single `String`. Let's say we have a more complicated type class that modifies the type of individual elements in the `HList`. A typical encoding of such looks like this:
```scala
trait TypeClass[In] {
  type Out

  def apply(t: In): Out
}

object TypeClass {
  type Aux[In, Out0] = TypeClass[In] {type Out = Out0}

  implicit val hNilTypeClass: TypeClass[HNil] =
    new TypeClass[HNil] {
      type Out = HNil

      def apply(nil: HNil): Out =
        nil
    }

  implicit def hConsTypeClass[H, T <: HList, HOut, TOut <: HList](
    implicit headTC: TypeClass.Aux[H, HOut],
    tailTC: TypeClass.Aux[T, TOut]
  ): TypeClass[H :: T] =
    new TypeClass[H :: T] {
      type Out = HOut :: TOut

      def apply(list: H :: T): Out =
        headTC(list.head) :: tailTC(list.tail)
    }
}
```
If you compare this to the implementation we had for `Show`, you'll see that how we infer instances of the `TypeClass` for the `HNil` and `Cons` cases are almost exactly the same. However, we've added this `Aux` thing - why?

Effectively, `Aux` is a technique for lifting path-dependent types into a type parameter list. When we're using shapeless, we frequently need to compose multiple type classes together in an implicit parameter list, where the input type of one type class is the output type of another. As mentioned previously, parameters can't reference path-dependent types of other parameters in the same parameter list[^2], but the problem is we can only have a single implicit parameter list in Scala. `Aux` acts as an escape hatch, allowing us to move the path-dependent types into a method's type parameters, thereby allowing us to reference them all in a single parameter list.

You might then ask why we don't just abandon path-dependent types and define everything in the type parameter list from the start. The reason for this choice will hopefully become clearer later on, when we talk about the currying that we have to do as a result of having to use `Aux`.

### Polymorphic functions
Shapeless also offers a construct known as polymorphic functions, which look like this:
```scala
object concat extends Poly2 {
  implicit def char: Case.Aux[Char, Char, String] =
    at((c1, c2) => s"$c1$c2")
    
  implicit def string[T]: Case.Aux[String, T, String] =
    at((s, t) => s + t)
    
  implicit def int: Case.Aux[Int, Int, Int] =
    at((i, j) => s"$i$j".toInt)
}
```
Effectively, we've defined an object `concat`, which extends a trait `PolyN`, where `N` corresponds to the number of arguments we expect our function to be provided with. Then we define a number of implicit functions according to the possible arguments we want our function to be able to support. The effective type signature of the functions are described by the return type, `Case.Aux[A, B, C]`. The last type, `C`, corresponds to the return type, and the earlier types, `A` and `B` here, correspond to the types of our function's arguments. So with the implementation above, our overall `concat` function provides a way of transforming the following cases:
```text
(Char, Char) => String
(String, T) => String
(Int, Int) => Int
```
We can combine these functions with `HList`s and `Coproduct`s, thus giving is further flexibility in how we can act on `HList`s and `Coproduct`s in a type-safe manner. We'll see how to do this later.

[Next page](05-initialising-the-map.md)

[^1]: Generic instances are not provided for non-sealed traits, as the Scala compiler is only able to determine a complete list of subtypes for sealed traits.
[^2]: Scala 3 actually supports this, removing the need for the `Aux` pattern. 
