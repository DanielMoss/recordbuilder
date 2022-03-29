# #7 Producing a record

[Previous page](06-understanding-frommap.md)

We've got a better understanding of what we need, so we'll start putting the pieces together. We'll encapsulate our functionality in an extension method on arbitrary maps.
```scala
import shapeless.{Coproduct, Witness}
import shapeless.ops.coproduct.LiftAll
object MapOps {
  implicit final class RichMap[T](val self: Map[T, _]) extends AnyVal {
    def asRecord: Curried[T] =
      new Curried[T](self)
  }
  
  final class Curried[T](val self: Map[T, _]) extends AnyVal {
    def apply[
      TRepr <: Coproduct,
      WitnessesRepr <: HList,
      TypeablesRepr <: HList
    ]()(
      implicit gen: Generic.Aux[T, TRepr],
      witnesses: LiftAll.Aux[Witness.Aux, TRepr, WitnessesRepr],
      typeables: LiftAll.Aux[Typeable, ?, TypeablesRepr]
    ) = 
      ???
  }
}
```
Things were going great until we got to summoning the `Typeable` instances. We're dealing with an arbitrary type `T` for the keys for which we have the `Coproduct` representation `TRepr`, but how do we get the `Typeable`s for the values? The problem is we don't have any way of representing the link between an arbitrary key and its value at the type level right now.

What we could do is define a polymorphic function that maps from subtypes of `Key` to a typeable instance of the associated `Value`, and then restrict `T` to be a subtype of `Key`. We'd then summon a `Mapper` instance for our polymorphic function. i.e.
```scala
implicit final class RichMap[T <: RecordBuilder.Key](val self: Map[T, _]) extends AnyVal {
  def asRecord: Curried[T] =
    new Curried[T](self)
}

final class Curried[T <: RecordBuilder.Key](val self: Map[T, _]) extends AnyVal {
  def apply[
    TRepr <: Coproduct,
    WitnessesRepr <: HList,
    TypeablesRepr <: HList
  ]()(
    implicit gen: Generic.Aux[T, TRepr],
    witnesses: LiftAll.Aux[Witness.Aux, TRepr, WitnessesRepr],
    toTypeables: Mapper.Aux[toValueTypeable.type, WitnessesRepr, TypeablesRepr]
  ): TypeablesRepr =
    witnesses.instances.map(toValueTypeable)
}

object toValueTypeable extends Poly1 {
  implicit def default[T <: RecordBuilder.Key](
    implicit typeable: Typeable[T#Value]
  ): Case.Aux[Witness.Aux[T], Typeable[T#Value]] =
    at(_ => typeable)
}
```
If you'd like, you can run the following code to verify that this works:
```scala
val map: Map[EntityFields, _] = Map(Field1 -> "", Field2 -> 2)
println(map.asRecord()) // Prints Typeable[String] :: Typeable[Int] :: HNil
```
The downside of this approach is that we've tied our method to only working on subtypes of `RecordBuilder.Key`, but there's another way. Instead, let's take a look at how instances of `LiftAll` are constructed by shapeless.
```scala
implicit def liftAllCnil[F[_]]: LiftAll.Aux[F, CNil, HNil] = new LiftAll[F, CNil] {
  type Out = HNil
  def instances = HNil
}

implicit def liftAllCcons[F[_], H, T <: Coproduct, TI <: HList]
(implicit headInstance: F[H], tailInstances: Aux[F, T, TI]): Aux[F, H :+: T, F[H] :: TI] =
  new LiftAll[F, H :+: T] {
    type Out = F[H] :: TI
    def instances = headInstance :: tailInstances.instances
  }
```
There's a fair few types being thrown around, but it's pretty straightforward overall. We're recursing through our `Coproduct` maintaining both a head, `H`, and a tail, `T`. We're summoning an instance of our type class, `F`, for the head, and then appending it to a `HList`, `TI`, which acts as our accumulator. 

What we need is a `LiftAll` that summons an instance of `F` not for `H`, but for a type linked to `H`. Let's try and build our own `LiftAll`. First we're going to need a way to link two types together. That's pretty straightforward.
```scala
trait TypeLink[A, B]
```
And we'll add an instance of `TypeLink` to our main `Key` trait, so that we automatically have correct instances of `TypeLink` available without needing to add any extra boilerplate to our implementations.
```scala
trait Key {
  type Value
  
  implicit final lazy val typeLink: TypeLink[this.type, Value] =
    new TypeLink[this.type, Value] {}
}
```
Now that we've got that, making our owned `LiftAll` is easy:
```scala
sealed trait LinkedLiftAll[F[_], In <: Coproduct] {
  type Out <: HList
  def instances: Out
}

object LinkedLiftAll {
  type Aux[F[_], In <: Coproduct, Out0 <: HList] =
    LinkedLiftAll[F, In] { type Out = Out0 }

  implicit def cnil[F[_]]: LinkedLiftAll.Aux[F, CNil, HNil] =
    new LinkedLiftAll[F, CNil] {
      type Out = HNil
      val instances: Out = HNil
    }

  implicit def ccons[F[_], HIn, HOut, T <: Coproduct, TI <: HList](
    implicit typeLink: TypeLink[HIn, HOut],
    headInstance: F[HOut],
    tailInstances: Aux[F, T, TI]
  ): LinkedLiftAll.Aux[F, HIn :+: T, F[HOut] :: TI] =
    new LinkedLiftAll[F, HIn :+: T] {
      type Out = F[HOut] :: TI
      val instances: Out = headInstance :: tailInstances.instances
    }
}
```
We'll now plug this new `LinkedLiftAll` into our `asRecord` function.
```scala
def apply[
  TRepr <: Coproduct,
  WitnessesRepr <: HList,
  TypeablesRepr <: HList,
  Record <: HList
]()(
  implicit gen: Generic.Aux[T, TRepr],
  witnesses: LiftAll.Aux[Witness.Aux, TRepr, WitnessesRepr],
  typeables: LinkedLiftAll.Aux[Typeable, TRepr, TypeablesRepr]
): TypeablesRepr =
  typeables.instances
```
This returns the same thing as we had earlier, but without needing to force our type `T` to be a `Key`. The eagle-eyed might spot that we're not done yet though. For a key `k`, we're actually finding a `Typeable[k.Value]`, when what we really need is a `Typeable[Option[k.Value]]`. Remember that we stored all the values in our `Map` as `Option`s. Therefore we need to change what we're summoning with `LinkedLiftAll`.

But how do we do that? If we just type
```scala
typeables: LinkedLiftAll.Aux[Typeable[Option], TRepr, TypeablesRepr]
```
then we'll get a compilation error. That's because we can't pass a type constructor (`Option` without a type parameter) as an argument to another type constructor (`Typeable`). It's similar to how, if you were mapping a regular value, you could write
```scala
Option(x).flatMap(Option)
```
as the compiler will expand this to
```scala
Option(x).flatMap(x => Option.apply(x))
```
but you wouldn't be able to write
```scala
Option(x).flatMap(Option(Option))
```
To work around this, we're going to need the [kind-projector](https://github.com/typelevel/kind-projector) plugin[^1]. Skipping over the details, the plugin allows us to write type-level lambdas, much like we can do at the value level. The syntax we'll use for this is rather simple:
```scala
typeables: LinkedLiftAll.Aux[Lambda[A => Typeable[Option[A]]], TRepr, TypeablesRepr]
```
This works perfectly, which means we have all the `Witness` and `Typeable` instances we need in order to produce our `FromMap`! As mentioned, we'll do this by folding over the type classes we just summoned, and we'll need to zip the two lists of type classes together first. We'll skip over the individual steps here, as we'd be treading ground we've largely covered previously. Instead, here's the result:
```scala
def apply[
  TRepr <: Coproduct,
  WitnessesRepr <: HList,
  TypeablesRepr <: HList,
  ZippedRepr <: HList
]()(
  implicit gen: Generic.Aux[T, TRepr],
  witnesses: LiftAll.Aux[Witness.Aux, TRepr, WitnessesRepr],
  typeables: LinkedLiftAll.Aux[Lambda[A => Typeable[Option[A]]], TRepr, TypeablesRepr],
  zipper: Zip.Aux[WitnessesRepr :: TypeablesRepr :: HNil, ZippedRepr],
  folder: LeftFolder[ZippedRepr, FromMap[HNil], fromMapBuilder.type]
) =
  zipper(witnesses.instances :: typeables.instances :: HNil)
    .foldLeft(FromMap.hnilFromMap)(fromMapBuilder)
```
This is great, but we're still not done. We know we've built a `FromMap`, but the compiler doesn't. At least, not yet in the context of the `apply` method. We'll use one last trick to sort that out.
```scala
def apply[
      TRepr <: Coproduct,
      WitnessesRepr <: HList,
      TypeablesRepr <: HList,
      ZippedRepr <: HList,
      FoldedRepr,
      Record <: HList
    ]()(
      implicit gen: Generic.Aux[T, TRepr],
      witnesses: LiftAll.Aux[Witness.Aux, TRepr, WitnessesRepr],
      typeables: LinkedLiftAll.Aux[Lambda[A => Typeable[Option[A]]], TRepr, TypeablesRepr],
      zipper: Zip.Aux[WitnessesRepr :: TypeablesRepr :: HNil, ZippedRepr],
      folder: LeftFolder.Aux[ZippedRepr, FromMap[HNil], fromMapBuilder.type, FoldedRepr],
      ev: FoldedRepr =:= FromMap[Record]
    ): Option[Record] =
      zipper(witnesses.instances :: typeables.instances :: HNil)
        .foldLeft(FromMap.hnilFromMap)(fromMapBuilder)
        .apply(self)
```
We've made use of `=:=` to prove that the output type of `FoldedRepr` is equal to a `FromMap` for a particular type of `HList`, which we'll let the compiler find out for us. We've finally got a function which can be used to turn our `Map` into a `Record`. The only abstraction work left to do is to allow a client of this code to choose whether they want their `Typeable`s to be for `Option`s or not, or any other kind of monad. We can do this by accepting an arbitrary type constructor in our `Curried` class:
```scala
implicit final class RichMap[T <: RecordBuilder.Key](val self: Map[T, _]) extends AnyVal {
  def asRecord: Curried[Id, T] =
    new Curried[Id, T](self)
      
  def asRecordF[F[_]]: Curried[F, T] =
    new Curried[F, T](self)
}

final class Curried[F[_], T](val self: Map[T, _]) extends AnyVal {
  // and then below
  typeables: MappedLiftAll.Aux[Lambda[A => Typeable[F[A]]], TRepr, TypeablesRepr],
```
And we're all wrapped up. We can try out our new method in the [playground](../src/main/scala/ddm/Playground.scala):
```scala
val map: Map[EntityFields, _] = Map(Field1 -> Option("hi"), Field2 -> None)
println(map.asRecordF[Option]()) // Prints Some(None :: Some(hi) :: HNil)
```

[^1]: As an aside, this plugin has my favourite [`README` opener](https://github.com/typelevel/kind-projector#dedication) 
