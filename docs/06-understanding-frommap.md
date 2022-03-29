# #6 Understanding fromMap

[Previous page](05-initialising-the-map.md)

Let's move on to the last big thing - building the `Record` from our `Map`. Shapeless provides the following type for doing that:
```scala
trait FromMap[R <: HList] {
  def apply[K, V](m: Map[K, V]): Option[R]
}

object FromMap {
  def apply[R <: HList](implicit fm: FromMap[R]) = fm

  implicit def hnilFromMap[T]: FromMap[HNil] =
    new FromMap[HNil] {
      def apply[K, V](m: Map[K, V]): Option[HNil] = Some(HNil)
    }
  
  implicit def hlistFromMap[K0, V0, T <: HList]
    (implicit wk: Witness.Aux[K0], tv: Typeable[V0], fmt: FromMap[T]): FromMap[FieldType[K0, V0] :: T] =
      new FromMap[FieldType[K0, V0] :: T] {
        def apply[K, V](m: Map[K, V]): Option[FieldType[K0, V0] :: T] = {
          for {
            value <- m.get(wk.value.asInstanceOf[K])
            typed <- tv.cast(value)
            rest <- fmt(m)
          } yield field[K0](typed) :: rest
        }
      }
}
```
Alright, so we've got this `FromMap` type with an `apply` method that does exactly what we're looking for. Seeing this, we might then say, "cool, we can ask for an implicit `FromMap` instance in our `asRecord` method and be done."
```scala
def asRecord[Record <: HList](implicit fm: FromMap[Record]): Record =
  fm(underlying).get
```
Indeed, this compiles, and we're not throwing an exception from the `get` call either... but the result of the method will always be `HNil` - not a `Record` with all our keys and values in. Why?

The problem is that we're not going to be providing a type parameter to `asRecord` ourselves, because we don't want to have to manually work out exactly what the `HList` representation of the `Record` will be. Thus, the compiler is free to look for any `FromMap[_ <: HList]`, and the simplest one it can find is the `hnilFromMap` implementation above. Oops. Unfortunately we're going to have to build the appropriate `FromMap` ourselves. 

We can see that shapeless builds instances of this type using the same recursive tricks we've seen earlier, starting from the `HNil` case. We know our end `Record` won't be empty, so we'll need to provide the compiler with all the types it needs for each step of the `hlistFromMap` resolver. Let's take a look at the method again, after substituting `HNil` for the `T` type parameter.
```scala
implicit def hlistFromMap[K0, V0]
  (implicit wk: Witness.Aux[K0], tv: Typeable[V0]): FromMap[FieldType[K0, V0] :: HNil] =
    new FromMap[FieldType[K0, V0] :: HNil] {
      def apply[K, V](m: Map[K, V]): Option[FieldType[K0, V0] :: HNil] = {
        for {
          value <- m.get(wk.value.asInstanceOf[K])
          typed <- tv.cast(value)
        } yield field[K0](typed) :: HNil
      }
    }
```
Still pretty complicated. We know that whatever happens, we're going to want `FromMap` to produce a `Some[Record]` at the end, so let's consider how the `Map` we pass through to the `apply` method gets used.
```scala
value <- m.get(wk.value.asInstanceOf[K])
typed <- tv.cast(value)
```
This gives us some hints about what's going on. We know that `wk.value.asInstanceOf[K]` has to be one of the keys in our `Map`. The `Witness` type looks like this:
```scala
/** Provides the value corresponding to a singleton type.
 *
 * See SIP-23 for a related proposed language change.
 */
trait Witness {
  type T
  val value: T
}
```
I haven't included the companion object in this case because the instances of this type are produced via macros, which are more complicated than I want to get into. We get a hint of what `Witness` corresponds to from the comment though. If you get déjà vu when looking at this, it might be because [`Witness` is the predecessor of the `ValueOf` type class](https://gitter.im/fthomas/refined?at=575dd4b4e9fbf4267bf36a7d) introduced to the standard library in Scala 2.13. Shapeless still uses `Witness` internally in order to provide Scala 2.12 support. Anyway, using this knowledge we can be reasonably sure that we'll have instances of `Witness.Aux[K0]` available, because we made use of `ValueOf[K0]`s when populating our initial `Map`. We'll just have to summon the `Witness` instances in much the same way we did for `ValueOf`.

Going back to the for-comprehension, and we know that `tv.cast(value)` has to be able to produce a `Some` based on the value associated with that key. `Typeable` looks like this:
```scala
trait Typeable[T] {
  def cast(t: Any): Option[T]
}
```
Based on the signature and method name, we can reasonably assume that the `Typeable` type is about providing the ability to cast arbitrary values to a corresponding `Type`, providing a `Some[T]` if that's possible, and a `None` if not. We're not going to go into how this works, but the reason shapeless has this type is because of type erasure in the JVM, causing types like `List[Int]` to have the same runtime type as `List[String]`. If you're interested in this stuff, you might first want to read [this question/answer on stackoverflow](https://stackoverflow.com/questions/12218641/what-is-a-typetag-and-how-do-i-use-it), which describes `TypeTag`s, `ClassTag`s, and `WeakTypeTag`s from the standard library. Shapeless' `Typeable` is broadly similar to a `TypeTag` in terms of when you might have access to one. Here's how they'd both allow you to pattern match on a `List[T]`:
```scala
def method1[T : TypeTag](l: List[T]) =
  typeOf[T] match {
    case t if t =:= typeOf[Boolean] => 
      val castL = l.asInstanceOf[List[Boolean]]
      ???
    case _ => ???
  }
  
def method2[T : Typeable](l: List[T]) =
  Typeable[List[T]].cast(l) match {
    case Some(castL) => ???
    case None => ???
  }
```
You won't always have an instance of `Typeable` available for a type, but it's normally pretty straightforward to implement such instances yourself if you need to. There's plenty of examples in the `Typeable` companion object to get ideas from.

Returning to our for-comprehension and `typed <- tv.cast(value)`, we now know that the `V` type in `Typeable[V]` should correspond to the type of value we expect to be stored against our key (i.e. `k.Value`, if `k` is our key). We've got enough information now to get through the comprehension. If you're worried about the `FieldType[K, V]` type and what it represents, it's just how shapeless encodes the individual fields in our final `Record`. We don't need to dig into this very much, except for knowing that it's the result type of combining each individual key-value pair.

Putting it all together, we know we're going to need
- a `Witness.Aux[K]` for each key, which we know we have because we had a `ValueOf[K]`
- a `Typeable[k.V]` for each value, which we'll assume exists

Once we've got a list of instances of these types for all key-value pairs in our `Map`, we should be able to fold over that list to create an appropriate `FromMap` instance, where the zeroth case is covered by `hnilFromMap`.
