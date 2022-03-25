# #5 Initialising the map

[Previous page](04-a-shapeless-detour.md)

Now that we've refreshed on some shapeless basics, let's take a look at the initial state of `RecordBuilder`'s `Map`. As discussed, in order to create a `Record` we need all the keys the `Record` expects to already be present in the `Map`. Something we can take advantage of is that we expect all of our `Key` implementations to be singletons, since they represent unchanging fields. The `ValueOf` type class was introduced to the standard library in Scala 2.13, which allows us to summon the instance of a singleton type. For example:
```scala
case object A
val a = implicitly[ValueOf[A.type]].value
// this is equivalent to
val a = A
```
We can combine this with a type called `LiftAll` from the shapeless library, which gives us the ability to summon all the instances of a type class (assuming they all exist) for a given `HList` or `Coproduct`'s members. Let's take a look at how this works in the [playground](../src/main/scala/ddm/Playground.scala):
```scala
val generic = Generic[EntityFields]
val lifted = LiftAll[ValueOf, generic.Repr]

println(implicitly[ValueOf[Field1.type]]) // Prints scala.ValueOf@7d6a6a97
println(implicitly[ValueOf[Field2.type]]) // Prints scala.ValueOf@7d6a6a98
println(lifted.instances)                 // Prints scala.ValueOf@7d6a6a97 :: scala.ValueOf@7d6a6a98 :: HNil
```
So first, we summon the `Generic` instance for our sealed trait. We then pass the `Coproduct` representation of our trait, along with the type class we want to summon` to `LiftAll`, which will recursively build up a `HList` of the type class instances using the same methods we described earlier. If we wanted to do this in a method, we'd write
```scala
def summon[GenRepr <: Coproduct](
  implicit generic: Generic.Aux[EntityFields, GenRepr],
  lifted: LiftAll[ValueOf, GenRepr]
): lifted.Out =
  lifted.instances
```
We can call `summon` without actually knowing what `GenRepr` is, as the compiler is able to derive that type for us given how we've written out our implicit arguments. You can see we've taken advantage of `Generic.Aux` to work around the path-dependent type issue. If we print the output of `summon`, we'll see the same `scala.ValueOf@7d6a6a97 :: scala.ValueOf@7d6a6a98 :: HNil` as we saw earlier. We'll now go further with this pattern, and introduce two more shapeless types.
```scala
def summon[
  GenRepr <: Coproduct,
  LiftedRepr <: HList,
  MappedRepr <: HList
](
  implicit generic: Generic.Aux[EntityFields, GenRepr],
  lifted: LiftAll.Aux[ValueOf, GenRepr, LiftedRepr],
  mapper: Mapper.Aux[toValue.type, LiftedRepr, MappedRepr],
  toTraversable: ToTraversable.Aux[MappedRepr, List, EntityFields]
): List[EntityFields] =
  lifted.instances.map(toValue).toList

object toValue extends Poly1 {
  implicit def default[T]: Case.Aux[ValueOf[T], T] =
    at(_.value)
}
```
`Mapper` is a type that provides evidence for running the provided polymorphic function, `toValue`, on the `HList` represented by `LiftedRepr`. Since we know that all entries we get back from `lifted.instances` are instances of `ValueOf`, we can define a generic case in our `toValue` polymorphic function that handles arbitrary `ValueOf` instances, and rips out the singleton instance of the type they represent. Therefore, we have a way of going from a `HList` of `ValueOf`s, to a `HList` of the subtypes of `EntityFields`. The final piece of the shapeless part of the puzzle is to convert our `HList` back into a regular `List[EntityFields]`. We do this through the `ToTraversable` type (whose last parameter corresponds to the least upper bound of the types in our `HList`, which we know to be `EntityFields`).

The next natural step is to make `summon` work for arbitrary sealed traits. We can do this easily right? Just promote `EntityFields` in the sample above to a type parameter?
```scala
def summon[
  T,
  GenRepr <: Coproduct,
  LiftedRepr <: HList,
  MappedRepr <: HList
](
  implicit generic: Generic.Aux[T, GenRepr],
  lifted: LiftAll.Aux[ValueOf, GenRepr, LiftedRepr],
  mapper: Mapper.Aux[toValue.type, LiftedRepr, MappedRepr],
  toTraversable: ToTraversable.Aux[MappedRepr, List, T]
): List[EntityFields] =
  lifted.instances.map(toValue).toList
]
```
At first glance, this is fine - everything compiles. But how do we actually call our method now?
```scala
summon[EntityFields, ?, ?, ?]
```
This is the problem with not being able to use path-dependent types within a single implicit parameter list. If we could do that, then we could trim the spurious type parameters out of our function definition and simple use `summon[EntityFields]`. As it stands, a user of our function would have to know exactly what all the type parameters should expand to, and write them all out. That's far too large a burden to place on the user.

Fortunately, there's a trick we can use to get around this. Let's take a look:
```scala
def summon[T]: Curried[T] =
  new Curried[T]
  
final class Curried[T] {
  def apply[
    GenRepr <: Coproduct,
    LiftedRepr <: HList,
    MappedRepr <: HList
  ]()(
    implicit generic: Generic.Aux[T, GenRepr],
    lifted: LiftAll.Aux[ValueOf, GenRepr, LiftedRepr],
    mapper: Mapper.Aux[toValue.type, LiftedRepr, MappedRepr],
    toTraversable: ToTraversable.Aux[MappedRepr, List, T]
  ): List[EntityFields] =
    lifted.instances.map(toValue).toList
}
```
We're now able to write `summon[EntityFields]()` and have it produce `List(Field1, Field2)`! We've split the type parameter list in two through the use of an intermediary class, where the first type parameter list is something that the user of our code can fill in, and then the Scala compiler can populate the second type parameter list itself. You'll sometimes see Scala libraries write out `Curried` as
```scala
final class Curried[T](val dummy: Boolean = true) extends AnyVal {
```
This is a performance trick to remove the runtime allocation of the intermediary `Curried` class, which is made possible by extending the `AnyVal` type.

We'll throw this all together into a `SummonInstances` object, and then use the functionality in order to fetch all the implementations of our `Key` type. Here's our new `RecordBuilder` constructor:
```scala
def apply[K <: RecordBuilder.Key]: Curried[K] =
  new Curried[K]

final class Curried[K <: RecordBuilder.Key](val dummy: Boolean = true) extends AnyVal {
  def apply[
    KRepr <: Coproduct,
    ValueOfsRepr <: HList,
    MapperRepr <: HList
  ]()(
    implicit gen: Generic.Aux[K, KRepr],
    lift: LiftAll.Aux[ValueOf, KRepr, ValueOfsRepr],
    mapper: Mapper.Aux[SummonInstances.toValue.type, ValueOfsRepr, MapperRepr],
    toTraversable: ToTraversable.Aux[MapperRepr, List, K]
  ): RecordBuilder[K] =
    new RecordBuilder(SummonInstances[K]().map(_ -> ???).toMap)
}
```
We've had to use the same currying trick as we did for `SummonInstances`, for exactly the same reasons. Our last step is a straightforward one. We need something to store in the `Map` against each key. For this, we're going to lift the values in our `Map` into `Option`, such that we can initialise all keys to `None`. Let's take a look at how things are working:
```scala
val builder = RecordBuilder[EntityFields]()
println(builder)                // Prints RecordBuilder(Field1 -> None, Field2 -> None)
println(builder.set(Field2)(3)) // Prints RecordBuilder(Field1 -> None, Field2 -> Some(3))
```
Awesome!
