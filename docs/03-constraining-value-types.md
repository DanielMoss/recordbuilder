# #3 Constraining value types

[Previous page](02-locking-down-instance-creation.md)

Next we're going to take a look at the values we can insert into our `Map`. We need some way of determining what a valid value is based on the key, which means that our keys must also be distinct types[^1]. Let's introduce a `Key` type in the `RecordBuilder` companion.
```scala
trait Key {
  type Value
}
```
We'll then parameterise `RecordBuilder` by a subtype `K` of `Key`.
```scala
final class RecordBuilder[K <: RecordBuilder.Key] private (underlying: Map[K, Any]) {
  def set(k: K)(v: k.Value): RecordBuilder[K] =
    new RecordBuilder(underlying + (k -> v))

  def modify(k: K)(f: Option[k.Value] => k.Value): RecordBuilder[K] =
    underlying
      .get(k)
      .asInstanceOf[Option[k.Value]]
      .pipe(maybeV => set(k)(f(maybeV)))
```
You might have been wondering why we defined the `Value` type on `Key` as a path-dependent type, rather than as a type argument. The reason for doing so can be seen from the new type signature of the `set` method. We've been able to completely remove type parameters from the method itself! Had we gone with a `trait Key[Value]` approach, then the type signature of the `set` method would have had to have been
```scala
def set[V, K1 <: K[V]](k: K1, v: V): RecordBuilder[K[_]]
```
Handling of the `Key` type would also have been a bit messier in the code, due to having to specify the type arguments everywhere. Path-dependent types allow us to hide that information.

A downside is that we've had to split off the `v` parameter to a second argument list, which is a due to a compilation limitation. You can't reference path-dependent types on parameters defined within the same argument list. 

If you're interested, take a look at the [playground](../src/main/scala/ddm/Playground.scala), where an instance of `RecordBuilder` has been constructed.
```scala
sealed trait EntityFields extends RecordBuilder.Key
case object Field1 extends EntityFields { type Value = String }
case object Field2 extends EntityFields { type Value = Int }

val builder = RecordBuilder[EntityFields]()
builder.set(Field1)("test")
```
This code passes compilation, but if we were to change the last line to `builder.set(Field2)("test")`, then we'll get this error:
```text
[error] recordbuilder\src\main\scala\ddm\Playground.scala:11:23: type mismatch;
[error]  found   : String("test")
[error]  required: ddm.Playground.Field2.Value
[error]     (which expands to)  Int
[error]   builder.set(Field2)("test")
[error]                       ^
```

[Next page](04-populating-initial-keys.md)

[^1]: In theory we could use [literal types](https://docs.scala-lang.org/sips/42.type.html), but type inference can get a little confusing when you do that.
