# #1 Defining the API

As discussed in the [README](../README.md), I was aware of shapeless `Record`s, but not `HMap`, so we're going to try constructing a `Record`. Constructing a `Record` requires a `Map` with all the fields known upfront. Now, remember that an arbitrary number of entities can be defined in a single parsed object through field versioning? Well that's a problem. It means we can't just index values in our `Map` according to the raw key we see. Instead we're going to need to have a way to update the values held in the `Map` so that we can track the differences between versions. Once we build our `Record` from our `Map`, it'll then be the responsibility of some other code to take the versioned values in the `Record` and convert them into appropriate entities.


So to recap, our `RecordBuilder` type wants:
- a method to set a field in the `Map` (for when we're parsing `fieldA = blah`)
- a method to modify a field in the `Map` (for when we're parsing `fieldA1 = blah`, followed by `fieldA2 = blahblah`)
- a method to turn out `Map` into a `Record` (which are typed as `HList`s in shapeless)
```scala
final class RecordBuilder(underlying: Map[Any, Any]) {
  def set[K, V](k: K, v: V): RecordBuilder =
    new RecordBuilder(underlying + (k -> v))

  def modify[K, V](k: K)(f: Option[V] => V): RecordBuilder =
    underlying
      .get(k)
      .asInstanceOf[Option[V]]
      .pipe(maybeV => set(k, f(maybeV)))

  def asRecord: HList =
    HNil
}
```
The code we've introduced has a lot of problems. Let's start addressing them.
