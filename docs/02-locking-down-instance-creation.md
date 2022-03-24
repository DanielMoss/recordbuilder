# #2 Locking down instance creation

[Previous page](01-defining-the-api.md)

Since `Record` creation needs all expected fields to exist in the `Map`, we're definitely going to need to privatise the `RecordBuilder` constructor. To not do so would mean that `asRecord` would have to return an `Option` instead.
```scala
object RecordBuilder {
  def apply(): RecordBuilder =
    new RecordBuilder(Map.empty)
}

final class RecordBuilder private (underlying: Map[Any, Any]) {
```
For now, we'll just use `Map.empty` as the initial value, but we'll come back here later on and populate initial values.
