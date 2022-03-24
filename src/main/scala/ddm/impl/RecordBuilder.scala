package ddm.impl

import shapeless.{HList, HNil}

import scala.util.chaining.scalaUtilChainingOps

object RecordBuilder {
  def apply[K <: Key](): RecordBuilder[K] =
    new RecordBuilder(Map.empty)

  trait Key {
    type Value
  }
}

final class RecordBuilder[K <: RecordBuilder.Key] private (underlying: Map[K, Any]) {
  def set(k: K)(v: k.Value): RecordBuilder[K] =
    new RecordBuilder(underlying + (k -> v))

  def modify(k: K)(f: Option[k.Value] => k.Value): RecordBuilder[K] =
    underlying
      .get(k)
      .asInstanceOf[Option[k.Value]]
      .pipe(maybeV => set(k)(f(maybeV)))

  def asRecord: HList =
    HNil
}
