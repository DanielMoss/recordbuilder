package ddm.impl

import shapeless.{HList, HNil}

import scala.util.chaining.scalaUtilChainingOps

object RecordBuilder {
  def apply(): RecordBuilder =
    new RecordBuilder(Map.empty)
}

final class RecordBuilder private (underlying: Map[Any, Any]) {
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
