package ddm.impl

import shapeless.ops.coproduct.LiftAll
import shapeless.ops.hlist.{Mapper, ToTraversable}
import shapeless.{Coproduct, Generic, HList, HNil}

import scala.util.chaining.scalaUtilChainingOps

object RecordBuilder {
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
      new RecordBuilder(SummonInstances[K]().map(_ -> None).toMap)
  }

  trait Key {
    type Value

    implicit final lazy val typeLink: TypeLink[this.type, Value] =
      new TypeLink[this.type, Value] {}
  }
}

final class RecordBuilder[K <: RecordBuilder.Key] private (underlying: Map[K, Option[_]]) {
  def set(k: K)(v: k.Value): RecordBuilder[K] =
    new RecordBuilder(underlying + (k -> Some(v)))

  def modify(k: K)(f: Option[k.Value] => k.Value): RecordBuilder[K] =
    underlying
      .get(k)
      .asInstanceOf[Option[k.Value]]
      .pipe(maybeV => set(k)(f(maybeV)))

  def asRecord: HList =
    HNil

  override def toString: String =
    s"RecordBuilder(${underlying.mkString(", ")})"
}
