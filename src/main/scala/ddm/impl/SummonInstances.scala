package ddm.impl

import shapeless.ops.coproduct.LiftAll
import shapeless.ops.hlist.{Mapper, ToTraversable}
import shapeless.{Coproduct, Generic, HList, Poly1}

object SummonInstances {
  def apply[T]: Curried[T] =
    new Curried[T]

  final class Curried[T](val dummy: Boolean = true) extends AnyVal {
    def apply[
      TRepr <: Coproduct,
      ValueOfsRepr <: HList,
      MappersRepr <: HList
    ]()(
      implicit gen: Generic.Aux[T, TRepr],
      lift: LiftAll.Aux[ValueOf, TRepr, ValueOfsRepr],
      mapper: Mapper.Aux[toValue.type, ValueOfsRepr, MappersRepr],
      toTraversable: ToTraversable.Aux[MappersRepr, List, T]
    ): List[T] =
      lift.instances.map(toValue).toList
  }

  object toValue extends Poly1 {
    implicit def default[T]: Case.Aux[ValueOf[T], T] =
      at(_.value)
  }
}
