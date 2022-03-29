package ddm.impl

import shapeless.labelled.FieldType
import shapeless.ops.coproduct.LiftAll
import shapeless.ops.hlist.{LeftFolder, Zip}
import shapeless.ops.maps.FromMap
import shapeless.{::, Coproduct, Generic, HList, HNil, Id, Poly2, Typeable, Witness}

object MapOps {
  implicit final class RichMap[T <: RecordBuilder.Key](val self: Map[T, _]) extends AnyVal {
    def asRecord: Curried[Id, T] =
      new Curried[Id, T](self)

    def asRecordF[F[_]]: Curried[F, T] =
      new Curried[F, T](self)
  }

  final class Curried[F[_], T](val self: Map[T, _]) extends AnyVal {
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
      typeables: LinkedLiftAll.Aux[Lambda[A => Typeable[F[A]]], TRepr, TypeablesRepr],
      zipper: Zip.Aux[WitnessesRepr :: TypeablesRepr :: HNil, ZippedRepr],
      folder: LeftFolder.Aux[ZippedRepr, FromMap[HNil], fromMapBuilder.type, FoldedRepr],
      ev: FoldedRepr =:= FromMap[Record]
    ): Option[Record] =
      zipper(witnesses.instances :: typeables.instances :: HNil)
        .foldLeft(FromMap.hnilFromMap)(fromMapBuilder)
        .apply(self)
  }

  object fromMapBuilder extends Poly2 {
    implicit def default[K, V, T <: HList]: Case.Aux[
      FromMap[T],
      (Witness.Aux[K], Typeable[V]),
      FromMap[FieldType[K, V] :: T]
    ] =
      at { case (fmt, (wk, tv)) => FromMap.hlistFromMap(wk, tv, fmt) }
  }
}
