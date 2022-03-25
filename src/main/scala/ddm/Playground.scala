package ddm

import ddm.impl._
import shapeless.ops.coproduct.LiftAll
import shapeless.ops.hlist.{Mapper, ToTraversable}
import shapeless.{Coproduct, Generic, HList, Poly1}

object Playground extends App {
  sealed trait EntityFields extends RecordBuilder.Key
  case object Field1 extends EntityFields { type Value = String }
  case object Field2 extends EntityFields { type Value = Int }

  println(implicitly[ValueOf[Field1.type]].value)
  println(valueOf[Field1.type])

  val generic = Generic[EntityFields]
  val lifted = LiftAll[ValueOf, generic.Repr]

  println(implicitly[ValueOf[Field1.type]])
  println(implicitly[ValueOf[Field2.type]])
  println(lifted.instances)

  def summon[GenRepr <: Coproduct](
    implicit generic: Generic.Aux[EntityFields, GenRepr],
    lifted: LiftAll[ValueOf, GenRepr]
  ): lifted.Out =
    lifted.instances

  println(summon)

  def summon2[
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

  println(summon2)

  val builder = RecordBuilder[EntityFields]()
  println(builder)
  println(builder.set(Field2)(3))
}
