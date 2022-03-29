package ddm.impl

import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil}

sealed trait LinkedLiftAll[F[_], In <: Coproduct] {
  type Out <: HList
  def instances: Out
}

object LinkedLiftAll {
  type Aux[F[_], In <: Coproduct, Out0 <: HList] =
    LinkedLiftAll[F, In] { type Out = Out0 }

  implicit def cnil[F[_]]: LinkedLiftAll.Aux[F, CNil, HNil] =
    new LinkedLiftAll[F, CNil] {
      type Out = HNil
      val instances: Out = HNil
    }

  implicit def ccons[F[_], HIn, HOut, T <: Coproduct, TI <: HList](
    implicit typeLink: TypeLink[HIn, HOut],
    headInstance: F[HOut],
    tailInstances: Aux[F, T, TI]
  ): LinkedLiftAll.Aux[F, HIn :+: T, F[HOut] :: TI] =
    new LinkedLiftAll[F, HIn :+: T] {
      type Out = F[HOut] :: TI
      val instances: Out = headInstance :: tailInstances.instances
    }
}
