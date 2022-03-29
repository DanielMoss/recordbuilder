package ddm

import ddm.impl._

object Playground extends App {
  sealed trait EntityFields extends RecordBuilder.Key
  case object Field1 extends EntityFields { type Value = String }
  case object Field2 extends EntityFields { type Value = Int }

  import MapOps._
  val map: Map[EntityFields, _] = Map(Field1 -> Option("hi"), Field2 -> None)
  println(map.asRecordF[Option]())
}
