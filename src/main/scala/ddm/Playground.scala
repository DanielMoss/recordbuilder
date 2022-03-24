package ddm

import ddm.impl._

object Playground extends App {
  sealed trait EntityFields extends RecordBuilder.Key
  case object Field1 extends EntityFields { type Value = String }
  case object Field2 extends EntityFields { type Value = Int }

  val builder = RecordBuilder[EntityFields]()
  builder.set(Field1)("test")
}
