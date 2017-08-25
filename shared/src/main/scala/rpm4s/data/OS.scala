package rpm4s.data

sealed trait OS extends Product with Serializable
object OS {
  case object Linux extends OS
}
