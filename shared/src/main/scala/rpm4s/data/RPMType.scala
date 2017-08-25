package rpm4s.data

sealed trait RPMType extends Product with Serializable
object RPMType {
  case object Binary extends RPMType
  case object Source extends RPMType
}
