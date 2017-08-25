package rpm4s.data

sealed trait Distribution extends Product with Serializable
object Distribution {
  def fromString(value: String): Distribution = value match {
    case "openSUSE Leap 42.3" => OpenSUSELeap42_3
    case x                    => Unknown(x)
  }
  case object OpenSUSELeap42_3 extends Distribution
  case class Unknown(value: String) extends Distribution
}
