package rpm4s.data

sealed trait HeaderType extends Product with Serializable {
  def byteAlignment: Int = 1
  def bitAlignment: Int = byteAlignment * 8
}
object HeaderType {
  case object Null extends HeaderType
  case object Char extends HeaderType
  case object Int8 extends HeaderType
  case object Int16 extends HeaderType {
    override val byteAlignment = 2
  }
  case object Int32 extends HeaderType {
    override val byteAlignment = 4
  }
  case object Int64 extends HeaderType {
    override val byteAlignment = 8
  }
  case object String extends HeaderType
  case object Bin extends HeaderType
  case object StringArray extends HeaderType
  case object I18NString extends HeaderType
}
