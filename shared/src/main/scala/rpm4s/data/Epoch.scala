package rpm4s.data

import rpm4s.codecs.ConvertingError

case class Epoch private (value: Int) {
  def copy(value: Int = value): Nothing = ???
}
object Epoch {

  def fromString(value: String): Either[ConvertingError, Epoch] = {
    try {
      fromInt(value.toInt)
    } catch {
      case _: NumberFormatException => Left(ConvertingError(s"$value is not a valid epoch number"))
    }
  }

  def fromInt(value: Int): Either[ConvertingError, Epoch] = {
    if (value >= 1)
      Right(new Epoch(value))
    else Left(ConvertingError(s"epoch $value must be >= 1"))
  }
}
