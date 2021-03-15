package rpm4s.data

import rpm4s.codecs.ConvertingError

case class Epoch private (value: Int) {
  def copy(value: Int = value): Either[ConvertingError, Epoch] =
    Epoch(value)

}
object Epoch {

  val ZERO = new Epoch(0)

  implicit val ordering: Ordering[Epoch] = Ordering.by(_.value)

  def apply(value: Int): Either[ConvertingError, Epoch] = fromInt(value)

  def fromString(value: String): Either[ConvertingError, Epoch] = {
    try {
      fromInt(value.toInt)
    } catch {
      case _: NumberFormatException => Left(ConvertingError(s"$value is not a valid epoch number"))
    }
  }

  def fromInt(value: Int): Either[ConvertingError, Epoch] = {
    if (value >= 0)
      Right(new Epoch(value))
    else Left(ConvertingError(s"epoch $value must be >= 0"))
  }
}
