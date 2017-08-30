package rpm4s.data

import rpm4s.codecs.ConvertingError
import rpm4s.utils._

case class Name private (value: String) {
  def copy(value: String = value): Either[ConvertingError, Name] =
    Name(value)
}
object Name {
  def apply(value: String): Either[ConvertingError, Name] = fromString(value)
  def fromString(value: String): Either[ConvertingError, Name] = {
    if (value.forall(isAlphaNumOr(_, ".-_+%{}")))
      Right(new Name(value))
    else Left(ConvertingError(s"$value is not valid rpm name"))
  }
}
