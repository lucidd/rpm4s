package rpm4s.data

import rpm4s.codecs.ConvertingError
import rpm4s.utils._

case class Name private (value: String) {
  def copy(value: String = value): Nothing = ???
}
object Name {
  def fromString(value: String): Either[ConvertingError, Name] = {
    if (value.forall(isAlphaNumOr(_, ".-_+%{}")))
      Right(new Name(value))
    else Left(ConvertingError(s"$value is not valid rpm name"))
  }
}
