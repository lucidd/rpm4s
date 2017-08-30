package rpm4s.data

import rpm4s.codecs.ConvertingError
import rpm4s.utils._

case class Release private (value: String) {
  def copy(value: String = value): Either[ConvertingError, Release] =
    Release(value)
}
object Release {
  def apply(value: String): Either[ConvertingError, Release] = fromString(value)
  def fromString(value: String): Either[ConvertingError, Release] = {
    if (value.forall(isAlphaNumOr(_, "{}%+_.~")))
      Right(new Release(value))
    else Left(ConvertingError(s"invalid value $value for release"))
  }
}
