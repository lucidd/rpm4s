package rpm4s.data

import rpm4s.codecs.ConvertingError
import rpm4s.utils._

case class Release private (value: String) {
  def copy(value: String = value): Either[ConvertingError, Release] =
    Release(value)
}
object Release {
  //according to https://blog.jasonantman.com/2014/07/how-yum-and-rpm-compare-versions/ release is also compared with the logic for versions
  implicit val ordering: Ordering[Release] = new Ordering[Release] {
    override def compare(x: Release, y: Release): Int = {
      Version.rpmvercmp(x.value, y.value).toOption.get.toInt
    }
  }
  def apply(value: String): Either[ConvertingError, Release] = fromString(value)
  val validChars: String = (('a' to 'z') ++ ('A' to 'Z')).mkString + "{}%+_.~"
  def isValidReleaseChar(char: Char): Boolean = isAlphaNumOr(char, "{}%+_.~")
  def fromString(value: String): Either[ConvertingError, Release] = {
    if (value.nonEmpty && value.forall(isValidReleaseChar))
      Right(new Release(value))
    else Left(ConvertingError(s"invalid value $value for release"))
  }
}
