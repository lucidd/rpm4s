package rpm4s.data

import cats.kernel.Comparison
import rpm4s.codecs.ConvertingError


case class Version private (private val value: String) {
  def string: String = value
  // value has been validated before so it should be safe to assume no errors
  lazy val segment: Segment = Segment.segment(value).toOption.flatten.get
}

object Version {

  def fromString(value: String): Either[ConvertingError, Version] = {
    if (value.isEmpty) {
      Left(ConvertingError(s"version can not be empty"))
    } else {
      if (value.forall(Segment.isValidSegmentChar)) {
        Right(Version(value))
      } else Left(ConvertingError(s"$value contains invalid version chars."))
    }
  }

  implicit val ordering: Ordering[Version] = Ordering.by(_.segment)

  def rpmvercmp(v1: String, v2: String): Either[ConvertingError, Comparison] = {
    for {
      a <- fromString(v1)
      b <- fromString(v2)
    } yield Comparison.fromInt(ordering.compare(a, b))
  }

}
