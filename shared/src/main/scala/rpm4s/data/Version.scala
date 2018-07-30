package rpm4s.data

import cats.kernel.Comparison
import rpm4s.codecs.ConvertingError


case class Version (segment: Segment) {
  def string: String = segment.string
}

object Version {

  def parse(value: String): Either[ConvertingError, Version] = {
    Segment.segment(value).flatMap {
      case Some(seg) => Right(new Version(seg))
      case None => Left(ConvertingError(s"version can not be empty"))
    }
  }

  implicit val ordering: Ordering[Version] = Ordering.by(_.segment)

  def rpmvercmp(v1: String, v2: String): Either[ConvertingError, Comparison] = {
    for {
      a <- parse(v1)
      b <- parse(v2)
    } yield Comparison.fromInt(ordering.compare(a, b))
  }

}
