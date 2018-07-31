package rpm4s.data

import rpm4s.codecs.ConvertingError

case class Release private (value: String) {
  lazy val segment: Segment = Segment.segment(value).toOption.flatten.get
}
object Release {
  //according to https://blog.jasonantman.com/2014/07/how-yum-and-rpm-compare-versions/ release is also compared with the logic for versions
  implicit val ordering: Ordering[Release] = new Ordering[Release] {
    override def compare(x: Release, y: Release): Int = {
      Version.rpmvercmp(x.value, y.value).toOption.get.toInt
    }
  }

  val validChars: String = Segment.validChars
  def fromString(value: String): Either[ConvertingError, Release] = {
    if (value.isEmpty) {
      Left(ConvertingError(s"version can not be empty"))
    } else {
      if (value.forall(Segment.isValidSegmentChar)) {
        Right(Release(value))
      } else Left(ConvertingError(s"$value contains invalid release chars."))
    }
  }
}
