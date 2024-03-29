import org.scalacheck.{Arbitrary, Gen}
import rpm4s.data.Segment.Separator
import rpm4s.data.{EVR, Epoch, Release, Version}

object Utils {

  val numeric = Gen.numStr.filter(_.nonEmpty)
  val alpha = Gen.alphaStr.filter(_.nonEmpty)
  val tilde = Gen.const("~")
  val sep = Gen.nonEmptyListOf(Gen.oneOf(Separator.validSeparatorChars.toSeq)).map(_.mkString)

  val segment = Gen.oneOf(numeric, alpha, tilde, sep)
  val genVersion = Gen.nonEmptyListOf(segment).map(_.mkString)
    .map { verStr => Version.fromString(verStr).toOption.get }

  val genEpoch: Gen[Epoch] = Gen.posNum[Int].map(i => Epoch(i).toOption.get)
  val genRelease: Gen[Release] = Gen.nonEmptyListOf(Gen.oneOf(Release.validChars))
    .map { x => Release.fromString(x.mkString).toOption.get }
  val genEVR: Gen[EVR] = for {
    e <- genEpoch
    r <- Gen.option(genRelease)
    v <- genVersion
  } yield EVR(v, r, e)

  implicit val arbVersion: Arbitrary[Version] = Arbitrary(genVersion)
  implicit val arbEpoch: Arbitrary[Epoch] = Arbitrary(genEpoch)
  implicit val arbRelease: Arbitrary[Release] = Arbitrary(genRelease)
  implicit val arbEVR: Arbitrary[EVR] = Arbitrary(genEVR)

}
