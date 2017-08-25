import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import rpm4s.data.Version._
import rpm4s.data.Version

class VersionSpec extends FlatSpec with Matchers with PropertyChecks {

  val numeric = implicitly[Arbitrary[BigInt]].arbitrary.map(
    x => Numeric(x.abs)
  )
  val alpha = Gen.alphaStr.map(Alpha)
  val tilde = Gen.const(Tilde)
  val segment = Gen.oneOf(numeric, alpha, tilde)
  val version = Gen.listOf(segment).map(Version(_))
  implicit val arbVersion: Arbitrary[Version] = Arbitrary(version)


  "rpmvercmp" should "be symmetric" in {
    forAll { (v1: Version, v2: Version) =>
      compare(v1, v2) shouldBe -compare(v2, v1)
    }
  }
  val upstreamTestSet: List[(String, String, Int)] = List(
      ("1.0", "1.0", 0),
      ("1.0", "2.0", -1),
      ("2.0", "1.0", 1),
      ("2.0.1", "2.0.1", 0),
      ("2.0", "2.0.1", -1),
      ("2.0.1", "2.0", 1),
      ("2.0.1a", "2.0.1a", 0),
      ("2.0.1a", "2.0.1", 1),
      ("2.0.1", "2.0.1a", -1),
      ("5.5p1", "5.5p1", 0),
      ("5.5p1", "5.5p2", -1),
      ("5.5p2", "5.5p1", 1),
      ("5.5p10", "5.5p10", 0),
      ("5.5p1", "5.5p10", -1),
      ("5.5p10", "5.5p1", 1),
      ("10xyz", "10.1xyz", -1),
      ("10.1xyz", "10xyz", 1),
      ("xyz10", "xyz10", 0),
      ("xyz10", "xyz10.1", -1),
      ("xyz10.1", "xyz10", 1),
      ("xyz.4", "xyz.4", 0),
      ("xyz.4", "8", -1),
      ("8", "xyz.4", 1),
      ("xyz.4", "2", -1),
      ("2", "xyz.4", 1),
      ("5.5p2", "5.6p1", -1),
      ("5.6p1", "5.5p2", 1),
      ("5.6p1", "6.5p1", -1),
      ("6.5p1", "5.6p1", 1),
      ("6.0.rc1", "6.0", 1),
      ("6.0", "6.0.rc1", -1),
      ("10b2", "10a1", 1),
      ("10a2", "10b2", -1),
      ("1.0aa", "1.0aa", 0),
      ("1.0a", "1.0aa", -1),
      ("1.0aa", "1.0a", 1),
      ("10.0001", "10.0001", 0),
      ("10.0001", "10.1", 0),
      ("10.1", "10.0001", 0),
      ("10.0001", "10.0039", -1),
      ("10.0039", "10.0001", 1),
      ("4.999.9", "5.0", -1),
      ("5.0", "4.999.9", 1),
      ("20101121", "20101121", 0),
      ("20101121", "20101122", -1),
      ("20101122", "20101121", 1),
      ("2_0", "2_0", 0),
      ("2.0", "2_0", 0),
      ("2_0", "2.0", 0),
      ("a", "a", 0),
      ("a+", "a+", 0),
      ("a+", "a_", 0),
      ("a_", "a+", 0),
      ("+a", "+a", 0),
      ("+a", "_a", 0),
      ("_a", "+a", 0),
      ("+_", "+_", 0),
      ("_+", "+_", 0),
      ("_+", "_+", 0),
      ("+", "_", 0),
      ("_", "+", 0),
      ("1.0~rc1", "1.0~rc1", 0),
      ("1.0~rc1", "1.0", -1),
      ("1.0", "1.0~rc1", 1),
      ("1.0~rc1", "1.0~rc2", -1),
      ("1.0~rc2", "1.0~rc1", 1),
      ("1.0~rc1~git123", "1.0~rc1~git123", 0),
      ("1.0~rc1~git123", "1.0~rc1", -1),
      ("1.0~rc1", "1.0~rc1~git123", 1)
  )

  it should "pass upstream librpm version tests" in {
    upstreamTestSet.foreach { case (v1, v2, result) =>
      rpmvercmp(v1, v2) shouldBe Some(result)
    }
  }

  "version" should "not allow empty string" in {
    Version.parse("") should be(None)
  }

}
