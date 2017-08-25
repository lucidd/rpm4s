import org.scalatest._
import org.scalatest.prop.PropertyChecks
import rpm4s.data.Version._
import rpm4s.data.Version

class VersionSpec extends FlatSpec with Matchers with PropertyChecks {

  /*
  val numeric = Gen.posNum[Long].map(Version.Numeric)
  val alpha = Gen.alphaStr.map(Version.Alpha)
  val segment = Gen.oneOf(numeric, alpha)
  val vstring = segment.map {
    case Version.Alpha(v) => v
    case Version.Numeric(n) => n.toString
  }


  val version = Gen.listOf(vstring).map(s => Version(s.mkString(".")))
  implicit val arbVersion = Arbitrary(version)

  "rpmvercmp" should "be symmetric" in {
    val rpmvercmp = new RPMSpacewalk
    forAll { (v1: Version, v2: Version) =>
      rpmvercmp.compare(v1.value, v2.value) shouldBe -rpmvercmp.compare(v2.value, v1.value)
    }
  }
   */

  "versioncmd" should "" in {}

  it should "pass upstream rpm version tests" in {
    rpmvercmp("1.0", "1.0") shouldBe 0
    rpmvercmp("1.0", "2.0") shouldBe -1
    rpmvercmp("2.0", "1.0") shouldBe 1
    rpmvercmp("2.0.1", "2.0.1") shouldBe 0
    rpmvercmp("2.0", "2.0.1") shouldBe -1
    rpmvercmp("2.0.1", "2.0") shouldBe 1
    rpmvercmp("2.0.1a", "2.0.1a") shouldBe 0
    rpmvercmp("2.0.1a", "2.0.1") shouldBe 1
    rpmvercmp("2.0.1", "2.0.1a") shouldBe -1
    rpmvercmp("5.5p1", "5.5p1") shouldBe 0
    rpmvercmp("5.5p1", "5.5p2") shouldBe -1
    rpmvercmp("5.5p2", "5.5p1") shouldBe 1
    rpmvercmp("5.5p10", "5.5p10") shouldBe 0
    rpmvercmp("5.5p1", "5.5p10") shouldBe -1
    rpmvercmp("5.5p10", "5.5p1") shouldBe 1
    rpmvercmp("10xyz", "10.1xyz") shouldBe -1
    rpmvercmp("10.1xyz", "10xyz") shouldBe 1
    rpmvercmp("xyz10", "xyz10") shouldBe 0
    rpmvercmp("xyz10", "xyz10.1") shouldBe -1
    rpmvercmp("xyz10.1", "xyz10") shouldBe 1
    rpmvercmp("xyz.4", "xyz.4") shouldBe 0
    rpmvercmp("xyz.4", "8") shouldBe -1
    rpmvercmp("8", "xyz.4") shouldBe 1
    rpmvercmp("xyz.4", "2") shouldBe -1
    rpmvercmp("2", "xyz.4") shouldBe 1
    rpmvercmp("5.5p2", "5.6p1") shouldBe -1
    rpmvercmp("5.6p1", "5.5p2") shouldBe 1
    rpmvercmp("5.6p1", "6.5p1") shouldBe -1
    rpmvercmp("6.5p1", "5.6p1") shouldBe 1
    rpmvercmp("6.0.rc1", "6.0") shouldBe 1
    rpmvercmp("6.0", "6.0.rc1") shouldBe -1
    rpmvercmp("10b2", "10a1") shouldBe 1
    rpmvercmp("10a2", "10b2") shouldBe -1
    rpmvercmp("1.0aa", "1.0aa") shouldBe 0
    rpmvercmp("1.0a", "1.0aa") shouldBe -1
    rpmvercmp("1.0aa", "1.0a") shouldBe 1
    rpmvercmp("10.0001", "10.0001") shouldBe 0
    rpmvercmp("10.0001", "10.1") shouldBe 0
    rpmvercmp("10.1", "10.0001") shouldBe 0
    rpmvercmp("10.0001", "10.0039") shouldBe -1
    rpmvercmp("10.0039", "10.0001") shouldBe 1
    rpmvercmp("4.999.9", "5.0") shouldBe -1
    rpmvercmp("5.0", "4.999.9") shouldBe 1
    rpmvercmp("20101121", "20101121") shouldBe 0
    rpmvercmp("20101121", "20101122") shouldBe -1
    rpmvercmp("20101122", "20101121") shouldBe 1
    rpmvercmp("2_0", "2_0") shouldBe 0
    rpmvercmp("2.0", "2_0") shouldBe 0
    rpmvercmp("2_0", "2.0") shouldBe 0
    rpmvercmp("a", "a") shouldBe 0
    rpmvercmp("a+", "a+") shouldBe 0
    rpmvercmp("a+", "a_") shouldBe 0
    rpmvercmp("a_", "a+") shouldBe 0
    rpmvercmp("+a", "+a") shouldBe 0
    rpmvercmp("+a", "_a") shouldBe 0
    rpmvercmp("_a", "+a") shouldBe 0
    rpmvercmp("+_", "+_") shouldBe 0
    rpmvercmp("_+", "+_") shouldBe 0
    rpmvercmp("_+", "_+") shouldBe 0
    rpmvercmp("+", "_") shouldBe 0
    rpmvercmp("_", "+") shouldBe 0
    rpmvercmp("1.0~rc1", "1.0~rc1") shouldBe 0
    rpmvercmp("1.0~rc1", "1.0") shouldBe -1
    rpmvercmp("1.0", "1.0~rc1") shouldBe 1
    rpmvercmp("1.0~rc1", "1.0~rc2") shouldBe -1
    rpmvercmp("1.0~rc2", "1.0~rc1") shouldBe 1
    rpmvercmp("1.0~rc1~git123", "1.0~rc1~git123") shouldBe 0
    rpmvercmp("1.0~rc1~git123", "1.0~rc1") shouldBe -1
    rpmvercmp("1.0~rc1", "1.0~rc1~git123") shouldBe 1
  }

  "version" should "not allow empty string" in {
    Version.fromString("") should be(None)
  }

}
