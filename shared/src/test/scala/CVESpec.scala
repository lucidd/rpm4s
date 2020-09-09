import org.scalacheck.Gen
import org.scalatest._
import rpm4s.data.CVE
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CVESpec extends FlatSpec with Matchers with ScalaCheckPropertyChecks {

  "CVE string / fromString" should "roundtrip" in {
    val yearGen = Gen.chooseNum(1000, 9999)
    val cveIdGen = Gen.chooseNum(0, Int.MaxValue)
    forAll(yearGen, cveIdGen) { (year, id) =>
      val cve = CVE.fromParts(year, id)
      cve shouldBe defined
      cve.flatMap(x => CVE.fromString(x.string)) shouldBe cve
    }
  }

}
