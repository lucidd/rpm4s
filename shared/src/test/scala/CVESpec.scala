import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import rpm4s.data.CVE

class CVESpec extends FlatSpec with Matchers with PropertyChecks {

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
