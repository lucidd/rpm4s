import org.scalatest._
import rpm4s.data.{And, License, Or}
import rpm4s.data.License._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class LicenseSpec extends FlatSpec with Matchers with ScalaCheckPropertyChecks {

  "License.parse" should "handle unknown licenses" in {
    License.parse("Unknown license") shouldBe Right(UnknownLicense("Unknown license"))
  }

  it should "handle simple licenses" in {
    License.parse("SUSE-mirror") shouldBe Right(`SUSE-mirror`)
  }

  it should "handle licenses with whitespace" in {
    License.parse("Python and (BSD with advertising and QPL)") shouldBe Right(And(Python, And(`BSD with advertising`, QPL)))
  }

  it should "parse or" in {
    License.parse("GPL-2.0+ or MIT") shouldBe Right(Or(`GPL-2.0+`, MIT))
  }

  it should "parse and" in {
    License.parse("GPL-2.0+ and MIT") shouldBe Right(And(`GPL-2.0+`, MIT))
  }

  it should "handle grouping" in {
    License.parse("(MIT and ISC) and GPL-2.0+") shouldBe Right(And(And(MIT, ISC), `GPL-2.0+`))
  }

  it should "handle whitespace" in {
    License.parse("  GPL-2.0+   ") shouldBe Right(`GPL-2.0+`)
    License.parse("  ( GPL-2.0+  ) ") shouldBe Right(`GPL-2.0+`)
    License.parse("  MIT  and  ISC  and   GPL-2.0+ ") shouldBe Right(And(MIT, And(ISC, `GPL-2.0+`)))
    License.parse("  MIT  or  ISC  and   GPL-2.0+ ") shouldBe Right(Or(MIT, And(ISC, `GPL-2.0+`)))
  }

  it should "handle missing )" in {
    License.parse("(GPL-2.0+") shouldBe Left("missing )")
  }

  it should "handle trailing operation" in {
    License.parse("GPL-2.0+ and ") shouldBe Left("expected license but got ")
  }

  it should "handle trailing garbage" in {
    License.parse("(GPL-2.0+ and MIT) garbage") shouldBe Left("unexpected trailing content  garbage")
  }

  "LicenseOps.collectUnknown" should "return unknown Licenses" in {
    val u1 = UnknownLicense("unknown1")
    val u2 = UnknownLicense("unknown2")
    val u3 = UnknownLicense("unknown3")
    val license = And(`GPL-1.0+`, Or(u1, And(u2, u3)))
    license.collectUnknown should contain.allOf(u1, u2, u3)
  }

  "License.format" should "format properly" in {
    License.format(
      And(`GPL-1.0+`, Or(MIT, And(`Apache-2.0+`, UnknownLicense("unknown3"))))
    ) shouldBe "(GPL-1.0+ and (MIT or (Apache-2.0+ and unknown3)))"
  }


}
