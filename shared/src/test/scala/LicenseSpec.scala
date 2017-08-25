import org.scalatest._
import org.scalatest.prop.PropertyChecks
import rpm4s.data.{And, License, Or}
import rpm4s.data.License._

class LicenseSpec extends FlatSpec with Matchers with PropertyChecks {

  "License.parse" should "handle unknown licenses" in {
    License.parse("Unknown license") should be(
      Right(UnknownLicense("Unknown license"))
    )
  }

  it should "handle simple licenses" in {
    License.parse("SUSE-mirror") should be(
      Right(`SUSE-mirror`)
    )
  }

  it should "handle licenses with whitespace" in {
    License.parse("Python and (BSD with advertising and QPL)") should be(
      Right(And(Python, And(`BSD with advertising`, QPL)))
    )
  }

  it should "parse or" in {
    License.parse("GPL-2.0+ or MIT") should be(Right(Or(`GPL-2.0+`, MIT)))
  }

  it should "parse and" in {
    License.parse("GPL-2.0+ and MIT") should be(Right(And(`GPL-2.0+`, MIT)))
  }

  it should "handle grouping" in {
    License.parse("(MIT and ISC) and GPL-2.0+") should be(
      Right(And(And(MIT, ISC), `GPL-2.0+`))
    )
  }

  it should "handle whitespace" in {
    License.parse("  GPL-2.0+   ") should be (Right(`GPL-2.0+`))
    License.parse("  ( GPL-2.0+  ) ") should be (Right(`GPL-2.0+`))
    License.parse("  MIT  and  ISC  and   GPL-2.0+ ") should be(
      Right(And(MIT, And(ISC, `GPL-2.0+`)))
    )
    License.parse("  MIT  or  ISC  and   GPL-2.0+ ") should be(
      Right(Or(MIT, And(ISC, `GPL-2.0+`)))
    )
  }

}
