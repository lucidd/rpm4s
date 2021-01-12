import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class GeneratedContentSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  "rpm4s.BuildInfo" should "be present with version info" in {
    rpm4s.BuildInfo.name.isInstanceOf[String] shouldBe true
    rpm4s.BuildInfo.version.isInstanceOf[String] shouldBe true
    rpm4s.BuildInfo.scalaVersion.isInstanceOf[String] shouldBe true
    rpm4s.BuildInfo.sbtVersion.isInstanceOf[String] shouldBe true
  }

}
