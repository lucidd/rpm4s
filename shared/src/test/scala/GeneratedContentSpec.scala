import org.scalatest._
import org.scalatest.prop.PropertyChecks

class GeneratedContentSpec extends FlatSpec with Matchers with PropertyChecks {

  "rpm4s.BuildInfo" should "be present with version info" in {
    rpm4s.BuildInfo.name.isInstanceOf[String] shouldBe true
    rpm4s.BuildInfo.version.isInstanceOf[String] shouldBe true
    rpm4s.BuildInfo.scalaVersion.isInstanceOf[String] shouldBe true
    rpm4s.BuildInfo.sbtVersion.isInstanceOf[String] shouldBe true
  }

}
