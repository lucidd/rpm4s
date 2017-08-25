import org.scalatest._
import org.scalatest.prop.PropertyChecks
import rpm4s.data.{EVR, Epoch, Release, Version}

class EVRSpec extends FlatSpec with Matchers with PropertyChecks {

  "version" should "not allow empty string" in {
    Version.fromString("") should be(None)
  }

  "evr" should "parse v correctly" in {
    EVR.fromString("1.2.3") should equal(Some(EVR(Version("1.2.3"))))
  }

  it should "parse e:v correctly" in {
    EVR.fromString("12:1.2.3") should equal(
      Some(EVR(Version("1.2.3"), None, Some(Epoch(12)))))
  }

  it should "parse v-r correctly" in {
    EVR.fromString("1.2.3-4.5") should equal(
      Some(EVR(Version("1.2.3"), Some(Release("4.5"))))
    )
  }

  it should "parse e:v-r correctly" in {
    EVR.fromString("12:1.2.3-4.5") should equal(
      Some(EVR(Version("1.2.3"), Some(Release("4.5")), Some(Epoch(12)))))
  }

}
