import org.scalatest._
import org.scalatest.prop.PropertyChecks
import rpm4s.data.{EVR, Epoch, Release, Version}

class EVRSpec extends FlatSpec with Matchers with PropertyChecks {

  "version" should "not allow empty string" in {
    Version.parse("") should be(None)
  }

  "EVR.parse" should "handle v correctly" in {
    EVR.parse("1.2.3") should equal(
      Some(EVR(Version.parse("1.2.3").get))
    )
  }

  it should "handle e:v correctly" in {
    EVR.parse("12:1.2.3") should equal(
      Some(EVR(Version.parse("1.2.3").get, None, Some(Epoch(12))))
    )
  }

  it should "handle v-r correctly" in {
    EVR.parse("1.2.3-4.5") should equal(
      Some(EVR(Version.parse("1.2.3").get, Some(Release("4.5"))))
    )
  }

  it should "handle e:v-r correctly" in {
    EVR.parse("12:1.2.3-4.5") should equal(
      Some(EVR(Version.parse("1.2.3").get, Some(Release("4.5")), Some(Epoch(12))))
    )
  }

}
