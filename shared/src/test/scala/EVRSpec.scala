import org.scalatest._
import org.scalatest.prop.PropertyChecks
import rpm4s.data.{EVR, Epoch, Release, Version}
import Utils._
import cats.Comparison

class EVRSpec extends FlatSpec with Matchers with PropertyChecks {

  "EVR.parse" should "handle v correctly" in {
    val expected = for {
      v <- Version.parse("1.2.3")
    } yield EVR(v, None, None)
    EVR.parse("1.2.3") shouldEqual expected
  }

  it should "handle e:v correctly" in {
    val expected = for {
      v <- Version.parse("1.2.3")
      e <-Epoch.fromInt(12)
    } yield EVR(v, None, Some(e))
    EVR.parse("12:1.2.3") shouldEqual expected
  }

  it should "handle v-r correctly" in {
    val expected = for {
      v <- Version.parse("1.2.3")
      r <- Release.fromString("4.5")
    } yield EVR(v, Some(r), None)
    EVR.parse("1.2.3-4.5") shouldEqual expected
  }

  it should "handle e:v-r correctly" in {
    val expected = for {
      v <- Version.parse("1.2.3")
      e <- Epoch.fromInt(12)
      r <- Release.fromString("4.5")
    } yield EVR(v, Some(r), Some(e))
    EVR.parse("12:1.2.3-4.5") shouldEqual expected
  }

  it should "roundtrip" in {
    forAll { evr: EVR =>
      EVR.parse(evr.string) shouldEqual Right(evr)
    }
  }

  it should "order correctly" in {
    //verified with zypper versioncmp
    val table = Table(
      ("lhs", "rhs", "result"),
      ("1.2.3", "1.2.3", Comparison.EqualTo),
      ("0:1.2.3", "1.2.3", Comparison.EqualTo),
      ("1.2.3-1", "1.2.3", Comparison.GreaterThan),
      ("1.2.3-1", "1.2.3-2", Comparison.LessThan),
      ("1.2.3-~12", "1.2.3-1", Comparison.LessThan),
      ("0:1.2.3", "1:1.2.3", Comparison.LessThan),
      ("1.2.3", "1:1.2.3", Comparison.LessThan),

    )
    forAll(table) { case (lhs, rhs, result) =>
      for {
        l <- EVR.parse(lhs)
        r <- EVR.parse(rhs)
      } yield Comparison.fromInt(EVR.ordering.compare(l, r)) shouldBe result

    }
  }

}
