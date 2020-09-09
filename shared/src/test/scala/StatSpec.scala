import org.scalatest._
import rpm4s.data._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class StatSpec extends FlatSpec with Matchers with ScalaCheckPropertyChecks {

  "Stat" should "be parsed correctly" in {
    val stat = Stat.fromShort(16877).get
    stat.tpe shouldBe Stat.FileType.Directory
    stat.permsString shouldBe "rwxr-xr-x"
  }

  "Stat.fromShort" should "not allow invalid values" in {
    Stat.fromShort(0) shouldBe None
  }

}
