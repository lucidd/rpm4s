import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import rpm4s.data._

class StatSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  "Stat" should "be parsed correctly" in {
    val stat = Stat.fromShort(16877).get
    stat.tpe shouldBe Stat.FileType.Directory
    stat.permsString shouldBe "rwxr-xr-x"
  }

  "Stat.fromShort" should "not allow invalid values" in {
    Stat.fromShort(0) shouldBe None
  }

}
