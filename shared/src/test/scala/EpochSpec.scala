import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import rpm4s.codecs.ConvertingError
import rpm4s.data.Epoch

class EpochSpec extends FlatSpec with Matchers with PropertyChecks {

  "Epoch.fromInt" should "only allow ints > 0" in {
    forAll(Gen.chooseNum(Int.MinValue, -1)) { i =>
      Epoch.fromInt(i) shouldBe Left(ConvertingError(s"epoch $i must be >= 0"))
    }
    forAll(Gen.chooseNum(1, Int.MaxValue)) { i =>
      val r = Epoch.fromInt(i)
      r.isRight shouldBe true
      r.right.get.value shouldBe i
    }
  }

  "Epoch.fromString" should "fail on non numeric strings" in {
    forAll(Gen.alphaStr) { str =>
      Epoch.fromString(str) shouldBe Left(ConvertingError(s"$str is not a valid epoch number"))
    }
    forAll(Gen.chooseNum(0, Int.MaxValue)) { i =>
      val str = i.toString
      val r = Epoch.fromString(str)
      r.isRight shouldBe true
      r.right.get.value shouldBe i
    }
  }

}
