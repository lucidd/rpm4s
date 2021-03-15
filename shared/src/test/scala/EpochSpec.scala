import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import rpm4s.codecs.ConvertingError
import rpm4s.data.Epoch

class EpochSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  "Epoch.fromInt" should "only allow ints > 0" in {
    forAll(Gen.chooseNum(Int.MinValue, -1)) { i =>
      Epoch.fromInt(i) shouldBe Left(ConvertingError(s"epoch $i must be >= 0"))
    }
    forAll(Gen.chooseNum(1, Int.MaxValue)) { i =>
      val r = Epoch.fromInt(i)
      r.isRight shouldBe true
      r.toOption.get.value shouldBe i
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
      r.toOption.get.value shouldBe i
    }
  }

}
