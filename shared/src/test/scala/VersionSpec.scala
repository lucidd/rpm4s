import org.scalacheck.{Gen}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import rpm4s.data.Version._
import rpm4s.data.Version
import cats.kernel.Comparison._
import rpm4s.codecs.ConvertingError
import Utils._

class VersionSpec extends FlatSpec with Matchers with PropertyChecks {

  "rpmvercmp" should "be symmetric" in {
    forAll { (v1: Version, v2: Version) =>
      compare(v1, v2).toInt shouldBe -compare(v2, v1).toInt
    }
  }

  val upstreamTestSet = Table(
      ("lhs version", "rhs version", "result"),
      ("1.0", "1.0", EqualTo),
      ("1.0", "2.0", LessThan),
      ("2.0", "1.0", GreaterThan),
      ("2.0.1", "2.0.1", EqualTo),
      ("2.0", "2.0.1", LessThan),
      ("2.0.1", "2.0", GreaterThan),
      ("2.0.1a", "2.0.1a", EqualTo),
      ("2.0.1a", "2.0.1", GreaterThan),
      ("2.0.1", "2.0.1a", LessThan),
      ("5.5p1", "5.5p1", EqualTo),
      ("5.5p1", "5.5p2", LessThan),
      ("5.5p2", "5.5p1", GreaterThan),
      ("5.5p10", "5.5p10", EqualTo),
      ("5.5p1", "5.5p10", LessThan),
      ("5.5p10", "5.5p1", GreaterThan),
      ("10xyz", "10.1xyz", LessThan),
      ("10.1xyz", "10xyz", GreaterThan),
      ("xyz10", "xyz10", EqualTo),
      ("xyz10", "xyz10.1", LessThan),
      ("xyz10.1", "xyz10", GreaterThan),
      ("xyz.4", "xyz.4", EqualTo),
      ("xyz.4", "8", LessThan),
      ("8", "xyz.4", GreaterThan),
      ("xyz.4", "2", LessThan),
      ("2", "xyz.4", GreaterThan),
      ("5.5p2", "5.6p1", LessThan),
      ("5.6p1", "5.5p2", GreaterThan),
      ("5.6p1", "6.5p1", LessThan),
      ("6.5p1", "5.6p1", GreaterThan),
      ("6.0.rc1", "6.0", GreaterThan),
      ("6.0", "6.0.rc1", LessThan),
      ("10b2", "10a1", GreaterThan),
      ("10a2", "10b2", LessThan),
      ("1.0aa", "1.0aa", EqualTo),
      ("1.0a", "1.0aa", LessThan),
      ("1.0aa", "1.0a", GreaterThan),
      ("10.0001", "10.0001", EqualTo),
      ("10.0001", "10.1", EqualTo),
      ("10.1", "10.0001", EqualTo),
      ("10.0001", "10.0039", LessThan),
      ("10.0039", "10.0001", GreaterThan),
      ("4.999.9", "5.0", LessThan),
      ("5.0", "4.999.9", GreaterThan),
      ("20101121", "20101121", EqualTo),
      ("20101121", "20101122", LessThan),
      ("20101122", "20101121", GreaterThan),
      ("2_0", "2_0", EqualTo),
      ("2.0", "2_0", EqualTo),
      ("2_0", "2.0", EqualTo),
      ("a", "a", EqualTo),
      ("a+", "a+", EqualTo),
      ("a+", "a_", EqualTo),
      ("a_", "a+", EqualTo),
      ("+a", "+a", EqualTo),
      ("+a", "_a", EqualTo),
      ("_a", "+a", EqualTo),
      ("+_", "+_", EqualTo),
      ("_+", "+_", EqualTo),
      ("_+", "_+", EqualTo),
      ("+", "_", EqualTo),
      ("_", "+", EqualTo),
      ("1.0~rc1", "1.0~rc1", EqualTo),
      ("1.0~rc1", "1.0", LessThan),
      ("1.0", "1.0~rc1", GreaterThan),
      ("1.0~rc1", "1.0~rc2", LessThan),
      ("1.0~rc2", "1.0~rc1", GreaterThan),
      ("1.0~rc1~git123", "1.0~rc1~git123", EqualTo),
      ("1.0~rc1~git123", "1.0~rc1", LessThan),
      ("1.0~rc1", "1.0~rc1~git123", GreaterThan)
  )

  // verified with rpmdev-vercmp utility
  val additionalTestSet = Table(
    ("lhs version", "rhs version", "result"),
    ("~", "~", EqualTo),
    ("123...", "123~", GreaterThan),
    ("~01", "~1", EqualTo),
    ( "~01", "~~1", GreaterThan)
  )

  it should "pass upstream librpm version tests" in {
    forAll(upstreamTestSet) { case (v1, v2, result) =>
      rpmvercmp(v1, v2) shouldBe Right(result)
    }
  }

  it should "pass additional version tests" in {
    forAll(additionalTestSet) { case (v1, v2, result) =>
      rpmvercmp(v1, v2) shouldBe Right(result)
    }
  }

  "version" should "not allow empty string" in {
    Version.parse("") shouldBe Left(ConvertingError(s"version can not be empty"))
  }

  it should "roundtrip" in {
    forAll { v: Version =>
      Version.parse(v.string) shouldBe Right(v)
    }
  }

  "Alpha" should "should allow a-zA-Z" in {
    forAll(Gen.alphaStr.filter(_.nonEmpty)) { str: String =>
      val a = Alpha(str ,None)
      a.isRight shouldBe true
      a.toOption.get.value shouldBe str
    }
  }

  it should "not allow chars other then a-zA-Z" in {
    forAll { str: String =>
      whenever(!str.forall(rpm4s.utils.isAlpha)) {
        Alpha(str, None).isLeft shouldBe true
      }
    }
  }

  it should "not allow empty string" in {
    Alpha("", None).isLeft shouldBe true
  }


  "Numeric" should "allow 0-9" in {
    forAll(Gen.numStr.filter(_.nonEmpty)) { str: String =>
      val a = Numeric(str ,None)
      a.isRight shouldBe true
      a.toOption.get.value shouldBe str
    }
  }

  it should "not allow chars other then 0-9" in {
    forAll { str: String =>
      whenever(!str.forall(rpm4s.utils.isNum)) {
        Numeric(str, None).isLeft shouldBe true
      }
    }
  }

  it should "not allow ໘" in {
    Numeric("໘", None).isLeft shouldBe true
  }

  it should "not allow empty string" in {
    Numeric("", None).isLeft shouldBe true
  }

  "Separator" should " allow {}%+_." in {
    forAll(Gen.nonEmptyListOf(Gen.oneOf("{}%+_.")).map(_.mkString)) { str: String =>
      val a = Separator(str ,None)
      a.isRight shouldBe true
      a.toOption.get.value shouldBe str
    }
  }

  it should "not allow chars other then {}%+_." in {
    forAll { str: String =>
      whenever(!str.forall(c => "{}%+_.".contains(c))) {
        Separator(str, None).isLeft shouldBe true
      }
    }
  }

  it should "not allow empty string" in {
    Separator("", None).isLeft shouldBe true
  }

}
