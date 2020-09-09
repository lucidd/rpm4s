import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest._
import rpm4s.data.Checksum
import rpm4s.data.Checksum.{Md5, Sha1, Sha256, Sha512}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class ChecksumSpec extends FlatSpec with Matchers with ScalaCheckPropertyChecks {

  val hashChars: Set[Char] = (('a' to 'f') ++ ('0' to '9')).toSet

  def nsizedString(n: Int, charGen: Gen[Char]): Gen[String] = Gen.listOfN(n, charGen).map(_.mkString)

  def validHashString(n: Int): Gen[String] = nsizedString(n, Gen.oneOf(hashChars.toSeq))

  def invalidHashString(n: Int): Gen[String] = nsizedString(n, Arbitrary.arbChar.arbitrary).filter(
    !_.forall(hashChars.contains)
  )

  def hashBytes(n: Int): Gen[Vector[Byte]] = Gen.listOfN(n, Gen.choose[Byte](-128, 127)).map(_.toVector)

  implicit val noShrink: Shrink[String] = Shrink(_ => Stream.empty)

  def testHash[T <: Checksum](
      byteSize: Int,
      prefix: String,
      fromBytes: Vector[Byte] => Option[T],
      fromHex: String => Option[T]
  ): Unit = {

    prefix should "parse hex correctly" in {
      forAll(validHashString(byteSize * 2)) { hexString =>
        val checksum = fromHex(hexString)
        checksum shouldBe defined
        checksum.map(_.toString) shouldBe Some(s"$prefix-" + hexString)
      }
    }

    it should "parse bytes correctly" in {
      forAll(hashBytes(byteSize)) { bytes =>
        val checksum = fromBytes(bytes)
        checksum shouldBe defined
        checksum.map(_.bytes) shouldBe Some(bytes)
      }
    }

    it should "reject bytes with wrong length" in {
      forAll(hashBytes(byteSize), Gen.choose(1, byteSize)) { (bytes, delete)=>
        val checksum = fromBytes(bytes.drop(delete))
        checksum shouldBe None
      }
    }

    it should "reject hex with wrong length" in {
      forAll(validHashString(byteSize * 2), Gen.choose(1, byteSize)) { (bytes, delete)=>
        val checksum = fromHex(bytes.drop(delete))
        checksum shouldBe None
      }
    }

    it should "reject invalid hex chars" in {
      forAll(invalidHashString(byteSize * 2)) { string=>
         val checksum = fromHex(string)
         checksum shouldBe None
      }
    }

    it should "roundtrip as self describing hash" in {
      forAll(validHashString(byteSize * 2)) { string =>
        val checksum = fromHex(string)
        checksum shouldBe defined
        checksum
          .map(_.toSelfDescribingHex)
          .flatMap(Checksum.fromString) shouldBe checksum
      }
    }


  }

  testHash[Md5](16, "md5", Md5.fromBytes, Md5.fromHex)
  testHash[Sha1](20, "sha1", Sha1.fromBytes, Sha1.fromHex)
  testHash[Sha256](32, "sha256", Sha256.fromBytes, Sha256.fromHex)
  testHash[Sha512](64, "sha512", Sha512.fromBytes, Sha512.fromHex)

}
