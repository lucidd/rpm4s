package rpm4s

import org.scalacheck.Arbitrary
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import rpm4s.codecs.IndexData.StringData
import scodec.{Attempt, Codec}
import scodec.bits.BitVector
import rpm4s.codecs._
import rpm4s.data.{Architecture, Name, RpmPrimaryEntry, Version}

class RpmParseSpec
    extends FlatSpec
    with Matchers
    with PropertyChecks
    with CustomMatchers {
  implicit val stringDataArb = Arbitrary(
    Arbitrary.arbString.arbitrary.map(StringData))

  "Codec[RpmFile]" should "be parseable" in {
    val bits = BitVector.fromInputStream(
      getClass.getResourceAsStream("/kernel-default-4.11.8-1.2.x86_64.rpm"))
    val rpmFile = Codec[RpmFile].decode(bits)
    rpmFile.require.remainder should equal(BitVector.empty)
  }

  it should "roundtrip" in {
    val bits = BitVector.fromInputStream(
      getClass.getResourceAsStream("/kernel-default-4.11.8-1.2.x86_64.rpm"))
    val rpmFile = Codec[RpmFile].decode(bits)
    rpmFile.require.remainder shouldBe BitVector.empty
    val encoded = Codec[RpmFile].encode(rpmFile.require.value)
    encoded shouldEqual Attempt.successful(bits)
  }

  "rpm.decode" should "correctly decode RpmPrimaryEntry" in {
    val bits = BitVector.fromInputStream(
      getClass.getResourceAsStream("/kernel-default-4.11.8-1.2.x86_64.rpm"))
    val rpe = rpm4s.decode[RpmPrimaryEntry](bits).require
    rpe.architecture shouldBe Architecture.x86_64
    rpe.name shouldBe Name.fromString("kernel-default").toOption.get
    rpe.version shouldBe Version.parse("4.11.8").toOption.get
  }

}
