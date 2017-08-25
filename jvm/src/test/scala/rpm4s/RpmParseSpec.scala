package rpm4s

import org.scalacheck.Arbitrary
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import rpm4s.IndexData.StringData
import scodec.{Attempt, Codec}
import scodec.bits.BitVector
import rpm4s.codecs._

class RpmParseSpec
    extends FlatSpec
    with Matchers
    with PropertyChecks
    with CustomMatchers {
  implicit val stringDataArb = Arbitrary(
    Arbitrary.arbString.arbitrary.map(StringData))

  "RPM" should "be parseable" in {
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

}
