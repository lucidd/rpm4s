package rpm4s

import org.scalacheck.Arbitrary
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import rpm4s.IndexData.StringData
import rpm4s.codecs._
import rpm4s.data.HeaderType
import scodec.bits.BitVector
import scodec.{Attempt, DecodeResult}

class IndexDataSpec extends FlatSpec with Matchers with PropertyChecks {
  implicit val stringDataArb = Arbitrary(
    Arbitrary.arbString.arbitrary.map(StringData))

  "StringData" should "roundtrip" in {
    forAll { data: StringData =>
      val result = for {
        encoded <- indexDataEncoder.encode(data)
        decoded <- indexDataDecoder(HeaderType.String).decode(encoded)
      } yield decoded

      result should equal(
        Attempt.successful(DecodeResult(data, BitVector.empty)))
    }
  }

}
