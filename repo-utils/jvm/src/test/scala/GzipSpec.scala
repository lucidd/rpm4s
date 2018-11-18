import cats.effect.IO
import org.scalacheck.Arbitrary
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import rpm4s.codecs.IndexData.StringData
import rpm4s.data.{Architecture, Name, RpmPrimaryEntry, Version}
import scodec.bits.BitVector
import scodec.{Attempt, Codec}
import fs2.Stream
import rpm4s.repo.utils.compress.{gunzip, gzip}

import scala.concurrent.ExecutionContext

class GzipSpec
    extends FlatSpec
    with Matchers
    with PropertyChecks {

  "gunzip" should "uncompress correctly" in {
    implicit val cs = IO.contextShift(ExecutionContext.global)
    val bits = fs2.io.readInputStream(
      IO(getClass.getResourceAsStream("/text.gz")), 4096, ExecutionContext.global
    )

    val text = bits
      .through(gunzip())
      .through(fs2.text.utf8Decode)
      .compile.toVector.map(_.mkString)
      .unsafeRunSync()


    text shouldEqual "hello world!\n"
  }

  it should "roundtrip" in {
    forAll { value: String =>
      val r = Stream.emit(value)
        .covary[IO]
        .through(fs2.text.utf8Encode)
        .through(gzip())
        .through(gunzip())
        .through(fs2.text.utf8Decode)
        .compile.toList.map(_.mkString)
        .unsafeRunSync()


      r shouldEqual value
    }
  }

}
