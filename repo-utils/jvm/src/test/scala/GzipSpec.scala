import java.util.concurrent.Executors
import cats.effect.{Blocker, IO}
import org.scalacheck.Arbitrary
import org.scalatest._
import rpm4s.codecs.IndexData.StringData
import rpm4s.data.{Architecture, Name, RpmPrimaryEntry, Version}
import scodec.bits.BitVector
import scodec.{Attempt, Codec}
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import rpm4s.repo.utils.compress.{gunzip, gzip}

import scala.concurrent.ExecutionContext

class GzipSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks {

  "gunzip" should "uncompress correctly" in {
    implicit val cs = IO.contextShift(ExecutionContext.global)
    val text = Blocker.fromExecutorService(IO(Executors.newCachedThreadPool())).use { blocker =>
      val bits = fs2.io.readInputStream(
        IO(getClass.getResourceAsStream("/text.gz")), 4096, blocker
      )
      bits
        .through(gunzip())
        .through(fs2.text.utf8Decode)
        .compile.toVector.map(_.mkString)
    }.unsafeRunSync()


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
