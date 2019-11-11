import java.util.concurrent.Executors

import cats.effect.{Blocker, IO}
import org.http4s.Uri
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import rpm4s.data.Checksum.Sha1
import rpm4s.data.{Architecture, Name}
import rpm4s.repo.data.Bytes
import rpm4s.repo.yast2.Yast2.{Location, PackageF, Size}
import rpm4s.repo.yast2.{Content, Yast2}

import scala.concurrent.ExecutionContext

class Yast2Spec
    extends FlatSpec
    with Matchers
    with PropertyChecks {

  "content" should "get parsed correctly" in {
    implicit val cs = IO.contextShift(ExecutionContext.global)
    val r  =
      Blocker.fromExecutorService(IO(Executors.newCachedThreadPool())).use { blocker =>
        fs2.io.readInputStream[IO](IO(getClass.getResourceAsStream("/yast2/content")), 4096, blocker)
          .through(fs2.text.utf8Decode)
          .through(fs2.text.lines).compile.toVector.map { lines =>
          Content.fromLines(lines.toList)
        }
      }
      .unsafeRunSync()


    r shouldEqual Some(
      Content(
        version = "11",
        datadir = Uri.unsafeFromString("suse"),
        descrdir = Uri.unsafeFromString("suse/setup/descr")
      )
    )

  }

  "packages" should "get parsed correctly" in {
    implicit val cs = IO.contextShift(ExecutionContext.global)
      val r = Blocker.fromExecutorService(IO(Executors.newCachedThreadPool())).use { blocker =>
        fs2.io.readInputStream[IO](IO(getClass.getResourceAsStream("/yast2/packages")), 4096, blocker)
          .through(fs2.text.utf8Decode)
          .through(fs2.text.lines)
          .through(Yast2.pipe)
          .compile.toVector
      }.unsafeRunSync()

    val expected = Vector(
      PackageF[cats.Id](
        name = Name("0ad").toOption.get,
        checksum = Sha1.fromHex("55305f880451e550d08ac2e282c3c95af69f0e87").get,
        arch = Architecture.i586,
        size = Size(
          pack = Bytes(5640346),
          installed = Bytes(18874672)
        ),
        loc = Location("0ad-0.0.22-1.2.i586.rpm")
      ),
      PackageF[cats.Id](
        name = Name("389-ds").toOption.get,
        checksum = Sha1.fromHex("44d4b08e9b0d5b768b9c7400ea147b8b17d6ebcf").get,
        arch = Architecture.i586,
        size = Size(
          pack = Bytes(2152714),
          installed = Bytes(7486047)
        ),
        loc = Location("389-ds-1.3.6.6-2.1.i586.rpm")
      ),
      PackageF[cats.Id](
        name = Name("389-ds-devel").toOption.get,
        checksum = Sha1.fromHex("1db863c833a9fc2b9616fc456c0226ae31f7ef66").get,
        arch = Architecture.i586,
        size = Size(
          pack = Bytes(115302),
          installed = Bytes(421061)
        ),
        loc = Location("389-ds-devel-1.3.6.6-2.1.i586.rpm")
      )
    )

    r shouldEqual expected
  }

}
