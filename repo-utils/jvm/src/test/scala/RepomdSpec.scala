import java.nio.file.Paths
import java.time.Instant
import java.util.concurrent.Executors
import cats.effect.{Blocker, ContextShift, IO}
import org.scalatest._
import rpm4s.data.Checksum.Sha256
import rpm4s.data._
import rpm4s.repo.data.{Bytes, Repomd}
import rpm4s.repo.data.Data.{Primary, UpdateInfo}
import rpm4s.repo.repomd._
import rpm4s.repo.utils.xml.xmlevents
import scodec.bits.{BitVector, ByteVector}
import fs2.{Pure, Stream}
import rpm4s.repo.data.primary.{PackageF, SizeInfo}
import rpm4s.repo.repomd.xml.primary.xml2packages
import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.ExecutionContext


class RepomdSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks {

  "repomd.xml" should "get parsed correctly" in {
    implicit val contextShift = IO.contextShift(ExecutionContext.global)
    val blocker = Blocker.liftExecutionContext(ExecutionContext.global)
    val r  = xmlevents[IO](getClass.getResourceAsStream("/repomd/repomd.xml"), blocker)
        .through(rpm4s.repo.repomd.xml2repomd)
        .compile.last.unsafeRunSync()

    r shouldEqual Some(
      Repomd(
        revision = Some(1504283245),
        primary = Some(Primary(
          location = "repodata/03b53138f32575f30326c7028857cb013b709570614b7bb6190ff3ec2eac3ea7-primary.xml.gz",
          checksum = Sha256.fromHex("03b53138f32575f30326c7028857cb013b709570614b7bb6190ff3ec2eac3ea7").get,
          timestamp = Instant.ofEpochSecond(1504285261),
          size = Bytes(17781467),
          openSize = Bytes(139577942),
          openChecksum = Sha256.fromHex("baea56d3261c44c07b609658b09fa56bd4c8ce939f74d1ae08deff152b7ca010").get,
        )),
        updateinfo = Some(UpdateInfo(
          location = "repodata/f85c74f37d6450efd12bc1770b74d6ca64a9081b2a7d2748969d48395fa64bc9-updateinfo.xml.gz",
          checksum = Sha256.fromHex("f85c74f37d6450efd12bc1770b74d6ca64a9081b2a7d2748969d48395fa64bc9").get,
          timestamp = Instant.ofEpochSecond(1542097131),
          size = Bytes(582817),
          openSize = Bytes(4804840),
          openChecksum = Sha256.fromHex("88d73b412e757e65acd17187d9f147a400f6bc5f8bb36fe77421504b07e29eab").get,
        ))
      )
    )

  }

  "primary.xml" should "get parsed correctly" in {
    implicit val contextShift = IO.contextShift(ExecutionContext.global)
    val blocker = Blocker.liftExecutionContext(ExecutionContext.global)
    val r  = xmlevents[IO](
      getClass.getResourceAsStream("/repomd/primary.xml"),
      blocker
    ).through(xml2packages)
     .compile.toVector.unsafeRunSync()

    //TODO: this is missing source rpm test case
    val expected = Vector(
      PackageF[cats.Id](
        name = Name("0ad").toOption.get,
        arch = Architecture.i586,
        version = Version.fromString("0.0.22").toOption.get,
        epoch = Epoch.ZERO,
        release = Release.fromString("1.2").toOption.get,
        checksum = Sha256.fromHex("526e7af8c90ef5a41a7fb17d7a42277e06727f3bbb443cc795819e0feb1e5021").get,
        size = SizeInfo(
          pack = Bytes(5640346),
          archive = Bytes(18877364),
          installed = Bytes(18874672)
        ),
        loc = "i586/0ad-0.0.22-1.2.i586.rpm"
      ),
      PackageF[cats.Id](
        name = Name("389-ds").toOption.get,
        arch = Architecture.i586,
        version = Version.fromString("1.3.6.6").toOption.get,
        epoch = Epoch.ZERO,
        release = Release.fromString("2.1").toOption.get,
        checksum = Sha256.fromHex("bc7b674119d3c37c2b595567a583cf41dc3296f944871f076e184db5525955e1").get,
        size = SizeInfo(
          pack = Bytes(2152714),
          archive = Bytes(7548544),
          installed = Bytes(7486047)
        ),
        loc = "i586/389-ds-1.3.6.6-2.1.i586.rpm"
      ),
      PackageF[cats.Id](
        name = Name("389-ds-devel").toOption.get,
        arch = Architecture.i586,
        version = Version.fromString("1.3.6.6").toOption.get,
        epoch = Epoch.ZERO,
        release = Release.fromString("2.1").toOption.get,
        checksum = Sha256.fromHex("9e231c26fd27defa79da040c0249091b6f44fd32b1b3d34e97d423e14a83643e").get,
        size = SizeInfo(
          pack = Bytes(115302),
          archive = Bytes(423752),
          installed = Bytes(421061)
        ),
        loc = "i586/389-ds-devel-1.3.6.6-2.1.i586.rpm"
      )
    )

    r shouldEqual expected
  }

  "createPrimary" should "should create primary.xml correctly" in {
    implicit val cs = IO.contextShift(ExecutionContext.global)
    //TODO: currently this ignores the pkgid attribute of the checksum which has been manually removed from the test data
    val rpm = rpm4s.decode[RpmPrimaryEntry](BitVector.fromInputStream(getClass.getResourceAsStream("/kernel-default-4.11.8-1.2.x86_64.rpm"))).require
    val expected =
    Blocker.fromExecutorService(IO(Executors.newCachedThreadPool())).use { blocker =>
      fs2.io.readInputStream[IO](IO(getClass.getResourceAsStream("/cc_primary.xml")), 4096, blocker)
        .through(fs2.text.utf8Decode)
        .compile.toVector
        .map(_.mkString)
    }
      .unsafeRunSync()
    val checksum = Sha256.fromHex("65f2c93d2bd4178590ac46531668108389729df9f7f0a527af4f1566e0ee94e8").get
    val generated = rpm4s.repo.repomd.xml.primary.create[IO](Some(1), Stream.emit(rpm -> checksum),
      (rpe, cksum) => "kernel-default-4.11.8-1.2.x86_64.rpm"
    ).compile.toVector.map(_.mkString).unsafeRunSync()
    generated shouldBe expected
  }

  "createUpdateinfo" should "should create updateinfo.xml correctly" in {
    implicit val cs = IO.contextShift(ExecutionContext.global)
    val blocker = Blocker.liftExecutionContext(ExecutionContext.global)
    //TODO: currently this ignores the pkgid attribute of the checksum which has been manually removed from the test data
    val expected =
      Blocker.fromExecutorService(IO(Executors.newCachedThreadPool())).use { blocker =>
        fs2.io.readInputStream[IO](IO(getClass.getResourceAsStream("/repomd/updateinfo-full.xml")), 4096, blocker)
          .through(rpm4s.repo.repomd.xml.updateinfo.bytes2updates(blocker))
          .compile
          .toList
      }
      .unsafeRunSync()
      
    val generated = rpm4s.repo.repomd.xml.updateinfo.create[IO](Stream.emits(expected).covary[IO])
      .through(fs2.text.utf8Encode)
      .through(rpm4s.repo.repomd.xml.updateinfo.bytes2updates(blocker))
      .compile
      .toList
      .unsafeRunSync()
    
   
    
    generated.size shouldBe expected.size
    generated shouldBe expected
  }

}
