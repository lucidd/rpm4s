import java.nio.file.{Files, Paths}
import java.time.Instant

import cats.effect.IO
import org.http4s.Uri
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import rpm4s.data.Checksum.Sha256
import rpm4s.data._
import rpm4s.repo.data.Bytes
import rpm4s.repo.repomd.Data.Primary
import rpm4s.repo.repomd._
import rpm4s.repo.utils.xml.xmlevents
import scodec.bits.{BitVector, ByteVector}
import fs2.{Pure, Stream}
import rpm4s.repo.repomd.xml.primary.{PackageF, SizeInfo, xml2packages}


class RepomdSpec
    extends FlatSpec
    with Matchers
    with PropertyChecks {

  "repomd.xml" should "get parsed correctly" in {
    val r  = xmlevents[IO](getClass.getResourceAsStream("/repomd/repomd.xml"))
        .through(rpm4s.repo.repomd.xml2repomd)
        .compile.last.unsafeRunSync()

    r shouldEqual Some(
      RepoMdF[cats.Id](
        revision = 1504283245,
        primary = Primary(
          location = "repodata/03b53138f32575f30326c7028857cb013b709570614b7bb6190ff3ec2eac3ea7-primary.xml.gz",
          checksum = Sha256.fromHex("03b53138f32575f30326c7028857cb013b709570614b7bb6190ff3ec2eac3ea7").get,
          timestamp = Instant.ofEpochSecond(1504285261),
          size = Bytes(17781467),
          openSize = Bytes(139577942),
          openChecksum = Sha256.fromHex("baea56d3261c44c07b609658b09fa56bd4c8ce939f74d1ae08deff152b7ca010").get,
        )
      )
    )

  }

  "primary.xml" should "get parsed correctly" in {
    val r  = xmlevents[IO](
      getClass.getResourceAsStream("/repomd/primary.xml")
    ).through(xml2packages)
     .compile.toVector.unsafeRunSync()

    //TODO: this is missing source rpm test case
    val expected = Vector(
      PackageF[cats.Id](
        name = Name("0ad").toOption.get,
        arch = Some(Architecture.i586),
        version = Version.fromString("0.0.22").toOption.get,
        epoch = None,
        release = Release.fromString("1.2").toOption.get,
        checksum = Sha256.fromHex("526e7af8c90ef5a41a7fb17d7a42277e06727f3bbb443cc795819e0feb1e5021").get,
        size = SizeInfo(
          pack = Bytes(5640346),
          archive = Bytes(18877364),
          installed = Bytes(18874672)
        ),
        loc = Uri.unsafeFromString("i586/0ad-0.0.22-1.2.i586.rpm")
      ),
      PackageF[cats.Id](
        name = Name("389-ds").toOption.get,
        arch = Some(Architecture.i586),
        version = Version.fromString("1.3.6.6").toOption.get,
        epoch = None,
        release = Release.fromString("2.1").toOption.get,
        checksum = Sha256.fromHex("bc7b674119d3c37c2b595567a583cf41dc3296f944871f076e184db5525955e1").get,
        size = SizeInfo(
          pack = Bytes(2152714),
          archive = Bytes(7548544),
          installed = Bytes(7486047)
        ),
        loc = Uri.unsafeFromString("i586/389-ds-1.3.6.6-2.1.i586.rpm")
      ),
      PackageF[cats.Id](
        name = Name("389-ds-devel").toOption.get,
        arch = Some(Architecture.i586),
        version = Version.fromString("1.3.6.6").toOption.get,
        epoch = None,
        release = Release.fromString("2.1").toOption.get,
        checksum = Sha256.fromHex("9e231c26fd27defa79da040c0249091b6f44fd32b1b3d34e97d423e14a83643e").get,
        size = SizeInfo(
          pack = Bytes(115302),
          archive = Bytes(423752),
          installed = Bytes(421061)
        ),
        loc = Uri.unsafeFromString("i586/389-ds-devel-1.3.6.6-2.1.i586.rpm")
      )
    )

    r shouldEqual expected
  }

  "createPrimary" should "should create primary.xml correctly" in {
    //TODO: currently this ignores the pkgid attribute of the checksum which has been manually removed from the test data
    val rpm = rpm4s.decode[RpmPrimaryEntry](BitVector.fromInputStream(getClass.getResourceAsStream("/kernel-default-4.11.8-1.2.x86_64.rpm"))).require
    val expected = fs2.io.readInputStream[IO](IO(getClass.getResourceAsStream("/cc_primary.xml")),4096)
      .through(fs2.text.utf8Decode)
      .compile.toVector
      .map(_.mkString)
      .unsafeRunSync()
    val checksum = Sha256.fromHex("65f2c93d2bd4178590ac46531668108389729df9f7f0a527af4f1566e0ee94e8").get
    val generated = rpm4s.repo.repomd.xml.primary.create[IO](Some(1), Stream.emit(rpm -> checksum),
      (rpe, cksum) => "kernel-default-4.11.8-1.2.x86_64.rpm"
    ).compile.toVector.map(_.mkString).unsafeRunSync()
    generated shouldBe expected
  }

}
