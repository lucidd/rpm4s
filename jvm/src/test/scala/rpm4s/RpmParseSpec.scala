package rpm4s

import org.scalacheck.Arbitrary
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import rpm4s.codecs.IndexData.StringData
import scodec.{Attempt, Codec}
import scodec.bits.BitVector
import rpm4s.codecs._
import rpm4s.data.Dependency.{Conflicts, Obsoletes, Provides, Requires}
import rpm4s.data._

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

  "payload extractor" should "extract rpm payload correctly" in {
    import scodec.bits._
    val bits = BitVector.fromInputStream(
      getClass.getResourceAsStream("/kernel-default-4.11.8-1.2.x86_64.rpm"))
    val rpe = rpm4s.decode[Payload](bits).require
    val sha256 = hex"a6c622dcbfc90c84332966370b73ce54cbd5388bf8561e28338b23529627572b".bits
    rpe.bitVector.digest("SHA-256") shouldBe sha256
  }

  "lead extractor" should "extract rpm lead correctly" in {
    import scodec.bits._
    val bits = BitVector.fromInputStream(
      getClass.getResourceAsStream("/kernel-default-4.11.8-1.2.x86_64.rpm"))
    val lead = rpm4s.decode[Lead](bits).require
    lead shouldBe Lead(3, 0, RPMType.Binary, 1, "kernel-default-4.11.8-1.2", OS.Linux, 5)
  }

  "rpm.decode" should "correctly decode RpmPrimaryEntry" in {
    val bits = BitVector.fromInputStream(
      getClass.getResourceAsStream("/kernel-default-4.11.8-1.2.x86_64.rpm"))
    val rpe = rpm4s.decode[RpmPrimaryEntry](bits).require
    rpe.architecture shouldBe Architecture.x86_64
    rpe.name shouldBe Name.fromString("kernel-default").toOption.get
    rpe.version shouldBe Version.parse("4.11.8").toOption.get

    rpe.vendor shouldBe Vendor("openSUSE")
    rpe.license shouldBe License.`GPL-2.0`

    rpe.description shouldBe Description(
      Map(
        "C" ->
          """The standard kernel for both uniprocessor and multiprocessor systems.


Source Timestamp: 2017-06-29 16:37:33 +0200
GIT Revision: 42bd7a027035420d318d4cb5a3db7233aff32b44
GIT Branch: stable"""
      )
    )

    rpe.summery shouldBe Summery(
      Map(
        "C" -> """The Standard Kernel"""
      )
    )

    rpe.group shouldBe Group(
      Map(
        "C" -> """System/Kernel"""
      )
    )

    rpe.headerRange shouldBe HeaderRange(5016, 1736491)

    rpe.provides.size shouldBe 202
    rpe.provides(0) shouldBe Provides(RpmRef(Name("ath3k-firmware").toOption.get, Some(EVR.parse("1.0").toOption.get), SenseFlags(8)))
    rpe.provides(23) shouldBe Provides(VirtualRef("firmware(4.11.8-1-default/3com/typhoon.bin)", None, SenseFlags(0)))
    rpe.provides(160) shouldBe Provides(VirtualRef("kernel-default(x86-64)", Some("4.11.8-1.2"), SenseFlags(8)))

    rpe.fileEntries.map(_.size) shouldBe Some(4976)
    rpe.fileEntries.map(_(0)) shouldBe Some(FileEntry("/boot/.vmlinuz-4.11.8-1-default.hmac", Stat.fromShort(-32348).get, FileFlags(0)))
    rpe.fileEntries.map(_(2488)) shouldBe Some(FileEntry("/lib/modules/4.11.8-1-default/kernel/drivers/net/ethernet/chelsio/cxgb4vf", Stat.fromShort(16877).get, FileFlags(0)))
    rpe.fileEntries.map(_(4975)) shouldBe Some(FileEntry("/lib/modules/4.11.8-1-default/vdso/vdsox32.so", Stat.fromShort(-32275).get, FileFlags(0)))

    rpe.requires.size shouldBe 20
    rpe.requires(0) shouldBe Requires(VirtualRef("/bin/sh", None, SenseFlags(512)))
    rpe.requires(8) shouldBe Requires(RpmRef(Name("awk").toOption.get, None, SenseFlags(512)))
    rpe.requires(18) shouldBe Requires(VirtualRef("rpmlib(PayloadIsLzma)", Some("4.4.6-1"), SenseFlags(16777226)))

    rpe.enhances.size shouldBe 0

    rpe.conflicts.size shouldBe 6
    rpe.conflicts(0) shouldBe Conflicts(RpmRef(Name("apparmor-parser").toOption.get, Some(EVR.parse("2.3").toOption.get), SenseFlags(2)))
    rpe.conflicts(3) shouldBe Conflicts(RpmRef(Name("lvm2").toOption.get, Some(EVR.parse("2.02.33").toOption.get), SenseFlags(2)))
    rpe.conflicts(5) shouldBe Conflicts(RpmRef(Name("udev").toOption.get, Some(EVR.parse("118").toOption.get), SenseFlags(2)))

    rpe.obsoletes.size shouldBe 49
    rpe.obsoletes(0) shouldBe Obsoletes(RpmRef(Name("ath3k-firmware").toOption.get, Some(EVR.parse("1.0").toOption.get), SenseFlags(10)))
    rpe.obsoletes(24) shouldBe Obsoletes(RpmRef(Name("kernel-default-base").toOption.get, Some(EVR.parse("3.1").toOption.get), SenseFlags(2)))
    rpe.obsoletes(48) shouldBe Obsoletes(RpmRef(Name("xen-kmp-default").toOption.get, Some(EVR.parse("4.6.1").toOption.get), SenseFlags(10)))

    rpe.recommends.size shouldBe 0

    rpe.supplements.size shouldBe 0

    rpe.suggests.size shouldBe 0

  }

}
