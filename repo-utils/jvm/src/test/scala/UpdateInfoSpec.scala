import java.time.Instant

import cats.effect.IO
import org.scalatest._
import rpm4s.data._
import rpm4s.repo.repomd.xml.updateinfo.xml2updates
import rpm4s.repo.data.updateinfo.UpdateF
import rpm4s.repo.data.updateinfo.UpdateF._
import rpm4s.repo.utils.xml.xmlevents
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class UpdateInfoSpec
    extends FlatSpec
    with Matchers
    with ScalaCheckPropertyChecks {

  "repomd/updateinfo.xml" should "get parsed correctly" in {
    val r  = xmlevents[IO](
      getClass.getResourceAsStream("/repomd/updateinfo.xml")
    ).through(xml2updates)
     .compile.toVector.unsafeRunSync()

    val expected = Vector(
      UpdateF[cats.Id](
        id = "openSUSE-2017-1025",
        from = "maint-coord@suse.de",
        title = "Recommended update for openconnect",
        version = "1",
        severity = Severity.Moderate,
        tpe = UpdateType.Recommended,
        references = Set(
          Bugzilla(
            href = "https://bugzilla.opensuse.org/show_bug.cgi?id=1056389",
            title = "Openconnect 7.06 fails on connect to Junos Pulse",
            id = "1056389"
          ),
          CVERef(
            href = "http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2017-2295",
            title = "http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2017-2295",
            cve = CVE.fromString("CVE-2017-2295").get
          ),
          Fate(
            href = "https://features.opensuse.org/323554",
            id = "323554",
            title = "[TRACKERBUG] FATE#323554: [ECO] FIPS: include libkcapi library and tools and use them in dracut-fips"
          )
        ),
        packages = Set(
          PackageF[cats.Id](
            name = Name("openconnect").toOption.get,
            epoch = None,
            version = Version.fromString("7.08").toOption.get,
            release = Release.fromString("7.1").toOption.get,
            arch = Architecture.Src,
            src = "src/openconnect-7.08-7.1.src.rpm",
            filename = "openconnect-7.08-7.1.src.rpm",
            rebootSuggested = false,
            reloginSuggested = false,
            restartSuggested = false
          ),
          PackageF[cats.Id](
            name = Name("openconnect").toOption.get,
            epoch = None,
            version = Version.fromString("7.08").toOption.get,
            release = Release.fromString("7.1").toOption.get,
            arch = Architecture.x86_64,
            src = "x86_64/openconnect-7.08-7.1.x86_64.rpm",
            filename = "openconnect-7.08-7.1.x86_64.rpm",
            rebootSuggested = false,
            reloginSuggested = false,
            restartSuggested = false
          )
        ),
        release = "openSUSE Leap 42.3 Update",
        issued = Instant.ofEpochSecond(1504936174),
        status = Status.Stable,
        description = "This update for openconnect fixes the following issues:\n\n- openconnect would fail to connect to a Junos Pulse gateway (bsc#1056389)\n\nThis update to version 7.08 also contains the following improvements and fixes:\n\n- Various cryptography related improvements and fixed\n- Improved support for Cisco DTLS, Pulse Secure 8.2R5, OpenVPN, Juniper VPN\n"
      ),
      UpdateF[cats.Id](
        from = "maint-coord@suse.de",
        status = Status.Stable,
        tpe = UpdateType.Security,
        version = "1",
        id = "openSUSE-2017-840",
        title = "Security update for Wireshark",
        severity = Severity.Important,
        release = "openSUSE Leap 42.3 Update",
        issued = Instant.ofEpochSecond(1501012170),
        description = "This update to Wireshark 2.2.8 fixes some minor vulnerabilities could be used\n      to trigger dissector crashes, infinite loops, or cause excessive use of memory\n      resources by making Wireshark read specially crafted packages from the network\n      or a capture file:\n\n      - CVE-2017-7702,CVE-2017-11410: WBMXL dissector infinite loop (wnpa-sec-2017-13)\n      - CVE-2017-9350,CVE-2017-11411: openSAFETY dissector memory exhaustion (wnpa-sec-2017-28)\n      - CVE-2017-11408: AMQP dissector crash (wnpa-sec-2017-34)\n      - CVE-2017-11407: MQ dissector crash (wnpa-sec-2017-35)\n      - CVE-2017-11406: DOCSIS infinite loop (wnpa-sec-2017-36)",
        references = Set(
          Bugzilla(
            href = "https://bugzilla.opensuse.org/show_bug.cgi?id=1049255",
            title = "VUL-0: wireshark: multiple vulnerabilties fixed in 2.2.8, 2.0.14",
            id = "1049255"
          ),
          CVERef(
            href = "http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2017-7702",
            title = "http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2017-7702",
            cve = CVE.fromString("CVE-2017-7702").get
          ),
        ),
        packages = Set(
          PackageF[cats.Id](
            name = Name("wireshark").toOption.get,
            epoch = Some(Epoch.fromInt(4).toOption.get),
            version = Version.fromString("2.2.8").toOption.get,
            release = Release.fromString("17.1").toOption.get,
            arch = Architecture.Src,
            src = "src/wireshark-2.2.8-17.1.src.rpm",
            filename = "wireshark-2.2.8-17.1.src.rpm",
            rebootSuggested = false,
            reloginSuggested = false,
            restartSuggested = false
          )
        )
      )
    )

    r shouldEqual expected
  }

}
