package rpm4s.data

sealed trait License extends Product with Serializable
sealed trait KnownLicense extends License
case class And(a: License, b: License) extends License
case class Or(a: License, b: License) extends License

// Resources
// https://fedoraproject.org/wiki/Licensing:Main?rd=Licensing
// https://en.opensuse.org/openSUSE:Packaging_guidelines
// https://fedoraproject.org/wiki/Packaging:LicensingGuidelines?rd=Packaging/LicensingGuidelines#Valid_License_Short_Names
// consider encoding additional information like license compatibility
// TODO find out if "license with ..." should be modeled as modifiers on existing licenses
// TODO find out if "... with exceptions" is a concrete license or a set of licenses
// TODO find duplicate (equivalent but differently named licenses) and normalize them
// TODO implement normalize for licences to make different format strings comparable
object License {

  implicit class LicenseOps(val license: License) extends AnyVal {
    def collectUnknown: List[UnknownLicense] = license match {
      case x@UnknownLicense(_) => List(x)
      case And(lhs, rhs) => lhs.collectUnknown ++ rhs.collectUnknown
      case Or(lhs, rhs) => lhs.collectUnknown ++ rhs.collectUnknown
      case _ => Nil
    }
  }

  private def parseLicence(v: String): Either[String, (String, License)] = {
    val v2 = v.dropWhile(_ == ' ')
    if (v2.startsWith("(")) {
      val v3 = v2.drop(1).dropWhile(_ == ' ')
      parseExpr(v3).flatMap { case (r, license) =>
        val rest = r.dropWhile(_ == ' ')
        if (rest.startsWith(")"))
          Right((rest.drop(1), license))
        else Left("missing )")
      }
    } else {
      val l = """(?i)\(|\)| or | and """.r.findFirstMatchIn(v2)
        .map( m => v2.substring(0, m.start)).getOrElse(v2).trim

      fromString(l).toRight(s"expected license but got $l").map { license =>
        (v2.drop(l.length), license)
      }
    }
  }

  private def parseOp(v: String): Either[String, (String, (License, License) => License)] = {
    v.take(3).takeWhile(_.isLetter).toLowerCase match {
      case "and" => Right((v.drop(3), And))
      case "or" => Right((v.drop(2), Or))
      case x => Left(s"expected operation and / or but got $x")
    }
  }

  private def parseExpr(v: String): Either[String, (String, License)] = {
    parseLicence(v).flatMap { case (lhsRest, lhs) =>
      parseOp(lhsRest.dropWhile(_ == ' ')) match {
        case Right((opRest, op)) =>
          parseExpr(opRest.dropWhile(_ == ' ')).map { case (rhsRest, rhs) =>
            (rhsRest, op(lhs, rhs))
          }
        case Left(_) => Right((lhsRest, lhs))
      }
    }
  }

  def parse(v: String): Either[String, License] = {
    parseExpr(v).flatMap { case (rest, l) =>
        if(rest.dropWhile(_ == ' ').nonEmpty)
          Left(s"unexpected trailing content $rest")
        else Right(l)
    }
  }

  def fromString(value: String): Option[License] = {
    name2license.get(value).orElse {
      value match {
        case ""             => None
        case unknown        => Some(UnknownLicense(unknown))
      }
    }
  }

  def format(license: License): String = license match {
    case And(lhs, rhs) => s"(${format(lhs)} and ${format(rhs)})"
    case Or(lhs, rhs) => s"(${format(lhs)} or ${format(rhs)})"
    case UnknownLicense(name) => name
    case l => l.toString
  }

  case class UnknownLicense(name: String) extends License
  case object `GPL-2.0` extends KnownLicense
  case object `GPL-2.0+` extends KnownLicense
  case object `LGPL-3.0+` extends KnownLicense
  case object `CC-BY-SA-3.0` extends KnownLicense
  case object `BSD with advertising` extends KnownLicense
  case object MIT extends KnownLicense
  case object ISC extends KnownLicense
  case object QPL extends KnownLicense
  case object Python extends KnownLicense
  case object `MPL-2.0` extends KnownLicense
  case object `BSD-3-Clause` extends KnownLicense
  case object Unicode extends KnownLicense
  case object `GPL-2.0-with-font-exception` extends KnownLicense
  case object `SUSE-MgOpen` extends KnownLicense
  case object `CC-BY-ND-2.0` extends KnownLicense
  case object `SUSE-PHP-2.02` extends KnownLicense
  case object `SUSE-Repoze` extends KnownLicense
  case object `SUSE-SLIB` extends KnownLicense
  case object `CC-BY-2.0` extends KnownLicense
  case object `CC-BY-SA-2.5` extends KnownLicense
  case object `SUSE-Ubuntu-Font-License-1.0` extends KnownLicense
  case object `Unicode-TOU` extends KnownLicense
  case object `Vim` extends KnownLicense
  case object `SUSE-Xano` extends KnownLicense
  case object `SUSE-Scrot` extends KnownLicense
  case object `SUSE-Gitslave` extends KnownLicense
  case object `CC-BY-2.5` extends KnownLicense
  case object `SUSE-IEEE` extends KnownLicense
  case object `SUSE-mplus` extends KnownLicense
  case object `SUSE-Freeware` extends KnownLicense
  case object `SUSE-SNIA-1.1` extends KnownLicense
  case object `MirOS` extends KnownLicense
  case object `OPL-1.0` extends KnownLicense
  case object `SISSL` extends KnownLicense
  case object `SUSE-TGPPL-1.0+` extends KnownLicense
  case object `SUSE-LGPL-3.0-with-openssl-exception` extends KnownLicense
  case object `SUSE-CC-Sampling-Plus-1.0` extends KnownLicense
  case object `CECILL-2.0` extends KnownLicense
  case object `SUSE-Permissive-Modify-By-Patch` extends KnownLicense
  case object `xinetd` extends KnownLicense
  case object `LPPL-1.3c+` extends KnownLicense
  case object `Artistic-1.0+` extends KnownLicense
  case object `SUSE-Arphic` extends KnownLicense
  case object `Sleepycat` extends KnownLicense
  case object `SUSE-LDPL-2.0` extends KnownLicense
  case object `SPL-1.0` extends KnownLicense
  case object `SUSE-CacertRoot` extends KnownLicense
  case object `SUSE-DMTF` extends KnownLicense
  case object `SUSE-GPL-3.0-with-template-exception` extends KnownLicense
  case object `SUSE-Oasis-Specification-Notice` extends KnownLicense
  case object `SUSE-XSL-Lint` extends KnownLicense
  case object `CC-BY-4.0` extends KnownLicense
  case object `SUSE-FHS` extends KnownLicense
  case object `CC-BY-SA-3.0+` extends KnownLicense
  case object `CC-BY-SA-1.0+` extends KnownLicense
  case object `SUSE-GPL-3.0+-with-font-exception` extends KnownLicense
  case object `FTL` extends KnownLicense
  case object `SUSE-Hack-Open-Font-2.0` extends KnownLicense
  case object `SUSE-FLTK` extends KnownLicense
  case object `IPA` extends KnownLicense
  case object `SUSE-BSD-3-Clause-with-non-nuclear-addition` extends KnownLicense
  case object `Libpng` extends KnownLicense
  case object `SUSE-OldFSFDocLicense` extends KnownLicense
  case object `PHP-3.01` extends KnownLicense
  case object `SUSE-NonFree` extends KnownLicense
  case object `ZPL-2.1` extends KnownLicense
  case object `TCL` extends KnownLicense
  case object `LPPL-1.0` extends KnownLicense
  case object `GPL-3.0+ WITH Autoconf-exception-3.0` extends KnownLicense
  case object `SUSE-Bitstream-Vera` extends KnownLicense
  case object `OFL-1.1` extends KnownLicense
  case object `SUSE-Docbook-XSL` extends KnownLicense
  case object `SUSE-Redistributable-Content` extends KnownLicense
  case object `LPPL-1.3c` extends KnownLicense
  case object `Apache-1.0` extends KnownLicense
  case object `SUSE-TeX` extends KnownLicense
  case object `SUSE-Egenix-1.1.0` extends KnownLicense
  case object `SUSE-Matplotlib` extends KnownLicense
  case object `ZPL-2.0` extends KnownLicense
  case object `LGPL-2.1 WITH OCCT-exception-1.0` extends KnownLicense
  case object `SUSE-Innernet-2.0` extends KnownLicense
  case object `Artistic-1.0-Perl` extends KnownLicense
  case object `SUSE-GPL-3.0+-with-openssl-exception` extends KnownLicense
  case object `CNRI-Python` extends KnownLicense
  case object `SUSE-SIP` extends KnownLicense
  case object `Qhull` extends KnownLicense
  case object `SUSE-LGPL-2.1-with-nokia-exception-1.1` extends KnownLicense
  case object `SUSE-GPL-2.0+-with-sane-exception` extends KnownLicense
  case object `CC-BY-NC-SA-2.5` extends KnownLicense
  case object `CC-BY-NC-SA-3.0` extends KnownLicense
  case object `GFDL-1.2+` extends KnownLicense
  case object `SUSE-SGI-FreeB-2.0` extends KnownLicense
  case object `SUSE-LGPL-2.1-with-digia-exception-1.1` extends KnownLicense
  case object `GPL-3.0-with-GCC-exception` extends KnownLicense
  case object `SUSE-GPL-2.0-with-OSI-exception` extends KnownLicense
  case object `GFDL-1.3+` extends KnownLicense
  case object `GPL-2.0 WITH GCC-exception-2.0` extends KnownLicense
  case object `SUSE-GL2PS-2.0` extends KnownLicense
  case object `SUSE-GPL-3.0-with-FLOSS-exception` extends KnownLicense
  case object `OLDAP-2.8` extends KnownLicense
  case object `Sendmail` extends KnownLicense
  case object `SUSE-GPL-2.0-with-FLOSS-exception` extends KnownLicense
  case object `NetCDF` extends KnownLicense
  case object `IJG` extends KnownLicense
  case object `APL-1.0` extends KnownLicense
  case object `SUSE-GPL-2.0-with-linking-exception` extends KnownLicense
  case object `Python-2.0` extends KnownLicense
  case object `SUSE-QWT-1.0` extends KnownLicense
  case object `Apache-2.0+` extends KnownLicense
  case object `Ruby` extends KnownLicense
  case object `OSL-2.1` extends KnownLicense
  case object `MS-PL` extends KnownLicense
  case object `CC-BY-ND-3.0` extends KnownLicense
  case object `SUSE-LGPL-2.0-with-linking-exception` extends KnownLicense
  case object `MakeIndex` extends KnownLicense
  case object `curl` extends KnownLicense
  case object `Artistic-2.0` extends KnownLicense
  case object `Artistic-1.0` extends KnownLicense
  case object `MIT-advertising` extends KnownLicense
  case object `SUSE-IDPL-1.0` extends KnownLicense
  case object `SUSE-IBPL-1.0` extends KnownLicense
  case object `SUSE-Freetype` extends KnownLicense
  case object `PostgreSQL` extends KnownLicense
  case object `AGPL-3.0+` extends KnownLicense
  case object `Unlicense` extends KnownLicense
  case object `CECILL-B` extends KnownLicense
  case object `SUSE-LGPL-2.1+-with-GCC-exception` extends KnownLicense
  case object `SGI-B-2.0` extends KnownLicense
  case object `SUSE-GPL-2.0-with-plugin-exception` extends KnownLicense
  case object `SUSE-Gnuplot` extends KnownLicense
  case object `MPL-2.0+` extends KnownLicense
  case object `SUSE-CPL-0.5` extends KnownLicense
  case object `QPL-1.0` extends KnownLicense
  case object `MPL-1.1+` extends KnownLicense
  case object `ICU` extends KnownLicense
  case object `GPL-2.0-with-classpath-exception` extends KnownLicense
  case object `Apache-1.1` extends KnownLicense
  case object `MPL-1.0` extends KnownLicense
  case object `W3C` extends KnownLicense
  case object `IPL-1.0` extends KnownLicense
  case object `X11` extends KnownLicense
  case object `BSD-4-Clause` extends KnownLicense
  case object `SUSE-Free-Art-1.3` extends KnownLicense
  case object `SUSE-GPL-2.0-with-openssl-exception` extends KnownLicense
  case object `GFDL-1.1+` extends KnownLicense
  case object `HPND` extends KnownLicense
  case object `SUSE-BSD-Mark-Modifications` extends KnownLicense
  case object `GFDL-1.2` extends KnownLicense
  case object `MPL-1.1` extends KnownLicense
  case object `CC-BY-SA-1.0` extends KnownLicense
  case object `Zlib` extends KnownLicense
  case object `GPL-1.0+` extends KnownLicense
  case object `BSL-1.0` extends KnownLicense
  case object `CC-BY-SA-4.0` extends KnownLicense
  case object `CDDL-1.0` extends KnownLicense
  case object `WTFPL` extends KnownLicense
  case object `GFDL-1.3` extends KnownLicense
  case object `SUSE-GPL-2.0+-with-openssl-exception` extends KnownLicense
  case object `CC-BY-3.0` extends KnownLicense
  case object `ClArtistic` extends KnownLicense
  case object `GPL-1.0` extends KnownLicense
  case object `NCSA` extends KnownLicense
  case object `AFL-2.1` extends KnownLicense
  case object `SUSE-GPL-3.0-with-openssl-exception` extends KnownLicense
  case object `SUSE-Copyleft-Next-0.3.0` extends KnownLicense
  case object `SUSE-Firmware` extends KnownLicense
  case object `SUSE-mirror` extends KnownLicense
  case object `GPL-3.0+` extends KnownLicense
  case object `GPL-3.0` extends KnownLicense
  case object `AGPL-3.0` extends KnownLicense
  case object `LGPL-2.1+` extends KnownLicense
  case object `LGPL-2.1` extends KnownLicense
  case object `CC0-1.0` extends KnownLicense
  case object `OML` extends KnownLicense
  case object `LGPL-2.0+` extends KnownLicense
  case object `ImageMagick` extends KnownLicense
  case object `GFDL-1.1` extends KnownLicense
  case object `BSD-2-Clause` extends KnownLicense
  case object `SUSE-wxWidgets-3.1` extends KnownLicense
  case object `EPL-1.0` extends KnownLicense
  case object `Apache-2.0` extends KnownLicense
  case object `LGPL-3.0` extends KnownLicense
  case object `SUSE-Permissive` extends KnownLicense
  case object `CPL-1.0` extends KnownLicense
  case object `SUSE-Public-Domain` extends KnownLicense
  case object `OpenSSL` extends KnownLicense
  case object `GPL-2.0-with-GCC-exception` extends KnownLicense
  case object `SUSE-Python-1.6` extends KnownLicense
  case object `LGPL-2.0` extends KnownLicense
  case object `BitTorrent-1.1` extends KnownLicense
  case object `SUSE-Sun-Laboratories` extends KnownLicense

  val allKnownLicenses: List[KnownLicense] = List(
    `GPL-2.0`,
    `GPL-2.0+`,
    `LGPL-3.0+`,
    `CC-BY-SA-3.0`,
    `BSD with advertising`,
    MIT,
    ISC,
    QPL,
    Python,
    `MPL-2.0`,
    `BSD-3-Clause`,
    Unicode,
    `GPL-2.0-with-font-exception`,
    `SUSE-MgOpen`,
    `CC-BY-ND-2.0`,
    `SUSE-PHP-2.02`,
    `SUSE-Repoze`,
    `SUSE-SLIB`,
    `CC-BY-2.0`,
    `CC-BY-SA-2.5`,
    `SUSE-Ubuntu-Font-License-1.0`,
    `Unicode-TOU`,
    `Vim`,
    `SUSE-Xano`,
    `SUSE-Scrot`,
    `SUSE-Gitslave`,
    `CC-BY-2.5`,
    `SUSE-IEEE`,
    `SUSE-mplus`,
    `SUSE-Freeware`,
    `SUSE-SNIA-1.1`,
    `MirOS`,
    `OPL-1.0`,
    `SISSL`,
    `SUSE-TGPPL-1.0+`,
    `SUSE-LGPL-3.0-with-openssl-exception`,
    `SUSE-CC-Sampling-Plus-1.0`,
    `CECILL-2.0`,
    `SUSE-Permissive-Modify-By-Patch`,
    `xinetd`,
    `LPPL-1.3c+`,
    `Artistic-1.0+`,
    `SUSE-Arphic`,
    `Sleepycat`,
    `SUSE-LDPL-2.0`,
    `SPL-1.0`,
    `SUSE-CacertRoot`,
    `SUSE-DMTF`,
    `SUSE-GPL-3.0-with-template-exception`,
    `SUSE-Oasis-Specification-Notice`,
    `SUSE-XSL-Lint`,
    `CC-BY-4.0`,
    `SUSE-FHS`,
    `CC-BY-SA-3.0+`,
    `CC-BY-SA-1.0+`,
    `SUSE-GPL-3.0+-with-font-exception`,
    `FTL`,
    `SUSE-Hack-Open-Font-2.0`,
    `SUSE-FLTK`,
    `IPA`,
    `SUSE-BSD-3-Clause-with-non-nuclear-addition`,
    `Libpng`,
    `SUSE-OldFSFDocLicense`,
    `PHP-3.01`,
    `SUSE-NonFree`,
    `ZPL-2.1`,
    `TCL`,
    `LPPL-1.0`,
    `GPL-3.0+ WITH Autoconf-exception-3.0`,
    `SUSE-Bitstream-Vera`,
    `OFL-1.1`,
    `SUSE-Docbook-XSL`,
    `SUSE-Redistributable-Content`,
    `LPPL-1.3c`,
    `Apache-1.0`,
    `SUSE-TeX`,
    `SUSE-Egenix-1.1.0`,
    `SUSE-Matplotlib`,
    `ZPL-2.0`,
    `LGPL-2.1 WITH OCCT-exception-1.0`,
    `SUSE-Innernet-2.0`,
    `Artistic-1.0-Perl`,
    `SUSE-GPL-3.0+-with-openssl-exception`,
    `CNRI-Python`,
    `SUSE-SIP`,
    `Qhull`,
    `SUSE-LGPL-2.1-with-nokia-exception-1.1`,
    `SUSE-GPL-2.0+-with-sane-exception`,
    `CC-BY-NC-SA-2.5`,
    `CC-BY-NC-SA-3.0`,
    `GFDL-1.2+`,
    `SUSE-SGI-FreeB-2.0`,
    `SUSE-LGPL-2.1-with-digia-exception-1.1`,
    `GPL-3.0-with-GCC-exception`,
    `SUSE-GPL-2.0-with-OSI-exception`,
    `GFDL-1.3+`,
    `GPL-2.0 WITH GCC-exception-2.0`,
    `SUSE-GL2PS-2.0`,
    `SUSE-GPL-3.0-with-FLOSS-exception`,
    `OLDAP-2.8`,
    `Sendmail`,
    `SUSE-GPL-2.0-with-FLOSS-exception`,
    `NetCDF`,
    `IJG`,
    `APL-1.0`,
    `SUSE-GPL-2.0-with-linking-exception`,
    `Python-2.0`,
    `SUSE-QWT-1.0`,
    `Apache-2.0+`,
    `Ruby`,
    `SUSE-mirror`,
    `OSL-2.1`,
    `MS-PL`,
    `CC-BY-ND-3.0`,
    `SUSE-LGPL-2.0-with-linking-exception`,
    `MakeIndex`,
    `curl`,
    `Artistic-2.0`,
    `Artistic-1.0`,
    `MIT-advertising`,
    `SUSE-IDPL-1.0`,
    `SUSE-IBPL-1.0`,
    `SUSE-Freetype`,
    `PostgreSQL`,
    `AGPL-3.0+`,
    `Unlicense`,
    `CECILL-B`,
    `SUSE-LGPL-2.1+-with-GCC-exception`,
    `SGI-B-2.0`,
    `SUSE-GPL-2.0-with-plugin-exception`,
    `SUSE-Gnuplot`,
    `MPL-2.0+`,
    `SUSE-CPL-0.5`,
    `QPL-1.0`,
    `MPL-1.1+`,
    `ICU`,
    `GPL-2.0-with-classpath-exception`,
    `Apache-1.1`,
    `MPL-1.0`,
    `W3C`,
    `IPL-1.0`,
    `X11`,
    `BSD-4-Clause`,
    `SUSE-Free-Art-1.3`,
    `SUSE-GPL-2.0-with-openssl-exception`,
    `GFDL-1.1+`,
    `HPND`,
    `SUSE-BSD-Mark-Modifications`,
    `GFDL-1.2`,
    `MPL-1.1`,
    `CC-BY-SA-1.0`,
    `Zlib`,
    `GPL-1.0+`,
    `BSL-1.0`,
    `CC-BY-SA-4.0`,
    `CDDL-1.0`,
    `WTFPL`,
    `GFDL-1.3`,
    `SUSE-GPL-2.0+-with-openssl-exception`,
    `CC-BY-3.0`,
    `ClArtistic`,
    `GPL-1.0`,
    `NCSA`,
    `AFL-2.1`,
    `SUSE-GPL-3.0-with-openssl-exception`,
    `SUSE-Copyleft-Next-0.3.0`,
    `SUSE-Firmware`,
    `GPL-3.0+`,
    `GPL-3.0`,
    `AGPL-3.0`,
    `LGPL-2.1+`,
    `LGPL-2.1`,
    `CC0-1.0`,
    `OML`,
    `LGPL-2.0+`,
    `ImageMagick`,
    `GFDL-1.1`,
    `BSD-2-Clause`,
    `SUSE-wxWidgets-3.1`,
    `EPL-1.0`,
    `Apache-2.0`,
    `LGPL-3.0`,
    `SUSE-Permissive`,
    `CPL-1.0`,
    `SUSE-Public-Domain`,
    `OpenSSL`,
    `GPL-2.0-with-GCC-exception`,
    `SUSE-Python-1.6`,
    `LGPL-2.0`,
    `BitTorrent-1.1`,
    `SUSE-Sun-Laboratories`
  )

  val name2license: Map[String, KnownLicense] = allKnownLicenses.map(
    license => license.toString -> license
  ).toMap

}
