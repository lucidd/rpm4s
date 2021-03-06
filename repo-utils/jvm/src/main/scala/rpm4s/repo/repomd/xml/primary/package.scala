package rpm4s.repo.repomd.xml

import javax.xml.namespace.QName
import javax.xml.stream.events.{StartElement, XMLEvent}
import cats.effect.{Blocker, ConcurrentEffect, ContextShift, Effect}
import fs2.{Pipe, Pull, RaiseThrowable, Stream}
import org.http4s.Uri
import rpm4s.data.Dependency.Requires
import rpm4s.data.SenseFlags.Sense
import rpm4s.data.Stat.FileType
import rpm4s.data._
import rpm4s.repo.data.Bytes
import rpm4s.repo.repomd.checksum2type
import rpm4s.repo.data.primary._
import rpm4s.repo.data.primary.PackageF.PackageBuilder
import rpm4s.repo.utils.xml.{EndEvent, StartEvent, xmlevents}
import cats._
import cats.implicits._
import cats.syntax._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.xml.Utility

package object primary {

  private val sizePackage = new QName("package")
  private val sizeInstalled = new QName("installed")
  private val sizeArchive = new QName("archive")
  private val hrefAttr = new QName("href")
  private val epochAttr = new QName("epoch")
  private val verAttr = new QName("ver")
  private val relAttr = new QName("rel")

  private def sense2xml(sense: Sense): String = sense match {
    case Sense.Any => ""
    case Sense.EQ  => """ flags="EQ""""
    case Sense.GE  => """ flags="GE""""
    case Sense.GT  => """ flags="GT""""
    case Sense.LE  => """ flags="LE""""
    case Sense.LT  => """ flags="LT""""
  }


  // this is basically a copy of EVR.parse with createrepos broken logic
  private def parse(evr: String): (String, Option[String], Option[String]) = {
    /*
      This is wrong but its how createrepo does it. In any case rhe repomd format
      is incapable of properly handling non rpm dependencies that do not conform to rpm
      name evr formatting.
     */
    val relIdx = evr.indexOf("-")

    val (release, ev) = if (relIdx == -1) {
      (None, evr)
    } else {
      (Some(evr.substring(relIdx + 1)), evr.substring(0, relIdx))
    }

    val epochIdx = ev.indexOf(":")
    val (epoch, version) = if (epochIdx == -1) {
      (None, ev)
    } else {
      (Some(ev.substring(0, epochIdx)), ev.substring(epochIdx + 1))
    }
    (version, epoch, release)
  }

  private def pkgref2xml(pkgRef: PkgRef): String = {
    val (name, epoch, version, release) = pkgRef match {
      case RpmRef(name, evr, flags) =>
        (name.value, evr.map(_.epoch).map(_.value.toString), evr.map(_.version.string), evr.flatMap(_.release).map(_.value))
      case VirtualRef(name, None, flags) =>
        (name, None, None, None)
      case VirtualRef(name, Some(version), flags) =>
        val (v, e, r) = parse(version)
        (name, e, Some(v), r)

    }
    val evr = version.map { v =>
      val rel = release.map(r => s""" rel="$r"""").getOrElse("")
      val ep = s""" epoch="${epoch.getOrElse("0")}""""
      val sense = sense2xml(pkgRef.flags.sense)
      val ver = s""" ver="$v""""
      s"""$sense$ep$ver$rel"""
    }.getOrElse("")

    val pre = if (pkgRef.flags.isPreReq) """ pre="1"""" else ""
    s"""<rpm:entry name="${Utility.escape(name)}"$evr$pre/>"""
  }

  private def rpm2xml(
    rpm: RpmPrimaryEntry,
    checksum: Checksum,
    location: String
  )(sb: StringBuilder): StringBuilder = {
    val name = rpm.name.value
    val arch = Architecture.toRpmString(rpm.architecture)
    val epoch = rpm.epoch.value.toString
    val version = rpm.version.string
    val release = rpm.release.value
    val ckType = checksum2type(checksum)
    val ckString = checksum.toHex.toLowerCase
    val summery = rpm.summery.locales.values.head
    val description = rpm.description.locales.values.head
    //val fileTime = ""
    val buildTime = rpm.buildtime.map(_.time.toEpochMilli / 1000).get
    //val packageSize = ""
    //val installedSize = ""
    //val archiveSize = ""
    val filePath = location
    val vendor = rpm.vendor.value
    val group = rpm.group.locales.values.head
    val headerStart = rpm.headerRange.start
    val headerEnd = rpm.headerRange.end
    import scala.xml._

    sb.append("""<package type="rpm">""")
    sb.append(s"<name>$name</name>")
    sb.append(s"<arch>$arch</arch>")
    sb.append(s"""<version epoch="$epoch" ver="$version" rel="$release"/>""")
    sb.append(s"""<checksum type="$ckType">$ckString</checksum>""")
    sb.append(s"<summary>${Utility.escape(summery)}</summary>")

    sb.append(s"<description>${Utility.escape(description)}</description>")
    //sb.append(s"<packager>$summery</packager>")
    //sb.append(s"<url>$summery</url>")
    //TODO: filetime should be set correctly at some point but not sure what even uses that
    sb.append(s"""<time file="$buildTime" build="$buildTime"/>""")
    //sb.append(s"""<size package="$packageSize" installed="$installedSize" archive="$archiveSize"/>""")
    sb.append(s"""<location href="${Utility.escape(filePath)}"/>""")
    sb.append("<format>")
    sb.append(
      s"<rpm:license>${Utility.escape(License.format(rpm.license))}</rpm:license>"
    )
    sb.append(s"<rpm:vendor>${Utility.escape(vendor)}</rpm:vendor>")
    sb.append(s"<rpm:group>${Utility.escape(group)}</rpm:group>")
    sb.append(s"""<rpm:header-range start="$headerStart" end="$headerEnd"/>""")

    def pkgRefs(pkgRefs: Vector[PkgRef], tag: String): Unit =
      if (pkgRefs.nonEmpty) {
        sb.append(s"<rpm:$tag>")
        pkgRefs
          .filter(_.rpmLib.isEmpty)
          .map(pkgref2xml)
          //filter out dependencies that result in the exact same xml
          .distinct
          .foreach(sb.append)
        sb.append(s"</rpm:$tag>")
        ()
      }

    //TODO: write a test for this
    //NOTE: this tries to implement the glibc collapse option https://github.com/pnasrat/yum/blob/master/yum/packages.py#L1225
    def collapseGLIBC(requires: Vector[Requires]): Vector[Requires] = {
      val (glibc, rest) =
        requires.partition(_.ref.nameString.startsWith("libc.so.6"))
      //TODO: quick and dirty solution used by createrepo refactor later
      val x = glibc.sortBy { r =>
        val start = r.ref.nameString.indexOf("(")
        val end = r.ref.nameString.indexOf("(", start)
        val version = r.ref.nameString.slice(start + 1, end)
        Version.fromString(version).toOption.get
      }(
        implicitly[Ordering[Version]].reverse
      )

      @tailrec
      def selectBest(r: Vector[Requires]): Option[Requires] =
        if (r.isEmpty) None
        else {
          val head = r.head
          val tail = r.tail
          if (head.ref.nameString != "libc.so.6()" || tail.isEmpty) Some(head)
          else selectBest(tail)
        }
      rest ++ selectBest(x)
    }

    //TODO: https://github.com/pnasrat/yum/blob/master/yum/packages.py#L1242
    val provideRefs = rpm.provides.map(_.ref)
    pkgRefs(provideRefs, "provides")
    pkgRefs(
      rpm.requires
        // filter out requires that are also present in provides
        //TODO: this should probably include all self provided dependencies
        .filterNot(r => provideRefs.contains(r.ref))
        .map(_.ref), "requires"
    )
    pkgRefs(rpm.conflicts.map(_.ref), "conflicts")
    pkgRefs(rpm.obsoletes.map(_.ref), "obsoletes")

    pkgRefs(rpm.enhances.map(_.ref), "enhances")
    pkgRefs(rpm.supplements.map(_.ref), "supplements")
    pkgRefs(rpm.suggests.map(_.ref), "suggests")
    pkgRefs(rpm.recommends.map(_.ref), "recommends")

    rpm.fileEntries.getOrElse(Vector.empty)
      .filter { e =>
        //TODO: the reason for those filters is that some files like binaries are used for requires. Find out exact files / directories here
        e.path.startsWith("/bin/") ||
        e.path.startsWith("/sbin/") ||
        e.path.startsWith("/etc/") ||
        e.path.startsWith("/usr/bin/") ||
        e.path.startsWith("/usr/sbin/")
      }
      .foreach { fe =>
        val tpe = if (fe.mode.tpe == FileType.Directory) {
          """ type="dir""""
        } else if (fe.flags.containsAll(FileFlags.Ghost)) {
          """ type="ghost""""
        } else ""
        sb.append(s"<file$tpe>${Utility.escape(fe.path)}</file>")
      }

    sb.append("</format>")
    sb.append("</package>")
    sb
  }

  def create[F[_]](
    count: Option[Long],
    rpms: Stream[F, (RpmPrimaryEntry, Checksum)],
    nameFn: (RpmPrimaryEntry, Checksum) => String
  ): Stream[F, String] = {
    val pkgCount = count.map(c => s""" packages="$c"""").getOrElse("")
    Stream.emit("<?xml version=\"1.0\" encoding=\"UTF-8\"?>") ++
      Stream.emit(
        s"""<metadata xmlns="http://linux.duke.edu/metadata/common" xmlns:rpm="http://linux.duke.edu/metadata/rpm"$pkgCount>""") ++
      rpms.map {
        case (rpm, checksum) =>
          val sb = new StringBuilder
          val result =
            rpm2xml(
              rpm,
              checksum,
              nameFn(rpm, checksum)
            )(sb)
            .toString
          result
      } ++
      Stream.emit("</metadata>")
  }

  def bytes2packages[F[_]: ConcurrentEffect: ContextShift](blocker: Blocker): Pipe[F, Byte, PackageF.Package] =
    _.through(fs2.io.toInputStream[F])
      .flatMap(is => xmlevents(is, blocker))
      .through(xml2packages)


  def xml2packages[F[_]: RaiseThrowable]: Pipe[F, XMLEvent, PackageF.Package] = { h =>
    def size(e: StartElement): SizeInfo = {
      //TODO: unsafe toLong
      val pack = e.getAttributeByName(sizePackage).getValue.toLong
      val installed = e.getAttributeByName(sizeInstalled).getValue.toLong
      val archive = e.getAttributeByName(sizeArchive).getValue.toLong
      SizeInfo(Bytes(pack), Bytes(installed), Bytes(archive))
    }

    def pack(h: Stream[F, XMLEvent], acc: PackageBuilder)
    : Pull[F, Nothing, (PackageF.Package, Stream[F, XMLEvent])] = {
      h.pull.uncons1.flatMap {
        case None => Pull.raiseError(new RuntimeException("premature end of xml."))
        case Some((event, h1)) =>
          event match {
            case StartEvent(se) => {
              se.getName.getLocalPart match {
                case "checksum" =>
                  checksum(se, h1).flatMap {
                    case Some((cs, h2)) =>
                      pack(h2, acc.copy[Option](checksum = Some(cs)))
                    case None => Pull.raiseError(new RuntimeException("expected checksum"))
                  }
                case "size" => {
                  pack(h1, acc.copy(size = Some(size(se))))
                }
                case "version" => {
                  val version = Version.fromString(se.getAttributeByName(verAttr).getValue)
                  val epochString = se.getAttributeByName(epochAttr).getValue
                  val epoch = Epoch.fromString(epochString)
                  val release = Release.fromString(se.getAttributeByName(relAttr).getValue)
                  (version, epoch, release) match {
                    case (Right(v), Right(e), Right(r)) =>
                      pack(h1, acc.copy(
                        version = Some(v),
                        epoch = Some(e),
                        release = Some(r)
                      ))
                    case _ => Pull.raiseError(new RuntimeException("expected arch"))
                  }
                }
                case "arch" => {
                  text(h1).flatMap {
                    case Some((text, h2)) =>
                      val arch = Architecture.fromString(text).get
                      pack(h2, acc.copy(arch = Some(arch)))
                    case None => Pull.raiseError(new RuntimeException("expected arch"))
                  }
                }
                case "name" => {
                  text(h1).flatMap {
                    case Some((text, h2)) =>
                      pack(h2, acc.copy(name = Name(text).toOption))
                    case None => Pull.raiseError(new RuntimeException("expected name"))
                  }
                }
                case "location" => {
                  val loc = se.getAttributeByName(hrefAttr).getValue
                  pack(h1, acc.copy(loc = Some(loc)))
                }
                case _ => pack(h1, acc)
              }
            }
            case EndEvent(ee) => {
              ee.getName.getLocalPart match {
                case "package" =>
                  Pull.pure((PackageBuilder.build(acc).get, h1))
                case _ => pack(h1, acc)
              }
            }
            case _ => pack(h1, acc)
          }
      }
    }

    def go(h: Stream[F, XMLEvent]): Pull[F, PackageF.Package, Unit] = {
      h.pull.uncons1.flatMap {
        case None =>
          Pull.done
        case Some((event, h1)) =>
          event match {
            case StartEvent(se) => {
              se.getName.getLocalPart match {
                case "package" =>
                  //val n = se.getAttributeByName(typeAttr).getValue
                  pack(h1, PackageBuilder()).flatMap {
                    case (p, h) =>
                      Pull.output1(p) >> go(h)
                  }
                case _ => go(h1)
              }
            }
            case _ => go(h1)
          }
      }
    }

    go(h).stream
  }


}
