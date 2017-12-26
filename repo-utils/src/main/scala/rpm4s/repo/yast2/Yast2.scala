package rpm4s.repo.yast2

import cats._
import cats.implicits._
import fs2.{Pipe, Pull, Stream}
import rpm4s.data.Checksum.{Sha1, Sha256}
import rpm4s.data.{Architecture, Checksum, Name}
import rpm4s.repo.data.Bytes

object Yast2 {

  case class Size(pack: Bytes, installed: Bytes)
  case class PackageF[F[_]](
      name: F[Name],
      arch: F[Architecture],
      loc: F[Location],
      checksum: F[Checksum],
      size: F[Size]
  )
  case class Location(filename: String, path: Option[String] = None)

  type Package = PackageF[cats.Id]
  type PackageBuilder = PackageF[Option]
  object PackageBuilder {
    val empty: PackageBuilder = apply()
    def apply(
        name: Option[Name] = None,
        arch: Option[Architecture] = None,
        loc: Option[Location] = None,
        checksum: Option[Checksum] = None,
        size: Option[Size] = None
    ): PackageBuilder = PackageF(name, arch, loc, checksum, size)
  }
  def build(packageBuilder: PackageBuilder): Option[Package] = {
    (packageBuilder.name |@|
      packageBuilder.arch |@|
      packageBuilder.loc |@|
      packageBuilder.checksum |@|
      packageBuilder.size).map(PackageF[Id])
  }

  case class Meta(checksum: Checksum, path: String)
  case class Key(checksum: Checksum, path: String)

  def pkg(line: String): Either[String, (Name, Architecture)] = {
    // "=Pkg: cg-devel 3.1.0013 8.39 i586"
    val parts = line.split(" ")
    if (parts.length == 5) {
      val name = Name(parts(1)).left.map(_.msg)
      val arch = Architecture
        .fromString(parts(4))
        .toRight(s"${parts(4)} invalid architecture")
      for {
        n <- name
        a <- arch
      } yield (n, a)
    } else Left(s"invalid pkg line '$line'")
  }

  def cks(line: String): Either[String, Checksum] = {
    val parts = line.split(" ")
    if (parts.length == 3) {
      val checksum = parts(1) match {
        case "SHA1"   => Sha1.fromHex(parts(2))
        case "SHA256" => Sha256.fromHex(parts(2))
        case _        => None
      }
      checksum.toRight(s"invalid checksum line $line")
    } else Left(s"invalid cks line '$line'")
  }

  def loc(line: String): Either[String, Location] = {
    val parts = line.split(" ")
    if (parts.length >= 3) {
      val filename = parts(2)
      //TODO: make use of optional path
      val path = if (parts.length == 4) Some(parts(3)) else None
      Right(Location(filename, path))
    } else Left(s"invalid loc line '$line'")
  }

  def siz(line: String): Either[String, Size] = {
    val parts = line.split(" ")
    if (parts.length == 3) {
      val pkgSize = parts(1).toLong
      val installSize = parts(2).toLong
      Right(Size(Bytes(pkgSize), Bytes(installSize)))
    } else Left(s"invalid siz line '$line'")
  }

  def pipe[F[_]]: Pipe[F, String, Package] = h => {
      def pack(h: Stream[F, String], acc: PackageBuilder)
        : Pull[F, Nothing, Option[(Package, Stream[F, String])]] = {
        h.pull.uncons1.flatMap {
          case None =>
            if (acc == PackageBuilder.empty)
              Pull.pure(None)
            else Pull.pure(Some((build(acc).get, h)))
          case Some((line, h1)) =>
            if (line.size >= 5) {
              line.head match {
                case '=' =>
                  line.substring(1, 5) match {
                    case "Cks:" =>
                      cks(line) match {
                        case Right(v) => pack(h1, acc.copy(checksum = Some(v)))
                        case Left(e)  => Pull.raiseError(new RuntimeException(e))
                      }
                    case "Loc:" =>
                      loc(line) match {
                        case Right(v) => pack(h1, acc.copy(loc = Some(v)))
                        case Left(e)  => Pull.raiseError(new RuntimeException(e))
                      }
                    case "Siz:" =>
                      siz(line) match {
                        case Right(v) => pack(h1, acc.copy(size = Some(v)))
                        case Left(e)  => Pull.raiseError(new RuntimeException(e))
                      }
                    case "Pkg:" =>
                      Pull.pure(
                        Some((build(acc).get, h))
                      )
                    case _ => pack(h1, acc)
                  }
                case '-' => pack(h1, acc)
                case '+' => pack(h1, acc)
                case _   => pack(h1, acc)
              }
            } else {
              pack(h1, acc)
            }
        }
      }

      def go(h: Stream[F, String]): Pull[F, Package, Option[Unit]] = {
        h.pull.uncons1.flatMap {
          case None => Pull.pure(None)
          case Some((line, h1)) =>
            if (line.startsWith("=Pkg:")) {
              val (name, arch) = pkg(line).toOption.get
              pack(h1, PackageBuilder(name = Some(name), arch = Some(arch)))
                .flatMap {
                  case Some((p, h)) =>
                    Pull.output1(p) >> go(h)
                  case None => Pull.pure(None)
                }
            } else go(h1)
        }
      }

      go(h).stream
    }

}
