package rpm4s.repo.repomd.xml.primary

import cats.Id
import cats.implicits._
import org.http4s.Uri
import rpm4s.data.{Checksum, Name}

case class PackageF[F[_]](
  name: F[Name],
  loc: F[Uri],
  checksum: F[Checksum],
  size: F[SizeInfo]
)

object PackageF {
  type Package = PackageF[cats.Id]
  type PackageBuilder = PackageF[Option]
  object PackageBuilder {
    def build(packageBuilder: PackageBuilder): Option[Package] = {
      (packageBuilder.name,
        packageBuilder.loc,
        packageBuilder.checksum,
        packageBuilder.size).mapN(PackageF[Id])
    }
    val empty: PackageBuilder = apply()
    def apply(
      name: Option[Name] = None,
      loc: Option[Uri] = None,
      checksum: Option[Checksum] = None,
      size: Option[SizeInfo] = None
    ): PackageBuilder = PackageF(name, loc, checksum, size)
  }
}
