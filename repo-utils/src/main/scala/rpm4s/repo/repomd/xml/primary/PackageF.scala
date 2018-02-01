package rpm4s.repo.repomd.xml.primary

import cats.Id
import cats.implicits._
import org.http4s.Uri
import rpm4s.data._

case class PackageF[F[_]](
  name: F[Name],
  version: F[Version],
  epoch: F[Option[Epoch]],
  release: F[Release],
  /**
  * arch is Some(arch) in case of binary rpms and None for source rpms
  */
  arch: F[Option[Architecture]],
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
        packageBuilder.version,
        packageBuilder.epoch,
        packageBuilder.release,
        packageBuilder.arch,
        packageBuilder.loc,
        packageBuilder.checksum,
        packageBuilder.size).mapN(PackageF[Id])
    }
    val empty: PackageBuilder = apply()
    def apply(
      name: Option[Name] = None,
      version: Option[Version] = None,
      epoch: Option[Option[Epoch]] = None,
      release: Option[Release] = None,
      arch: Option[Option[Architecture]] = None,
      loc: Option[Uri] = None,
      checksum: Option[Checksum] = None,
      size: Option[SizeInfo] = None
    ): PackageBuilder = PackageF(name, version, epoch, release, arch, loc, checksum, size)
  }
}
