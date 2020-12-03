package rpm4s.repo.data.primary

import cats.Id
import cats.implicits._
import rpm4s.data._

case class PackageF[F[_]](
  name: F[Name],
  version: F[Version],
  epoch: F[Epoch],
  release: F[Release],
  /**
  * arch is Some(arch) in case of binary rpms and None for source rpms
  */
  arch: F[Architecture],
  loc: F[String],
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
      epoch: Option[Epoch] = None,
      release: Option[Release] = None,
      arch: Option[Architecture] = None,
      loc: Option[String] = None,
      checksum: Option[Checksum] = None,
      size: Option[SizeInfo] = None
    ): PackageBuilder = PackageF(name, version, epoch, release, arch, loc, checksum, size)
  }
}
