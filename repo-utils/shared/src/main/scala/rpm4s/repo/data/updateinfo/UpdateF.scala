package rpm4s.repo.data.updateinfo

import java.time.Instant

import cats.Id
import cats.implicits._
import rpm4s.data.{CVE, _}
case class UpdateF[F[_]](
  from: F[String],
  status: F[UpdateF.Status],
  tpe: F[UpdateF.UpdateType],
  //TODO: find out what are valid values for version
  version: F[String],
  id: F[String],
  title: F[String],
  severity: F[UpdateF.Severity],
  release: F[String],
  issued: F[Instant],
  references: F[Set[UpdateF.Reference]],
  description: F[String],
  packages: F[Set[UpdateF.PackageF.Package]]
)

object UpdateF {
  type Update = UpdateF[cats.Id]
  type UpdateBuilder = UpdateF[Option]
  sealed trait Reference extends Product with Serializable {
    def href: String
    def id: String
    def title: String
  }
  case class Bugzilla(
    href: String,
    id: String,
    title: String
  ) extends Reference
  case class CVERef(
    href: String,
    cve: CVE,
    title: String
  ) extends Reference {
    def id = cve.string
  }
  case class Fate(
    href: String,
    id: String,
    title: String
  ) extends Reference

  sealed trait Status extends Product with Serializable
  object Status {
    def fromString(value: String): Option[Status] = value match {
      case "stable" => Some(Stable)
      case _ => None
    }
    def toString(value: Status): String = value match {
      case Stable => "stable"
    }
    case object Stable extends Status
  }

  sealed trait Severity extends Product with Serializable
  object Severity {
    def fromString(value: String): Option[Severity] = value match {
      case "critical" => Some(Critical)
      case "important" => Some(Important)
      case "moderate" => Some(Moderate)
      case "low" => Some(Low)
      case _ => None
    }
    def toString(value: Severity): String = value match {
      case Important => "important"
      case Moderate => "moderate"
      case Low => "low"
      case Critical => "critical"
    }
    case object Critical extends Severity
    case object Important extends Severity
    case object Moderate extends Severity
    case object Low extends Severity
  }

  sealed trait UpdateType extends Product with Serializable
  object UpdateType {
    def fromString(value: String): Option[UpdateType] = value match {
      case "recommended" => Some(Recommended)
      case "security" => Some(Security)
      case "optional" => Some(Optional)
      case "feature" => Some(Feature)
      case _ => None
    }
    def toString(value: UpdateType): String = value match {
      case Recommended => "recommended"
      case Security => "security"
      case Optional => "optional"
      case Feature => "feature"
    }
    case object Recommended extends UpdateType
    case object Security extends UpdateType
    case object Optional extends UpdateType
    case object Feature extends UpdateType
  }

  case class PackageF[F[_]](
    name: F[Name],
    version: F[Version],
    release: F[Release],
    epoch: F[Option[Epoch]],
    //TODO: find out how this arch relates to rpm arch
    arch: F[Architecture],
    src: F[String],
    //TODO: find out if filename is always also in src
    filename: F[String],
    restartSuggested: F[Boolean],
    rebootSuggested: F[Boolean],
    reloginSuggested: F[Boolean]
  )
  object PackageF {
    type Package = PackageF[cats.Id]
    type PackageBuilder = PackageF[Option]
    object PackageBuilder {
      def build(packageBuilder: PackageBuilder): Option[Package] = {
        (packageBuilder.name,
         packageBuilder.version,
         packageBuilder.release,
         packageBuilder.epoch,
         packageBuilder.arch,
         packageBuilder.src,
         packageBuilder.filename,
         packageBuilder.restartSuggested,
         packageBuilder.rebootSuggested,
         packageBuilder.reloginSuggested
        ).mapN(PackageF[Id])
      }
      val empty: PackageBuilder = apply()
      def apply(
        name: Option[Name] = None,
        version: Option[Version] = None,
        release: Option[Release] = None,
        epoch: Option[Option[Epoch]] = None,
        arch: Option[Architecture] = None,
        src: Option[String] = None,
        filename: Option[String] = None,
        restartSuggested: Option[Boolean] = Some(false),
        rebootSuggested: Option[Boolean] = Some(false),
        reloginSuggested: Option[Boolean] = Some(false)
      ): PackageBuilder = PackageF(
        name, version, release, epoch, arch,
        src, filename, rebootSuggested, rebootSuggested, reloginSuggested
      )
    }
  }


  object UpdateBuilder {
    def build(updateBuilder: UpdateBuilder): Option[Update] = {
      (updateBuilder.from,
        updateBuilder.status,
        updateBuilder.tpe,
        updateBuilder.version,
        updateBuilder.id,
        updateBuilder.title,
        updateBuilder.severity,
        updateBuilder.release,
        updateBuilder.issued,
        updateBuilder.references,
        updateBuilder.description,
        updateBuilder.packages
      ).mapN(UpdateF[Id])
    }
    val empty: UpdateBuilder = apply()
    def apply(
      from: Option[String] = None,
      status: Option[Status] = None,
      tpe: Option[UpdateType] = None,
      version: Option[String] = None,
      id: Option[String] = None,
      title: Option[String] = None,
      severity: Option[Severity] = None,
      release: Option[String] = None,
      issued: Option[Instant] = None,
      description: Option[String] = None,
      references: Option[Set[Reference]] = None,
      packages: Option[Set[UpdateF.PackageF.Package]] = None
    ): UpdateBuilder = UpdateF(
      from, status, tpe, version, id, title,
      severity, release, issued, references,
      description, packages
    )
  }
}
