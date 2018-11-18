package rpm4s.repo.data

import java.time.Instant

import cats.Id
import cats.implicits._
import rpm4s.data.Checksum


case class RMDataF[F[_]](
  location: F[String],
  checksum: F[Checksum],
  timestamp: F[Instant],
  size: F[Bytes],
  openSize: F[Bytes],
  openChecksum: F[Checksum]
)

object RMDataF {
  type RMData = RMDataF[cats.Id]
  type RMDataBuilder = RMDataF[Option]
  object RMDataBuilder {
    def build(rmdDataBuilder: RMDataBuilder): Option[RMData] = {
      (rmdDataBuilder.location,
        rmdDataBuilder.checksum,
        rmdDataBuilder.timestamp,
        rmdDataBuilder.size,
        rmdDataBuilder.openSize,
        rmdDataBuilder.openChecksum).mapN(RMDataF[Id])
    }
    val empty: RMDataBuilder = apply[Option](None, None, None, None, None, None)
  }
}
