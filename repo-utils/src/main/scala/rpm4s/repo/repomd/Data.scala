package rpm4s.repo.repomd

import java.time.Instant

import rpm4s.data.Checksum
import rpm4s.repo.data.Bytes

sealed trait Data extends Product with Serializable {
  def checksum: Checksum
  def openChecksum: Checksum
  def location: String
  def timestamp: Instant
  def openSize: Bytes
  def size: Bytes
}
object Data {

  case class UpdateInfo(
    checksum: Checksum,
    openChecksum: Checksum,
    location: String,
    timestamp: Instant,
    openSize: Bytes,
    size: Bytes
  ) extends Data
  case class FileLists(
    checksum: Checksum,
    openChecksum: Checksum,
    location: String,
    timestamp: Instant,
    openSize: Bytes,
    size: Bytes
  ) extends Data
  case class Other(
    checksum: Checksum,
    openChecksum: Checksum,
    location: String,
    timestamp: Instant,
    openSize: Bytes,
    size: Bytes
  ) extends Data
  case class Primary(
    checksum: Checksum,
    openChecksum: Checksum,
    location: String,
    timestamp: Instant,
    openSize: Bytes,
    size: Bytes
  ) extends Data
  case class AppData(
    checksum: Checksum,
    openChecksum: Checksum,
    location: String,
    timestamp: Instant,
    openSize: Bytes,
    size: Bytes
  ) extends Data
  case class AppIcons(
    checksum: Checksum,
    openChecksum: Checksum,
    location: String,
    timestamp: Instant,
    openSize: Bytes,
    size: Bytes
  ) extends Data
}
