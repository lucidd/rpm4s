package rpm4s.data

import java.time.Instant

//TODO: this should really be split up into individual cases which guarantee presence of certain properties
case class FileEntry(
  path: String,
  username: String,
  groupname: String,
  size: Long,
  rdev: Short,
  mtime: Instant,
  inode: Int,
  device: Int,
  mode: Stat,
  flags: FileFlags,
  checksum: Option[Checksum],
  linkto: Option[String]
)
