package rpm4s.build

import java.time.Instant

import rpm4s.data.FileEntry
import scodec.bits.ByteVector

case class FileInfo(
  fileEntry: FileEntry,
  content: ByteVector,
  modtime: Instant,
  user: String,
  group: String
)
