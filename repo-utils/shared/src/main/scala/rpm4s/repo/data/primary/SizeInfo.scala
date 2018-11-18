package rpm4s.repo.data.primary
import rpm4s.repo.data.Bytes

case class SizeInfo(
  pack: Bytes,
  installed: Bytes,
  archive: Bytes
)
