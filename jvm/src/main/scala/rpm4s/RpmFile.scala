package rpm4s

import rpm4s.codecs.{Header, HeaderTag, IndexData, IndexEntry, SignatureTag}
import rpm4s.data.Lead
import scodec.bits.BitVector

case class RpmFile(
  lead: Lead,
  signatureHeader: Header[SignatureTag],
  signatureData: List[(IndexEntry[SignatureTag], IndexData)],
  header: Header[HeaderTag[IndexData]],
  data: List[(IndexEntry[HeaderTag[IndexData]], IndexData)],
  payload: BitVector
)

object RpmFile {
}
