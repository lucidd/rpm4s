import rpm4s.codecs.Extractor
import scodec.Attempt
import scodec.bits.BitVector

package object rpm4s {

  def decode[T: Extractor](bits: BitVector): Attempt[T] =
    codecs.decoder[T].decodeValue(bits)

}
