package rpm4s.codecs

import scodec._
import scodec.bits._
import scodec.codecs._

package object utils {

  def alignTo[A](codec: Codec[A], alignBits: Long, minBits: Long = 0): Codec[A] = new Codec[A] {

    private def padAmount(size: Long): Long =
      if (size < minBits) {
        minBits - size
      } else {
        val mod = size % alignBits
        if (mod == 0) 0 else alignBits - mod
      }

    def sizeBound: SizeBound = {
      val sz = codec.sizeBound
      val lb = sz.lowerBound + padAmount(sz.lowerBound)
      val ub = sz.upperBound.map { ub =>
        ub + padAmount(ub)
      }
      SizeBound(lb, ub)
    }

    def encode(a: A): Attempt[BitVector] =
      codec.encode(a) map { enc =>
        val pad = padAmount(enc.size)
        if (pad == 0) enc
        else enc.padTo(enc.size + pad)
      }

    def decode(b: BitVector): Attempt[DecodeResult[A]] =
      codec.decode(b) map { res =>
        val taken = b.size - res.remainder.size
        val pad = padAmount(taken)
        if (pad == 0) res
        else DecodeResult(res.value, res.remainder.drop(pad))
      }

    override def toString = s"BitAligned($codec, $alignBits)"
  }

  val nulTerminated: Codec[BitVector] = new Codec[BitVector] {
    private val nul = BitVector.lowByte
    override def sizeBound: SizeBound = SizeBound.unknown
    override def encode(bits: BitVector): Attempt[BitVector] =
      Attempt.successful(bits ++ nul)
    override def decode(bits: BitVector): Attempt[DecodeResult[BitVector]] =
      indexOfByte(0x00, bits) match {
        case -1 =>
          Attempt.failure(Err("Does not contain a 'NUL' termination byte."))
        case i =>
          Attempt.successful(DecodeResult(bits.take(i), bits.drop(i + 8L)))
      }
  }

  def indexOfByte(byte: Byte, bytes: BitVector): Long = {
    val size = bytes.size / 8
    def go(idx: Long): Long =
      if (idx >= size) -1
      else if (bytes.getByte(idx) == byte) idx * 8
      else go(idx + 1)
    go(0)
  }

  // Does not handle any sensible encoding validation
  val yoloString: Codec[String] = nulTerminated.xmap(
    bits => new String(bits.toByteArray),
    string => BitVector(string.getBytes)
  )

  val utf8nul: Codec[String] = filtered(
    utf8,
    new Codec[BitVector] {
      private val nul = BitVector.lowByte
      override def sizeBound: SizeBound = SizeBound.unknown
      override def encode(bits: BitVector): Attempt[BitVector] =
        Attempt.successful(bits ++ nul)
      override def decode(bits: BitVector): Attempt[DecodeResult[BitVector]] =
        indexOfByte(0x00, bits) match {
          case -1 =>
            Attempt.failure(Err("Does not contain a 'NUL' termination byte."))
          case i =>
            Attempt.successful(DecodeResult(bits.take(i), bits.drop(i + 8L)))
        }
    }
  ).withToString("utf8nul")
}
