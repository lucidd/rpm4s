package rpm4s.utils

import java.security.MessageDigest

import rpm4s.data.Checksum.Sha256
import scodec.bits.{BitVector, ByteVector}

import scala.annotation.tailrec

package object hash {

  def digest(bitVector: BitVector,
             md: MessageDigest,
             chunkSize: Int = 1024 * 1000 * 16): ByteVector = {
    @tailrec
    def go(rest: BitVector): ByteVector = {
      if (rest.isEmpty)
        ByteVector.view(md.digest())
      else {
        val bytes = rest.take(chunkSize)
        md.update(bytes.toByteArray)
        go(rest.drop(chunkSize))
      }
    }
    go(bitVector)
  }

  def sha256(bytes: BitVector): Sha256 = {
    val sha256bytes = digest(bytes, MessageDigest.getInstance("SHA-256"))
    Sha256.fromBytes(sha256bytes.toArray.toVector).get
  }

}
