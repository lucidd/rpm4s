package rpm4s.data

import scodec.bits.ByteVector

sealed trait Checksum extends Product with Serializable {
  def toHex: String = bytes.toHex
  def toSelfDescribingHex: String
  def bytes: ByteVector
}
//TODO: cleanup and implement more checksum types
object Checksum {
  def fromString(value: String): Option[Checksum] = {
    value.split("-", 2) match {
      case Array("sha256", hex) => Sha256.fromHex(hex)
      case Array("sha1", hex) => Sha1.fromHex(hex)
      case _ => None
    }

  }
  case class Sha1(bytes: ByteVector) extends Checksum {
    def toSelfDescribingHex: String = s"sha1-$toHex"
    override def toString: String = toSelfDescribingHex
  }
  object Sha1 {
    def fromBytes(bytes: ByteVector): Option[Sha1] = {
      if (bytes.length != 20) None
      else Some(Sha1(bytes))
    }
    def fromBytes(bytes: Array[Byte]): Option[Sha1] = {
      if (bytes.length != 20) None
      else Some(Sha1(ByteVector(bytes)))
    }
    def fromHex(value: String): Option[Sha1] = {
      if (value.length == 40)
        ByteVector.fromHex(value).map(Sha1(_))
      else None
    }
  }

  case class Sha256(bytes: ByteVector) extends Checksum {
    def toSelfDescribingHex: String = s"sha256-$toHex"
    override def toString: String = toSelfDescribingHex
  }
  object Sha256 {
    def bytesFromHex(value: String): Option[Array[Byte]] = {
      def hexCharToByte(char: Char): Byte = char match {
        case c if c >= '0' && c <= '9' => (c - '0').toByte
        case c if c >= 'a' && c <= 'f' => (10 + (c - 'a')).toByte
        case c if c >= 'A' && c <= 'F' => (10 + (c - 'A')).toByte
        //TODO: dont crash on invalid input
      }
      if (value.length % 2 == 0) Some {
        value
          .sliding(2, 2)
          .map { pair =>
            (hexCharToByte(pair(0)) << 8 | hexCharToByte(pair(1))).toByte
          }
          .toArray
      } else None
    }

    def fromBytes(bytes: Array[Byte]): Option[Sha256] = {
      if (bytes.length != 32) None
      else Some(Sha256(ByteVector(bytes)))
    }

    def fromHex(value: String): Option[Sha256] = {
      if (value.length == 64)
        ByteVector.fromHex(value).map(Sha256(_))
      else None
    }
  }
}
