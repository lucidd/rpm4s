package rpm4s.data

import scala.annotation.tailrec


sealed trait Checksum extends Product with Serializable {
  def toHex: String = bytes.foldLeft(new StringBuilder) {
    case (sb, b) =>
      val h = Integer.toHexString(b & 0xFF)
      if (h.size == 1) sb.append("0" + h)
      else sb.append(h)
  }.toString
  def toSelfDescribingHex: String
  def bytes: Vector[Byte]

}
//TODO: cleanup and implement more checksum types
object Checksum {
  def fromString(value: String): Option[Checksum] = {
    value.split("-", 2) match {
      case Array("md5", hex) => Md5.fromHex(hex)
      case Array("sha1", hex) => Sha1.fromHex(hex)
      case Array("sha256", hex) => Sha256.fromHex(hex)
      case Array("sha512", hex) => Sha512.fromHex(hex)
      case _ => None
    }

  }

  def fromHex(value: String): Option[Vector[Byte]] = {
    val data = new Array[Byte](value.length / 2)
    @tailrec
    def loop(idx: Int): Option[Array[Byte]] = {
      if (idx < value.length) {
        val msb = Character.digit(value.charAt(idx), 16) << 4
        val lsb = Character.digit(value.charAt(idx + 1), 16)
        if (msb == -1 || lsb == -1) None
        else {
          data(idx / 2) = (msb | lsb).toByte
          loop(idx + 2)
        }
      } else Some(data)
    }
    loop(0).map(_.toVector)
  }

  case class Sha1(bytes: Vector[Byte]) extends Checksum {
    def toSelfDescribingHex: String = s"sha1-$toHex"
    override def toString: String = toSelfDescribingHex
  }
  object Sha1 {
    def fromBytes(bytes: Vector[Byte]): Option[Sha1] = {
      if (bytes.length != 20) None
      else Some(Sha1(bytes))
    }
    def fromHex(value: String): Option[Sha1] = {
      if (value.length == 40)
        Checksum.fromHex(value).map(Sha1(_))
      else None
    }
  }

  case class Sha256(bytes: Vector[Byte]) extends Checksum {
    def toSelfDescribingHex: String = s"sha256-$toHex"
    override def toString: String = toSelfDescribingHex
  }
  object Sha256 {
    def fromBytes(bytes: Vector[Byte]): Option[Sha256] = {
      if (bytes.length != 32) None
      else Some(Sha256(bytes))
    }

    def fromHex(value: String): Option[Sha256] = {
      if (value.length == 64)
        Checksum.fromHex(value).map(Sha256(_))
      else None
    }
  }

  case class Md5(bytes: Vector[Byte]) extends Checksum {
    def toSelfDescribingHex: String = s"md5-$toHex"
    override def toString: String = toSelfDescribingHex
  }
  object Md5 {
    def fromBytes(bytes: Vector[Byte]): Option[Md5] = {
      if (bytes.length != 16) None
      else Some(Md5(bytes))
    }

    def fromHex(value: String): Option[Md5] = {
      if (value.length == 32)
        Checksum.fromHex(value).map(Md5(_))
      else None
    }
  }

  case class Sha512(bytes: Vector[Byte]) extends Checksum {
    def toSelfDescribingHex: String = s"sha512-$toHex"
    override def toString: String = toSelfDescribingHex
  }
  object Sha512 {
    def fromBytes(bytes: Vector[Byte]): Option[Sha512] = {
      if (bytes.length != 64) None
      else Some(Sha512(bytes))
    }

    def fromHex(value: String): Option[Sha512] = {
      if (value.length == 128)
        Checksum.fromHex(value).map(Sha512(_))
      else None
    }
  }
}
