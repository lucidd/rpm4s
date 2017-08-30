package rpm4s.data

import rpm4s.data.Stat.FileType

class Stat private (raw: Short) {

  def sticky: Boolean = (raw & Stat.STICKY_BIT) != 0
  def sgid: Boolean = (raw & Stat.SET_GROUP_ID_BIT) != 0
  def suid: Boolean = (raw & Stat.SET_USER_ID_BIT) != 0
  def tpe: FileType = Stat.raw2fileType(raw).get

  def permsString: String = {
    val other = Stat.perm2string((raw & 7).toByte)
    val group = Stat.perm2string(((raw >> 3) & 7).toByte)
    val owner = Stat.perm2string(((raw >> 6) & 7).toByte)
    s"$owner$group$other"
  }

  override def toString: String = {
    val other = Stat.perm2string((raw & 7).toByte)
    val group = Stat.perm2string(((raw >> 3) & 7).toByte)
    val owner = Stat.perm2string(((raw >> 6) & 7).toByte)
    val f1 = if (sticky) Set("Sticky") else Set.empty
    val f2 = if (sgid) Set("SGID") else Set.empty
    val f3 = if (suid) Set("SUID") else Set.empty
    val flags = (f1 ++ f2 ++ f3).mkString("|")
    s"Stat(Owner($owner), Group($group), Other($other), Type(${Stat.raw2fileType(raw).getOrElse("Unknown")}), Flags($flags))"
  }
}

object Stat {

  def fromShort(value: Short): Option[Stat] = {
    raw2fileType(value).map { _ =>
      new Stat(value)
    }
  }

  private val STICKY_BIT = 1 << 9
  private val SET_GROUP_ID_BIT = 1 << 10
  private val SET_USER_ID_BIT = 1 << 11

  sealed trait FileType extends Product with Serializable
  object FileType {
    case object Socket extends FileType
    case object SymbolicLink extends FileType
    case object RegularFile extends FileType
    case object BlockDevice extends FileType
    case object Directory extends FileType
    case object CharDevice extends FileType
    case object FIFO extends FileType
  }

  private def perm2string(byte: Byte): String = byte match {
    case 0 => "---"

    case 1 => "--x"
    case 2 => "-w-"
    case 4 => "r--"

    case 3 => "-wx"
    case 5 => "r-x"
    case 6 => "rw-"

    case 7 => "rwx"
  }

  def raw2fileType(number: Short): Option[FileType] = number & 0xF000 match {
    case 0x1000 => Some(FileType.FIFO)
    case 0x2000 => Some(FileType.CharDevice)
    case 0x4000 => Some(FileType.Directory)
    case 0x6000 => Some(FileType.BlockDevice)
    case 0x8000 => Some(FileType.RegularFile)
    case 0xA000 => Some(FileType.SymbolicLink)
    case 0xC000 => Some(FileType.Socket)
    case _ => None
  }
}
