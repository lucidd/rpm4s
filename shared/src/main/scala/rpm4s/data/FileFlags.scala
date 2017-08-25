package rpm4s.data

class FileFlags(val value: Int) extends AnyVal {
  def &(other: FileFlags) = FileFlags(value & other.value)
  def |(other: FileFlags) = FileFlags(value | other.value)
  def ^(other: FileFlags) = FileFlags(value ^ other.value)
  def union(other: FileFlags) = this | other
  def intersection(other: FileFlags) = this & other
  def diff(other: FileFlags) = this ^ other
  def inverse: FileFlags = this ^ FileFlags.All
  def -(other: FileFlags) = this & other.inverse

  def containsAll(attributes: FileFlags): Boolean =
    (value & attributes.value) == attributes.value
  def containsAny(attributes: FileFlags): Boolean =
    (value & attributes.value) != 0
  override def toString: String = {
    val config = if (containsAll(FileFlags.Config)) Seq("Config") else Seq.empty
    val doc = if (containsAll(FileFlags.Doc)) Seq("Doc") else Seq.empty
    val doNotUse =
      if (containsAll(FileFlags.DoNotUse)) Seq("DoNotUse") else Seq.empty
    val missingOk =
      if (containsAll(FileFlags.MissingOk)) Seq("MissingOk") else Seq.empty
    val noReplace =
      if (containsAll(FileFlags.NoReplace)) Seq("NoReplace") else Seq.empty
    val specFile =
      if (containsAll(FileFlags.SpecFile)) Seq("SpecFile") else Seq.empty
    val ghost = if (containsAll(FileFlags.Ghost)) Seq("Ghost") else Seq.empty
    val licence =
      if (containsAll(FileFlags.License)) Seq("Licence") else Seq.empty
    val readme = if (containsAll(FileFlags.Readme)) Seq("Readme") else Seq.empty
    val exclude =
      if (containsAll(FileFlags.Exclude)) Seq("Exclude") else Seq.empty
    val pubkey = if (containsAll(FileFlags.PubKey)) Seq("PubKey") else Seq.empty
    val all = config ++ doc ++ doNotUse ++ missingOk ++ noReplace ++
      specFile ++ ghost ++ licence ++ readme ++ exclude ++ pubkey
    s"FileFlags(${all.mkString("|")})"
  }
}

object FileFlags {

  def apply(byte: Int): FileFlags = new FileFlags(byte)

  val None = FileFlags(0)
  //TODO: config seems to be specified like this %config(noreplace) so it probably can never be alone
  val Config = FileFlags(1 << 0)
  val Doc = FileFlags(1 << 1)
  val DoNotUse = FileFlags(1 << 2)
  val MissingOk = FileFlags(1 << 3)
  val NoReplace = FileFlags(1 << 4)
  val SpecFile = FileFlags(1 << 5)
  val Ghost = FileFlags(1 << 6)
  val License = FileFlags(1 << 7)
  val Readme = FileFlags(1 << 8)
  val Exclude = FileFlags(1 << 9)
  val PubKey = FileFlags(1 << 11)

  val All = FileFlags((1 << 10) - 1)

}
