package rpm4s.data

sealed trait Architecture
object Architecture {
  case object x86_64 extends Architecture
  case object i386 extends Architecture
  case object i486 extends Architecture
  case object i586 extends Architecture
  case object i686 extends Architecture
  //TODO: find out what the relationship between those architectures is
  //TODO: find out if modeling compatability between architectures is sanely possible
  case object ppc extends Architecture
  case object ppc64 extends Architecture
  case object ppc64le extends Architecture
  case object s390x extends Architecture
  case object aarch64 extends Architecture
  case object NoArch extends Architecture
  sealed trait SourceArch extends Architecture
  case object Src extends SourceArch
  case object NoSrc extends SourceArch

  def toRpmString(architecture: Architecture): String = architecture match {
    case Architecture.x86_64 => "x86_64"
    case Architecture.i586 => "i586"
    case Architecture.i686 => "i686"
    case Architecture.NoArch => "noarch"
    case Architecture.ppc => "ppc"
    case Architecture.ppc64 => "ppc64"
    case Architecture.ppc64le => "ppc64le"
    case Architecture.s390x => "s390x"
    case Architecture.Src => "src"
    case Architecture.NoSrc => "nosrc"
    case Architecture.aarch64 => "aarch64"
    case _ => ???
  }

  def fromString(value: String): Option[Architecture] = value match {
    case "x86_64" => Some(x86_64)
    case "i586" => Some(i586)
    case "i686" => Some(i686)
    case "noarch" => Some(NoArch)
    case "ppc" => Some(ppc)
    case "ppc64" => Some(ppc64)
    case "ppc64le" => Some(ppc64le)
    case "s390x" => Some(s390x)
    case "nosrc" => Some(Architecture.NoSrc)
    case "src" => Some(Architecture.Src)
    case "aarch64" => Some(Architecture.aarch64)
    case _ => None
  }
}
