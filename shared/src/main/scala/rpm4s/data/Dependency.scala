package rpm4s.data

sealed trait Dependency {
  def ref: PkgRef
}
object Dependency {
  case class Provides(ref: PkgRef) extends Dependency
  case class Obsoletes(ref: PkgRef) extends Dependency
  case class Recommends(ref: PkgRef) extends Dependency
  case class Order(ref: PkgRef) extends Dependency
  case class Requires(ref: PkgRef) extends Dependency
  case class Suggests(ref: PkgRef) extends Dependency
  case class Supplements(ref: PkgRef) extends Dependency
  case class Enhances(ref: PkgRef) extends Dependency
  case class Conflicts(ref: PkgRef) extends Dependency
}
