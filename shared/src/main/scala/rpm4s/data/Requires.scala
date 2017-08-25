package rpm4s.data

case class Requires(name: Name, evr: Option[EVR], flags: SenseFlags)
    extends PkgRef
