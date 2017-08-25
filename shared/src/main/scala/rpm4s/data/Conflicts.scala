package rpm4s.data

case class Conflicts(name: Name, evr: Option[EVR], flags: SenseFlags)
    extends PkgRef
