package rpm4s.data

case class Recommends(name: Name, evr: Option[EVR], flags: SenseFlags)
    extends PkgRef
