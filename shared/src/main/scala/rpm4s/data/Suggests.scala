package rpm4s.data

case class Suggests(name: Name, evr: Option[EVR], flags: SenseFlags)
    extends PkgRef
