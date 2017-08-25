package rpm4s.data

case class Enhances(name: Name, evr: Option[EVR], flags: SenseFlags)
    extends PkgRef
