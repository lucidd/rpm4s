package rpm4s.data

case class Supplements(name: Name, evr: Option[EVR], flags: SenseFlags)
    extends PkgRef
