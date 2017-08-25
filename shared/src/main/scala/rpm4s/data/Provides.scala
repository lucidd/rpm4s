package rpm4s.data

case class Provides(name: Name, evr: Option[EVR], flags: SenseFlags)
    extends PkgRef
