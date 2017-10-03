package rpm4s.data

case class Provides(name: String, evr: Option[EVR], flags: SenseFlags)
    extends PkgRef
