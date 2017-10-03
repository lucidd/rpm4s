package rpm4s.data

case class Requires(name: String, evr: Option[EVR], flags: SenseFlags)
    extends PkgRef
