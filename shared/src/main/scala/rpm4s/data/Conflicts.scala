package rpm4s.data

case class Conflicts(name: String, evr: Option[EVR], flags: SenseFlags)
    extends PkgRef
