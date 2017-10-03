package rpm4s.data

case class Recommends(name: String, evr: Option[EVR], flags: SenseFlags)
    extends PkgRef
