package rpm4s.data

case class Suggests(name: String, evr: Option[EVR], flags: SenseFlags)
    extends PkgRef
