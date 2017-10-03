package rpm4s.data

case class Obsoletes(name: String, evr: Option[EVR], flags: SenseFlags)
    extends PkgRef
