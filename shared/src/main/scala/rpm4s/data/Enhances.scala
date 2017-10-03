package rpm4s.data

case class Enhances(name: String, evr: Option[EVR], flags: SenseFlags)
    extends PkgRef
