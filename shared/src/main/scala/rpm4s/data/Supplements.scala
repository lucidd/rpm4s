package rpm4s.data

case class Supplements(name: String, evr: Option[EVR], flags: SenseFlags)
    extends PkgRef
