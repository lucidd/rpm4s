package rpm4s.data

case class NEVRA(
    name: Name,
    version: Version,
    arch: Architecture,
    release: Option[Release] = None,
    epoch: Option[Epoch] = None
)
