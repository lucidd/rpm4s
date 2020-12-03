package rpm4s.data

case class NEVRA(
    name: Name,
    epoch: Epoch,
    version: Version,
    arch: Architecture,
    release: Option[Release] = None,
)
