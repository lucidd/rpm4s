package rpm4s.data

import rpm4s.data.Dependency._

case class RPM(
    name: Name,
    version: Version,
    release: Release,
    architecture: Architecture,
    vendor: Vendor,
    license: License,
    summery: Summary,
    description: Description,
    group: Group,
    epoch: Epoch = Epoch.ZERO,
    buildhost: Option[BuildHost] = None,
    buildtime: Option[BuildTime] = None,
    requires: Vector[Requires] = Vector.empty,
    provides: Vector[Provides] = Vector.empty,
    obsoletes: Vector[Obsoletes] = Vector.empty,
    enhances: Vector[Enhances] = Vector.empty,
    conflicts: Vector[Conflicts] = Vector.empty,
    supplements: Vector[Supplements] = Vector.empty,
    recommentds: Vector[Recommends] = Vector.empty,
    suggests: Vector[Suggests] = Vector.empty
)
