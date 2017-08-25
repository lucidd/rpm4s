package rpm4s.data

case class RpmPrimaryEntry(
                            name: Name,
                            version: Version,
                            release: Release,
                            architecture: Architecture,
                            vendor: Vendor,
                            license: License,
                            summery: Summery,
                            description: Description,
                            group: rpm4s.data.Group,
                            headerRange: HeaderRange,
                            epoch: Option[Epoch] = None,
                            buildhost: Option[BuildHost] = None,
                            buildtime: Option[BuildTime] = None,
                            fileEntries: Vector[FileEntry] = Vector.empty,
                            requires: Vector[Requires] = Vector.empty,
                            provides: Vector[Provides] = Vector.empty,
                            obsoletes: Vector[Obsoletes] = Vector.empty,
                            enhances: Vector[Enhances] = Vector.empty,
                            conflicts: Vector[Conflicts] = Vector.empty,
                            supplements: Vector[Supplements] = Vector.empty,
                            recommends: Vector[Recommends] = Vector.empty,
                            suggests: Vector[Suggests] = Vector.empty
)
