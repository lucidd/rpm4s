package rpm4s.build

import rpm4s.data.Dependency._
import rpm4s.data._

case class RpmBlueprint(
  name: Name,
  version: Version,
  release: Release,
  epoch: Option[Epoch],
  license: License,
  architecture: Architecture,
  description: Description,
  summery: Summery,
  group: Group,
  requires: List[Requires] = List.empty,
  provides: List[Provides] = List.empty,
  enhances: List[Enhances] = List.empty,
  obsoletes: List[Obsoletes] = List.empty,
  recommends : List[Recommends] = List.empty,
  conflicts : List[Conflicts] = List.empty,
  supplements : List[Supplements] = List.empty,
  suggests : List[Suggests] = List.empty,
  files: List[FileInfo] = List.empty
)
