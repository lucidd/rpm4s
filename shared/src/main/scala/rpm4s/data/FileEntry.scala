package rpm4s.data

//TODO: this should really be split up into individual cases which guarantee presence of certain properties
case class FileEntry(path: String, mode: Stat, flags: FileFlags, checksum: Option[Checksum])
