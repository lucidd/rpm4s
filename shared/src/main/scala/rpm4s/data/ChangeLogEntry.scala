package rpm4s.data

import java.time.Instant

case class ChangeLogEntry(name: String, content: String, time: Instant)
