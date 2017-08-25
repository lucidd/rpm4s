package rpm4s.data

import java.time.OffsetDateTime

case class ChangeLogEntry(name: String, content: String, time: OffsetDateTime)
