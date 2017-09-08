package rpm4s.repo.data

import scala.util.matching.Regex

case class CVE(year: Int, number: Int) {
  def string: String = s"CVE-$year-$number"
}
object CVE {
  val regex: Regex = """CVE-(\d{4,4})-(\d{4,})""".r
  def fromString(value: String): Option[CVE] = {
    value match {
      case regex(y, n) => Some(CVE(y.toInt, n.toInt))
      case _ => None
    }
  }
  def findInText(text: String): List[CVE] = {
    regex
      .findAllMatchIn(text)
      .map { m =>
        CVE(m.group(1).toInt, m.group(2).toInt)
      }
      .toList
  }
}
