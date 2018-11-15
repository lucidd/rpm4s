package rpm4s.data

import scala.util.matching.Regex

case class CVE private (year: Int, number: Int) {
  def string: String = {
    val num = number.toString.reverse.padTo(4, '0').reverse
    s"CVE-$year-$num"
  }
}
object CVE {
  val regex: Regex = """CVE-(\d{4,4})-(\d{4,})""".r
  def fromParts(year: Int, number: Int): Option[CVE] = {
    if (year < 1000) None
    else Some(CVE(year, number))
  }
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
