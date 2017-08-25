package rpm4s.data

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

case class Version(value: String) extends AnyVal {
  import Version._
  def segments: List[Segment] = {
    def seg(rest: String, acc: ListBuffer[Segment]): List[Segment] = {
      rest.headOption match {
        case None => acc.toList
        case Some(c) if c.isLetter =>
          val alpha = rest.takeWhile(_.isLetter)
          seg(rest.drop(alpha.length), acc :+ Alpha(alpha))
        case Some(c) if c.isDigit =>
          val digits = rest.takeWhile(_.isDigit)
          seg(rest.drop(digits.length), acc :+ Numeric(BigInt(digits)))
        case Some('~') =>
          seg(rest.tail, acc :+ Tilde)
        case Some(_) =>
          val separator = rest.takeWhile(c => !(c.isLetterOrDigit || c == '~'))
          seg(rest.drop(separator.length), acc :+ Separator(separator))
      }
    }
    seg(value, new ListBuffer)
  }
}

object Version {

  implicit val ordering: Ordering[Version] = new Ordering[Version] {
    def compare(x: Version, y: Version): Int = Version.compare(x, y)
  }

  def rpmvercmp(v1: String, v2: String): Int = {
    compare(Version(v1), Version(v2))
  }
  def compare(v1: Version, v2: Version): Int = {
    @tailrec
    def segment(s1: List[Segment], s2: List[Segment]): Int = {
      (s1.headOption, s2.headOption) match {
        case (None, None) => 0
        case (Some(Alpha(_)), None) | (Some(Numeric(_)), None) => 1
        case (None, Some(Alpha(_))) | (None, Some(Numeric(_))) => -1

        case (Some(Separator(_)), Some(Separator(_))) =>
          segment(s1.tail, s2.tail)
        case (_, Some(Separator(_))) => segment(s1, s2.tail)
        case (Some(Separator(_)), _) => segment(s1.tail, s2)

        case (Some(Tilde), Some(Tilde)) => segment(s1.tail, s2.tail)
        case (Some(Tilde), _) => -1
        case (_, Some(Tilde)) => 1

        case (Some(Alpha(_)), Some(Numeric(_))) => -1
        case (Some(Numeric(_)), Some(Alpha(_))) => 1

        case (Some(Numeric(num1)), Some(Numeric(num2))) =>
          num1.compareTo(num2) match {
            case 0 => segment(s1.tail, s2.tail)
            case x => Integer.signum(x)
          }
        case (Some(Alpha(alpha1)), Some(Alpha(alpha2))) =>
          alpha1.compareTo(alpha2) match {
            case 0 => segment(s1.tail, s2.tail)
            case x => Integer.signum(x)
          }
      }
    }
    segment(v1.segments, v2.segments)
  }

  def fromString(value: String): Option[Version] = {
    if (value.isEmpty) None
    else Some(Version(value))
  }
  sealed trait Segment extends Product with Serializable
  case class Separator(value: String) extends Segment
  case object Tilde extends Segment
  case class Alpha(value: String) extends Segment
  case class Numeric(value: BigInt) extends Segment
}
