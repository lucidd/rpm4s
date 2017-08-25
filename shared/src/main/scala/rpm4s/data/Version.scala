package rpm4s.data

import rpm4s.data.Version.Segment

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

case class Version(segments: List[Segment]) extends AnyVal {
  import Version._
  def string: String = {
    segments.foldLeft(new StringBuilder) { case (sb, s) =>
      s match {
        case Alpha(v) => sb.append(v)
        case Numeric(v) => sb.append(v)
        case Separator(v) => sb.append(v)
        case Tilde => sb.append('~')
      }
    }.toString
  }
}

object Version {

  def parse(value: String): Option[Version] = {
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
    val segments = seg(value, new ListBuffer)
    if (segments.isEmpty) None
    else Some(Version(segments))
  }

  implicit val ordering: Ordering[Version] =
    (x: Version, y: Version) => Version.compare(x, y)

  def rpmvercmp(v1: String, v2: String): Option[Int] = {
    for {
      a <- parse(v1)
      b <- parse(v2)
    } yield compare(a, b)
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

  sealed trait Segment extends Product with Serializable
  case class Separator(value: String) extends Segment
  case object Tilde extends Segment
  case class Alpha(value: String) extends Segment
  case class Numeric(value: BigInt) extends Segment
}
