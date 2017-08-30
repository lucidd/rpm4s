package rpm4s.data

import cats.kernel.Comparison
import rpm4s.codecs.ConvertingError
import rpm4s.data.Version.{Alpha, Numeric, Segment, Separator, Tilde}
import rpm4s.utils.{isAlpha, isNum}

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

case class Version (segment: Segment) {
  def string: String = {
    @tailrec
    def str(seg: Option[Segment], acc: StringBuilder): String = seg match {
      case None => acc.toString()
      case Some(Tilde(next)) => str(next, acc.append('~'))
      case Some(Alpha(value, next)) => str(next, acc.append(value))
      case Some(Separator(value, next)) => str(next, acc.append(value))
      case Some(Numeric(value, next)) => str(next, acc.append(value))
    }
    str(Some(segment), new StringBuilder)
  }
}

object Version {
  sealed trait Segment {
    def next: Option[Segment]
  }
  sealed trait NotAlpha extends Segment
  sealed trait NotNumeric extends Segment
  sealed trait NotSeparator extends Segment

  def parse(value: String): Either[ConvertingError, Version] = {
    def alpha(rest: String): Either[ConvertingError, Option[Alpha]] = {
      val a = rest.takeWhile(isAlpha)
      notAlpha(rest.drop(a.length)).map { next =>
        Some(new Alpha(a, next))
      }
    }
    def numeric(rest: String): Either[ConvertingError, Option[Numeric]] = {
      val a = rest.takeWhile(isNum)
      notNum(rest.drop(a.length)).map { next =>
        Some(new Numeric(a, next))
      }
    }
    def separator(rest: String): Either[ConvertingError, Option[Separator]] = {
      val a = rest.takeWhile(Separator.validSeparatorChars.contains)
      notSep(rest.drop(a.length)).map { next =>
        Some(new Separator(a, next))
      }
    }
    def tilde(rest: String): Either[ConvertingError, Option[Tilde]] =
      segment(rest.drop(1)).map(x => Some(Tilde(x)))

    def notSep(rest: String): Either[ConvertingError, Option[NotSeparator]] = {
      rest.headOption match {
        case None => Right(None)
        case Some(c) if isAlpha(c) => alpha(rest)
        case Some(c) if isNum(c) => numeric(rest)
        case Some('~') => tilde(rest)
        case Some(c) => Left(ConvertingError(s"expected non sep segment got: '$c'"))
      }
    }
    def notNum(rest: String): Either[ConvertingError, Option[NotNumeric]] = {
      rest.headOption match {
        case None => Right(None)
        case Some(c) if isAlpha(c) => alpha(rest)
        case Some('~') => tilde(rest)
        case Some(c) if Separator.validSeparatorChars.contains(c) => separator(rest)
        case Some(c) => Left(ConvertingError(s"expected non num segment got: '$c'"))
      }
    }
    def notAlpha(rest: String): Either[ConvertingError, Option[NotAlpha]] = {
      rest.headOption match {
        case None => Right(None)
        case Some(c) if isNum(c) => numeric(rest)
        case Some('~') => tilde(rest)
        case Some(c) if Separator.validSeparatorChars.contains(c) => separator(rest)
        case Some(c) => Left(ConvertingError(s"expected non alpha segment got: '$c'"))
      }
    }
    def segment(rest: String): Either[ConvertingError, Option[Segment]] = {
      rest.headOption match {
        case None => Right(None)
        case Some(c) if isAlpha(c) => alpha(rest)
        case Some(c) if isNum(c) => numeric(rest)
        case Some('~') => tilde(rest)
        case Some(c) if Separator.validSeparatorChars.contains(c) => separator(rest)
        case Some(c) => Left(ConvertingError(s"'$c' is not a valid version char"))
      }
    }
    segment(value).flatMap {
      case Some(seg) => Right(new Version(seg))
      case None => Left(ConvertingError(s"version can not be empty"))
    }
  }


  implicit val ordering: Ordering[Version] =
    (x: Version, y: Version) => Version.compare(x, y).toInt

  def rpmvercmp(v1: String, v2: String): Either[ConvertingError, Comparison] = {
    for {
      a <- parse(v1)
      b <- parse(v2)
    } yield compare(a, b)
  }
  def compare(v1: Version, v2: Version): Comparison = {
    @tailrec
    def segment(s1: Option[Segment], s2: Option[Segment]): Comparison = {
      (s1, s2) match {
        case (None, None) => Comparison.EqualTo
        case (Some(Alpha(_, _)), None) | (Some(Numeric(_, _)), None) => Comparison.GreaterThan
        case (None, Some(Alpha(_, _))) | (None, Some(Numeric(_, _))) => Comparison.LessThan

        case (Some(Tilde(r1)), Some(Tilde(r2))) =>
          segment(r1, r2)
        case (Some(Tilde(_)), _) => Comparison.LessThan
        case (_, Some(Tilde(_))) => Comparison.GreaterThan

        case (Some(Separator(_, r1)), Some(Separator(_, r2))) =>
          segment(r1, r2)
        case (None, Some(Separator(_, r2))) =>
          segment(None, r2)
        case (Some(Separator(_, r1)), None) =>
          segment(r1, None)
        case (Some(seg), Some(Separator(_, r2))) =>
          segment(seg.next, r2)
        case (Some(Separator(_, r1)), Some(seg)) =>
          segment(r1, seg.next)


        case (Some(Alpha(_, _)), Some(Numeric(_, _))) => Comparison.LessThan
        case (Some(Numeric(_, _)), Some(Alpha(_, _))) => Comparison.GreaterThan

        case (Some(lhs@Numeric(_, r1)), Some(rhs@Numeric(_, r2))) =>
          lhs.toBigInt.compareTo(rhs.toBigInt) match {
            case 0 => segment(r1, r2)
            case x => Comparison.fromInt(x)
          }
        case (Some(Alpha(alpha1, r1)), Some(Alpha(alpha2, r2))) =>
          alpha1.compareTo(alpha2) match {
            case 0 => segment(r1, r2)
            case x => Comparison.fromInt(x)
          }
      }
    }
    segment(Some(v1.segment), Some(v2.segment))
  }


  case class Separator private[Version] (value: String, next: Option[NotSeparator]) extends Segment with NotAlpha with NotNumeric {
    def copy(value: String = value, next: Option[NotSeparator] = next): Nothing = ???
  }
  object Separator {
    // found in librpm source build/parsePreamble.c
    val validSeparatorChars = HashSet("{}%+_.": _*)
  }

  case class Tilde(next: Option[Segment]) extends Segment with NotAlpha with NotNumeric with NotSeparator

  /**
    * A version segment with one or more letters
    * @param value non empty string containing only letters
    */
  case class Alpha private[Version] (value: String, next: Option[NotAlpha]) extends Segment with NotNumeric with NotSeparator {
    def copy(value: String = value, next: Option[NotAlpha] = next): Nothing = ???
  }
  object Alpha {
  }

  /**
    * A version segment with one or more digits
    * @param value non empty string containing only digits
    *
    * Note: value is string to preserve leading zeros
    */
  case class Numeric private[Version] (value: String, next: Option[NotNumeric]) extends Segment with NotAlpha with NotSeparator {
    def toBigInt: BigInt = BigInt(value)
    def copy(value: String = value, next: Option[NotNumeric] = next): Nothing = ???
  }
  object Numeric {
  }

}
