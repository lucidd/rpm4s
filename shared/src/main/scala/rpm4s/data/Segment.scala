package rpm4s.data

import cats.Comparison
import rpm4s.codecs.ConvertingError
import rpm4s.data.Segment.{Alpha, Numeric, Separator, Tilde}
import rpm4s.utils
import rpm4s.utils.{isAlpha, isNum}

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

sealed trait Segment {
  def next: Option[Segment]
  def string: String = {
    @tailrec
    def str(seg: Option[Segment], acc: StringBuilder): String = seg match {
      case None => acc.toString()
      case Some(Tilde(next)) => str(next, acc.append('~'))
      case Some(Alpha(value, next)) => str(next, acc.append(value))
      case Some(Separator(value, next)) => str(next, acc.append(value))
      case Some(Numeric(value, next)) => str(next, acc.append(value))
    }
    str(Some(this), new StringBuilder)
  }
}

object Segment {

  val validChars: String = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).mkString + "{}%+_.~"

  def isValidSegmentChar(c: Char): Boolean =
    utils.isAlphaNumOr(c, Separator.validChars + "~")

  implicit val ordering: Ordering[Segment] = new Ordering[Segment] {
    override def compare(x: Segment, y: Segment): Int = {
      segment(Some(x), Some(y)).toInt
    }
  }

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

  //These traits ensure alpha, numeric and separator can not follow after themselves
  sealed trait NotAlpha extends Segment
  sealed trait NotNumeric extends Segment
  sealed trait NotSeparator extends Segment

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

  case class Separator private[Segment] (value: String, next: Option[NotSeparator]) extends Segment with NotAlpha with NotNumeric {
    def copy(value: String = value, next: Option[NotSeparator] = next): Either[ConvertingError, Separator] = {
      Separator(value, next)
    }
  }
  object Separator {
    def apply(value: String, next: Option[NotSeparator]): Either[ConvertingError, Separator] = {
      if (value.nonEmpty && value.forall(validSeparatorChars.contains))
        Right(new Separator(value, next))
      else Left(ConvertingError(s"$value does contain invalid chars for a separator segment"))
    }
    // found in librpm source build/parsePreamble.c
    val validChars = "{}%+_."
    val validSeparatorChars = HashSet(validChars: _*)
  }

  case class Tilde(next: Option[Segment]) extends Segment with NotAlpha with NotNumeric with NotSeparator

  /**
    * A version segment with one or more letters
    * @param value non empty string containing only letters
    */
  case class Alpha private[Segment] (value: String, next: Option[NotAlpha]) extends Segment with NotNumeric with NotSeparator {
    def copy(value: String = value, next: Option[NotAlpha] = next): Either[ConvertingError, Alpha] = {
      Alpha(value, next)
    }
  }
  object Alpha {
    def apply(value: String, next: Option[NotAlpha]): Either[ConvertingError, Alpha] = {
      if (value.nonEmpty && value.forall(isAlpha))
        Right(new Alpha(value, next))
      else Left(ConvertingError(s"$value does contain invalid chars for a alpha segment"))
    }
  }

  /**
    * A version segment with one or more digits
    * @param value non empty string containing only digits
    *
    * Note: value is string to preserve leading zeros
    */
  case class Numeric private[Segment] (value: String, next: Option[NotNumeric]) extends Segment with NotAlpha with NotSeparator {
    def toBigInt: BigInt = BigInt(value)
    def copy(value: String = value, next: Option[NotNumeric] = next): Either[ConvertingError, Numeric] = {
      Numeric(value, next)
    }
  }
  object Numeric {
    def apply(value: String, next: Option[NotNumeric]): Either[ConvertingError, Numeric] = {
      if (value.nonEmpty && value.forall(isNum))
        Right(new Numeric(value, next))
      else Left(ConvertingError(s"$value does contain invalid chars for a numeric segment"))
    }
  }
}
