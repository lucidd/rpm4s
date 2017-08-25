package rpm4s.data

import rpm4s.data.SenseFlags.Sense

class SenseFlags(val value: Int) extends AnyVal {
  def &(other: SenseFlags) = SenseFlags(value & other.value)
  def |(other: SenseFlags) = SenseFlags(value | other.value)
  def ^(other: SenseFlags) = SenseFlags(value ^ other.value)
  def union(other: SenseFlags) = this | other
  def intersection(other: SenseFlags) = this & other
  def diff(other: SenseFlags) = this ^ other
  def inverse: SenseFlags = this ^ SenseFlags.All
  def -(other: SenseFlags) = this & other.inverse

  def sense: Sense = {
    this & SenseFlags.SenseMask match {
      case SenseFlags.Any => Sense.Any
      case SenseFlags.Equal => Sense.EQ
      case SenseFlags.Greater => Sense.GT
      case SenseFlags.GreaterEqual => Sense.GE
      case SenseFlags.Less => Sense.LT
      case SenseFlags.LessEqual => Sense.LE
      case _ =>
        throw new RuntimeException(
          "This should not happen if SenseMask is correct")
    }
  }

  /**
    * @see https://github.com/rpm-software-management/yum/blob/8bbb3a9b79aed25815986d87f41902dae397814c/yum/packages.py#L1621
    * @return
    */
  def isPreReq: Boolean = containsAny(SenseFlags.PreReqMask)

  def containsAll(attributes: SenseFlags): Boolean =
    (value & attributes.value) == attributes.value
  def containsAny(attributes: SenseFlags): Boolean =
    (value & attributes.value) != 0
  override def toString: String = {
    import SenseFlags._
    if (this == Any) {
      "SenseFlags(Any)"
    } else {
      val flags: String = List(
        if (containsAny(Less)) List("Less") else List.empty,
        if (containsAny(Greater)) List("Greater") else List.empty,
        if (containsAny(Equal)) List("Equal") else List.empty,
        if (containsAny(PostTrans)) List("PostTrans") else List.empty,
        if (containsAny(PreReq)) List("PreReq") else List.empty,
        if (containsAny(PreTrans)) List("PreTrans") else List.empty,
        if (containsAny(Interp)) List("Interp") else List.empty,
        if (containsAny(ScriptPre)) List("ScriptPre") else List.empty,
        if (containsAny(ScriptPost)) List("ScriptPost") else List.empty,
        if (containsAny(ScriptPreUn)) List("ScriptPreUn") else List.empty,
        if (containsAny(ScriptPostUn)) List("ScriptPostUn") else List.empty,
        if (containsAny(ScriptVerify)) List("ScriptVerify") else List.empty,
        if (containsAny(FindRequires)) List("FindRequires") else List.empty,
        if (containsAny(FindProvides)) List("FindProvides") else List.empty,
        if (containsAny(TriggerIn)) List("TriggerIn") else List.empty,
        if (containsAny(TriggerUn)) List("TriggerUn") else List.empty,
        if (containsAny(TriggerPostUn)) List("TriggerPostUn") else List.empty,
        if (containsAny(MissingOk)) List("MissingOk") else List.empty,
        if (containsAny(SenseFlags.RpmLib)) List("RpmLib") else List.empty,
        if (containsAny(TriggerPreIn)) List("TriggerPreIn") else List.empty,
        if (containsAny(Keyring)) List("Keyring") else List.empty,
        if (containsAny(Config)) List("Config") else List.empty
      ).flatten.mkString("|")
      s"SenseFlags($flags)"
    }
  }
}

object SenseFlags {

  sealed trait Sense extends Product with Serializable
  object Sense {
    case object Any extends Sense
    case object GT extends Sense
    case object GE extends Sense
    case object LT extends Sense
    case object LE extends Sense
    case object EQ extends Sense
  }

  case class Provides(sense: Sense, findProvides: Boolean)
  case class Trigger(
      sense: Sense,
      in: Boolean,
      un: Boolean,
      preIn: Boolean,
      preUn: Boolean)
  case class Requires(
      sense: Sense,
      interp: Boolean,
      pre: Boolean,
      post: Boolean,
      preUn: Boolean,
      postUn: Boolean,
      verify: Boolean,
      findRequires: Boolean,
      rpmlib: Boolean,
      keyring: Boolean,
      preTrans: Boolean,
      postTrans: Boolean,
      preReq: Boolean,
      missingOk: Boolean
  )

  def apply(int: Int): SenseFlags = new SenseFlags(int)

  val Any = SenseFlags(0)
  val Less = SenseFlags(1 << 1)
  val Greater = SenseFlags(1 << 2)
  val Equal = SenseFlags(1 << 3)

  val GreaterEqual: SenseFlags = Greater | Equal
  val LessEqual: SenseFlags = Less | Equal

  val PostTrans = SenseFlags(1 << 5) /*!< %posttrans dependency */
  val PreReq = SenseFlags(1 << 6) /* legacy prereq dependency */
  val PreTrans = SenseFlags(1 << 7) /*!< Pre-transaction dependency. */
  val Interp = SenseFlags(1 << 8) /*!< Interpreter used by scriptlet. */
  val ScriptPre = SenseFlags(1 << 9) /*!< %pre dependency. */
  val ScriptPost = SenseFlags(1 << 10) /*!< %post dependency. */
  val ScriptPreUn = SenseFlags(1 << 11) /*!< %preun dependency. */
  val ScriptPostUn = SenseFlags(1 << 12) /*!< %postun dependency. */
  val ScriptVerify = SenseFlags(1 << 13) /*!< %verify dependency. */
  val FindRequires = SenseFlags(1 << 14) /*!< find-requires generated dependency. */
  val FindProvides = SenseFlags(1 << 15) /*!< find-provides generated dependency. */

  val TriggerIn = SenseFlags(1 << 16) /*!< %triggerin dependency. */
  val TriggerUn = SenseFlags(1 << 17) /*!< %triggerun dependency. */
  val TriggerPostUn = SenseFlags(1 << 18) /*!< %triggerpostun dependency. */
  val MissingOk = SenseFlags(1 << 19) /*!< suggests/enhances hint. */
  val RpmLib = SenseFlags(1 << 24) /*!< rpmlib(feature) dependency. */
  val TriggerPreIn = SenseFlags(1 << 25) /*!< %triggerprein dependency. */
  val Keyring = SenseFlags(1 << 26)
  val Config = SenseFlags(1 << 28)

  val SenseMask: SenseFlags =
    Less | Equal | Greater | Any

  val TriggerMask: SenseFlags =
    TriggerPreIn | TriggerIn | TriggerUn | TriggerPostUn

  val PreReqMask =
    PreReq | ScriptPre | ScriptPost

  val AllRequiresMask: SenseFlags =
    Interp | ScriptPre | ScriptPost | ScriptPreUn | ScriptPostUn
  ScriptVerify | FindRequires | RpmLib | Keyring | PreTrans
  PostTrans | PreReq | MissingOk

  val All: SenseFlags = Any | Less | Greater | Equal |
    PostTrans | PreReq | PreTrans | Interp |
    ScriptPre | ScriptPost | ScriptPreUn | ScriptPostUn |
    ScriptVerify | FindRequires | FindProvides |
    TriggerIn | TriggerUn | TriggerPostUn |
    MissingOk | RpmLib | TriggerPreIn |
    Keyring | Config

}
