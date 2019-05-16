package rpm4s.codecs

import rpm4s.codecs.IndexData._

sealed trait RPMTag extends Product with Serializable
sealed trait HeaderTag[+T <: IndexData] extends RPMTag

//TODO: add Optional / Required info https://docs.fedoraproject.org/ro/Fedora_Draft_Documentation/0.1/html/RPM_Guide/ch-package-structure.html
//TODO: add obsolete info
object HeaderTag {

  //TODO: Find out if Image Signature and Regions are really binary data
  case object HeaderImage extends HeaderTag[BinaryData] with SignatureTag
  case object HeaderSignatures extends HeaderTag[BinaryData] with SignatureTag
  case object HeaderImmutable extends HeaderTag[BinaryData] with SignatureTag
  case object HeaderRegions extends HeaderTag[BinaryData] with SignatureTag
  case object HeaderI18NTable extends HeaderTag[StringArrayData] with SignatureTag

  case object DSAHeader extends HeaderTag[BinaryData] with SignatureTag
  case object RSAHeader extends HeaderTag[BinaryData] with SignatureTag
  case object SHA1Header extends HeaderTag[StringData] with SignatureTag

  case object Name extends HeaderTag[StringData]
  case object Version extends HeaderTag[StringData]
  case object Release extends HeaderTag[StringData]
  case object Epoch extends HeaderTag[Int32Data]
  case object Summery extends HeaderTag[I18NStringArrayData]
  case object Description extends HeaderTag[I18NStringArrayData]
  case object BuildTime extends HeaderTag[Int32Data]
  case object BuildHost extends HeaderTag[StringData]
  case object InstallTime extends HeaderTag[Int32Data]
  case object Size extends HeaderTag[Int32Data]
  case object LongSize extends HeaderTag[Int64Data]
  case object Distribution extends HeaderTag[StringData]
  case object Vendor extends HeaderTag[StringData]

  case object GIF extends HeaderTag[BinaryData]
  case object XPM extends HeaderTag[BinaryData]

  case object License extends HeaderTag[StringData]
  case object Packager extends HeaderTag[StringData]
  case object Group extends HeaderTag[I18NStringArrayData]
  case object URL extends HeaderTag[StringData]
  case object OS extends HeaderTag[StringData]
  case object Arch extends HeaderTag[StringData]
  case object OldFileNames extends HeaderTag[StringArrayData]
  case object FileSizes extends HeaderTag[Int32Data]
  case object LongFileSizes extends HeaderTag[Int64Data]
  case object FileModes extends HeaderTag[Int16Data]
  case object FileRDevs extends HeaderTag[Int16Data]
  case object FileMTimes extends HeaderTag[Int32Data]
  case object FileDigests extends HeaderTag[StringArrayData]
  case object FileLinkTOS extends HeaderTag[StringArrayData]
  case object FileFlags extends HeaderTag[Int32Data]
  case object FileUserName extends HeaderTag[StringArrayData]
  case object FileGroupName extends HeaderTag[StringArrayData]
  case object Source extends HeaderTag[StringArrayData]
  case object Patch extends HeaderTag[StringArrayData]
  case object SourceRPM extends HeaderTag[StringData]
  case object FileVerifyFlags extends HeaderTag[Int32Data]
  case object ProvideName extends HeaderTag[StringArrayData]

  case object RequireFlags extends HeaderTag[Int32Data]
  case object RequireName extends HeaderTag[StringArrayData]
  case object RequireVersion extends HeaderTag[StringArrayData]

  case object NoSource extends HeaderTag[Int32Data]
  case object NoPatch extends HeaderTag[Int32Data]
  case object RPMVersion extends HeaderTag[StringData]

  case object TriggerScripts extends HeaderTag[StringArrayData]
  case object TriggerName extends HeaderTag[StringArrayData]
  case object TriggerVersion extends HeaderTag[StringArrayData]
  case object TriggerFlags extends HeaderTag[Int32Data]
  case object TriggerIndex extends HeaderTag[Int32Data]
  case object VerifyScript extends HeaderTag[StringData]

  case object ChangeLogTime extends HeaderTag[Int32Data]
  case object ChangeLogName extends HeaderTag[StringArrayData]
  case object ChangeLogText extends HeaderTag[StringArrayData]
  case object ObsoleteName extends HeaderTag[StringArrayData]

  case object TriggerScriptProg extends HeaderTag[StringArrayData]
  case object VerifyScriptProg extends HeaderTag[StringArrayData]

  case object Cookie extends HeaderTag[StringData]
  case object FileDevices extends HeaderTag[Int32Data]
  /*
   https://www.redhat.com/archives/rhl-list/2008-December/msg03025.html
   Hysterical as it is, the inode numbers in packages are from the host used to build the package so they'll never match what you have installed. The inode numbers in packages are only used for rpm internal hardlink handling. 
  **/
  case object FileINodes extends HeaderTag[Int32Data]
  case object FileLangs extends HeaderTag[StringArrayData]
  case object Prefixes extends HeaderTag[StringArrayData]
  case object InstPrefixes extends HeaderTag[StringArrayData]
  case object ProvideFlags extends HeaderTag[Int32Data]
  case object SourcePackage extends HeaderTag[Int32Data]
  case object ProvideVersion extends HeaderTag[StringArrayData]
  case object ObsoleteFlags extends HeaderTag[Int32Data]
  case object ObsoleteVersion extends HeaderTag[StringArrayData]
  case object DirIndexes extends HeaderTag[Int32Data]
  case object BaseNames extends HeaderTag[StringArrayData]
  case object DirNames extends HeaderTag[StringArrayData]
  case object OptFlags extends HeaderTag[StringData]
  case object DistURL extends HeaderTag[StringData]
  case object PayloadFormat extends HeaderTag[StringData]
  case object PayloadCompressor extends HeaderTag[StringData]
  case object PayloadFlags extends HeaderTag[StringData]
  case object Platform extends HeaderTag[StringData]
  case object FileColors extends HeaderTag[Int32Data]
  case object FileClass extends HeaderTag[Int32Data]
  case object ClassDict extends HeaderTag[StringArrayData]
  case object FileDependSX extends HeaderTag[Int32Data]
  case object FileDependSN extends HeaderTag[Int32Data]
  case object DependsDict extends HeaderTag[Int32Data]
  case object SourcePkgId extends HeaderTag[BinaryData]

  case object PreTrans extends HeaderTag[StringData]
  case object PostTrans extends HeaderTag[StringData]
  case object PreTransProg extends HeaderTag[StringData]
  case object PostTransProg extends HeaderTag[StringData]

  case object ConflictFlags extends HeaderTag[Int32Data]
  case object ConflictName extends HeaderTag[StringArrayData]
  case object ConflictVersion extends HeaderTag[StringArrayData]

  case object ExcludeArch extends HeaderTag[StringArrayData]
  case object ExcludeOS extends HeaderTag[StringArrayData]
  case object ExclusiveArch extends HeaderTag[StringArrayData]
  case object ExclusiveOS extends HeaderTag[StringArrayData]

  case object PreIn extends HeaderTag[StringData]
  case object PostIn extends HeaderTag[StringData]
  case object PreUn extends HeaderTag[StringData]
  case object PostUn extends HeaderTag[StringData]
  case object PreInProg extends HeaderTag[StringArrayData]
  case object PostInProg extends HeaderTag[StringArrayData]
  case object PreUnProg extends HeaderTag[StringArrayData]
  case object PostUnProg extends HeaderTag[StringArrayData]

  case object BuildArchs extends HeaderTag[StringArrayData]

  case object FileCaps extends HeaderTag[StringArrayData]
  case object FileDigestAlgo extends HeaderTag[Int32Data]

  case object OrderName extends HeaderTag[StringArrayData]
  case object OrderVersion extends HeaderTag[StringArrayData]
  case object OrderFlags extends HeaderTag[Int32Data]

  case object RecommendName extends HeaderTag[StringArrayData]
  case object RecommendVersion extends HeaderTag[StringArrayData]
  case object RecommendFlags extends HeaderTag[Int32Data]

  case object SuggestName extends HeaderTag[StringArrayData]
  case object SuggestVersion extends HeaderTag[StringArrayData]
  case object SuggestFlags extends HeaderTag[Int32Data]

  case object SupplementName extends HeaderTag[StringArrayData]
  case object SupplementVersion extends HeaderTag[StringArrayData]
  case object SupplementFlags extends HeaderTag[Int32Data]

  case object EnhanceName extends HeaderTag[StringArrayData]
  case object EnhanceVersion extends HeaderTag[StringArrayData]
  case object EnhanceFlags extends HeaderTag[Int32Data]

  case object Encoding extends HeaderTag[StringData]

  case object FileTriggerScripts extends HeaderTag[StringArrayData]
  case object FileTriggerScriptProg extends HeaderTag[StringArrayData]
  case object FileTriggerScriptFlags extends HeaderTag[Int32Data]
  case object FileTriggerName extends HeaderTag[StringArrayData]
  case object FileTriggerIndex extends HeaderTag[Int32Data]
  case object FileTriggerVersion extends HeaderTag[StringArrayData]
  case object FileTriggerFlags extends HeaderTag[Int32Data]

  case object FileTriggerPriorities extends HeaderTag[Int32Data]
  case object TransFileTriggerPriorities extends HeaderTag[Int32Data]

  case object FileSignatures extends HeaderTag[StringArrayData]
  case object FileSignatureLength extends HeaderTag[Int32Data] // single value
  case object PayloadDigest extends HeaderTag[StringArrayData]
  case object PayloadDigestAlgo extends HeaderTag[Int32Data] // single value
}

sealed trait SignatureTag extends RPMTag {
  type Data = IndexData
}
object SignatureTag {
  case object Size extends SignatureTag
  case object LEMD5_1 extends SignatureTag
  case object PGP extends SignatureTag
  case object LEMD5_2 extends SignatureTag
  case object MD5 extends SignatureTag
  case object GPG extends SignatureTag
  case object PGP5 extends SignatureTag
  case object PayloadSize extends SignatureTag
  case object ReservedSpace extends SignatureTag
}
