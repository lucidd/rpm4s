package rpm4s

import rpm4s.codecs._
import scodec._
import scodec.codecs._
import scodec.bits._
import rpm4s.codecs.utils._
import HeaderTag._
import rpm4s.data.HeaderType._
import rpm4s.data.OS._
import rpm4s.data.Architecture._
import SignatureTag._
import rpm4s.codecs.IndexData._
import rpm4s.data.{Architecture, HeaderRange, HeaderType, RPMType}

import scala.annotation.tailrec

package object codecs {

  implicit val rpmType: Codec[RPMType] = int16.narrowc {
    case 0     => Attempt.successful(RPMType.Binary)
    case 1     => Attempt.successful(RPMType.Source)
    case value => Attempt.failure(Err(s"Unknown value $value for RPM Type"))
  } {
    case RPMType.Binary => 0
    case RPMType.Source => 1
  }

  implicit def indexEntryCodec[T <: RPMTag](implicit t: Codec[T]): Codec[IndexEntry[T]] =
    (
      ("tag" | Codec[T]) ::
        ("type" | Codec[HeaderType]) ::
        ("offset" | uint32) ::
        ("count" | uint32)
    ).as[IndexEntry[T]]

  implicit val headerType: Codec[HeaderType] = uint32.narrowc[HeaderType] {
    case 0     => Attempt.successful(Null)
    case 1     => Attempt.successful(Char)
    case 2     => Attempt.successful(Int8)
    case 3     => Attempt.successful(Int16)
    case 4     => Attempt.successful(Int32)
    case 5     => Attempt.successful(Int64)
    case 6     => Attempt.successful(String)
    case 7     => Attempt.successful(Bin)
    case 8     => Attempt.successful(StringArray)
    case 9     => Attempt.successful(I18NString)
    case value => Attempt.failure(Err(s"Unknown value $value for RPM Type"))
  } {
    case Null        => 0
    case Char        => 1
    case Int8        => 2
    case Int16       => 3
    case Int32       => 4
    case Int64       => 5
    case String      => 6
    case Bin         => 7
    case StringArray => 8
    case I18NString  => 9
  }

  implicit val os: Codec[data.OS] = int16.narrowc[data.OS] {
    case 1     => Attempt.successful(Linux)
    case value => Attempt.failure(Err(s"Unknown value $value for RPM OS"))
  } {
    case Linux => 1
  }

  implicit val architecture: Codec[Architecture] =
    int16.narrowc[Architecture] {
      case 1  => Attempt.successful(i386)
      case 16 => Attempt.successful(ppc64)
      case value =>
        Attempt.failure(Err(s"Unknown value $value for RPM Architecture"))
    } {
      case `i386`  => 1
      case `ppc64` => 16
      case _       => ???
    }

  implicit val lead: Codec[Lead] = (
    ("magic" | constant(hex"ED AB EE DB")) ::
      ("major" | uint8) ::
      ("minor" | uint8) ::
      ("type" | Codec[RPMType]) ::
      //TODO: there is no real documentation for archnum other then the rpmrc.in file in the rpm source
      // each archnum value seems to map to multiple related architectures so
      // one to one mapping is not possible
      //("archnum"        | Codec[Architecture]) ::
      ("archnum" | short16) ::
      ("name" | fixedSizeBytes(66, cstring)) ::
      ("osnum" | Codec[data.OS]) ::
      ("signature_type" | int16) ::
      ("reserved" | bytes(16).unit(ByteVector.fromByte(0)))
  ).as[Lead]

  implicit val signatureTag: Codec[SignatureTag] =
    uint32.narrowc[SignatureTag] {
      case 62    => Attempt.successful(HeaderSignatures)
      case 267   => Attempt.successful(DSAHeader)
      case 268   => Attempt.successful(RSAHeader)
      case 269   => Attempt.successful(SHA1Header)
      case 1000  => Attempt.successful(SignatureTag.Size)
      case 1001  => Attempt.successful(LEMD5_1)
      case 1002  => Attempt.successful(PGP)
      case 1003  => Attempt.successful(LEMD5_2)
      case 1004  => Attempt.successful(MD5)
      case 1005  => Attempt.successful(GPG)
      case 1006  => Attempt.successful(PGP5)
      case 1007  => Attempt.successful(PayloadSize)
      case 1008  => Attempt.successful(ReservedSpace)
      case value => Attempt.failure(Err(s"Unknown value $value for RPM Tag"))
    } {
      case HeaderSignatures  => 62
      case DSAHeader         => 267
      case RSAHeader         => 268
      case SHA1Header        => 269
      case SignatureTag.Size => 1000
      case LEMD5_1           => 1001
      case PGP               => 1002
      case LEMD5_2           => 1003
      case MD5               => 1004
      case GPG               => 1005
      case PGP5              => 1006
      case PayloadSize       => 1007
      case ReservedSpace     => 1008
      case _                 => ???
    }

  def indexData[T <: RPMTag](header: Header[T]): Codec[List[(IndexEntry[T], IndexData)]] = {
    val codecs = header.index.sortBy(_.offset)
    new Codec[List[(IndexEntry[T], IndexData)]] {
      override def encode(value: List[(IndexEntry[T], IndexData)]): Attempt[BitVector] = {
        @tailrec
        def encodeEntry(
          acc: BitVector,
          entries: List[(IndexEntry[T], IndexData)]
        ): Attempt[BitVector] =
          entries match {
            case Nil => Attempt.successful(acc)
            case (index, data) :: nextEntries =>
              indexDataEncoder.encode(data) match {
                case r @ Attempt.Failure(_) => r
                case Attempt.Successful(bits) =>
                  val bitAlignment = index.tpe.bitAlignment
                  val padding = (bitAlignment - (acc.size % bitAlignment)) % bitAlignment
                  val paddingBits = BitVector.low(padding)
                  val result = paddingBits ++ bits
                  encodeEntry(acc ++ result, nextEntries)
              }

          }
        encodeEntry(BitVector.empty, value)
      }

      override def sizeBound: SizeBound = SizeBound.unknown

      def decode(bits: BitVector): Attempt[DecodeResult[List[(IndexEntry[T], IndexData)]]] = {
        @tailrec
        def decodeEntry(
          rest: BitVector,
          entries: List[IndexEntry[T]],
          acc: Vector[(IndexEntry[T], IndexData)]
        ): Attempt[DecodeResult[Vector[(IndexEntry[T], IndexData)]]] =
          entries match {
            case Nil =>
              val newRest = bits.drop(header.hsize * 8)
              Attempt.successful(DecodeResult(acc, newRest))
            case entry :: tail => {
              indexDataCodec(entry).decode(rest.drop(entry.offset * 8)) match {
                case Attempt.Successful(DecodeResult(value, _)) =>
                  decodeEntry(rest, tail, acc :+ ((entry, value)))
                case Attempt.Failure(err) =>
                  Attempt.failure(
                    err.pushContext(entry.toString)
                  )
              }
            }
          }
        decodeEntry(bits.take(header.hsize * 8), codecs, Vector())
          .map(_.map(_.toList))
      }
    }
  }

  implicit val headerTag: Codec[HeaderTag[_ <: IndexData]] =
    uint32.narrowc[HeaderTag[_ <: IndexData]] {
      case 62   => Attempt.successful(HeaderSignatures)
      case 63   => Attempt.successful(HeaderImmutable)
      case 100  => Attempt.successful(HeaderI18NTable)
      case 267  => Attempt.successful(DSAHeader)
      case 268  => Attempt.successful(RSAHeader)
      case 269  => Attempt.successful(SHA1Header)
      case 1000 => Attempt.successful(Name)
      case 1001 => Attempt.successful(Version)
      case 1002 => Attempt.successful(Release)
      case 1003 => Attempt.successful(Epoch)
      case 1004 => Attempt.successful(Summery)
      case 1005 => Attempt.successful(Description)
      case 1006 => Attempt.successful(BuildTime)
      case 1007 => Attempt.successful(BuildHost)
      case 1008 => Attempt.successful(InstallTime)
      case 1009 => Attempt.successful(HeaderTag.Size)
      case 1010 => Attempt.successful(Distribution)
      case 1011 => Attempt.successful(Vendor)
      case 1012 => Attempt.successful(GIF)
      case 1013 => Attempt.successful(XPM)
      case 1014 => Attempt.successful(License)
      case 1015 => Attempt.successful(Packager)
      case 1016 => Attempt.successful(Group)
      case 1018 => Attempt.successful(Source)
      case 1019 => Attempt.successful(Patch)
      case 1020 => Attempt.successful(URL)
      case 1021 => Attempt.successful(OS)
      case 1022 => Attempt.successful(Arch)

      case 1023 => Attempt.successful(PreIn)
      case 1024 => Attempt.successful(PostIn)
      case 1025 => Attempt.successful(PreUn)
      case 1026 => Attempt.successful(PostUn)
      case 1027 => Attempt.successful(OldFileNames)

      case 1085 => Attempt.successful(PreInProg)
      case 1086 => Attempt.successful(PostInProg)
      case 1087 => Attempt.successful(PreUnProg)
      case 1088 => Attempt.successful(PostUnProg)
      case 1089 => Attempt.successful(BuildArchs)

      case 1028 => Attempt.successful(FileSizes)
      case 1030 => Attempt.successful(FileModes)
      case 1033 => Attempt.successful(FileRDevs)
      case 1034 => Attempt.successful(FileMTimes)
      case 1035 => Attempt.successful(FileDigests)
      case 1036 => Attempt.successful(FileLinkTOS)
      case 1037 => Attempt.successful(FileFlags)
      case 1039 => Attempt.successful(FileUserName)
      case 1040 => Attempt.successful(FileGroupName)
      case 1044 => Attempt.successful(SourceRPM)
      case 1045 => Attempt.successful(FileVerifyFlags)
      case 1047 => Attempt.successful(ProvideName)
      case 1048 => Attempt.successful(RequireFlags)
      case 1049 => Attempt.successful(RequireName)
      case 1050 => Attempt.successful(RequireVersion)
      case 1051 => Attempt.successful(NoSource)
      case 1052 => Attempt.successful(NoPatch)
      case 1053 => Attempt.successful(ConflictFlags)
      case 1054 => Attempt.successful(ConflictName)
      case 1055 => Attempt.successful(ConflictVersion)

      case 1059 => Attempt.successful(ExcludeArch)
      case 1060 => Attempt.successful(ExcludeOS)
      case 1061 => Attempt.successful(ExclusiveArch)
      case 1062 => Attempt.successful(ExclusiveOS)

      case 1064 => Attempt.successful(RPMVersion)

      case 1065 => Attempt.successful(TriggerScripts)
      case 1066 => Attempt.successful(TriggerName)
      case 1067 => Attempt.successful(TriggerVersion)
      case 1068 => Attempt.successful(TriggerFlags)
      case 1069 => Attempt.successful(TriggerIndex)

      case 1079 => Attempt.successful(VerifyScript)

      case 1080 => Attempt.successful(ChangeLogTime)
      case 1081 => Attempt.successful(ChangeLogName)
      case 1082 => Attempt.successful(ChangeLogText)
      case 1090 => Attempt.successful(ObsoleteName)

      case 1091 => Attempt.successful(VerifyScriptProg)
      case 1092 => Attempt.successful(TriggerScriptProg)

      case 1094 => Attempt.successful(Cookie)
      case 1095 => Attempt.successful(FileDevices)
      case 1096 => Attempt.successful(FileINodes)
      case 1097 => Attempt.successful(FileLangs)
      case 1098 => Attempt.successful(Prefixes)
      case 1099 => Attempt.successful(InstPrefixes)

      case 1112 => Attempt.successful(ProvideFlags)
      case 1113 => Attempt.successful(ProvideVersion)
      case 1114 => Attempt.successful(ObsoleteFlags)
      case 1115 => Attempt.successful(ObsoleteVersion)
      case 1116 => Attempt.successful(DirIndexes)
      case 1117 => Attempt.successful(BaseNames)
      case 1118 => Attempt.successful(DirNames)
      case 1122 => Attempt.successful(OptFlags)
      case 1123 => Attempt.successful(DistURL)
      case 1124 => Attempt.successful(PayloadFormat)
      case 1125 => Attempt.successful(PayloadCompressor)
      case 1126 => Attempt.successful(PayloadFlags)
      case 1132 => Attempt.successful(Platform)
      case 1140 => Attempt.successful(FileColors)
      case 1141 => Attempt.successful(FileClass)
      case 1142 => Attempt.successful(ClassDict)
      case 1143 => Attempt.successful(FileDependSX)
      case 1144 => Attempt.successful(FileDependSN)
      case 1145 => Attempt.successful(DependsDict)
      case 1146 => Attempt.successful(SourcePkgId)

      case 1151 => Attempt.successful(PreTrans)
      case 1152 => Attempt.successful(PostTrans)
      case 1153 => Attempt.successful(PreTransProg)
      case 1154 => Attempt.successful(PostTransProg)

      case 5046 => Attempt.successful(RecommendName)
      case 5047 => Attempt.successful(RecommendVersion)
      case 5048 => Attempt.successful(RecommendFlags)

      case 5049 => Attempt.successful(SuggestName)
      case 5050 => Attempt.successful(SuggestVersion)
      case 5051 => Attempt.successful(SuggestFlags)

      case 5052 => Attempt.successful(SupplementName)
      case 5053 => Attempt.successful(SupplementVersion)
      case 5054 => Attempt.successful(SupplementFlags)

      case 5055 => Attempt.successful(EnhanceName)
      case 5056 => Attempt.successful(EnhanceVersion)
      case 5057 => Attempt.successful(EnhanceFlags)

      case 5062 => Attempt.successful(Encoding)
      case 5066 => Attempt.successful(FileTriggerScripts)
      case 5067 => Attempt.successful(FileTriggerScriptProg)
      case 5068 => Attempt.successful(FileTriggerScriptFlags)
      case 5069 => Attempt.successful(FileTriggerName)
      case 5070 => Attempt.successful(FileTriggerIndex)
      case 5071 => Attempt.successful(FileTriggerVersion)
      case 5072 => Attempt.successful(FileTriggerFlags)

      case 5084 => Attempt.successful(FileTriggerPriorities)
      case 5085 => Attempt.successful(TransFileTriggerPriorities)

      case 5090 => Attempt.successful(FileSignatures)
      case 5091 => Attempt.successful(FileSignatureLength)

      case value => Attempt.failure(Err(s"Unknown value $value for RPM Tag"))
    } {
      case HeaderSignatures => 62
      case HeaderImmutable  => 63
      case HeaderI18NTable  => 100
      case DSAHeader        => 267
      case RSAHeader        => 268
      case SHA1Header       => 269
      case Name             => 1000
      case Version          => 1001
      case Release          => 1002
      case Epoch            => 1003
      case Summery          => 1004
      case Description      => 1005
      case BuildTime        => 1006
      case BuildHost        => 1007
      case InstallTime      => 1008
      case HeaderTag.Size   => 1009
      case Distribution     => 1010
      case Vendor           => 1011
      case GIF              => 1012
      case XPM              => 1013
      case License          => 1014
      case Packager         => 1015
      case Group            => 1016
      case Source           => 1018
      case Patch            => 1019
      case URL              => 1020
      case OS               => 1021
      case Arch             => 1022

      case PreIn        => 1023
      case PostIn       => 1024
      case PreUn        => 1025
      case PostUn       => 1026
      case OldFileNames => 1027

      case PreInProg  => 1085
      case PostInProg => 1086
      case PreUnProg  => 1087
      case PostUnProg => 1088
      case BuildArchs => 1089

      case FileSizes       => 1028
      case FileModes       => 1030
      case FileRDevs       => 1033
      case FileMTimes      => 1034
      case FileDigests     => 1035
      case FileLinkTOS     => 1036
      case FileFlags       => 1037
      case FileUserName    => 1039
      case FileGroupName   => 1040
      case SourceRPM       => 1044
      case FileVerifyFlags => 1045
      case ProvideName     => 1047
      case RequireFlags    => 1048
      case RequireName     => 1049
      case RequireVersion  => 1050
      case NoSource        => 1051
      case NoPatch         => 1052
      case ConflictFlags   => 1053
      case ConflictName    => 1054
      case ConflictVersion => 1055

      case ExcludeArch   => 1059
      case ExcludeOS     => 1060
      case ExclusiveArch => 1061
      case ExclusiveOS   => 1062

      case RPMVersion => 1064

      case TriggerScripts => 1065
      case TriggerName    => 1066
      case TriggerVersion => 1067
      case TriggerFlags   => 1068
      case TriggerIndex   => 1069

      case VerifyScript => 1079

      case ChangeLogTime => 1080
      case ChangeLogName => 1081
      case ChangeLogText => 1082
      case ObsoleteName  => 1090

      case VerifyScriptProg  => 1091
      case TriggerScriptProg => 1092

      case Cookie            => 1094
      case FileDevices       => 1095
      case FileINodes        => 1096
      case FileLangs         => 1097
      case Prefixes          => 1098
      case InstPrefixes      => 1099
      case ProvideFlags      => 1112
      case ProvideVersion    => 1113
      case ObsoleteFlags     => 1114
      case ObsoleteVersion   => 1115
      case DirIndexes        => 1116
      case BaseNames         => 1117
      case DirNames          => 1118
      case OptFlags          => 1122
      case DistURL           => 1123
      case PayloadFormat     => 1124
      case PayloadCompressor => 1125
      case PayloadFlags      => 1126
      case Platform          => 1132
      case FileColors        => 1140
      case FileClass         => 1141
      case ClassDict         => 1142
      case FileDependSX      => 1143
      case FileDependSN      => 1144
      case DependsDict       => 1145
      case SourcePkgId       => 1146

      case PreTrans      => 1151
      case PostTrans     => 1152
      case PreTransProg  => 1153
      case PostTransProg => 1154

      case RecommendName    => 5046
      case RecommendVersion => 5047
      case RecommendFlags   => 5048

      case SuggestName    => 5049
      case SuggestVersion => 5050
      case SuggestFlags   => 5051

      case SupplementName    => 5052
      case SupplementVersion => 5053
      case SupplementFlags   => 5054

      case EnhanceName    => 5055
      case EnhanceVersion => 5056
      case EnhanceFlags   => 5057

      case HeaderImage   => ???
      case HeaderRegions => ???
      case Encoding      => 5062

      case FileTriggerScripts     => 5066
      case FileTriggerScriptProg  => 5067
      case FileTriggerScriptFlags => 5068
      case FileTriggerName        => 5069
      case FileTriggerIndex       => 5070
      case FileTriggerVersion     => 5071
      case FileTriggerFlags       => 5072

      case FileTriggerPriorities      => 5084
      case TransFileTriggerPriorities => 5085

      case FileSignatures      => 5090
      case FileSignatureLength => 5091
    }

  implicit def headerCodec[T <: RPMTag](implicit t: Codec[T]): Codec[Header[T]] =
    (
      ("magic" | constant(hex"8E AD E8")) ::
        ("version" | uint8) ::
        ("reserved" | bytes(4).unit(ByteVector.fromByte(0))) ::
        (("index_size" | int32) >>:~ { indexSize =>
        ("signature_size" | uint32) ::
          ("index" | listOfN(provide(indexSize), Codec[IndexEntry[T]]))
      })
    ).as[Header[T]]

  val stringDataCodec = yoloString.as[StringData]
  val stringArrayCodec = vector(yoloString).as[StringArrayData]
  val i18nStringCodec = vector(yoloString).as[I18NStringArrayData]
  def stringArrayCodec2(count: Int) =
    vectorOfN(provide(count), yoloString).as[StringArrayData]
  def i18nStringCodec2(count: Int) =
    vectorOfN(provide(count), yoloString).as[I18NStringArrayData]

  def indexDataDecoder(tpe: HeaderType, count: Int = 1): Decoder[IndexData] =
    tpe match {
      case HeaderType.String      => stringDataCodec
      case HeaderType.Bin         => bytes(count).as[BinaryData]
      case HeaderType.Int8        => vectorOfN(provide(count), byte).as[Int8Data]
      case HeaderType.Int16       => vectorOfN(provide(count), short16).as[Int16Data]
      case HeaderType.Int32       => vectorOfN(provide(count), int32).as[Int32Data]
      case HeaderType.Int64       => vectorOfN(provide(count), int64).as[Int64Data]
      case HeaderType.StringArray => stringArrayCodec2(count)
      case HeaderType.I18NString  => i18nStringCodec2(count)
      case HeaderType.Char        => ??? //TODO: what to do with this?
      case HeaderType.Null        => ??? //TODO: what to do with this?
    }

  val indexDataEncoder: Encoder[IndexData] = new Encoder[IndexData] {
    override def encode(value: IndexData): Attempt[BitVector] = value match {
      case data @ StringData(_) => stringDataCodec.encode(data)
      case _ @BinaryData(bytes) => Attempt.successful(bytes.bits)
      case _ @Int8Data(values) =>
        Attempt.successful(BitVector.apply(values.toArray))
      case _ @Int16Data(values)          => vector(short16).encode(values)
      case _ @Int32Data(values)          => vector(int32).encode(values)
      case _ @Int64Data(values)          => vector(int64).encode(values)
      case data @ StringArrayData(_)     => stringArrayCodec.encode(data)
      case data @ I18NStringArrayData(_) => i18nStringCodec.encode(data)
    }
    override def sizeBound: SizeBound = SizeBound.unknown
  }

  def indexDataCodec[T <: RPMTag](indexEntry: IndexEntry[T]): Codec[IndexData] =
    Codec(indexDataEncoder, indexDataDecoder(indexEntry.tpe, indexEntry.count.toInt))

  def sequentialIndexDataDecoder[T <: RPMTag](
    header: Header[T]
  ): Decoder[Vector[(IndexEntry[T], IndexData)]] = {
    val codecs = header.index.sortBy(_.offset)
    (bits: BitVector) =>
      {
        @tailrec
        def decodeEntry(
          rest: BitVector,
          entries: List[IndexEntry[T]],
          bitcount: Long,
          acc: Vector[(IndexEntry[T], IndexData)]
        ): Attempt[DecodeResult[Vector[(IndexEntry[T], IndexData)]]] =
          entries match {
            case Nil =>
              Attempt.successful(DecodeResult(acc, rest))
            case entry :: tail => {
              val next = tail.headOption
              val byteSize = next match {
                case Some(n) => n.offset - entry.offset
                case None    => header.hsize - (bitcount / 8)
              }
              val bitsSize = byteSize * 8
              val bitAlignment = entry.tpe.bitAlignment
              val padding = (bitAlignment - (bitcount % bitAlignment)) % bitAlignment

              rest.acquire(padding) match {
                case Left(error) => Attempt.failure(Err.apply(error))
                case Right(_) =>
                  val r = rest.drop(padding)
                  fixedSizeBits(bitsSize, indexDataCodec(entry))
                    .decode(r) match {
                    case Attempt.Successful(DecodeResult(value, rest)) =>
                      decodeEntry(rest, entries.tail, bitcount + bitsSize, acc :+ ((entry, value)))
                    case Attempt.Failure(err) =>
                      Attempt.failure(
                        err.pushContext(entry.toString)
                      )
                  }
              }
            }
          }

        decodeEntry(bits, codecs, 0, Vector())
      }
  }

  /**
    * Creates an encoder for type T as long as there is a Extractor
    * for it.
    *
    * This method tries to be as efficient as possible in only
    * decoding and extracting the information needed to construct T.
    *
    */
  def decoder[T](implicit extractor: Extractor[T]): Decoder[T] =
    Decoder { bits =>
      val leadSkipped = bits.drop((96 + 4 + 4) * 8L)
      val attempt = for {
        indexCount <- uint32.decode(leadSkipped)
        dataSize <- uint32.decode(indexCount.remainder)
        padding = {
          val mod = (dataSize.value * 8) % 64
          if (mod == 0) 0 else 64 - mod
        }
        sigDataSize = (indexCount.value * (4 + 4 + 4 + 4) * 8) + (dataSize.value * 8) + padding
        // TODO parse signature headers and data
        rest = dataSize.remainder.drop(sigDataSize)
        // TODO: parse only headers until we collected all relevant and skip rest
        header <- headerCodec[HeaderTag[IndexData]].decode(rest)
        head = {
          header.value.copy(index = header.value.index.filter(x => extractor.tags.contains(x.tag)))
        }
        data <- indexData[HeaderTag[IndexData]](
          head
        ).decode(header.remainder)
        map = data.value.map {
          case (k, v) =>
            k.tag -> v
        }.toMap
        result <- extractor
          .extract(
            new Extractor.Data {
              override val headerRange = Some(
                HeaderRange.startSize(
                  (bits.size - rest.size) / 8,
                  (rest.size - data.remainder.size) / 8
                )
              )

              override val payload: Option[BitVector] =
                if (extractor.payload) Some(data.remainder)
                else None

              def apply[A <: IndexData](tag: HeaderTag[A]): Extractor.Result[A] =
                map.get(tag).map(_.asInstanceOf[A]) match {
                  case Some(x) => Right(x)
                  case None    => Left(MissingHeader(tag))
                }
           }
          )
          .fold(
            err => Attempt.failure(Err(err.toString)),
            x => Attempt.successful(DecodeResult(x, data.remainder))
          )
      } yield result
      attempt
    }

  implicit val codec: Codec[RpmFile] = (
    ("lead" | Codec[Lead]) ::
      (("signature" | Codec[Header[SignatureTag]]) >>:~ { sigHeader =>
      ("data" | alignTo(indexData(sigHeader), 64)) ::
        (("header" | Codec[Header[HeaderTag[IndexData]]]) >>:~ { header =>
        ("data" | indexData(header)) ::
          ("payload" | scodec.codecs.bits)
      })
    })
  ).as[RpmFile]

}
