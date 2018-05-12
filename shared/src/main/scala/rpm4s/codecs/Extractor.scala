package rpm4s.codecs

import java.time.{Instant, ZoneOffset}

import rpm4s.codecs.Extractor.Data
import rpm4s.codecs.IndexData.{Int32Data, StringArrayData}
import rpm4s.data.Checksum.{Md5, Sha1, Sha256, Sha512}
import rpm4s.data.Dependency._
import rpm4s.data.Stat.FileType
import rpm4s.data._
import scodec.bits.BitVector
import shapeless.{::, Generic, HList, HNil, Lazy}

/**
  * An extractor is used to extract certain information out of an RPM.
  *
  * It contains a description of what information should be kept when parsing
  * an RPM and provides the "extract" function to use this information to provide
  * the final value of type T
  *
  * @tparam T the type of the extracted value
  */
trait Extractor[T] {
  val lead: Boolean = false
  val headerRange: Boolean = false
  val payload: Boolean = false
  val tags: Set[HeaderTag[_ <: IndexData]]
  val sigTags: Set[SignatureTag]

  /**
    * Converts the raw data to a value of type T
    *
    * @param data raw data from the RPM
    * @return the converted value of type T
    *         or an error if something went wrong
    */
  def extract(data: Data): Extractor.Result[T]
}

object Extractor {

  /**
    * Holds the data for an [[Extractor]]
    */
  trait Data {
    val lead: Option[Lead] = None
    val headerRange: Option[HeaderRange] = None
    val payload: Option[BitVector] = None
    def apply[A <: IndexData](tag: HeaderTag[A]): Extractor.Result[A]
  }
  type Result[T] = Either[Error, T]
  def apply[T: Extractor]: Extractor[T] = implicitly[Extractor[T]]

  implicit def genericExtractor[T, H <: HList](
    implicit gen: Generic.Aux[T, H],
    e: Extractor[H]
  ): Extractor[T] =
    new Extractor[T] {
      def extract(data: Data): Result[T] = e.extract(data).map(gen.from)
      val sigTags: Set[SignatureTag] = e.sigTags
      val tags: Set[HeaderTag[_ <: IndexData]] = e.tags
    }

  implicit val hnilExtractor: Extractor[HNil] = new Extractor[HNil] {
    def extract(data: Data): Result[HNil] = Right(HNil)
    val sigTags: Set[SignatureTag] = Set.empty
    val tags: Set[HeaderTag[_ <: IndexData]] = Set.empty
  }

  implicit def hlistExtractor[H, T <: HList](
    implicit he: Lazy[Extractor[H]],
    te: Lazy[Extractor[T]]
  ): Extractor[H :: T] = new Extractor[H :: T] {
    def extract(data: Data): Result[H :: T] =
      for {
        h <- he.value.extract(data)
        t <- te.value.extract(data)
      } yield h :: t
    val sigTags: Set[SignatureTag] = he.value.sigTags ++ te.value.sigTags
    val tags: Set[HeaderTag[_ <: IndexData]] = he.value.tags ++ te.value.tags
  }

  def dependencyExtractor[T](
    namesTag: HeaderTag[StringArrayData],
    versionTag: HeaderTag[StringArrayData],
    flagsTag: HeaderTag[Int32Data],
    construct: PkgRef => T
  ): Extractor[Vector[T]] = new Extractor[Vector[T]] {
    val tags: Set[HeaderTag[_ <: IndexData]] = Set(
      namesTag,
      versionTag,
      flagsTag
    )
    val sigTags: Set[SignatureTag] = Set.empty
    def extract(data: Data): Result[Vector[T]] = {
      import cats.implicits._
      //TODO: needs more validation
      val a = for {
        names <- data(namesTag)
        versions <- data(versionTag)
        flags <- data(flagsTag)
        result <- names.values
          .zip(versions.values)
          .zip(flags.values)
          .traverse { case ((name, evr), flags) =>


            val a = for {
              name <- Name(name)
              evr <- if (evr.isEmpty) Right(None)
                     else EVR.parse(evr).map(Some(_))
            } yield RpmRef(name, evr, SenseFlags(flags))

            val b = a.getOrElse(VirtualRef(name, if (evr.isEmpty) None else Some(evr), SenseFlags(flags)))
            Either.right(construct(b))
          }
      } yield result
      Right(a.getOrElse(Vector.empty))
    }
  }

  implicit val providesExtractor = dependencyExtractor[Provides](
    HeaderTag.ProvideName,
    HeaderTag.ProvideVersion,
    HeaderTag.ProvideFlags,
    Provides
  )

  implicit val requiresExtractor = dependencyExtractor[Requires](
    HeaderTag.RequireName,
    HeaderTag.RequireVersion,
    HeaderTag.RequireFlags,
    Requires
  )

  implicit val obsoletesExtractor = dependencyExtractor[Obsoletes](
    HeaderTag.ObsoleteName,
    HeaderTag.ObsoleteVersion,
    HeaderTag.ObsoleteFlags,
    Obsoletes
  )

  implicit val enhancesExtractor = dependencyExtractor[Enhances](
    HeaderTag.EnhanceName,
    HeaderTag.EnhanceVersion,
    HeaderTag.EnhanceFlags,
    Enhances
  )

  implicit val conflictsExtractor = dependencyExtractor[Conflicts](
    HeaderTag.ConflictName,
    HeaderTag.ConflictVersion,
    HeaderTag.ConflictFlags,
    Conflicts
  )

  implicit val suggestsExtractor = dependencyExtractor[Suggests](
    HeaderTag.SuggestName,
    HeaderTag.SuggestVersion,
    HeaderTag.SuggestFlags,
    Suggests
  )

  implicit val recommendsExtractor = dependencyExtractor[Recommends](
    HeaderTag.RecommendName,
    HeaderTag.RecommendVersion,
    HeaderTag.RecommendFlags,
    Recommends
  )

  implicit val supplementsExtractor = dependencyExtractor[Supplements](
    HeaderTag.SupplementName,
    HeaderTag.SupplementVersion,
    HeaderTag.SupplementFlags,
    Supplements
  )

  implicit val fileListExtractor: Extractor[Vector[FileEntry]] =
    new Extractor[Vector[FileEntry]] {
      val tags: Set[HeaderTag[_ <: IndexData]] = Set(
        HeaderTag.DirNames,
        HeaderTag.DirIndexes,
        HeaderTag.BaseNames,
        HeaderTag.FileFlags,
        HeaderTag.FileModes,
        HeaderTag.FileDigests,
        HeaderTag.FileDigestAlgo
      )
      val sigTags: Set[SignatureTag] = Set.empty
      def extract(data: Data): Result[Vector[FileEntry]] = {
        import cats.implicits._
        for {
          dirNames <- data(HeaderTag.DirNames)
          dirIndexes <- data(HeaderTag.DirIndexes)
          baseNames <- data(HeaderTag.BaseNames)
          flags <- data(HeaderTag.FileFlags)
          modes <- data(HeaderTag.FileModes)
          digests <- data(HeaderTag.FileDigests)
          result <- baseNames.values.zip(dirIndexes.values).zipWithIndex.traverse {
            case ((base, idx), index) =>
              val rawMode = modes.values(index)
              val flag = FileFlags(flags.values(index))
              Stat.fromShort(rawMode).toRight(ConvertingError(s"$rawMode is not a valid mode value")).flatMap { mode =>
                  if (mode.tpe == FileType.RegularFile && !flag.containsAll(FileFlags.Ghost)) {
                    val hasher = data(HeaderTag.FileDigestAlgo).toOption.flatMap(_.values.headOption).map {
                      case 1 =>  Md5.fromHex _
                      case 2 =>  Sha1.fromHex _
                      //case 3 =>   /*!< RIPEMD160 */
                      //case 5 =>   /*!< MD2 */
                      //case 6 =>   /*!< TIGER192 */
                      //case 7 =>   /*!< HAVAL-5-160 */
                      case 8 =>   Sha256.fromHex _
                      //case 9 =>   /*!< SHA384 */
                      case 10 =>   Sha512.fromHex _
                    }.getOrElse(Md5.fromHex _)
                    hasher(digests.values(index)).toRight(ConvertingError(s"'${digests.values(index)}' ${dirNames.values(idx) + base} ${flag} is not a valid checksum")).map {
                      d => FileEntry(dirNames.values(idx) + base, mode, flag, Some(d))
                    }
                  } else {
                    Either.right(FileEntry(dirNames.values(idx) + base, mode, flag, None))
                  }

              }
          }
        } yield result
    }
    }

  implicit val changelogExtractor: Extractor[Vector[ChangeLogEntry]] =
    new Extractor[Vector[ChangeLogEntry]] {
      val tags: Set[HeaderTag[_ <: IndexData]] = Set(
        HeaderTag.ChangeLogName,
        HeaderTag.ChangeLogText,
        HeaderTag.ChangeLogTime
      )
      val sigTags: Set[SignatureTag] = Set.empty
      def extract(data: Data): Result[Vector[ChangeLogEntry]] =
        for {
          names <- data(HeaderTag.ChangeLogName)
          texts <- data(HeaderTag.ChangeLogText)
          times <- data(HeaderTag.ChangeLogTime)
        } yield {
          names.values.zip(texts.values).zip(times.values).map {
            case ((name, text), time) =>
              ChangeLogEntry(
                name,
                text,
                Instant.ofEpochSecond(time.toLong).atOffset(ZoneOffset.UTC)
              )
          }
        }
    }

  implicit val nameExtractor: Extractor[Name] = new Extractor[Name] {
    val tags: Set[HeaderTag[_ <: IndexData]] = Set(HeaderTag.Name)
    val sigTags: Set[SignatureTag] = Set.empty
    def extract(data: Data): Result[Name] =
      data(HeaderTag.Name).flatMap { d =>
        Name.fromString(d.value)
      }
  }

  implicit val buildTimeExtractor: Extractor[BuildTime] =
    new Extractor[BuildTime] {
      val tags: Set[HeaderTag[_ <: IndexData]] = Set(HeaderTag.BuildTime)
      val sigTags: Set[SignatureTag] = Set.empty
      def extract(data: Data): Result[BuildTime] =
        data(HeaderTag.BuildTime)
          .map(d => BuildTime(Instant.ofEpochSecond(d.values.head.toLong)))
    }

  implicit val headerRangeExtractor: Extractor[HeaderRange] =
    new Extractor[HeaderRange] {
      val tags: Set[HeaderTag[_ <: IndexData]] = Set.empty
      val sigTags: Set[SignatureTag] = Set.empty
      override val headerRange = true
      def extract(data: Data): Result[HeaderRange] =
        data.headerRange.toRight(ConvertingError("headerRange missing."))
    }

  implicit val archExtractor: Extractor[Architecture] =
    new Extractor[Architecture] {
      val tags: Set[HeaderTag[_ <: IndexData]] = Set(HeaderTag.Arch)
      val sigTags: Set[SignatureTag] = Set.empty
      def extract(data: Data): Result[Architecture] =
        data(HeaderTag.Arch).flatMap { d =>
          Architecture
            .fromString(d.value)
            .toRight(ConvertingError(s"unknown architecture ${d.value}"))
        }
    }

  implicit val epochExtractor: Extractor[Epoch] = new Extractor[Epoch] {
    val tags: Set[HeaderTag[_ <: IndexData]] = Set(HeaderTag.Epoch)
    val sigTags: Set[SignatureTag] = Set.empty
    def extract(data: Data): Result[Epoch] =
      data(HeaderTag.Epoch).flatMap { x =>
        Epoch.fromInt(x.values.head)
      }
  }

  implicit val releaseExtractor: Extractor[Release] = new Extractor[Release] {
    val tags: Set[HeaderTag[_ <: IndexData]] = Set(HeaderTag.Release)
    val sigTags: Set[SignatureTag] = Set.empty
    def extract(data: Data): Result[Release] =
      data(HeaderTag.Release).flatMap(
        x => Release.fromString(x.value)
      )
  }

  implicit val vendorExtractor: Extractor[Vendor] = new Extractor[Vendor] {
    val tags: Set[HeaderTag[_ <: IndexData]] = Set(HeaderTag.Vendor)
    val sigTags: Set[SignatureTag] = Set.empty
    def extract(data: Data): Result[Vendor] =
      data(HeaderTag.Vendor).map(x => Vendor(x.value))
  }

  implicit val licenseExtractor: Extractor[License] =
    new Extractor[License] {
      val tags: Set[HeaderTag[_ <: IndexData]] = Set(HeaderTag.License)
      val sigTags: Set[SignatureTag] = Set.empty
      def extract(data: Data): Result[License] =
        data(HeaderTag.License).flatMap(
          x => License.parse(x.value) match {
            case Right(l) => Right(l)
            case Left(err) => Left(ConvertingError(err))
          }
        )
    }

  implicit val versionExtractor: Extractor[Version] = new Extractor[Version] {
    val tags: Set[HeaderTag[_ <: IndexData]] = Set(HeaderTag.Version)
    val sigTags: Set[SignatureTag] = Set.empty
    def extract(data: Data): Result[Version] =
      data(HeaderTag.Version).flatMap { x =>
        Version.parse(x.value)
      }
  }

  implicit val distributionExtractor: Extractor[Distribution] =
    new Extractor[Distribution] {
      val tags: Set[HeaderTag[_ <: IndexData]] = Set(HeaderTag.Distribution)
      val sigTags: Set[SignatureTag] = Set.empty
      def extract(data: Data): Result[Distribution] =
        data(HeaderTag.Distribution).map(x => Distribution.fromString(x.value))
    }

  implicit val sha1Extractor: Extractor[Sha1] = new Extractor[Sha1] {
    val tags: Set[HeaderTag[_ <: IndexData]] = Set(HeaderTag.SHA1Header)
    val sigTags: Set[SignatureTag] = Set.empty
    def extract(data: Data): Result[Sha1] =
      data(HeaderTag.SHA1Header).flatMap(
        x =>
          Sha1
            .fromHex(x.value)
            .toRight(
              ConvertingError(s"string '${x.value}' not valid sha1")
          )
      )
  }

  implicit val buildhostExtractor: Extractor[BuildHost] =
    new Extractor[BuildHost] {
      val tags: Set[HeaderTag[_ <: IndexData]] = Set(HeaderTag.BuildHost)
      val sigTags: Set[SignatureTag] = Set.empty
      def extract(data: Data): Result[BuildHost] =
        data(HeaderTag.BuildHost).map(x => BuildHost(x.value))
    }

  implicit val summeryExtractor: Extractor[Summery] = new Extractor[Summery] {
    val tags: Set[HeaderTag[_ <: IndexData]] = Set(
      HeaderTag.Summery,
      HeaderTag.HeaderI18NTable
    )
    val sigTags: Set[SignatureTag] = Set.empty
    def extract(data: Data): Result[Summery] =
      for {
        summery <- data(HeaderTag.Summery)
        i18n <- data(HeaderTag.HeaderI18NTable)
      } yield Summery(i18n.values.zip(summery.values).toMap)
  }

  implicit val descriptionExtractor: Extractor[Description] =
    new Extractor[Description] {
      val tags: Set[HeaderTag[_ <: IndexData]] = Set(
        HeaderTag.Description,
        HeaderTag.HeaderI18NTable
      )
      val sigTags: Set[SignatureTag] = Set.empty
      def extract(data: Data): Result[Description] =
        for {
          description <- data(HeaderTag.Description)
          i18n <- data(HeaderTag.HeaderI18NTable)
        } yield Description(i18n.values.zip(description.values).toMap)
    }

  implicit val groupExtractor: Extractor[Group] = new Extractor[Group] {
    val tags: Set[HeaderTag[_ <: IndexData]] = Set(
      HeaderTag.Group,
      HeaderTag.HeaderI18NTable
    )
    val sigTags: Set[SignatureTag] = Set.empty
    def extract(data: Data): Result[Group] =
      for {
        group <- data(HeaderTag.Group)
        i18n <- data(HeaderTag.HeaderI18NTable)
      } yield Group(i18n.values.zip(group.values).toMap)
  }

  implicit val packagerExtractor: Extractor[Packager] = new Extractor[Packager] {
    val tags: Set[HeaderTag[_ <: IndexData]] = Set(
      HeaderTag.Packager
    )
    val sigTags: Set[SignatureTag] = Set.empty
    def extract(data: Data): Result[Packager] =
        data(HeaderTag.Packager).map(str => Packager(str.value))
  }

  implicit val leadExtractor: Extractor[Lead] = new Extractor[Lead] {
    override val lead: Boolean = true
    val tags: Set[HeaderTag[_ <: IndexData]] = Set.empty
    val sigTags: Set[SignatureTag] = Set.empty
    def extract(data: Data): Result[Lead] =
      data.lead.toRight(ConvertingError("missing lead."))
  }

  implicit val payloadCompressionExtractor: Extractor[Compression] = new Extractor[Compression] {
    val tags: Set[HeaderTag[_ <: IndexData]] = Set(
      HeaderTag.PayloadCompressor
    )
    val sigTags: Set[SignatureTag] = Set.empty
    def extract(data: Data): Result[Compression] =
      for {
        str <- data(HeaderTag.PayloadCompressor)
        x <- Compression.fromString(str.value).toRight(ConvertingError(s"${str.value} unknown compression."))
      } yield x
  }

  implicit val payloadExtractor: Extractor[Payload] =
    new Extractor[Payload] {
      val tags: Set[HeaderTag[_ <: IndexData]] = Set.empty
      val sigTags: Set[SignatureTag] = Set.empty
      override val payload: Boolean = true
      def extract(data: Data): Result[Payload] =
        data.payload.map(Payload).toRight(ConvertingError("missing payload."))
    }

  implicit def optionExtractor[T](implicit t: Extractor[T]): Extractor[Option[T]] =
    new Extractor[Option[T]] {
      val tags: Set[HeaderTag[_ <: IndexData]] = t.tags
      val sigTags: Set[SignatureTag] = t.sigTags
      def extract(data: Data): Result[Option[T]] =
        t.extract(data) match {
          case Left(MissingHeader(_)) => Right(None)
          case x                      => x.map(Some(_))
        }
    }

  implicit def convertingExtractor[T](implicit e: Extractor[Vector[T]]): Extractor[List[T]] =
    new Extractor[List[T]] {
      val tags: Set[HeaderTag[_ <: IndexData]] = e.tags
      val sigTags: Set[SignatureTag] = e.sigTags
      def extract(data: Data): Result[List[T]] =
        e.extract(data).map(_.toList)
    }

}
