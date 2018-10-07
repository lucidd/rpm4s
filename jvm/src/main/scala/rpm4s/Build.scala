package rpm4s

import java.io.ByteArrayOutputStream
import java.nio.file.{Path, Paths}

import org.apache.commons.compress.archivers.cpio.{CpioArchiveEntry, CpioArchiveOutputStream, CpioConstants}
import org.apache.commons.compress.compressors.xz.XZCompressorOutputStream
import rpm4s.build.{FileInfo, RpmBlueprint}
import rpm4s.codecs.HeaderTag.HeaderI18NTable
import rpm4s.codecs.IndexData._
import rpm4s.codecs.SignatureTag.MD5
import rpm4s.codecs.{Header, HeaderTag, IndexData, IndexEntry, RPMTag, SignatureTag}
import rpm4s.data._
import scodec.bits.ByteVector

import scala.annotation.tailrec

object Build {

  def buildRpm(blueprint: RpmBlueprint): RpmFile = {

    def encodeHeaderList[T <: RPMTag](headers: List[(T, IndexData)]): Header[T] = {
      val data = headers.map(_._2)
      val (bits, offsets) = rpm4s.codecs.encodeIndexEntry(data).require
      val index = headers.zip(offsets).map { case ((tag, data), offset) =>
        IndexEntry(tag, data.headerType, offset, data.count)
      }
      println(index)
      val header = Header(1, index.size, bits.size / 8, index)
      header
    }

    @tailrec
    def list(
              dirindex: Vector[Path],
              files: List[FileInfo],
              names: Vector[String],
              modes: Vector[Short],
              flags: Vector[Int],
              sizes: Vector[Int],
              users: Vector[String],
              groups: Vector[String],
              checksums: Vector[String],
              mtimes: Vector[Int],
              dirindices: Vector[Int]
            ): (Vector[String], Vector[Short], Vector[Int], Vector[Int], Vector[Path], Vector[Int], Vector[String], Vector[String], Vector[String], Vector[Int]) =
      files match {
        case file :: tail =>
          val path = Paths.get(file.fileEntry.path)

          val (diridx, newDirindex) = dirindex.indexOf(path.getParent) match {
            case -1 =>
              (dirindex.length, dirindex :+ path.getParent)
            case x => (x, dirindex)
          }

          list(
            newDirindex, tail,
            names :+ path.getFileName.toString,
            modes :+ file.fileEntry.mode.toShort,
            flags :+ file.fileEntry.flags.value,
            sizes :+ file.content.size.toInt,
            users :+ file.user,
            groups :+ file.group,
            checksums :+ file.content.digest("MD5").toHex,
            mtimes :+ file.modtime.getEpochSecond.toInt,
            dirindices :+ diridx
          )
        case Nil =>
          (names, modes, flags, dirindices, dirindex, sizes, users, groups, checksums, mtimes)
      }


    val size = blueprint.files.map(_.content.size).sum.toInt
    val (baseNames, modes, flags, dirindices, dirindex, sizes, users, groups, checksums, mtimes) = list(
        Vector.empty,
        blueprint.files,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty
      )

    val (reqnames, reqversions, reqflags) = blueprint.requires.foldLeft((Vector.empty[String], Vector.empty[String] ,Vector.empty[Int])) {
      case (acc, req) => (acc._1 :+ req.ref.nameString, acc._2 :+ req.ref.versionString.getOrElse(""), acc._3 :+ req.ref.flags.value)
    }

    val (provnames, provversions, provflags) = blueprint.provides.foldLeft((Vector.empty[String], Vector.empty[String] ,Vector.empty[Int])) {
      case (acc, prov) => (acc._1 :+ prov.ref.nameString, acc._2 :+ prov.ref.versionString.getOrElse(""), acc._3 :+ prov.ref.flags.value)
    }

    val requireHeaders = if(blueprint.requires.isEmpty) List.empty
    else {
      List(
        HeaderTag.RequireName -> StringArrayData(reqnames),
        HeaderTag.RequireVersion -> StringArrayData(reqversions),
        HeaderTag.RequireFlags -> Int32Data(reqflags),
      )
    }
    val provideHeaders = if (blueprint.provides.isEmpty) List.empty
    else {
      List(
        HeaderTag.ProvideName -> StringArrayData(provnames),
        HeaderTag.ProvideVersion -> StringArrayData(provversions),
        HeaderTag.ProvideFlags -> Int32Data(provflags),
      )
    }

    val signatureList = List(
      SignatureTag.Size -> Int32Data(Vector(58681993)),
      MD5 -> BinaryData(ByteVector.fromValidHex("9ef1cdd1cb60d4687799497052a233f0"))
    )
    val headerList = List(
      HeaderTag.DirIndexes -> Int32Data(dirindices),
      HeaderTag.DirNames -> StringArrayData(dirindex.map(x => {
        val str = x.toString
        if (str.endsWith("/")) str
        else str + "/"
      })),
      HeaderTag.BaseNames -> StringArrayData(baseNames),
      HeaderTag.FileFlags -> Int32Data(flags),
      HeaderTag.FileModes -> Int16Data(modes),
      HeaderTag.FileSizes ->  Int32Data(sizes),
      HeaderTag.FileMTimes -> Int32Data(mtimes),
      HeaderTag.FileGroupName -> StringArrayData(groups),
      HeaderTag.FileDigests -> StringArrayData(checksums),
      HeaderTag.FileUserName -> StringArrayData(users),

      HeaderTag.Name -> StringData(blueprint.name.value),
      HeaderTag.Version -> StringData(blueprint.version.string),
      HeaderTag.Release -> StringData(blueprint.release.value),
      HeaderTag.Size -> Int32Data(Vector(size)),
      HeaderTag.License -> StringData(License.format(blueprint.license)),
      HeaderTag.Arch -> StringData(Architecture.toRpmString(blueprint.architecture)),
      HeaderTag.PayloadFormat -> StringData("cpio"),
      HeaderTag.PayloadCompressor -> StringData("lzma"),
      HeaderTag.PayloadFlags -> StringData("6"),
      HeaderTag.OS -> StringData("linux"),

      HeaderI18NTable -> StringArrayData(Vector("C")),
      HeaderTag.Description -> I18NStringArrayData(
        blueprint.description.locales.values.toVector
      ),
      HeaderTag.Summery -> I18NStringArrayData(
        blueprint.summery.locales.values.toVector
      ),
      HeaderTag.Group -> I18NStringArrayData(
        blueprint.group.locales.values.toVector
      )
    ) ++ provideHeaders ++ requireHeaders



    def createPayload(): ByteVector = {
      val buffer = new ByteArrayOutputStream(1024)
      val lzmaOut = new XZCompressorOutputStream(buffer, 6)


      val cpioOut = blueprint.files.foldLeft(new CpioArchiveOutputStream(lzmaOut, CpioConstants.FORMAT_NEW)) { case (cpio, fileinfo) =>
        val entry = new CpioArchiveEntry(CpioConstants.FORMAT_NEW, "." + fileinfo.fileEntry.path, fileinfo.content.size)
        entry.setMode(fileinfo.fileEntry.mode.toShort.toLong)
        cpio.putArchiveEntry(entry)
        cpio.write(fileinfo.content.toArray)
        cpio.closeArchiveEntry()
        cpio
      }

      cpioOut.close()
      ByteVector(buffer.toByteArray)
    }


    val sigHeader = encodeHeaderList(signatureList)
    val sigData = sigHeader.index.zip(signatureList).map { case (indexEntry, (_, data)) =>
      (indexEntry, data)
    }
    val header = encodeHeaderList(headerList)
    val data = header.index.zip(headerList).map { case (indexEntry, (_, data)) =>
      (indexEntry, data)
    }
    new RpmFile(
      Lead(3, 0, RPMType.Binary, 1, blueprint.name.value, OS.Linux, 5),
      sigHeader,
      sigData,
      header,
      data,
      createPayload().bits
    )
  }

}
