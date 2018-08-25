package rpm4s.cli.commands
import java.io.{ByteArrayInputStream, OutputStream}
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, Paths}
import java.util.zip.GZIPInputStream

import better.files.File
import org.apache.commons.compress.archivers.cpio.{CpioArchiveEntry, CpioArchiveInputStream}
import org.apache.commons.compress.compressors.lzma.LZMACompressorInputStream
import org.apache.commons.compress.compressors.xz.XZCompressorInputStream
import rpm4s.data.{Compression, FileEntry, Payload, PayloadFormat}
import scodec.Attempt.{Failure, Successful}
import scodec.bits.BitVector

import scala.annotation.tailrec

object extract {

  @tailrec
  def copyCpioEntry(in: CpioArchiveInputStream, out: OutputStream, buffer: Array[Byte], remaining: Int): Unit = {
    if (remaining > 0) {
      val bytes = in.read(buffer, 0, remaining.min(buffer.length))
      out.write(buffer, 0, bytes)
      copyCpioEntry(in, out, buffer, remaining - bytes)
    }
  }

  @tailrec
  def unpackCpio(base: Path, in: CpioArchiveInputStream, hardlinks: List[CpioArchiveEntry]): Unit = {
    val entry = in.getNextCPIOEntry
    if (entry != null) {
      val path = base.resolve(entry.getName)
      println(path.normalize().toAbsolutePath.toString)
      if (entry.isDirectory) {
        Files.createDirectories(path)
        unpackCpio(base, in, hardlinks)
      } else if (entry.isRegularFile) {
        if (entry.getSize == 0) {
          unpackCpio(base, in, hardlinks :+ entry)
        } else {
          path.toFile.getParentFile.mkdirs()
          path.toFile.getParentFile.mkdirs()
          val buffer = new Array[Byte](1024 * 16)
          val out = Files.newOutputStream(path)
          copyCpioEntry(in, out, buffer, entry.getSize.toInt)
          out.close()
          //TODO: set file attributes from rpm header data

          hardlinks.foreach { hardlink =>
            val linkPath = base.resolve(hardlink.getName)
            Files.createLink(linkPath, path)
          }

          unpackCpio(base, in, List.empty)
        }
      }
    }
  }

  case class ExtractData(
    payload: Payload,
    format: PayloadFormat,
    compression: Compression,
    fileEntries: List[FileEntry]
  )

  def apply(path: Path, outOpt: Option[Path]): Unit = {
    val bits = BitVector.fromMmap(FileChannel.open(path))
    rpm4s.decode[ExtractData](bits) match {
      case Successful(data) =>
        val bytes = data.payload.bitVector.toByteArray
        val in = new ByteArrayInputStream(bytes)


        val uncompressed = data.compression match {
          case Compression.LZMA => new LZMACompressorInputStream(in)
          case Compression.GZIP => new GZIPInputStream(in)
          case Compression.XZ   => new XZCompressorInputStream(in)
        }
        val cpioIn = data.format match {
          case PayloadFormat.CPIO => new CpioArchiveInputStream(uncompressed)
        }

        val out = outOpt.getOrElse(Paths.get(File(path).nameWithoutExtension))

        unpackCpio(out, cpioIn, List.empty)

      case Failure(cause) =>
        println(s"Error parsing rpm: $cause")
        System.exit(-1)
    }
  }

}
