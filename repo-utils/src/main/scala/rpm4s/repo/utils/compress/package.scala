package rpm4s.repo.utils

import java.util.zip.{CRC32, Deflater}

import fs2.{Chunk, Pipe, Pull, RaiseThrowable, Stream}

package object compress {

  private val header: Chunk[Byte] = Chunk.bytes(
    Array(0x1f.toByte,
          0x8b.toByte,
          Deflater.DEFLATED.toByte,
          0.toByte,
          0.toByte,
          0.toByte,
          0.toByte,
          0.toByte,
          0.toByte,
          0.toByte))

  def gzip[F[_]](
      level: Int = Deflater.DEFAULT_COMPRESSION,
      bufferSize: Int = 1024 * 32,
      strategy: Int = Deflater.DEFAULT_STRATEGY
  ): Pipe[F, Byte, Byte] =
    in =>
      Stream.suspend {
        val crc = new CRC32()
        var inputSize = 0

        Stream.chunk[F, Byte](header) ++
          in.through(_.chunks.flatMap { chunk =>
              val bytes = chunk.toBytes
              inputSize = inputSize + bytes.size
              crc.update(bytes.values)
              Stream.chunk(chunk)
            })
            .through(
              fs2.compress.deflate(
                nowrap = true,
                level = level,
                bufferSize = bufferSize,
                strategy = strategy
              )
            ) ++
          Stream.chunk[F, Byte](Chunk.bytes {
            val c = crc.getValue
            val size = inputSize % 4294967296L //2^32
            Array(
              (c & 0xFF).toByte,
              ((c >> 8) & 0xFF).toByte,
              ((c >> 16) & 0xFF).toByte,
              ((c >> 24) & 0xFF).toByte,
              (size & 0xFF).toByte,
              ((size >> 8) & 0xFF).toByte,
              ((size >> 16) & 0xFF).toByte,
              ((size >> 24) & 0xFF).toByte
            )
          })
    }


  private def awaitShort[F[_]](h: Stream[F, Byte]): Pull[F, Nothing, Option[(Int, Stream[F, Byte])]] = {
    h.pull.uncons1.flatMap {
      case Some((b1, h)) =>
        h.pull.uncons1.flatMap {
          case Some((b2, h)) =>
            Pull.pure(Some((((b1 & 0xFF) << 8) | (b2 & 0xFF), h)))
          case None => Pull.pure(None)
        }
      case None => Pull.pure(None)
    }
  }

  private def skipFlags[F[_]: RaiseThrowable](flags: Byte, s: Stream[F, Byte]): Pull[F, Byte, Unit] = {
    val FTEXT = 1
    val FHCRC = 2
    val FEXTRA = 4
    val FNAME = 8
    val FCOMMENT = 16
    val a = for {
      optRest <- if ((flags & FEXTRA) == FEXTRA) {
        awaitShort(s).flatMap {
          case Some((size, h)) =>
            h.pull.drop(size)
          case None => Pull.raiseError(new RuntimeException("premature end of stream"))
        }
      } else Pull.pure(Some(s))
      s <- optRest match {
        case Some(s) =>
          if ((flags & FNAME) == FNAME) {
            Pull.pure(s.dropWhile(_ != 0).drop(1))
          } else Pull.pure(s)
        case None => Pull.raiseError(new RuntimeException("premature end of stream"))
      }
      s <- if ((flags & FCOMMENT) == FCOMMENT) {
            Pull.pure(s.dropWhile(_ != 0).drop(1))
           } else Pull.pure(s)
      s <- if ((flags & FHCRC) == FHCRC) Pull.pure(s.drop(2))
           else Pull.pure(s)
      _ <- s.pull.echo
    } yield ()
    a
  }

  //TODO: refactor and upstream
  //http://www.zlib.org/rfc-gzip.html#header-trailer
  def gunzip[F[_]: RaiseThrowable](bufferSize: Int = 1024 * 32): Pipe[F, Byte, Byte] =  h => {
        val a = for {
          idOpt <- awaitShort(h)
          cmOpt <- idOpt match {
            case None => Pull.raiseError(new RuntimeException("premature end of stream"))
            case Some((id, h)) =>
              if (id == 0x1f8b)
                h.pull.uncons1
              else Pull.raiseError(
                new RuntimeException(
                  s"invalid gzip header ${Integer.toHexString(id)} =! 0x1f8b"))
          }
          flagsOpt <- cmOpt match {
            case None => Pull.raiseError(new RuntimeException("premature end of stream"))
            case Some((cm, h)) =>
              if (cm == 8)
                h.pull.uncons1.flatMap {
                  case Some((flags, h)) =>
                    skipFlags(flags, h.drop(6))
                  case None => Pull.raiseError(new RuntimeException("premature end of stream"))
                }
              else Pull.raiseError(new RuntimeException(s"unsupported compression method $cm"))
          }
        } yield ()
        a
      }.stream.through(fs2.compress.inflate(nowrap = true, bufferSize))

}
