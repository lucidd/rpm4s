package rpm4s.repo.utils

import java.security.MessageDigest

import cats.effect.Sync
import fs2.{Pipe, Sink, Stream}
import rpm4s.data.Checksum
import cats.implicits._

package object hash {

  def checksumsBeforeAndAfter[F[_]: Sync](
    in: Stream[F, Byte],
    pipe: Pipe[F, Byte, Byte],
    digest: => MessageDigest,
    fromBytes: Vector[Byte] => Option[Checksum],
    sink: Sink[F, Byte]
  ): F[(Checksum, Long, Checksum, Long)] = {
    Sync[F].suspend {
      val before = digest
      val after = digest
      var sizeBefore = 0
      var sizeAfter = 0
      in.through(_.chunks.flatMap { chunk =>
        val bytes = chunk.toBytes.values
        sizeBefore = sizeBefore + bytes.length
        before.update(bytes)
        Stream.chunk(chunk)
      })
        .through(pipe)
        .through(_.chunks.flatMap { chunk =>
          val bytes = chunk.toBytes.values
          sizeAfter = sizeAfter + bytes.length
          after.update(bytes)
          Stream.chunk(chunk)
        })
        .to(sink)
        .run
        .map(_ => {
          (
            fromBytes(before.digest().toVector).get,
            sizeBefore,
            fromBytes(after.digest().toVector).get,
            sizeAfter
          )
        })
    }
  }

}
