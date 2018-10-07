package rpm4s.utils

import java.nio.file.{Files, Path}

import fs2.Stream
import cats.effect.Sync

package object file {

  def ls[F[_]: Sync](path: Path): Stream[F, Path] = {
    Stream.bracket(Sync[F].delay(Files.newDirectoryStream(path)))(
      s => Sync[F].delay(s.close())
    ).flatMap {
      s => Stream.unfold(s.iterator()) { iter =>
        if (iter.hasNext) Some((iter.next(), iter))
        else None
      }
    }
  }

}
