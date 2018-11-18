package rpm4s.repo.repomd

import java.time.Instant
import javax.xml.namespace.QName
import javax.xml.stream.events.{StartElement, XMLEvent}

import fs2.{Pull, Stream}
import rpm4s.data.Checksum.{Sha1, Sha256}
import rpm4s.repo.data.Bytes

import scala.util.Try

package object xml {

  private val checksumType = new QName("type")

  private[repomd] def checksum[F[_]](e: StartElement, h: Stream[F, XMLEvent]) = {
    val tpe = e.getAttributeByName(checksumType).getValue
    val fromHex = tpe match {
      case "sha" => Sha1.fromHex _
      case "sha1" => Sha1.fromHex _
      case "sha256" => Sha256.fromHex _
    }
    text(h).map(_.map {
      case (text, h) => (fromHex(text).get, h)
    })
  }

  private[repomd] def text[F[_]](h: Stream[F, XMLEvent]) = {
    def go(h: Stream[F, XMLEvent], acc: StringBuilder): Pull[F, Nothing, Option[(String, Stream[F, XMLEvent])]] = {
      h.pull.uncons1.flatMap {
        case Some((event, h)) =>
          if (event.isCharacters) {
            val text = event.asCharacters().getData
            go(h, acc.append(text))
          } else Pull.pure(Some((acc.toString, h)))
        case None  => Pull.pure(None)
      }
    }
    go(h, new StringBuilder)
  }

  private[repomd] def long[F[_]](h: Stream[F, XMLEvent]) = {
    text(h).map(_.map {
      case (text, h) =>
        (Try(text.toLong).toOption, h)
    })
  }

  private[repomd] def fromLong[F[_], T](
                                 h: Stream[F, XMLEvent],
                                 fn: Long => T)  = {
    long(h).map(_.map {
      case (l, h) =>
        (l.map(fn), h)
    })
  }

  private[repomd] def bytes[F[_]](h: Stream[F, XMLEvent]) =
    fromLong(h, Bytes(_))

  private[repomd] def instant[F[_]](h: Stream[F, XMLEvent]) =
    fromLong(h, Instant.ofEpochSecond)

}
