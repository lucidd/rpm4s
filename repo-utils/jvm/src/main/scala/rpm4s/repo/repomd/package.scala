package rpm4s.repo

import java.time.Instant

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent
import cats.effect.ConcurrentEffect
import fs2.{Pipe, Pull, RaiseThrowable, Stream}
import rpm4s.data.Checksum
import rpm4s.data.Checksum.{Md5, Sha1, Sha256, Sha512}
import rpm4s.repo.data.Data.{Primary, UpdateInfo}
import rpm4s.repo.data.RMDataF.{RMData, RMDataBuilder}
import rpm4s.repo.data.Repomd
import rpm4s.repo.data.Repomd.RepoMdBuilder
import rpm4s.repo.data.updateinfo.UpdateF
import rpm4s.repo.utils.xml.{EndEvent, StartEvent, xmlevents}
import rpm4s.repo.repomd.xml._

package object repomd {

  type Update = UpdateF.Update

  private val typeAttr = new QName("type")
  private val hrefAttr = new QName("href")

  private[repomd] def checksum2type(checksum: Checksum): String = checksum match {
    case Md5(_)   => "md5"
    case Sha1(_)   => "sha1"
    case Sha256(_) => "sha256"
    case Sha512(_) => "sha512"
  }

  private def data[F[_]: RaiseThrowable](h: Stream[F, XMLEvent], acc: RMDataBuilder)
  : Pull[F, Nothing, (RMData, Stream[F, XMLEvent])] = {
    h.pull.uncons1.flatMap {
      case None => Pull.raiseError(new RuntimeException("premature end of xml."))
      case Some((event, h1)) =>
        event match {
          case StartEvent(se) => {
            se.getName.getLocalPart match {
              case "location" =>
                val href = se.getAttributeByName(hrefAttr).getValue
                data(h1, acc.copy(location = Some(href)))
              case "checksum" =>
                checksum(se, h1).flatMap {
                  case Some((cs, h2)) =>
                    data(h2, acc.copy[Option](checksum = Some(cs)))
                  case None => Pull.raiseError(new RuntimeException("expected checksum"))
                }
              case "timestamp" =>
                instant(h1).flatMap {
                  case Some((long, h2)) =>
                    data(h2, acc.copy(timestamp = long))
                  case None => Pull.raiseError(new RuntimeException("expected timestamp"))
                }
              case "size" =>
                bytes(h1).flatMap {
                  case Some((long, h2)) =>
                    data(h2, acc.copy(size = long))
                  case None => Pull.raiseError(new RuntimeException("expected size"))
                }
              case "open-size" =>
                bytes(h1).flatMap {
                  case Some((long, h2)) =>
                    data(h2, acc.copy(openSize = long))
                  case None => Pull.raiseError(new RuntimeException("expected open-size"))
                }
              case "open-checksum" =>
                checksum(se, h1).flatMap {
                  case Some((cs, h2)) =>
                    data(h2, acc.copy[Option](openChecksum = Some(cs)))
                  case None => Pull.raiseError(new RuntimeException("expected open-size"))
                }
              case _ => data(h1, acc)
            }
          }
          case EndEvent(ee) => {
            ee.getName.getLocalPart match {
              case "data" =>
                Pull.pure((RMDataBuilder.build(acc).get, h1))
              case _ => data(h1, acc)
            }
          }
          case _ => data(h1, acc)
        }
    }
  }


  private def rmd[F[_]: RaiseThrowable](h: Stream[F, XMLEvent], acc: RepoMdBuilder)
  : Pull[F, Nothing, (Repomd, Stream[F, XMLEvent])] = {
    h.pull.uncons1.flatMap {
      case None => Pull.raiseError(new RuntimeException("premature end of xml."))
      case Some((event, h1)) =>
        event match {
          case StartEvent(se) => {
            se.getName.getLocalPart match {
              case "revision" =>
                long(h1).flatMap {
                  case None => Pull.raiseError(new RuntimeException("premature end of xml."))
                  case Some((long, h2)) =>
                    rmd(h2, acc.copy(revision = long))
                }
              case "data" =>

                  se.getAttributeByName(typeAttr)
                  .getValue match {
                    case "primary" =>
                      data(h1, RMDataBuilder.empty).flatMap {
                        case (data, h) =>
                          val primary = Primary(
                            data.checksum,
                            data.openChecksum,
                            data.location,
                            data.timestamp,
                            data.openSize,
                            data.size
                          )
                          rmd(h, acc.copy(primary = Some(primary)))
                      }
                    case "updateinfo" =>
                      data(h1, RMDataBuilder.empty).flatMap {
                        case (data, h) =>
                          val updateinfo = UpdateInfo(
                            data.checksum,
                            data.openChecksum,
                            data.location,
                            data.timestamp,
                            data.openSize,
                            data.size
                          )
                          rmd(h, acc.copy(updateinfo = Some(updateinfo)))
                      }
                    case _ => rmd(h1, acc)
                  }
              case _ => rmd(h1, acc)
            }
          }
          case EndEvent(ee) => {
            ee.getName.getLocalPart match {
              case "repomd" =>
                Pull.pure((RepoMdBuilder.build(acc).get, h1))
              case _ => rmd(h1, acc)
            }
          }
          case _ => rmd(h1, acc)
        }
    }
  }

  def xml2repomd[F[_]: RaiseThrowable]: Pipe[F, XMLEvent, Repomd] = s => {
    //TODO validate first event
    def go(h: Stream[F, XMLEvent]): Pull[F, Repomd, Option[Unit]] = {
      h.pull.uncons1.flatMap {
        case None => Pull.pure(None)
        case Some((event, h1)) =>
          event match {
            case StartEvent(se) => {
              se.getName.getLocalPart match {
                case "repomd" =>
                  rmd(h1, RepoMdBuilder.empty).flatMap {
                    case (p, h) =>
                      Pull.output1(p) >> go(h)
                  }
                case _ => go(h1)
              }
            }
            case _ => go(h1)
          }
      }
    }

    go(s).stream
  }

  def bytes2repomd[F[_]: ConcurrentEffect]: Pipe[F, Byte, Repomd] =
    _.through(fs2.io.toInputStream)
      .flatMap(is => xmlevents(is))
      .through(xml2repomd)

  def create(
    primSize: Long,
    primOpenSize: Long,
    primChecksum: Checksum,
    primOpenChecksum: Checksum,
    upSize: Long,
    upOpenSize: Long,
    upChecksum: Checksum,
    upOpenChecksum: Checksum,
    revision: Long,
    location: String = "repodata/primary.xml.gz"
  )(sb: StringBuilder): StringBuilder = {
    val timestamp = Instant.now().getEpochSecond
    val ctype = checksum2type(primChecksum)
    val octype = checksum2type(primOpenChecksum)

    sb.append("""<?xml version="1.0" encoding="UTF-8"?>""")
    sb.append("""<repomd>""")
    sb.append(s"""<revision>$revision</revision>""")

    sb.append("""<data type="primary">""")
    sb.append(
      s"""<checksum type="$ctype">${primChecksum.toHex.toLowerCase}</checksum>""")
    sb.append(
      s"""<open-checksum type="$octype">${primOpenChecksum.toHex.toLowerCase}</open-checksum>""")
    sb.append(s"""<location href="$location"/>""")
    sb.append(s"""<timestamp>$timestamp</timestamp>""")
    sb.append(s"""<size>$primSize</size>""")
    sb.append(s"""<open-size>$primOpenSize</open-size>""")
    sb.append("""</data>""")

    sb.append("""<data type="updateinfo">""")
    sb.append(
      s"""<checksum type="$ctype">${upChecksum.toHex.toLowerCase}</checksum>""")
    sb.append(
      s"""<open-checksum type="$octype">${upOpenChecksum.toHex.toLowerCase}</open-checksum>""")
    sb.append(s"""<location href="$location"/>""")
    sb.append(s"""<timestamp>$timestamp</timestamp>""")
    sb.append(s"""<size>$upSize</size>""")
    sb.append(s"""<open-size>$upOpenSize</open-size>""")
    sb.append("""</data>""")

    sb.append("""</repomd>""")
  }

}
