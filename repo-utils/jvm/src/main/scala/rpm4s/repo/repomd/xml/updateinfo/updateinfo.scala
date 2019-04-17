package rpm4s.repo.repomd.xml

import java.time.{Instant, ZoneOffset}

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent
import cats.effect.{ConcurrentEffect, Effect}
import fs2.{Pipe, Pull, RaiseThrowable, Stream}
import org.http4s.Uri
import rpm4s.data.{CVE, _}
import rpm4s.repo.utils.xml.{EndEvent, StartEvent, xmlevents}

import scala.concurrent.ExecutionContext
import rpm4s.repo.repomd.xml._
import rpm4s.repo.data.updateinfo.UpdateF.PackageF.PackageBuilder
import rpm4s.repo.data.updateinfo.UpdateF
import rpm4s.repo.data.updateinfo.UpdateF._

package object updateinfo {

  private val fromAttr = new QName("from")
  private val statusAttr = new QName("status")
  private val typeAttr = new QName("type")
  private val idAttr = new QName("id")
  private val titleAttr = new QName("title")
  private val versionAttr = new QName("version")
  private val srcAttr = new QName("src")
  private val releaseAttr = new QName("release")
  private val archAttr = new QName("arch")
  private val epochAttr = new QName("epoch")
  private val nameAttr = new QName("name")

  private val hrefAttr = new QName("href")
  private val dateAttr = new QName("date")


  def bytes2updates[F[_]: ConcurrentEffect]: Pipe[F, Byte, UpdateF.Update] =
    _.through(fs2.io.toInputStream)
     .flatMap(is => xmlevents(is))
     .through(xml2updates)


  def xml2updates[F[_]: RaiseThrowable]: Pipe[F, XMLEvent, UpdateF.Update] = { h =>

    def boolean(h: Stream[F, XMLEvent]):
      Pull[F, Nothing, (Boolean, Stream[F, XMLEvent])] =
        text(h).flatMap {
          case Some((text, h2)) =>
            val v = text match {
              case "True" => true
              case "False" => false
            }
            Pull.pure((v, h2))
          case None => Pull.raiseError(new RuntimeException("expected boolean"))
        }


    def pack(h: Stream[F, XMLEvent], acc: UpdateF.PackageF.PackageBuilder):
      Pull[F, Nothing, Option[(UpdateF.PackageF.Package, Stream[F, XMLEvent])]] = {
      h.pull.uncons1.flatMap {
        case None => Pull.raiseError(new RuntimeException("premature end of xml."))
        case Some((event, h1)) =>
          event match {
            case StartEvent(se) => se.getName.getLocalPart match {
              case "package" =>
                val name = Name(se.getAttributeByName(nameAttr).getValue).toOption.get
                val epoch = Option(se.getAttributeByName(epochAttr).getValue)
                  .flatMap(x => Epoch.fromString(x).toOption)
                val version = Version.fromString(se.getAttributeByName(versionAttr).getValue).toOption.get
                val release = Release.fromString(se.getAttributeByName(releaseAttr).getValue).toOption.get
                val arch = Architecture.fromString(se.getAttributeByName(archAttr).getValue).get
                val src = se.getAttributeByName(srcAttr).getValue
                val acc2 = acc.copy(
                    name = Some(name),
                    epoch = Some(epoch),
                    version = Some(version),
                    release = Some(release),
                    arch = Some(arch),
                    src = Some(src)
                )
                pack(h1, acc2)
              case "filename" => text(h1).flatMap {
                case Some((text, h2)) => pack(h2, acc.copy(filename = Some(text)))
                case None => Pull.raiseError(new RuntimeException("expected text"))
              }
              case "relogin_suggested" =>
                boolean(h1).flatMap {
                  case (bool, h2) =>
                    pack(h2, acc.copy(
                      reloginSuggested = Some(bool)
                    ))
                }
              case "restart_suggested" =>
                boolean(h1).flatMap {
                  case (bool, h2) =>
                    pack(h2, acc.copy(
                      restartSuggested = Some(bool)
                    ))
                }
              case "reboot_suggested" =>
                boolean(h1).flatMap {
                  case (bool, h2) =>
                    pack(h2, acc.copy(
                      restartSuggested = Some(bool)
                    ))
                }
            }
            case EndEvent(ee) =>
              ee.getName.getLocalPart match {
                case "package" =>
                  Pull.pure(Some((PackageBuilder.build(acc).get, h1)))
              }
            case _ => pack(h1, acc)
          }
      }
    }

    def collection(h: Stream[F, XMLEvent], acc: Vector[UpdateF.PackageF.Package]):
      Pull[F, Nothing, (Vector[UpdateF.PackageF.Package], Stream[F, XMLEvent])] = {
        h.pull.peek1.flatMap {
          case Some((event, h1)) => event match {
            case StartEvent(se) if se.getName.getLocalPart == "package" =>
              pack(h1, PackageBuilder.empty).flatMap {
                case None => Pull.raiseError(new RuntimeException("premature end of xml."))
                case Some((p, h3)) => collection(h3, acc :+ p)
              }
            case EndEvent(ee) if ee.getName.getLocalPart == "collection" =>
              h1.pull.drop(1).flatMap {
                case None => Pull.raiseError(new RuntimeException("premature end of xml."))
                case Some(h2) => Pull.pure((acc, h2))
              }
            case x =>
              h1.pull.drop(1).flatMap {
                case None => Pull.raiseError(new RuntimeException("premature end of xml."))
                case Some(h2) => collection(h2, acc)
              }
          }
          case None => Pull.raiseError(new RuntimeException("premature end of stream"))
        }
      }

    def references(h: Stream[F, XMLEvent], acc: Vector[Reference]):
      Pull[F, Nothing, Option[(Vector[Reference], Stream[F, XMLEvent])]] = {
      h.pull.uncons1.flatMap {
        case Some((event, h1)) => event match {
          case StartEvent(se) =>
            se.getName.getLocalPart match {
              case "reference" =>
                val href = se.getAttributeByName(hrefAttr).getValue
                val id = se.getAttributeByName(idAttr).getValue
                val title = se.getAttributeByName(titleAttr).getValue
                val tpe = se.getAttributeByName(typeAttr).getValue
                val ref = tpe match {
                  case "bugzilla" => Bugzilla(href, id, title)
                  case "cve" => UpdateF.CVERef(href, CVE.fromString(id).get, title)
                  case "fate" => Fate(href, id, title)
                }
                references(h1, acc :+ ref)
            }
          case EndEvent(ee) if ee.getName.getLocalPart == "references" =>
            Pull.pure(Some((acc, h1)))
          case _ => references(h1, acc)
        }
        case None => Pull.pure(None)
      }
    }

    def update(h: Stream[F, XMLEvent], acc: UpdateBuilder)
    : Pull[F, Nothing, (UpdateF.Update, Stream[F, XMLEvent])] = {
      h.pull.uncons1.flatMap {
        case None => Pull.pure((UpdateBuilder.build(acc).get, Stream.empty))
        case Some((event, h1)) =>
          event match {
            case StartEvent(se) => {
              se.getName.getLocalPart match {
                case "pkglist" =>
                  update(h1, acc)
                case "collection" =>
                  collection(h1, Vector.empty).flatMap{
                    case (packs, h2) =>
                      update(h2, acc.copy(
                        packages = Some(packs.toSet)
                      ))
                  }
                case "references" => {
                  references(h1, Vector.empty).flatMap {
                    case Some((refs, h2)) =>
                      update(h2, acc.copy(references = Some(refs.toSet)))
                    case None => Pull.raiseError(new RuntimeException("expected release"))
                  }
                }
                case "release" => {
                  text(h1).flatMap {
                    case Some((text, h2)) =>
                      update(h2, acc.copy(release = Some(text)))
                    case None => Pull.raiseError(new RuntimeException("expected release"))
                  }
                }
                case "title" => {
                  text(h1).flatMap {
                    case Some((text, h2)) =>
                      update(h2, acc.copy(title = Some(text)))
                    case None => Pull.raiseError(new RuntimeException("expected title"))
                  }
                }
                case "id" => {
                  text(h1).flatMap {
                    case Some((text, h2)) =>
                      update(h2, acc.copy(id = Some(text)))
                    case None => Pull.raiseError(new RuntimeException("expected id"))
                  }
                }
                case "description" => {
                  text(h1).flatMap {
                    case Some((text, h2)) =>
                      update(h2, acc.copy(description = Some(text)))
                    case None => Pull.raiseError(new RuntimeException("expected description"))
                  }
                }
                case "severity" => {
                  text(h1).flatMap {
                    case Some((text, h2)) =>
                      update(h2, acc.copy(severity = Severity.fromString(text)))
                    case None => Pull.raiseError(new RuntimeException("expected severity"))
                  }
                }
                case "issued" => {
                  val issued = se.getAttributeByName(dateAttr).getValue
                  update(h1, acc.copy(issued = Some(
                    Instant.ofEpochSecond(issued.toLong)
                  )))
                }
                case _ => update(h1, acc)
              }
            }
            case EndEvent(ee) => {
              ee.getName.getLocalPart match {
                case "update" =>
                  Pull.pure((UpdateBuilder.build(acc).get, h1))
                case _ => update(h1, acc)
              }
            }
            case x =>
              update(h1, acc)
          }
      }
    }

    def go(h: Stream[F, XMLEvent]): Pull[F, UpdateF.Update, Option[Unit]] = {
      h.pull.uncons1.flatMap {
        case None =>
          Pull.pure(None)
        case Some((event, h1)) =>
          event match {
            case StartEvent(se) => {
              se.getName.getLocalPart match {
                case "update" =>
                  val tpe = UpdateType.fromString(se.getAttributeByName(typeAttr).getValue)
                  val from = Some(se.getAttributeByName(fromAttr).getValue)
                  val version = Some(se.getAttributeByName(versionAttr).getValue)
                  val status = Status.fromString(se.getAttributeByName(statusAttr).getValue)
                  val builder = UpdateBuilder(from, status, tpe, version)
                  update(h, builder).flatMap {
                    case (u, h) =>
                      Pull.output1(u) >> go(h)
                  }
                case _ => go(h1)
              }
            }
            case EndEvent(ee) if ee.getName.getLocalPart == "updates" =>
              Pull.pure(None)

            case x =>
              go(h1)
          }
      }
    }

    go(h).stream
  }


}
