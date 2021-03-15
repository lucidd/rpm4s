package rpm4s.repo.utils

import java.io.InputStream

import javax.xml.stream.{XMLInputFactory, XMLStreamReader}
import javax.xml.stream.events.{EndElement, StartElement, XMLEvent}
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.util.XMLEventAllocator
import cats.effect.{Blocker, ContextShift, Sync}
import com.sun.xml.internal.stream.events.XMLEventAllocatorImpl
import fs2.{Pipe, Stream}

package object xml {

  implicit class EventOps(val event: XMLEvent) extends AnyVal {
    def startElement: Option[StartElement] = {
      if (event.isStartElement) Some(event.asStartElement())
      else None
    }
  }

  object StartEvent {
    def unapply(event: XMLEvent): Option[StartElement] = {
      event.startElement
    }
  }

  object EndEvent {
    def unapply(event: XMLEvent): Option[EndElement] = {
      if (event.isEndElement) Some(event.asEndElement())
      else None
    }
  }

  def xmlreader[F[_]]: Pipe[F, InputStream, XMLStreamReader] = _.map { in =>
    val xmlif = XMLInputFactory.newInstance()
    xmlif.setProperty(XMLInputFactory.SUPPORT_DTD, false); // This disables DTDs entirely for that factory
    xmlif.setProperty("javax.xml.stream.isSupportingExternalEntities", false); // disable external entities
    xmlif.createXMLStreamReader(in)
  }

  def xmlevents[F[_]: Sync: ContextShift](in: InputStream, blocker: Blocker): Stream[F, XMLEvent] = {
    val xmlif = XMLInputFactory.newInstance()
    xmlif.setEventAllocator(new XMLEventAllocatorImpl())
    Stream.bracket(blocker.delay(xmlif.createXMLStreamReader(in)))(xmlsr => blocker.delay(xmlsr.close()))
      .flatMap { xmlsr =>
        Stream.unfoldEval[F, XMLStreamReader, XMLEvent](xmlsr) { xmlr =>
          blocker.delay {
            if (xmlr.hasNext) {
              xmlr.next()
              val event = xmlif.getEventAllocator.allocate(xmlr)
              Some((event, xmlr))
            } else None
          }
        }
      }
  }

}
