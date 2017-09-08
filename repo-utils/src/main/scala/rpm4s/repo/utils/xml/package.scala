package rpm4s.repo.utils

import java.io.InputStream
import javax.xml.stream.{XMLInputFactory, XMLStreamReader}
import javax.xml.stream.events.{EndElement, StartElement, XMLEvent}

import cats.effect.Sync
import com.sun.xml.internal.fastinfoset.stax.factory.StAXInputFactory
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
    val xmlif = StAXInputFactory.newInstance()
    xmlif.setProperty(XMLInputFactory.SUPPORT_DTD, false); // This disables DTDs entirely for that factory
    xmlif.setProperty("javax.xml.stream.isSupportingExternalEntities", false); // disable external entities
    xmlif.createXMLStreamReader(in)
  }

  def xmlevents[F[_]: Sync](in: InputStream): Stream[F, XMLEvent] = {
    val xmlif = StAXInputFactory.newInstance()
    xmlif.setEventAllocator(new XMLEventAllocatorImpl())
    Stream.bracket(Sync[F].delay(xmlif.createXMLStreamReader(in)))(
      xmlsr => {
        Stream.unfoldEval[F, XMLStreamReader, XMLEvent](xmlsr) { xmlr =>
          Sync[F].delay {
            if (xmlr.hasNext) {
              xmlr.next()
              val event = xmlif.getEventAllocator.allocate(xmlr)
              Some((event, xmlr))
            } else None
          }
        }
      },
      xmlsr => Sync[F].delay(xmlsr.close())
    )
  }

}
