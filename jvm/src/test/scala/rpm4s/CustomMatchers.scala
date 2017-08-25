package rpm4s

import org.scalatest.{Assertion, Matchers}
import scodec.Codec

trait CustomMatchers { this: Matchers =>
  def roundtrip[T: Codec](value: T): Assertion = {
    val bits = Codec[T].encode(value).require
    Codec[T].decode(bits).require.value should equal(value)
  }
}
