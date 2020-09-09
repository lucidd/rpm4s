package rpm4s

import org.scalatest._
import rpm4s.data.{Lead, OS, RPMType}
import rpm4s.codecs._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class LeadSpec
    extends FlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with CustomMatchers {

  "Lead" should "roundtrip" in {
    val value =
      Lead(3, 0, RPMType.Binary, 1, "kernel-default-4.8.12-1.1", OS.Linux, 5)
    roundtrip(value)
  }

}
