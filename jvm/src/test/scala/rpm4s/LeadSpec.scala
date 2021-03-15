package rpm4s

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import rpm4s.data.{Lead, OS, RPMType}
import rpm4s.codecs._

class LeadSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with CustomMatchers {

  "Lead" should "roundtrip" in {
    val value =
      Lead(3, 0, RPMType.Binary, 1, "kernel-default-4.8.12-1.1", OS.Linux, 5)
    roundtrip(value)
  }

}
