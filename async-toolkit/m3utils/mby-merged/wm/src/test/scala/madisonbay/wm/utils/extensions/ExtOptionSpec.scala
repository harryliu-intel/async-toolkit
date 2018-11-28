package madisonbay.wm.utils.extensions

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.OptionValues._

//scalastyle:off magic.number
class ExtOptionSpec extends FlatSpec with Matchers {

  "Ext Option" should "apply ifThenOpt" in {
    ExtOption.ifThenOpt(true) { 5 }.value shouldEqual 5
    ExtOption.ifThenOpt(false) { 5 } shouldBe empty
  }

}
