package switch_wm

import java.io.File

import com.intel.cg.hpfd.csr.generated.{parser_ext_r, parser_ext_rf}
import org.scalatest._

/**
  * Demonstrate some operations on CSRs.
  */
class CsrSpec extends FlatSpec with Matchers {
  "Read result " should " match set result " in {
    val csr = parser_ext_rf()
    csr(0).foreachResetableField(x => x.reset())
    csr(0).FLAG_NUM() = 15
    csr(0).FLAG_NUM() shouldEqual 15
  }
}
