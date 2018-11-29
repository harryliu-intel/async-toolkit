//scalastyle:off
package madisonbay

import monocle.Optional
import scala.collection.immutable.HashMap

import com.intel.cg.hpfd.madisonbay.BitVector
import com.intel.cg.hpfd.madisonbay.Memory._

/** Provides classes and associated objects for interfacing the configuration and status registers
  *
  * ==Overview==
  * There are two categories of classes/objects in this package. The majority of these are derived from the
  * RDL structure of the design. These are created with the 'genviews' tools within the M3 directory (the
  * parser is written in Modula-3).
  *
  * Other classes and objects are human-written to support these classes and provide elegant mechanisms
  * for using them.
 **/
package object csr {

  /**
    * Can hold any top element, e.g.
    * {{{
    *  import monocle.Optional
    *  import madisonbay.csr.all._
    *
    *  val root = mby_top_map(Address at 0.bytes)
    *  val paths = mby_top_map.genOpticsLookup(top, Optional.id)
    *
    *  CsrContext(root, paths)
    * }}}
    */
  case class CsrContext[A](root: A, paths: HashMap[Address, Optional[A, BitVector]])
}
