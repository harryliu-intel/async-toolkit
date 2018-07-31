package switch_wm

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
    * Implicit method to allow direct usage of the value of a field, without a () to explicitly
    * call the apply method.
    *
    * Controversial whether we want to allow this.
    * @param f
    * @return
    */
  implicit def fieldToLong(f : RdlRegister[Long]#HardwareReadable) : Long = f.apply

}
