/** Implement an executable spec for the Madison Bay 25 Terabit Switch Product
  *
  * ==Overview==
  * The architecture team provides a 'zero-time' executable spec of the intended architecture for our
  * switch product.
  *
  * ==Limitations==
  * ===Zero-time abstraction===
  * The model does not allow us to simulate more than one packet 'in-flight' at a time, which makes modeling congestion
  * handling difficult.
  * ===Security Policy Groups===
  * The model does not yet implement the security policy described in the SystemRDL, we believe this should be fixed.
 **/
package object switch_wm {
}
