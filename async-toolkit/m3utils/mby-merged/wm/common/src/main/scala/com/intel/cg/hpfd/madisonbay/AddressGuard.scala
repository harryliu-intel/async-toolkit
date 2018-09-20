//scalastyle:off
package com.intel.cg.hpfd.madisonbay

import java.util.TreeMap

import com.intel.cg.hpfd.madisonbay.Memory._

import scala.collection.JavaConverters._
import scala.collection.breakOut
import scala.collection.immutable.SortedMap


case class AddressOverlap(first: AddressRange, second: AddressRange) extends Exception

/** Guards non-overlapping of address ranges.
  *
  * V --- value type, defaults to String
  */
class AddressGuard[V] {
  type K = AddressRange

  var map = new TreeMap[K,V]((x,y) => x.pos.compare(y.pos))

  def +=(ar: K, el: V) {
    val lb = map.floorKey(ar)
    val hb = map.ceilingKey(ar)
    // ..b c..d e..
    if( !(lb == null || (lb.lim <= ar.pos)) ) { throw new AddressOverlap(lb, ar) }
    if( !(hb == null || (ar.lim <= hb.pos)) ) { throw new AddressOverlap(ar, hb) }
    val _ = map.put(ar, el)
  }

  def +=(tup: (K,V)) {
    val (ar, el) = tup
    this += (ar, el)
  }

  def ++=(src: Traversable[(K,V)]) {
    for(el <- src) {
      this += el
    }
  }

  def length: Int = map.size()

  def pos: Address = map.firstKey.pos
  def lim: Address = map.lastKey.lim
  def width: Bits = lim - pos
  def range = AddressRange(pos, lim)

  /** Converts to a plain address map. */
  def toAddrMap: SortedMap[Address, V] = map.asScala.map { case(ar, el) => (ar.pos, el) } (breakOut)
}
object AddressGuard {
  def apply[V]() = new AddressGuard[V]

  def apply[V](el: (AddressRange, V), rest: (AddressRange, V)*): AddressGuard[V] = {
    val guard = AddressGuard[V]()
    guard += el
    guard ++= rest
    guard
  }
}
