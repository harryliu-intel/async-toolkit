package com.intel.cg.hpfd.madisonbay

import java.util.TreeMap

import com.intel.cg.hpfd.madisonbay.Memory._

import scala.collection.JavaConverters._
import scala.collection.breakOut
import scala.collection.immutable.SortedMap


case class AddressOverlap(first: AddressRange, second: AddressRange) extends Exception

/** Guards non-overlapping of address ranges. */
class AddressGuard {
  import AddressGuard.RdlName

  var map = new TreeMap[AddressRange, RdlName]((x,y) => x.pos.compare(y.pos))

  def +=(ar: AddressRange, el: RdlName): this.type = {
    val lb = map.floorKey(ar)
    val hb = map.ceilingKey(ar)
    // ..b c..d e..
    if( !(lb == null || (lb.lim <= ar.pos)) ) { throw new AddressOverlap(lb, ar) }
    if( !(hb == null || (ar.lim <= hb.pos)) ) { throw new AddressOverlap(ar, hb) }
    map.put(ar, el)
    this
  }

  def +=(tup: (AddressRange, RdlName)): this.type = this += (tup._1, tup._2)

  def pos: Address = map.firstKey.pos
  def lim: Address = map.lastKey.lim
  def width: Bits = lim - pos
  def range = AddressRange(pos, lim)
  def length: Int = ???

  /** Converts to a plain address map. */
  def toAddrMap: SortedMap[Address, RdlName] = map.asScala.map(x => (x._1.pos, x._2))(breakOut)
}
object AddressGuard {
  type RdlName = String

  def apply() = new AddressGuard

  def apply(el: (AddressRange, RdlName), rest: (AddressRange, RdlName)*): AddressGuard = {
    var guard = AddressGuard()
    guard += el
    for(r <- rest) {
      guard += r
    }
    guard
  }
}