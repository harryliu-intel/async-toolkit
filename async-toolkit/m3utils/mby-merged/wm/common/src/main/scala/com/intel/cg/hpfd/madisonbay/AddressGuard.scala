//scalastyle:off
package com.intel.cg.hpfd.madisonbay

import com.intel.cg.hpfd.madisonbay.Memory._

import scala.collection.breakOut
import scala.collection.immutable.SortedMap


/** Guards non-overlapping of address ranges.
  *
  * V --- value type, defaults to String
  */
class AddressGuard[+V] private (val map: SortedMap[AddressRange, V]) {
  import AddressGuard._

  /** Try to add a single element.
    * @param key address range to be added
    * @param value corresponding value
    * @return Some for success, None for failure
    */
  def tryAdd[V1 >: V](key: K, value: V1): GuardResult[V1] = {
    val left = map.to(key).lastOption.map(_._1)
    val right = map.from(key).headOption.map(_._1)
    val pairOp = (left, right) match {
      case (Some(lb), _) if lb.lim > key.pos => Left(lb)
      case (_, Some(hb)) if key.lim > hb.pos => Left(hb)
      case _ => Right((key, value))
    }
    pairOp.map(x => new AddressGuard(map + x))
  }

  /** Shortcut */
  def tryAdd[V1 >: V](pair: (K, V1)): GuardResult[V1] = {
    val (key, value) = pair
    tryAdd(key, value)
  }

  /** Try adding all elements in collection. */
  def tryAddAll[V1 >: V](src: Traversable[(K, V1)]): GuardResult[V1] = {
    src.view.foldRight(Right(this): GuardResult[V1])((el, guard) => guard.flatMap(_.tryAdd(el)))
  }

  def length: Int = map.size

  def pos: Address = map.firstKey.pos
  def lim: Address = map.lastKey.lim
  def width: Bits = lim - pos
  def range = AddressRange(pos, width)

  /** Converts to an address map. */
  def toAddrMap: SortedMap[K, V] = map
}
object AddressGuard {
  type K = AddressRange

  type GuardResult[V] = Either[AddressRange, AddressGuard[V]]

  def rangeOrdering[V]: Ordering[AddressRange] = Ordering.by[AddressRange, Address](_.pos)

  def apply[V]() = new AddressGuard(SortedMap.empty[K, V](rangeOrdering))

  def apply[V](elements: (AddressRange, V)*): AddressGuard[V] = {
    new AddressGuard(SortedMap[K, V](elements: _*)(rangeOrdering))
  }
}
