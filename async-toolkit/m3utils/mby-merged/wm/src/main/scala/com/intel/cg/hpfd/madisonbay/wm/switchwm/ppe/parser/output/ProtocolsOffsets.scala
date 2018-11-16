package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.ProtocolsOffsets.Pointer.PointerNumber
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.ProtocolsOffsets._

case class ProtocolsOffsets(headerPointers: Map[PointerNumber, HeaderPointer]) {

  def updated(pointerNumber: PointerNumber, protocolId: ProtocolId, offset: Int): ProtocolsOffsets =
    ProtocolsOffsets(headerPointers.updated(pointerNumber, HeaderPointer(protocolId, offset)))

  def protocolPointers(protocolId: ProtocolId): List[HeaderPointer] = headerPointers.collect {
      case (_, headerPointer) if headerPointer.protocolId == protocolId => headerPointer
    }.toList

  def validPointer(pointerNumber: PointerNumber): Boolean = headerPointers.contains(pointerNumber)

  def apply(pointerNumber: PointerNumber): HeaderPointer = headerPointers(pointerNumber)

}

object ProtocolsOffsets {

  type ProtocolId = Int

  object Pointer {
    type PointerNumber = Int

    val OuterL3: PointerNumber = 2
    val OuterL4: PointerNumber = 3
  }

  case class HeaderPointer(protocolId: ProtocolId, offset: Int)

  def apply(): ProtocolsOffsets = new ProtocolsOffsets(Map())

  def apply(protosHeaderPtrs: Map[PointerNumber, HeaderPointer]): ProtocolsOffsets = new ProtocolsOffsets(protosHeaderPtrs)

}
