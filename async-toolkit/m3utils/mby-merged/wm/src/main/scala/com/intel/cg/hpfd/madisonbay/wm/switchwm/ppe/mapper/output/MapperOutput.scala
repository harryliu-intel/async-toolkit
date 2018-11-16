package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper.output

sealed trait MapperVlanLearningMode
case object SharedVlanLearning extends MapperVlanLearningMode // 0
case object IndependentVlanLearning extends MapperVlanLearningMode // 1

case class ClassifierKeys(
                         // Isn't this PacketFields?...
                           key32: Vector[Int],
                           key16: Vector[Short],
                           key8:  Vector[Byte]
                         )

case class ActionPrecVal(
                        prec: Byte,
                        value: Int
                        )
case class ClassifierActions(
                            act24: Vector[ActionPrecVal],
                            act4:  Vector[ActionPrecVal],
                            act1:  Vector[ActionPrecVal]
                            )
case class MapperOutput(
                         classifierActions: ClassifierActions, //TODO
                       // TODO uncomment as they start to be used/available
                         classifierKeys: ClassifierKeys,
                         classifierProfile: Byte,
                         ipOption: Array[Boolean],
                         priorityProfile: Byte,
                         noPriorityEncoding: Boolean,
                         learningMode: MapperVlanLearningMode,
                         l2IngressVlan1Counter: Short
                         /*l2IngressDomain: Short,
                         l2IngressVlan1Counter: Short,
                         l3IngressDomain: Byte,
                         learningMode: MapperVlanLearningMode,
                         noPriorityEncoding: Boolean, // TODO remove negative? C is this way...
                         networkAddressingDomain: Boolean,
                         outerMplsPacketValid: Boolean,
                         parserError: Boolean,
                         parserInfo: Int, //TODO
                         priorityProfile: Byte,
                         rxPort: Int,
                         trafficClass: TrafficClass,
                         passthru: ParserOutputUnchanged
                       // pass-thru:
                         parityError: Boolean,
                         parserDrop: Boolean,
                         parserPointers: ProtocolsOffsets,
                         parserL3LengthError: Boolean,
                         rxData: Packet
                         */
)
