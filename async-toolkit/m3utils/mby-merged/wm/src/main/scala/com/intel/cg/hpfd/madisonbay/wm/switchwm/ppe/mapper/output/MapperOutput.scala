package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper.output

/*
TODO use when possible (see below)
sealed trait MapperVlanLearningMode
case object SharedVlanLearning extends MapperVlanLearningMode // 0
case object IndependentVlanLearning extends MapperVlanLearningMode // 1
 */


case class MapperOutput(
                         classifierActions: Int //TODO
                       // TODO uncomment as they start to be used/available
                       /*
                         classifierKeys: Int, //TODO
                         classifierProfile: Byte,
                         ipOption: Array[Boolean],
                         l2IngressDomain: Short,
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
                       // pass-thru:
                         parityError: Boolean,
                         parserDrop: Boolean,
                         parserPointers: ProtocolsOffsets,
                         parserL3LengthError: Boolean,
                         rxData: Packet
                         */
)
