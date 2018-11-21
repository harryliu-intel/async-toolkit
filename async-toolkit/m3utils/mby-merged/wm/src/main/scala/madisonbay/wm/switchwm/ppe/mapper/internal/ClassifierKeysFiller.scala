package madisonbay.wm.switchwm.ppe.mapper.internal

import madisonbay.wm.switchwm.ppe.parser.output.PacketFields

object ClassifierKeysFiller {
val classifierKeySourceIp = 0

// Range of MAP_PORT_DEFAULT indices
val mapDefaultKeysRange = 0 to 79
val mapForceDefaultKeysRange = 80 to 95

def getKey32(index: Int, parserKeys: PacketFields): Option[Int] =  ???
}
