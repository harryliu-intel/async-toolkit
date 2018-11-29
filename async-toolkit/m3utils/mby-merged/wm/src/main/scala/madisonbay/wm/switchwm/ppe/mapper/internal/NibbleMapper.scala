package madisonbay.wm.switchwm.ppe.mapper.internal

import madisonbay.wm.switchwm.ppe.mapper.Mapper.MapRewriteEntry
import madisonbay.wm.switchwm.ppe.mapper.internal.Registers.{MacToMap, MappingRegisters}
import madisonbay.wm.switchwm.ppe.parser.defs.ParserKeys
import madisonbay.wm.switchwm.ppe.parser.defs.ParserKeys.ParserKey
import madisonbay.wm.switchwm.ppe.parser.output.PacketFields

object NibbleMapper {
  object SourceId extends Enumeration {
    val OuterDestinationMacHigh = Value(4)
    val OuterDestinationMacLow  = Value(5)
    val OuterSourceMacHigh      = Value(6)
    val OuterSourceMacLow       = Value(7)
  }

  private def getMacFromParserKeys(key: ParserKey, parserKeys: PacketFields) = {
    // Here we take advantage of the fact that MAC is stored as 3 consecutive parser keys.
    (0 until 3).map(n => ParserKeys.getConstant(n + key.index)).foldLeft(0L)((acc, key) =>
      acc << 16 | parserKeys.key16(key))
  }

  private def mapMacNibble(regs: MappingRegisters, parserKeys: PacketFields, srcId: Int): Option[Int] = {
    val mapOfMacMappings = Map[SourceId.Value, (MacToMap.Value, Boolean)](
      SourceId.OuterDestinationMacHigh -> (MacToMap.OuterDestinationMac, true),
      SourceId.OuterDestinationMacLow  -> (MacToMap.OuterDestinationMac, false),
      SourceId.OuterSourceMacHigh      -> (MacToMap.OuterSourceMac,      true),
      SourceId.OuterSourceMacLow       -> (MacToMap.OuterSourceMac,      false))

    val maybeMappingTuple = SourceId.values.collectFirst{case x if x.id == srcId => mapOfMacMappings(x)}

    maybeMappingTuple.flatMap{case (mapping, isHigh) =>
      val macToCompareAgainst = mapping match {
        case MacToMap.OuterDestinationMac => getMacFromParserKeys(ParserKeys.OuterDestinationMac0, parserKeys)
        case MacToMap.OuterSourceMac => getMacFromParserKeys(ParserKeys.OuterSourceMac0, parserKeys)
        case _ => ???
      }

        val registerDescribingThisMappingOption = regs.getMacMappings(mapping).reverse.headOption
        registerDescribingThisMappingOption.flatMap { registerDescribingThisMapping =>
          val ignoreMask = ~((1L << registerDescribingThisMapping.ignoreLength) - 1)
          // now we get reg entries only for the correct (source/dest and inner/outer wise) mapping
          if (((registerDescribingThisMapping.mac & ignoreMask) == (macToCompareAgainst & ignoreMask)) && // make sure MAC matches on bits we care...
              ((registerDescribingThisMapping.mac & ~ignoreMask) == 0)) { // ...and bits we don't care about are 0
            if (isHigh) Some(registerDescribingThisMapping.mappedMac >> 4)
              else Some(registerDescribingThisMapping.mappedMac & 0xF)
          }
          else None
        }
    }
  }

  def mapNibbleInIfApplicable(regs: MappingRegisters, parserKeys: PacketFields,
                                        entry: MapRewriteEntry, nibbleNo: Int, from: Int): Int = {
    val mappingFunctions = List(mapMacNibble _)

    val nibbleToMapInOption = mappingFunctions.map(f => f(regs, parserKeys, entry.srcId)).collectFirst{case Some(x) => x}

    nibbleToMapInOption match {
      case Some(nibbleToMapIn) => {
        val maskForBlanking = ~(0xF << nibbleNo * 4)
        val blanked = from & maskForBlanking
        blanked | (nibbleToMapIn << nibbleNo * 4)
      }
      case None => from
    }
  }
}
