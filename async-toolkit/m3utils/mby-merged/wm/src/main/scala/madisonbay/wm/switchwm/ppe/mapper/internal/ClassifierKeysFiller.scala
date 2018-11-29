package madisonbay.wm.switchwm.ppe.mapper.internal

import madisonbay.wm.switchwm.ppe.mapper.Mapper._
import madisonbay.wm.switchwm.ppe.mapper.defs.Classifier16BitKeys.Classifier16BitKey
import madisonbay.wm.switchwm.ppe.mapper.defs.{Classifier16BitKeys, Classifier32BitKeys}
import madisonbay.wm.switchwm.ppe.mapper.defs.Classifier32BitKeys.Classifier32BitKey
import madisonbay.wm.switchwm.ppe.mapper.internal.Registers.MappingRegisters
import madisonbay.wm.switchwm.ppe.parser.defs.ParserKeys
import madisonbay.wm.switchwm.ppe.parser.defs.ParserKeys.ParserKey
import madisonbay.wm.switchwm.ppe.parser.output.PacketFields
import madisonbay.wm.switchwm.ppe.ppe.Port

object ClassifierKeysFiller {
  // Range of MAP_PORT_DEFAULT TARGET indices, where if the Parser key is absent, we take the default.
  // Synonymous with the range of Parser keys that can be overwritten.
  private val mapDefaultTargetsRange = 0 to 79
  // Range of MAP_PORT_DEFAULT TARGET indices, where we overwrite the Parser key with VALUE.
  private val mapForceDefaultTargetsRange = 80 to 95
  // Range of Parser keys that can be force overwritten as specified above.
  private val mapForceDefaultKeysRange = 12 to 27

  private val key16RewritableNibbles = List(13, 19)
  //private val key8FirstRange = 0 to 7
  //private val key8SecondRange = 16 to 19

  /** Gets a single 32-bit Classifier keys based of Parser keys and/or defaults. */
  def getKey32(defaults: MapPortDefault, rxPort: Port, parserKeys: PacketFields, key: Classifier32BitKey): Option[Int] = {
    import Classifier32BitKeys._
    // 32-bit keys are assembled from two 16-bit keys
    val classifierKeysToParserKeys = Map[Classifier32BitKey, List[ParserKey]](
      OuterSourceIp      -> List(ParserKeys.OuterIpSourceAddress0, ParserKeys.OuterIpSourceAddress1),
      OuterDestiantionIp -> List(ParserKeys.OuterIpDestinationAddress0, ParserKeys.OuterIpDestinationAddress1))

    classifierKeysToParserKeys.get(key).flatMap(assembleFromParserKeys(defaults, rxPort, parserKeys, _, 0))
  }

  /** Gets a single 16-bit Classifier keys based of Parser keys and/or defaults. */
  def getKey16(mappingRegisters: MappingRegisters, mapRewrite: MapRewrite, profileAction: MapProfileAction,
               defaults: MapPortDefault, rxPort: Port, parserKeys: PacketFields,
               key: Classifier16BitKey): Option[Int] = {
    import Classifier16BitKeys._
    val classifierKeysToParserKeys = Map[Classifier16BitKey, List[ParserKey]](
      InnerSourceMac0      -> List(ParserKeys.InnerSourceMac0),
      InnerSourceMac1      -> List(ParserKeys.InnerSourceMac1),
      InnerSourceMac2      -> List(ParserKeys.InnerSourceMac2),
      OuterDestinationMac0 -> List(ParserKeys.OuterDestinationMac0),
      OuterDestinationMac1 -> List(ParserKeys.OuterDestinationMac1),
      OuterDestinationMac2 -> List(ParserKeys.OuterDestinationMac2),
      OuterSourceMac0      -> List(ParserKeys.OuterSourceMac0),
      OuterSourceMac1      -> List(ParserKeys.OuterSourceMac1),
      OuterSourceMac2      -> List(ParserKeys.OuterSourceMac2),
      MappedNibbles0       -> List(),
      OuterEthertype       -> List(ParserKeys.OuterEthertype),
      OuterVlan1           -> List(ParserKeys.OuterVlan1),
      OuterVlan2           -> List(ParserKeys.OuterVlan2),
      OuterL4Source        -> List(ParserKeys.OuterL4Source),
      OuterL4Destination   -> List(ParserKeys.OuterL4Destination)
    )

    // TODO mappings and others...

    classifierKeysToParserKeys.get(key).flatMap(assembleFromParserKeys(defaults, rxPort, parserKeys, _, 0)).
      map(value => rewrite16BitKey(mappingRegisters, mapRewrite, profileAction, key, parserKeys, value))
  }

  /**
    * This function gets the specified Parser keys, keeping in mind things such as defaults and their precedence.
    * Since classifier keys can be made from multiple parser keys, they are specified in a list, and the product
    * are the parser keys appended together - first one in the list is in the most significant position.
    */
  private def assembleFromParserKeys(defaults: MapPortDefault,
                                      rxPort: Port,
                                      parserKeys: PacketFields,
                                      parserKeysForThisClassifierKey: List[ParserKey],
                                      accumulatedValue: Int): Option[Int] = {
    def getDefault(parserKey: ParserKey, keysRange: Range, targetsRange: Range) = {
      for {
        portDefaultEntryTarget <- if (keysRange.contains(parserKey.index))
          Some(targetsRange.head + parserKey.index - keysRange.head) else None
        portDefaultEntryValue <- defaults.forRxPort(rxPort.index).collectFirst{case MapPortDefaultEntry(value, `portDefaultEntryTarget`) => value}
      } yield portDefaultEntryValue
    }

    def getFromParser(parserKey: ParserKey) = {
      parserKeys.fields.get(parserKey).map(_.toInt)
    }

    def getForParserKey(parserKey: ParserKey) = {
      val whatWasFound = getDefault(parserKey, mapForceDefaultKeysRange, mapForceDefaultTargetsRange).orElse(
        getFromParser(parserKey)).orElse(
        getDefault(parserKey, mapDefaultTargetsRange, mapDefaultTargetsRange))

      whatWasFound
    }

    parserKeysForThisClassifierKey match {
      case k :: ks =>  getForParserKey(k).flatMap(keyValue => assembleFromParserKeys(defaults, rxPort, parserKeys,
        ks, (accumulatedValue << 16) + keyValue))
      case Nil => Some(accumulatedValue)
    }
  }

  def rewrite16BitKey(mappingRegisters: MappingRegisters, mapRewrite: MapRewrite,  profileAction: MapProfileAction,
                      classifierKey: Classifier16BitKey, parserKeys: PacketFields,
                      initialValue: Int): Int = {
    // TODO most of it can be probably generalized with key8 once we support it anyhow
    if (key16RewritableNibbles.contains(classifierKey.index)) {
      // TODO comment this
      val (_, which) = key16RewritableNibbles.zipWithIndex.find{case (index, _) => index == classifierKey.index}.get
      // We have 4 nibbles per each 16-bit word. Those == indices of MAP_REWRITE[_][n]
      val matchingNibbles = (0 until 4).map(which * 4 + _)
      val actionProfile = profileAction.lookupProfile()
      actionProfile match {
        case Some (profile) => {
          val rewritesForThis16 = mapRewrite.forProfile(profile.rewriteProfile).zipWithIndex.
            filter{case (_, oneRewrite) => matchingNibbles.contains(oneRewrite)}
          rewritesForThis16.foldLeft(initialValue){(returnValue, next) =>
            val (oneRewrite, index) = next
            val nibbleInWord = index % 4

            NibbleMapper.mapNibbleInIfApplicable(mappingRegisters, parserKeys, oneRewrite, nibbleInWord, returnValue)
          }
        }
        case None => initialValue
      }
    } else {
      initialValue
    }
  }
}
