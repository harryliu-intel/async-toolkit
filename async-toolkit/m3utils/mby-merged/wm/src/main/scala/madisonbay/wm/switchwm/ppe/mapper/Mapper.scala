package madisonbay.wm.switchwm.ppe.mapper

import madisonbay.wm.switchwm.csr.Csr.CsrMapper
import madisonbay.wm.switchwm.ppe.mapper.output._
import madisonbay.wm.switchwm.ppe.parser.output.ParserOutput
import madisonbay.wm.switchwm.ppe.mapper.internal.ClassifierActionsFiller._
import madisonbay.wm.switchwm.ppe.mapper.internal.ClassifierKeysFiller._
import madisonbay.csr.all._
import madisonbay.wm.switchwm.csr.lens.MapperLenses
import madisonbay.wm.switchwm.ppe.mapper.defs.{Classifier16BitKeys, Classifier32BitKeys}
import madisonbay.wm.switchwm.ppe.mapper.internal.Registers.MappingRegisters

//scalastyle:off magic.number
object Mapper
{
  // TODO move away into some bigger object once even more registers are used
  case class MapPortDefaultEntry(value: Int, target: Int)
  class MapPortDefault(csrs: mby_ppe_mapper_map) {

    def forRxPort(rxPort: Int): List[MapPortDefaultEntry] = {
      csrs.MAP_PORT_DEFAULT(rxPort).MAP_PORT_DEFAULT.map(singleDefault =>
        MapPortDefaultEntry(singleDefault.VALUE().toInt, singleDefault.TARGET().toInt))
    }
  }

  case class MapProfileActionProfile(profile: Int, rewriteProfile: Int)
  class MapProfileAction(csrs: mby_ppe_mapper_map) {
    def lookupProfile(): Option[MapProfileActionProfile] = {
      MapperLenses.profileActionProfileTraversal.getAll(csrs).
        foldLeft[Option[MapProfileActionProfile]](None)((result, actionProfile) => {
        // TODO a bunch of checks whether the profile matches the packet at all...
        if (actionProfile.PROFILE_VALID() == 1) { Some(MapProfileActionProfile(actionProfile.PROFILE().toInt,
                                                                        actionProfile.REWRITE_PROFILE().toInt)) }
          else { result }
      })
    }

    def lookupTrigger = ???
    def lookupPriority = ???
  }

  case class MapRewriteEntry(srcId: Int)
  class MapRewrite(csrs: mby_ppe_mapper_map) {
    def forProfile(profileId: Int): Vector[MapRewriteEntry] = {
      MapperLenses.mapRewriteForProfileLens(profileId).getOption(csrs).get.MAP_REWRITE.
        map(oneRewrite => MapRewriteEntry(oneRewrite.SRC_ID().toInt)).toVector
    }
  }

  def runMapper(csrMapper: CsrMapper, parserOutput: ParserOutput): MapperOutput = {
    val _ = (csrMapper, parserOutput)

    val mapPortDefaults = new MapPortDefault(csrMapper.ppeMapperMap)
    val mapProfileAction = new MapProfileAction(csrMapper.ppeMapperMap)
    val mapRewrite = new MapRewrite(csrMapper.ppeMapperMap)
    val mappingRegisters = new MappingRegisters(csrMapper.ppeMapperMap)

    MapperOutput(classifierActions = ClassifierActions(
      act24 = Vector.tabulate[ActionPrecVal](16)(index => getAct24(mapPortDefaults, parserOutput.rxPort, index)),
      act4 = Vector.tabulate[ActionPrecVal](26)(index => getAct4(mapPortDefaults, parserOutput.rxPort, index)),
      act1 = Vector.fill[ActionPrecVal](24)(ActionPrecVal(0, 0))
    ),
      classifierKeys = ClassifierKeys(
      key32 = mapCollect(Classifier32BitKeys.definedConstants, getKey32(mapPortDefaults, parserOutput.rxPort, parserOutput.parserKeys, _)),
      key16 = mapCollect(Classifier16BitKeys.definedConstants, getKey16(mappingRegisters, mapRewrite, mapProfileAction, mapPortDefaults,
        parserOutput.rxPort, parserOutput.parserKeys, _)),
      key8 = Vector.fill[Byte](16)(0)),
      classifierProfile = mapProfileAction.lookupProfile().map(_.profile.toByte).getOrElse(0.toByte),
      ipOption = Array.fill[Boolean](2)(false),
      priorityProfile = 0,
      noPriorityEncoding = false,
      learningMode = SharedVlanLearning,
      l2IngressVlan1Counter = 0)
  }

  private def mapCollect[A, B](list: List[A], fn: A => Option[B]): Map[A, B] =
    list.map(key => (key, fn(key))).collect{case (k, Some(v)) => (k, v)}.toMap
}
