package madisonbay.wm.switchwm.ppe.mapper

import madisonbay.wm.switchwm.csr.Csr.CsrMapper
import madisonbay.wm.switchwm.ppe.mapper.output._
import madisonbay.wm.switchwm.ppe.parser.output.ParserOutput
import madisonbay.wm.switchwm.ppe.mapper.internal.ClassifierActionsFiller

import madisonbay.csr.all._

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

  def runMapper(csrMapper: CsrMapper, parserOutput: ParserOutput): MapperOutput = {
    val _ = (csrMapper, parserOutput)

    val mapPortDefaults = new MapPortDefault(csrMapper.ppeMapperMap)

    MapperOutput(classifierActions = ClassifierActions(
      act24 = Vector.tabulate[ActionPrecVal](16)(index => ClassifierActionsFiller.getAct24(mapPortDefaults, parserOutput.rxPort, index)),
      act4 = Vector.tabulate[ActionPrecVal](26)(index => ClassifierActionsFiller.getAct4(mapPortDefaults, parserOutput.rxPort, index)),
      act1 = Vector.fill[ActionPrecVal](24)(ActionPrecVal(0, 0))
    ),
      classifierKeys = ClassifierKeys(
      key32 = Vector.fill[Int](16)(0),
      key16 = Vector.fill[Short](32)(0.toShort),
      key8 = Vector.fill[Byte](16)(0)),
      classifierProfile = 0,
      ipOption = Array.fill[Boolean](2)(false),
      priorityProfile = 0,
      noPriorityEncoding = false,
      learningMode = SharedVlanLearning,
      l2IngressVlan1Counter = 0)
  }
}
