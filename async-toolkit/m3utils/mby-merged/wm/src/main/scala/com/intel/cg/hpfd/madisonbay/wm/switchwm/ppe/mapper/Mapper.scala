//scalastyle:off
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper

import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr.CsrMapper
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper.output._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.ParserOutput

object Mapper
{
  def runMapper(csrMapper: CsrMapper, parserOutput: ParserOutput): MapperOutput = {
    val _ = (csrMapper, parserOutput)

    MapperOutput(classifierActions = ClassifierActions(
      act24 = Vector.fill[ActionPrecVal](16)(ActionPrecVal(0, 0)),
      act4 = Vector.fill[ActionPrecVal](26)(ActionPrecVal(0, 0)),
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
