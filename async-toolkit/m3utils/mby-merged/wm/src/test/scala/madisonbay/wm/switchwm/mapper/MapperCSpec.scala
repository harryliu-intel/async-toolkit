//scalastyle:off
package madisonbay.wm.switchwm.mapper

import madisonbay.wm.switchwm.csr.Csr
import madisonbay.wm.switchwm.csr.Csr.CsrMapper
import madisonbay.wm.switchwm.ppe.mapper.Mapper
import madisonbay.wm.switchwm.ppe.mapper.output.{IndependentVlanLearning, MapperOutput}
import madisonbay.wm.switchwm.ppe.parser.output.{CheckSums, PacketFields, ParserOutput, ProtocolsOffsets}
import madisonbay.wm.switchwm.ppe.ppe.Port
import madisonbay.wm.utils.BitFlags
import org.scalatest.{FlatSpec, Matchers}
import monocle.state.all._

import scala.collection.mutable
import madisonbay.csr.all._
import madisonbay.wm.switchwm.csr.lens.{CsrLenses, MapperLenses}
import madisonbay.wm.switchwm.ppe.mapper.defs.{Classifier16BitKeys, Classifier32BitKeys}
import madisonbay.wm.switchwm.ppe.parser.defs.ParserKeys

class MapperCSpec extends FlatSpec with Matchers {
  case class  MapperTestPacketData private (
                                           srcIp: Int,
                                           dstIp: Int,
                                           srcMac: Long,
                                           dstMac: Long,
                                           srcL4Port: Short,
                                           dstL4Port: Short
                                           )
def loadPaKeys(packetData: MapperTestPacketData, into: ParserOutput): ParserOutput = {

  val ipHeader = Array(        0x45, 0x00, 0x00,   62, // 62 is len
    0xAB, 0xCD, 0x00, 0x00, // ABCD = identification
    0x40, 0x06, 0xFF, 0xFF, // TTL = 0x40, protocol = TCP
    // ffs is checksum
    (packetData.srcIp >> 24) & 0xFF,
    (packetData.srcIp >> 16) & 0xFF,
    (packetData.srcIp >>  8) & 0xFF,
     packetData.srcIp        & 0xFF,
    (packetData.dstIp >> 24) & 0xFF,
    (packetData.dstIp >> 16) & 0xFF,
    (packetData.dstIp >>  8) & 0xFF,
     packetData.dstIp        & 0xFF)

  val MBY_PA_KEYS_OUTER_DMAC          =  6
  val MBY_PA_KEYS_OUTER_SMAC          =  9
  val MBY_PA_KEYS_OUTER_ETYPE         = 12
  val MBY_PA_KEYS_OUTER_L4SRC         = 16
  val MBY_PA_KEYS_OUTER_L4DST         = 17
  val MBY_PA_KEYS_OUTER_IP_HEADER     = 42
  val MBY_PA_KEYS_OUTER_SIPDIP        = 48

  val MBY_PA_FLAGS_OTR_L4_TCP_V       =  5
  val MBY_PA_FLAGS_OTR_L3_V           = 22
  val MBY_PA_FLAGS_OTR_L4_V           = 23

  val keys = mutable.Map[Int, Short]()
  keys += ((MBY_PA_KEYS_OUTER_DMAC + 0, ((packetData.dstMac >> 32) & 0xFFFF).toShort))
  keys += ((MBY_PA_KEYS_OUTER_DMAC + 1, ((packetData.dstMac >> 16) & 0xFFFF).toShort))
  keys += ((MBY_PA_KEYS_OUTER_DMAC + 2, (packetData.dstMac & 0xFFFF).toShort))

  keys += ((MBY_PA_KEYS_OUTER_SMAC + 0, ((packetData.srcMac >> 32) & 0xFFFF).toShort))
  keys += ((MBY_PA_KEYS_OUTER_SMAC + 1, ((packetData.srcMac >> 16) & 0xFFFF).toShort))
  keys += ((MBY_PA_KEYS_OUTER_SMAC + 2, (packetData.srcMac & 0xFFFF).toShort))

  keys += ((MBY_PA_KEYS_OUTER_ETYPE, 0x0800))

  keys += ((MBY_PA_KEYS_OUTER_L4SRC, packetData.srcL4Port))
  keys += ((MBY_PA_KEYS_OUTER_L4DST, packetData.dstL4Port))

  ipHeader.grouped(2).zipWithIndex.foreach{pair =>
    val (bytes, i) = pair
    keys += ((MBY_PA_KEYS_OUTER_IP_HEADER + i, ((bytes(0) << 8) + bytes(1)).toShort))
  }

  keys += ((MBY_PA_KEYS_OUTER_SIPDIP + 0, ((packetData.srcIp >> 16) & 0xFFFF).toShort))
  keys += ((MBY_PA_KEYS_OUTER_SIPDIP + 1, ( packetData.srcIp & 0xFFFF).toShort))
  keys += ((MBY_PA_KEYS_OUTER_SIPDIP + 2, ((packetData.dstIp >> 16) & 0xFFFF).toShort))
  keys += ((MBY_PA_KEYS_OUTER_SIPDIP + 3, ( packetData.dstIp & 0xFFFF).toShort))

  val flags = mutable.BitSet()

  flags += MBY_PA_FLAGS_OTR_L4_TCP_V
  flags += MBY_PA_FLAGS_OTR_L3_V
  flags += MBY_PA_FLAGS_OTR_L4_V

  into.copy(parserKeys = PacketFields(keys.toMap.map{case (k, v) => (ParserKeys.getConstant(k), v)}), parserFlags = BitFlags(flags))
}

  def checkAgainstKeys(packetData: MapperTestPacketData, name: String, mapperOutput: MapperOutput): Unit = {
    "Mapper/" + name should "put source IP into FFU_KEYS" in {
      mapperOutput.classifierKeys.key32(Classifier32BitKeys.getConstant(0)) shouldEqual packetData.srcIp
    }

    it should "put destination IP into FFU_KEYS" in {
      mapperOutput.classifierKeys.key32(Classifier32BitKeys.getConstant(1)) shouldEqual packetData.dstIp
    }

    it should "put destination MAC into FFU_KEYS" in {
      mapperOutput.classifierKeys.key16(Classifier16BitKeys.getConstant(6)) shouldEqual ((packetData.dstMac >> 32) & 0xFFFF).toShort
      mapperOutput.classifierKeys.key16(Classifier16BitKeys.getConstant(7)) shouldEqual ((packetData.dstMac >> 16) & 0xFFFF).toShort
      mapperOutput.classifierKeys.key16(Classifier16BitKeys.getConstant(8)) shouldEqual ( packetData.dstMac & 0xFFFF).toShort
    }

    it should "put source MAC into FFU_KEYS" in {
      mapperOutput.classifierKeys.key16(Classifier16BitKeys.getConstant( 9)) shouldEqual ((packetData.srcMac >> 32) & 0xFFFF).toShort
      mapperOutput.classifierKeys.key16(Classifier16BitKeys.getConstant(10)) shouldEqual ((packetData.srcMac >> 16) & 0xFFFF).toShort
      mapperOutput.classifierKeys.key16(Classifier16BitKeys.getConstant(11)) shouldEqual ( packetData.srcMac & 0xFFFF).toShort
    }

    it should "put source L4 port into FFU_KEYS" in {
      mapperOutput.classifierKeys.key16(Classifier16BitKeys.getConstant(16)) shouldEqual packetData.srcL4Port
    }

    it should "put destination L4 port into FFU_KEYS" in {
      mapperOutput.classifierKeys.key16(Classifier16BitKeys.getConstant(17)) shouldEqual packetData.dstL4Port
    }
  }

  def runOnSimpleTcp(csr: CsrMapper, name: String, check: MapperOutput => Unit): Unit = {
    val simpleTcp = MapperTestPacketData(
      srcIp  = 0x01020304,
      dstIp  = 0x05060708,
      srcMac = 0x111111111111L,
      dstMac = 0x222222222222L,
      srcL4Port = 0xA123.toShort,
      dstL4Port = 0xA456.toShort
    )

    val empty = ParserOutput(Csr().getParser(0), Port(0), 0, PacketFields(), BitFlags(), ProtocolsOffsets(),
      None, 0, CheckSums(None, drop = false))

    val updated = loadPaKeys(simpleTcp, empty)

    val output = Mapper.runMapper(csr, updated)

    checkAgainstKeys(simpleTcp, name, output)
    check(output)
  }


  val csr = Csr().getMapper(0)
  runOnSimpleTcp(csr, "Simple TCP", _ => ())

  def act24Default(): Unit = {
    val targetOne = MapperLenses.portDefaultTargetLens(0, 0)
    val valueOne = MapperLenses.portDefaultValueLens(0, 0)
    val targetTwo = MapperLenses.portDefaultTargetLens(0, 1)
    val valueTwo = MapperLenses.portDefaultValueLens(0, 1)

    val updatedCsr = CsrLenses.execute(csr.ppeMapperMap, for {
      _ <- targetOne.assign_(96)
      _ <- valueOne.assign_(0xabcd)
      _ <- targetTwo.assign_(112)
      _ <- valueTwo.assign_(0xff)
    } yield ())

    runOnSimpleTcp(CsrMapper(0, updatedCsr), "act24 default", output => {
      it should "fill act24 using MAP_PORT_DEFAULT" in {
        output.classifierActions.act24(0).value shouldEqual 0xffabcd
      }
    })
  }
  act24Default()

  def act4Default(): Unit = {
    val targetLens = MapperLenses.portDefaultTargetLens(0, 0)
    val valueLens = MapperLenses.portDefaultValueLens(0, 0)

    val updatedCsr = CsrLenses.execute(csr.ppeMapperMap, for {
      _ <- targetLens.assign_(192)
      _ <- valueLens.assign_(0xb)
    } yield ())

    runOnSimpleTcp(CsrMapper(0, updatedCsr), "act4 default", output => {
      it should "fill act4 using MAP_PORT_DEFAULT" in {
        output.classifierActions.act4(0).value shouldEqual 0xb
      }
    })
  }
  act4Default()

  def act4DoubleDefault(): Unit = {
    val targetLens = MapperLenses.portDefaultTargetLens(0, 0)
    val valueLens = MapperLenses.portDefaultValueLens(0, 0)

    val updatedCsr = CsrLenses.execute(csr.ppeMapperMap, for {
      _ <- targetLens.assign_(160)
      _ <- valueLens.assign_(0xfe)
    } yield ())

    runOnSimpleTcp(CsrMapper(0, updatedCsr), "act4 double default", output => {
      it should "fill act4 using MAP_PORT_DEFAULT" in {
        output.classifierActions.act4(0).value shouldEqual 0xe
        output.classifierActions.act4(1).value shouldEqual 0xf
      }
    })
  }
  act4DoubleDefault()

  def act4QuadDefault(): Unit = {
    val targetLens = MapperLenses.portDefaultTargetLens(0, 0)
    val valueLens = MapperLenses.portDefaultValueLens(0, 0)

    val updatedCsr = CsrLenses.execute(csr.ppeMapperMap, for {
      _ <- targetLens.assign_(129)
      _ <- valueLens.assign_(0xabcd)
    } yield ())

    runOnSimpleTcp(CsrMapper(0, updatedCsr), "act4 quad default", output => {
      it should "fill act4 using MAP_PORT_DEFAULT" in {
        output.classifierActions.act4(1).value shouldEqual 0xd
        output.classifierActions.act4(2).value shouldEqual 0xc
        output.classifierActions.act4(3).value shouldEqual 0xb
        output.classifierActions.act4(4).value shouldEqual 0xa
      }
    })
  }
  act4QuadDefault()

  def default0(): Unit = {
    val targetLens = MapperLenses.portDefaultTargetLens(0, 0)
    val valueLens = MapperLenses.portDefaultValueLens(0, 0)

    val updatedCsr = CsrLenses.execute(csr.ppeMapperMap, for {
      _ <- targetLens.assign_(3)
      _ <- valueLens.assign_(0xba)
    } yield ())

    runOnSimpleTcp(CsrMapper(0, updatedCsr), "default0", output => {
      it should "fill key16 using MAP_PORT_DEFAULT" in {
        output.classifierKeys.key16(Classifier16BitKeys.getConstant(3)) shouldEqual 0xba
      }
    })
  }
  default0()

  def RE_KEYS_OUTER_VLAN1(): Unit = {
    val targetOne = MapperLenses.portDefaultTargetLens(0, 1)
    val valueOne = MapperLenses.portDefaultValueLens(0, 1)
    val targetTwo = MapperLenses.portDefaultTargetLens(0, 2)
    val valueTwo = MapperLenses.portDefaultValueLens(0, 2)

    val MBY_RE_KEYS_OUTER_VLAN1 = 14

    val updatedCsr = CsrLenses.execute(csr.ppeMapperMap, for {
      _ <- targetOne.assign_(MBY_RE_KEYS_OUTER_VLAN1)
      _ <- valueOne.assign_(0)
      _ <- targetTwo.assign_(MBY_RE_KEYS_OUTER_VLAN1)
      _ <- valueTwo.assign_(0xfef)
    } yield ())

    runOnSimpleTcp(CsrMapper(0, updatedCsr), "RE_KEYS_OUTER_VLAN1", output => {
      ignore should "fill key16 using MAP_PORT_DEFAULT" in {
        output.classifierKeys.key16(Classifier16BitKeys.getConstant(MBY_RE_KEYS_OUTER_VLAN1)) shouldEqual 0xfef
      }
    })
  }
  RE_KEYS_OUTER_VLAN1()

  def default_forced(): Unit = {
    val targetLens = MapperLenses.portDefaultTargetLens(0, 3)
    val valueLens = MapperLenses.portDefaultValueLens(0, 3)

    val updatedCsr = CsrLenses.execute(csr.ppeMapperMap, for {
      _ <- targetLens.assign_(80)
      _ <- valueLens.assign_(0xdede)
    } yield ())

    runOnSimpleTcp(CsrMapper(0, updatedCsr), "default forced", output => {
      it should "fill key16 using MAP_PORT_DEFAULT" in {
        output.classifierKeys.key16(Classifier16BitKeys.getConstant(12)) shouldEqual 0xdede
      }
    })
  }
  default_forced()

  def activateProfile0(csr : mby_ppe_mapper_map) : mby_ppe_mapper_map = {
    CsrLenses.execute(csr, for {
      _ <- MapperLenses.profileActionProfileLens(0).assign_(3)
      _ <- MapperLenses.profileActionTrigValidLens(0).assign_(1)
      _ <- MapperLenses.profileActionIpOptionsMaskLens(0).assign_(64)
      _ <- MapperLenses.profileActionProfileValidLens(0).assign_(1)
    } yield ())
  }

  def mapSmac(): Unit = {
    val mapped_mac = 0x7E.toByte

    val SOURCE_MAP_OUTER_SMAC_H = 6
    val SOURCE_MAP_OUTER_SMAC_L = 7

    val updatedCsr = CsrLenses.execute(csr.ppeMapperMap, for {
      _ <- MapperLenses.macMacLens(0).assign_(0x111111111111L)
      _ <- MapperLenses.macValidLens(0).assign_(2)
      _ <- MapperLenses.macMapMacLens(0).assign_(mapped_mac)
      _ <- MapperLenses.mapRewriteSrcId(0, 0).assign_(SOURCE_MAP_OUTER_SMAC_L)
      _ <- MapperLenses.mapRewriteSrcId(0, 1).assign_(SOURCE_MAP_OUTER_SMAC_H)
    } yield ())

    runOnSimpleTcp(CsrMapper(0, activateProfile0(updatedCsr)), "map SMAC", output => {
      ignore should "fill key16" in {
        output.classifierKeys.key16(Classifier16BitKeys.getConstant(13)) shouldEqual mapped_mac
      }
      ignore should "fill classifierProfile" in {
        output.classifierProfile shouldEqual 0x3
      }
      ignore should "fill ipOption" in {
        output.ipOption(0) shouldBe true
      }
    })
  }
  mapSmac()

  def mapProt(): Unit = {
    val SOURCE_MAP_OUTER_PROT = 2

    val updatedCsr = CsrLenses.execute(csr.ppeMapperMap, for {
      _ <- MapperLenses.mapProtProtLens(0).assign_(6) // map TCP (6)
      _ <- MapperLenses.mapProtMapProtLens(0).assign_(5) // to 101b
      _ <- MapperLenses.mapRewriteSrcId(0, 0).assign_(SOURCE_MAP_OUTER_PROT)
    } yield ())

    runOnSimpleTcp(CsrMapper(0, activateProfile0(updatedCsr)), "map Outer Protocol", output => {
      ignore should "fill key16" in {
        output.classifierKeys.key16(Classifier16BitKeys.getConstant(13)) shouldEqual 5
      }
    })
  }
  mapProt()

  def mapL4Dst(): Unit = {
    val SOURCE_MAP_OUTER_L4_DST_H = 19

    val updatedCsr = CsrLenses.execute(csr.ppeMapperMap, for {
      _ <- MapperLenses.mapL4DstL4Dst(0).assign_(0xA456)
      _ <- MapperLenses.mapL4DstMapProt(0).assign_(0x5)
      _ <- MapperLenses.mapL4DstValid(0).assign_(0x1)
      _ <- MapperLenses.mapL4DstMapL4Dst(0).assign_(0x1234)
    } yield ())

    val evenUpdatedCsr = (0 until 4).foldLeft(updatedCsr){(runningCsr, i) =>
      MapperLenses.mapRewriteSrcId(0, i).set(SOURCE_MAP_OUTER_L4_DST_H - i)(runningCsr)
    }

    runOnSimpleTcp(CsrMapper(0, activateProfile0(evenUpdatedCsr)), "map Outer L4 Destination", output => {
      ignore should "fill key16" in {
        output.classifierKeys.key16(Classifier16BitKeys.getConstant(13)) shouldEqual 0x1234
      }
    })
  }
  mapL4Dst()

  def PriorityProfile(): Unit = {
    val updatedCsr = CsrLenses.execute(csr.ppeMapperMap, for {
      _ <- MapperLenses.mapDomainProfilePriorityProfileLens(0).assign_(0xa)
    } yield ())

    runOnSimpleTcp(CsrMapper(0, activateProfile0(updatedCsr)), "map Priority Profile", output => {
      ignore should "fill priorityProfile" in {
        output.priorityProfile shouldEqual 0xa
      }
      ignore should "set noPriorityEncoding" in {
        output.noPriorityEncoding shouldBe true
      }
    })
  }
  PriorityProfile()

  def noPriEnc(): Unit = {
    val TC_SOURCE_DSCP = 2

    val updatedCsr = CsrLenses.execute(csr.ppeMapperMap, for {
      _ <- MapperLenses.mapDomainAction0DefaultPri(0).assign_(0x1)
      _ <- MapperLenses.mapDomainAction0PriSource(0).assign_(TC_SOURCE_DSCP << 6)
    } yield ())

    runOnSimpleTcp(CsrMapper(0, activateProfile0(updatedCsr)), "set noPriorityEncoding to false", output => {
      it should "set noPriorityEncoding to false" in {
        output.noPriorityEncoding shouldBe false
      }
    })
  }
  noPriEnc()

  def learnMode(): Unit = {
    val updatedCsr = CsrLenses.execute(csr.ppeMapperMap, for {
      _ <- MapperLenses.mapDomainAction0LearnMode(0).assign_(1)
    } yield ())

    runOnSimpleTcp(CsrMapper(0, activateProfile0(updatedCsr)), "learning mode", output => {
      ignore should "set learningMode to IndependentVlanLearning" in {
        output.learningMode shouldEqual IndependentVlanLearning
      }
    })
  }
  learnMode()

  def vlanCounter(): Unit = {
    val updatedCsr = CsrLenses.execute(csr.ppeMapperMap, for {
      _ <- MapperLenses.mapDomainAction1VlanCounter(0).assign_(0x805)
    } yield ())

    runOnSimpleTcp(CsrMapper(0, activateProfile0(updatedCsr)), "ingress VLAN counter", output => {
      ignore should "set ingress VLAN counter" in {
        output.l2IngressVlan1Counter shouldEqual 0x805
      }
    })
  }
  vlanCounter()
}
