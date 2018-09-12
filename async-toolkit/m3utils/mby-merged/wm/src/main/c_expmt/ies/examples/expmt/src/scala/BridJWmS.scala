import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import HlpWm._
import org.bridj.BridJ
import org.bridj.Pointer
import util.control.Breaks._
import HlpWm.WmLibrary.msg_direction_t

object BridJWmS {

  def ntohs(in: Short): Short = {
    val buf = ByteBuffer.allocate(2) // short = 2 bytes
    buf.order(ByteOrder.BIG_ENDIAN) // network side
    buf.clear()
    buf.putShort(0, in)
    buf.order(ByteOrder.nativeOrder()) // host side (LITTLE_ENDIAN on x86)
    val out = buf.getShort(0)
    out
  }

  def ntohl(in: Integer): Integer = {
    val buf = ByteBuffer.allocate(4) // long = 4 bytes
    buf.order(ByteOrder.BIG_ENDIAN) // network side
    buf.clear()
    buf.putInt(0, in)
    buf.order(ByteOrder.nativeOrder()) // host side (LITTLE_ENDIAN on x86)
    val out = buf.getInt(0)
    out
  }

  def printVersionTag: Unit = {
    val pb: Pointer[java.lang.Byte] = WmLibrary.fmModelGetVersionTag()
    val version: String = pb.getCString()

    println(s"======================================")
    println(s"-- fmModelVersionTag = $version")
    println(s"======================================")
  }

  def openMsgInject(dir: msg_direction_t, msgFile: String): Unit = {
    val msgFilePtr = Pointer.pointerToCString(msgFile);
    var status: Integer = WmLibrary.FM_OK;
    status = WmLibrary.openMsgInject(dir, msgFilePtr)
    if (status == WmLibrary.FM_OK)
      println(s"Opened file \'$msgFile\' for reading.")
    else
      println(s"ERROR: could not open file \'$msgFile\' for reading!")
  }

  def initWM: Unit = {
    var status: Integer = WmLibrary.FM_OK;
    val msgEchoMode: Byte = WmLibrary.FALSE
    val msgDumpMode: Byte = WmLibrary.FALSE
    val msgInjectMode: Byte = WmLibrary.TRUE

    WmLibrary.setMsgEchoMode(msgEchoMode)
    WmLibrary.setMsgDumpMode(msgDumpMode)
    WmLibrary.setMsgInjectMode(msgInjectMode)

    val sw = 0
    val libCfg = new fm_libCfg()
    val libCfgPtr = Pointer.getPointer(libCfg)
    status = WmLibrary.fmModelLibInit(sw, libCfgPtr)
    if (status != WmLibrary.FM_OK)
      println(s"FAILED to init model lib: err = $status")

    status = WmLibrary.fmModelReset(sw)
    if (status != WmLibrary.FM_OK)
      println(s"FAILED to reset chip: err = $status")

    val m = WmLibrary.UNDEF_VAL
    val alBitmaskPtr = Pointer.pointerToInts(m, m, m, m, m, m, m, m)
    val nvmImgFilePtr = Pointer.pointerToCString(WmLibrary.FM_NVM_IMG_FILE_NAME)
    status = WmLibrary.loadImg(nvmImgFilePtr, alBitmaskPtr)
    if (status != WmLibrary.FM_OK)
      println(s"FAILED to load NVM image: err = $status")

    /*    val parserCfgFilePtr = Pointer.pointerToCString("")
    status = WmLibrary.loadParserCfg(parserCfgFilePtr)
    if (status != WmLibrary.FM_OK)
      println(s"FAILED to load parser config: err = $status")
    */
    WmLibrary.initPhysicalPorts();
  }

  def processMessages: Unit = {
    val sendIntr: Byte = WmLibrary.TRUE
    var status: Integer = WmLibrary.FM_OK;
    val msgNumClients = 1;
    val numSockets = 2 + msgNumClients;

    val arrayLength = WmLibrary.MAX_PERSISTENT_CONNECTIONS + 1;
    val socketsPtrPtr = Pointer.allocatePointers(classOf[fm_socket], arrayLength);

    val it = socketsPtrPtr.iterator()
    while (it.hasNext()) {
      it.next()
      it.set(null)
    }

    breakable {
      while (true) {
        if (sendIntr == WmLibrary.TRUE)
          WmLibrary.handleIntr(numSockets, socketsPtrPtr)

        val iMsg = new fm_modelMessage()
        val iMsgPtr = Pointer.getPointer(iMsg)

        status = WmLibrary.readMsgInject(WmLibrary.msg_direction_t.INGRESS, iMsgPtr) // inject ingress message from file
        if (status != WmLibrary.FM_OK) break

        val socketType = ntohs(iMsg.`type`()) // type is a scala keyword
        val serverPort = ntohs(iMsg.port())
        val msgLength = ntohl(iMsg.msgLength())
        val socketTypeEnum = WmLibrary.fm_socketType.fromValue(socketType)

        val socket = new fm_socket() // fake socket
        socket.sock(-1)
        socket.`type`(socketTypeEnum)
        socket.serverPort(serverPort)

        val socketPtr = Pointer.getPointer(socket)
        status = WmLibrary.ProcessMessage(socketPtr, iMsgPtr, msgLength)
        if (status != WmLibrary.FM_OK) break
      }
    }
  }

  def configureMsg(testName: String): Unit = {
    //val testName = "dat/AclApiFunctions_hlp-model"

    val iMsgFile = testName + ".imsg";
    val ingress = WmLibrary.msg_direction_t.INGRESS;
    openMsgInject(ingress, iMsgFile)

    val eMsgFile = testName + ".emsg";
    val egress = WmLibrary.msg_direction_t.EGRESS
    openMsgInject(egress, eMsgFile)
  }

  def closeMsg: Unit = {
    var status: Integer = WmLibrary.FM_OK;
    status = WmLibrary.closeMsgInject(WmLibrary.msg_direction_t.INGRESS)
    if (status != WmLibrary.FM_OK)
      println(s"FAILED to close INGRESS Msg Inject")

    status = WmLibrary.closeMsgInject(WmLibrary.msg_direction_t.EGRESS)
    if (status != WmLibrary.FM_OK)
      println(s"FAILED to close INGRESS Msg Inject")
  }

  def showResults: Unit = {
    // Get egress message match counts:
    val eMsgMismatchCount = WmLibrary.getMsgMismatchCount()
    val eMsgMatchCount = WmLibrary.getMsgMatchCount()
    val eMsgTotalCount = eMsgMismatchCount + eMsgMatchCount

    val status = if (eMsgMismatchCount == 0) WmLibrary.FM_OK else WmLibrary.FM_FAIL
    val checkStatus = if (status == WmLibrary.FM_OK) "passed." else "FAILED!"

    println(
      f"""Check egress messages: $eMsgMismatchCount%10d mismatch / $eMsgMatchCount%10d match / $eMsgTotalCount%10d total :  test $checkStatus%s""")
  }

  def main(args: Array[String]): Unit = {
    //val testName = "dat/AclApiFunctions_hlp-model"

    val testName = if (args.length == 0) "dat/VlanTagging_hlp-model_999159022" else args(0)

    printVersionTag

    initWM

    configureMsg(testName)

    processMessages

    closeMsg

    showResults
  }
}

