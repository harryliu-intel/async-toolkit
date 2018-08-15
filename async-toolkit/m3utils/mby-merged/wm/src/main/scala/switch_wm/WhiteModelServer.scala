package switch_wm
import com.intel.cg.hpfd.madisonbay.wm.server.dto._
import com.intel.cg.hpfd.madisonbay.wm.server.dto.Implicits._
import java.io._

import com.intel.cg.hpfd.csr._
import com.intel.cg.hpfd.csr.generated._
import com.intel.cg.hpfd.madisonbay.wm.server.dto.PrimitiveTypes.U64

object WhiteModelServer {
  val legacyProtocol = false

  // use "duck-typing" to specify which classes are have certain IOSF characteristics
  // (we do not not provide subclasses or trait definitions in the scheme-based generator)
  type dataSig = { def data0 : Long ; def data1 : Long }
  type addrSig = { def addr0 : Long ; def addr1 : Long; def addr2 : Long  }
  type iosfSig = dataSig with addrSig

  // Rich-Wrappers pattern automatically enables 'addr' and 'data' shorthands when appropriate
  implicit class IosfHasData(i: dataSig) {
    def data : Long = {
      i.data0 | (i.data1 << 32)
    }
  }
  implicit class IosfHasAddress(i: addrSig) {
    def addr : Long = {
      i.addr0 | (i.addr1 << 16) | (i.addr2 << (16 + 12))
    }
  }
  // better to generate these automatically from scheme
  // and put them in a companion object, etc.
  object IosfReadExtractor {
    def unapply(a : Array[Byte]) : Option[IosfRegReadReq] = {
      if (a.size != IosfRegReadReq.LengthBits / 8) None
      else {
        val candidate = IosfRegReadReq(a)
        if (candidate.opcode != IOSF.RegRead) None
        else Some(candidate)
      }
    }
  }
  object IosfBlkWriteExtractor {
    def unapply(a : Array[Byte]) : Option[IosfRegBlkWriteReqHdr] = {
      if (a.size != IosfRegBlkWriteReqHdr.LengthBits / 8) return None
      else {
        val candidate = IosfRegBlkWriteReqHdr(a)
        if (candidate.opcode != IOSF.RegBlkWrite) None
        else Some(candidate)
      }
    }
  }
  object IosfRegWriteExtractor {
    def unapply(a : Array[Byte]) : Option[IosfRegWriteReq] = {
      if (a.size != 24) return None
      val candidate = IosfRegWriteReq(a)
      if (candidate.opcode != IOSF.RegWrite) {
        // println("Rejecting, opcode is " + candidate.opcode)
        // println("Rejecting, byte array is " + a.toList )
        // println("Rejecting data is " + candidate.data.toHexString)
        // println("Rejecting address is " + candidate.address.toHexString)
        // println("Rejecting, operation is: " + candidate)
        None
      }
      else Some(candidate)
    }
  }

  // prototype example! -- obviously this ought to be the 'real' CSR state/white model
  val csrSpace = new scala.collection.mutable.HashMap[Int, Byte]


  def processWriteBlk(iosf : IosfRegBlkWriteReqHdr)(implicit is: DataInputStream, os: DataOutputStream): Unit = {
    val addr = iosf.addr
    println("Processing block write @" + addr.toHexString + " of "  + iosf.ndw + " words")
    val array = Array.ofDim[Byte](iosf.ndw.toInt * 4)
    is.readFully(array)
   array.hexdump
//    + array.toList.map(f => f"$f%x"))
    for(i <- addr until addr + 4 * iosf.ndw) {
        csrSpace.put(i.toInt, array((i - addr).toInt))
    }
    val response = makeResponse(iosf)
    val hdr = FmModelMessageHdr(20, 2.shortValue(), FmModelMsgType.Iosf, 0x0.shortValue, 0.shortValue)
    os.writeFmModelMessageHdr(hdr)
    os.writeIosfRegCompNoData(response)
    os.flush()
    println(" Wrote the response, ok")
  }

  type respondable = { def opcode: Long; def dest : Long;  def source : Long; def tag : Long }

  def makeResponse[T <: respondable] (req: T) : IosfRegCompNoData = {
    require (req.opcode != IOSF.RegRead && req.opcode != IOSF.RegBlkRead)
    new IosfRegCompNoData(
      sai = 1,
      dest = req.source,
      source = req.dest,
      tag = req.tag,
      rsp = 0,
      rsvd0 = 0)
  }

  def makeReadResponse[T <: respondable] (req: T) : IosfRegCompDataHdr = {
    require (req.opcode == IOSF.RegRead || req.opcode == IOSF.RegBlkRead)
    new IosfRegCompDataHdr(
      sai = 1,
      dest = req.source,
      source = req.dest,
      tag = req.tag,
      rsp = 0,
      rsvd0 = 0)
  }


  def processWriteReg(iosf : IosfRegWriteReq)(implicit is: DataInputStream, os: DataOutputStream): Unit = {
    val addr = iosf.addr
    val data = is.readIosfRegBlkData()
    val response = makeResponse(iosf)
    val hdr = FmModelMessageHdr(3 * 4 + 2*4, 2.shortValue(), FmModelMsgType.Iosf, 0x0.shortValue, 0.shortValue)
    os.writeFmModelMessageHdr(hdr)
    os.writeIosfRegCompNoData(response)
    os.flush()
  }

  def processReadReg(iosf : IosfRegReadReq)(implicit is: DataInputStream, os: DataOutputStream): Unit = {
    val addr = iosf.addr
    println("Processing read of 0x" + addr.toHexString)
    val theArray = Array.ofDim[Byte](8)
    (0 until 8).map( x => theArray(x) = csrSpace.getOrElse((addr + x).toInt, 0))
    val msgLength = 3*4 + IosfRegCompDataHdr.LengthBits / 8 + 8 // 12 bytes of ModelMsgHdr, 8 bytes of IOSF header, 8 bytes of data
    os.writeFmModelMessageHdr( FmModelMessageHdr(msgLength, 2.shortValue(), FmModelMsgType.Iosf, 0x0.shortValue, 0.shortValue))
    val response = makeReadResponse(iosf)
    os.writeIosfRegCompDataHdr(response)
    (0 until 8).map(x => os.writeByte(theArray(x)))

    os.flush()
    println("Wrote the response back " + theArray.toIndexedSeq.map(f => f"$f%x"))
  }

  def processIosf(implicit is : DataInputStream, os : DataOutputStream, toGo  : Int) = {
    val array = Array.ofDim[Byte](128 / 8)  // IOSF headers are 16 bytes, except for reg-write, which is 24

    is.readFully(array)
    array match {
      case IosfBlkWriteExtractor(writeReg) => processWriteBlk(writeReg)
      case IosfReadExtractor(readReg) => processReadReg(readReg)
      case _ => {
        val extra = Array.ofDim[Byte](64 / 8)
        is.readFully(extra)
        val expandedArray = array ++ extra
        expandedArray match {
          case IosfRegWriteExtractor(writeReg) => processWriteReg(writeReg)
          case _ => assert(false, "Failed to parse IOSF packet, after trying 192-bit sized regwrite")
        }
      }
    }
  }

  import java.net._
  val egressPortToSocketMap = new collection.mutable.HashMap[Int, Socket]()
  val socketToOs = new collection.mutable.HashMap[Socket, DataOutputStream]
  def portToOs(i : Int) = socketToOs(egressPortToSocketMap(i))


  /** Implement configuration of an egress port
    *
    * In 'new' protocol, a port-machine tuple defines a socket as well as a connection. I.e. if ports 1 and 2 are
    * connected to machine sccj0010 on port 8000 then only a single socket connection is created.
    *
    * After each ingress process is completed being processed, an Eot signal is sent on _all_ opened conections, including
    * the 'original' server socket.
    *
    * The most typical usage of this should be that clients provide their own hostname and the port of the port of their
    * _already opened_ socket (i.e. the IOSF, etc., communication channel). But this approach admits the possibility of
    * communication against several 'clients' at the same time. A client timeout in this model (waiting for an EOT) indicates
    * that an error condition exists.
    *
    * In the legacy protocol, new connections are opened on every egress info signal. No EOTs are sent, the model
    * depends on timing out on the connections to decide there is no more data to receive.
    */
  def processEgressInfo(implicit is : DataInputStream, os : DataOutputStream, toGo : Int, hdr : FmModelMessageHdr) : Unit = {
    //val eih = is.readFmModelMsgSetEgressInfoHdr()
    val switchPort = hdr.Port
    val ei = is.readFmModelSetEgressInfoHdr()
    val tcpPort = ei.Tcpport.toInt & 0xffff
    val stringSize = toGo - 2
    val hostnameArray = Array.ofDim[Byte](stringSize)
    is.readFully(hostnameArray)

    val hostname =  hostnameArray.map(_.toChar).mkString
    val remote = (InetAddress.getByName(hostname), tcpPort)
    println(s"Configuring = $hostname:$tcpPort as egress to switch port $switchPort")

    // first determine whether we already have a connection to the client on this port
    egressPortToSocketMap.get(switchPort) match {
      case Some(s) => {
        s.close()
        egressPortToSocketMap.remove(switchPort)
        assert(false, "Reassinging an egress port not tested functionality")
      }
      case None => { }
    }

    def matchingSocket(s: Socket) = {
      (s.getInetAddress.getCanonicalHostName == remote._1.getCanonicalHostName) && (s.getPort == remote._2)
    }
    def alwaysNewSocket(s : Socket) = false
    val newSocketFunc : Socket => Boolean = if (legacyProtocol) { alwaysNewSocket } else { matchingSocket }

    egressPortToSocketMap(switchPort) = egressPortToSocketMap.values.filter(newSocketFunc).headOption match {
      case Some(s) => {
        println(" * Socket connection already exists!")
        s
      }
      case None => {
        val s = new Socket(hostname, tcpPort)
        println(s" * Allocating new socket connection to $hostname:$tcpPort")
        val os = new DataOutputStream(new BufferedOutputStream(s.getOutputStream()))
        socketToOs(s) = os
        s
      }
    }
  }

  def pushEot(os : DataOutputStream): Unit = {
    val resultHdr = new FmModelMessageHdr(
      // outer header + inner header (type + integer) + size of contents
      Msglength = 12 + 2,
      Version = 2.shortValue(), Type = FmModelMsgType.PacketEot, Sw = 0.shortValue(), Port = 0.shortValue)
    os.writeFmModelMessageHdr(resultHdr)
    os.writeFmModelMsgPacketEot(FmModelMsgPacketEot(0.shortValue))
    os.flush()
  }

  def pushPacket(port: Short, contents : Array[Byte]): Unit = {
    val resultHdr = new FmModelMessageHdr(
      // outer header + inner header (type + integer) + size of contents
      Msglength = 12 + (FmModelDataType.Length + 4) + contents.length,
      Version = 2.shortValue(), Type = FmModelMsgType.Packet, Sw = 0.shortValue(), Port = port)
    val os = portToOs(port)

    os.writeFmModelMessageHdr(resultHdr)
    os.writeFmModelDataType(FmModelDataType.Packet)
    os.writeInt(contents.length)
    os.write(contents)
    os.flush()

  }

  def processPacket(implicit is : DataInputStream, toGo : Int, hdr : FmModelMessageHdr ) : Unit = {
    def extractFragment() : (FmModelDataType.Value, Array[Byte]) = {
       val t = is.readFmModelDataType()
      val len = is.readInt()
      val contents = Array.ofDim[Byte](len)
      is.readFully(contents)
      t match {
        case FmModelDataType.PacketMeta => { }
        case FmModelDataType.SbId => { assert(false, "SbID not supported")}
        case FmModelDataType.SbTc => { assert(false, " SbTc not supported")}
        case FmModelDataType.Packet => {}
      }
      (t, contents)
    }
    var toParse = hdr.Msglength - 12
    while(toParse > 0) {
      val thisFrag = extractFragment()
        toParse -= (thisFrag._2.length + 4 + FmModelDataType.Length)
      println("Got fragment. To go in this frame " + toParse)
      thisFrag._2.hexdump

      thisFrag match {
        case (FmModelDataType.Packet, contents) => {
          println("Reflecting back packet of size: " + contents.length)
          pushPacket(hdr.Port, contents)
        }
        case (FmModelDataType.PacketMeta, contents) => {
          println("Ignoring " + contents.length + " bytes of meta data")
        }
        case _ => { assert(false, "Unmatched packet fragment")}
      }
    }
    //done 'reflecting', signal we are done with this packet on all ports (transmission size at 0, not sure
    // what the point of this is)
    if (!legacyProtocol) for (os <- socketToOs.values) pushEot(os)
  }

  def processCommandQuit() = {
    println("Received quit operation!")
  }

  def processMessage(implicit is : DataInputStream, os : DataOutputStream) = {

    implicit val hdr : FmModelMessageHdr = is.readFmModelMessageHdr()

    println ("Processing message with hdr" + hdr)
    implicit val toGo = hdr.Msglength - 12

    hdr.Type match  {
      case FmModelMsgType.Packet => processPacket
      case FmModelMsgType.Mgmt => Unit
      case FmModelMsgType.Iosf => processIosf
      case FmModelMsgType.SetEgressInfo => processEgressInfo
      case FmModelMsgType.CommandQuit => assert(false)
      case _ =>
    }
  }

  def runModelServer(file : File) = {
    val server = new ServerSocket(0) // 0 means pick any available port

    val port = server.getLocalPort()
    val myaddress = server.getInetAddress.getHostName
    val hostname = InetAddress.getLocalHost().getCanonicalHostName()
    val descText = "0:" + hostname + ":" + port
    checkoutCsr
    println("Scala White Model Server for Madison Bay Switch Chip")
    println("Socket port open at " +  descText)
    println("Write server description file to " + file)
    val psFile = new FileWriter(file)
    psFile.write(descText)
    psFile.write("\n")
    psFile.close()
    file.deleteOnExit()

    var done = false
    while (!done) {
      val s : Socket = server.accept()
      s.setTcpNoDelay(true)
      println("Accepted new connection:" + s)
      egressPortToSocketMap(-1) = s
      // for some reason, doesn't appear to work if Buffered* is removed from the stack
      // this is suspicious
      implicit val is = new DataInputStream(new BufferedInputStream(s.getInputStream))
      implicit val os = new DataOutputStream(new BufferedOutputStream(s.getOutputStream))
      socketToOs(s) = os

      try {
        while (true) processMessage
      } catch {
        case eof: EOFException => {
          println("Termination of IO from client without shutdown command " + s.getInetAddress().getHostName())
        }
      }
      println("Disconnected.")
      // cleanup the socket, and remove any egress port mappings to that socket
      is.close()
      os.flush()
      os.close()
      s.close()
      socketToOs.remove(s)
      egressPortToSocketMap.retain((x, mapsock) => mapsock != s)
    }
    server.close()
  }

  def checkoutCsr : Unit = {
    val startCreationTime = System.currentTimeMillis()
    val theCsr = mby_top_map()
    val doneCreationTime = System.currentTimeMillis()
    val elapsedTime = doneCreationTime - startCreationTime
    val x = theCsr.shm(0).FWD_TABLE0(0).FWD_TABLE0(1).DATA


    // a, b, c are the same field:
    val a = theCsr.mpt(0).rx_ppe(0).policers(0).POL_CFG(0).POL_CFG(1).CREDIT_FRAME_ERR
    /// rx_ppe is not an array, so it can be referenced through transparently
    val b = theCsr.mpt(0).rx_ppe.policers(0).POL_CFG(0).POL_CFG(1).CREDIT_FRAME_ERR
    /// pol_cfg_rf is a 'degenerate' level of hierarchy, so it can be referenced through directly (see how it just looks like another array dinemsion
    val c : Long = theCsr.mpt(0).rx_ppe.policers.POL_CFG(0)(1).CREDIT_FRAME_ERR

    val test = theCsr.mpt(0).rx_ppe.policers
    println("theCsr.mpt(0).rx_ppe.policers path is " + test.path)
    theCsr.mpt(0).rx_ppe.cm_apply(0).CM_APPLY_SOFTDROP_STATE(0).USAGE_OVER_LIMIT() = 1
    theCsr.mpt(0).rx_ppe.cm_apply(0).CM_APPLY_SOFTDROP_STATE(0).OVER_LIMIT() = 1



    //
    implicit class csum_convenience (val x : mby_ppe_modify_map) {
       def ipv4_hdr = List(x.MOD_CSUM_CFG1.IPV4_0, x.MOD_CSUM_CFG1.IPV4_1, x.MOD_CSUM_CFG1.IPV4_2)
       def ipv6_hdr = List(x.MOD_CSUM_CFG1.IPV6_1, x.MOD_CSUM_CFG1.IPV6_2, x.MOD_CSUM_CFG1.IPV6_2)
    }
    implicit class profile_group_abstraction(val x : mod_profile_group_r) {
      def get_group(i : Int) : mod_profile_group_r#HardwareWritable = {
        i match {
          case 1 => x.GROUP_1
          case 2 => x.GROUP_2
          case 3 => x.GROUP_3
          case 4 => x.GROUP_4
          case 5 => x.GROUP_5
          case 6 => x.GROUP_6
          case 7 => x.GROUP_7
        }
      }
    }
    // now, can reference these guys more abstractly
    val ipv4_h = theCsr.mpt(0).tx_ppe.modify(0).ipv4_hdr
    // or
    val group2 = theCsr.mpt(0).tx_ppe.modify(0).MOD_PROFILE_GROUP(0).get_group(1).assign(4)
    //
    val outputshift = theCsr.mpt(0).tx_ppe.modify(0).MOD_MAP_CFG(1).OUTPUT_SHIFT.reset()
    val memoryMap = theCsr.addressRegisterMap(0)
    //outputshift assign 1


    println("Reseting fields that are resetable")
    val resetStartTime = System.currentTimeMillis()
    var resetCount = 0
    def countFieldsReset(r : RdlRegister[U64]#HardwareResetable) = { r.reset() ; resetCount += 1}
    theCsr.foreachResetableField(countFieldsReset(_) )
    val resetDoneTime = System.currentTimeMillis()
    val resetElapsedTime = resetDoneTime - resetStartTime
    println(" Reset took " + resetElapsedTime + "ms and hit " + resetCount + " fields!")

  }

  def main(args : Array[String]) : Unit = {
    case class Config(serverDir : File = new File("."), serverName : File = new File("models.packetServer"))
    val parser = new scopt.OptionParser[Config]("whitemodel") {
      head("whitemodel", "0.1")
      opt[File]("ip").valueName("<file>").action { (x, c) =>
        c.copy(serverDir = x) } text("the directory of the generated network port file")
      opt[File]("if").valueName("<file>").action { (x, c) =>
        c.copy(serverName = x) } text("the name of the generated network port file")
    }
    parser.parse(args, Config()) match {
      case Some(config) => {
        val fullyQualifiedFile = new File(config.serverDir + "/" + config.serverName)
        runModelServer( fullyQualifiedFile )
      }
      case None =>  // arguments are bad, error message will have been displayed
    }
  }
}
