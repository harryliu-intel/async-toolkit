package switch_wm
import Implicits._
import SwitchModelImplicits._
import java.io._



object WhiteModelServer {

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
  val portToOs = new collection.mutable.HashMap[Int, DataOutputStream]

  def processEgressInfo(implicit is : DataInputStream, os : DataOutputStream, toGo : Int, hdr : FmModelMessageHdr ) : Unit = {
    //val eih = is.readFmModelMsgSetEgressInfoHdr()
    val switchPort = hdr.Port
    val ei = is.readFmModelSetEgressInfoHdr()
    val tcpPort = ei.Tcpport.toInt & 0xffff
    val stringSize = toGo - 2
    val hostnameArray = Array.ofDim[Byte](stringSize)
    is.readFully(hostnameArray)
    val hostname =  hostnameArray.map(_.toChar).mkString
    println("Configuring port " + tcpPort + " of  " + " as egress to switch port "  + switchPort)
    val socket = new Socket(hostname, tcpPort)

    portToOs(switchPort) = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream()))
  }

  def pushPacket(port: Short, contents : Array[Byte]): Unit = {
    val resultHdr = new FmModelMessageHdr(Msglength = contents.length, Version = 2.shortValue(), Type = FmModelMsgType.Packet, Sw = 0.shortValue(), Port = port)
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
        case _ => { assert(false)}
      }
    }

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

  def runModelServer(fileName : String) = {
    val server = new ServerSocket(0) // 0 means pick any available port

    val port = server.getLocalPort()
    val myaddress = server.getInetAddress.getHostName
    val hostname = InetAddress.getLocalHost().getCanonicalHostName()
    val descText = "0:" + hostname + ":" + port
    println("Scala White Model Server for Madison Bay Switch Chip")
    println("Socket port open at " +  descText)
    println("Write server description file to " + fileName)
    val psFile = new FileWriter(fileName)
    psFile.write(descText)
    psFile.write("\n")
    psFile.close()

    var done = false
    while (!done) {
      val s : Socket = server.accept()
      s.setTcpNoDelay(true)
      println("Accepted new connection:" + s)
      // for some reason, doesn't appear to work if Buffered* is removed from the stack
      // this is suspicious
      implicit val is = new DataInputStream(new BufferedInputStream(s.getInputStream))
      implicit val os = new DataOutputStream(new BufferedOutputStream(s.getOutputStream))

      try {
        while (true) processMessage
      } catch {
        case eof: EOFException => {
          println("Termination of IO from client without shutdown command " + s.getInetAddress().getHostName())
        }
      }
      println("Disconnected.")
      is.close()
      os.flush()
      os.close()
      s.close()
    }
    server.close()
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
        val fullyQualifiedFile = config.serverDir + "/" + config.serverName
        runModelServer( fullyQualifiedFile )
      }
      case None =>  // arguments are bad, error message will have been displayed
    }
  }
}