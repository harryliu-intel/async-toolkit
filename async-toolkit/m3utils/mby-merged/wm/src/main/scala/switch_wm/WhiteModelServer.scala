package switch_wm
import Implicits._
import java.io._
object WhiteModelServer {

  type dataSig = { def data0 : Long ; def data1 : Long }
  type addrSig = { def addr0 : Long ; def addr1 : Long; def addr2 : Long  }
  type iosfSig = dataSig with addrSig


  def getData[T <: dataSig ] (t: T): Long = {
    t.data0 | (t.data1 << 32)
  }
  def getAddress[T <: addrSig] (t: T): Long = {
    t.addr0 | (t.addr1 << 16) | (t.addr2 << (16 + 12))
  }

  class IosfGeneric[T <: iosfSig] (t : T) {
    def address = getAddress(t)
    def data = getData(t)
  }
  implicit def toIosfGeneric[T <: iosfSig](t:T) : IosfGeneric[T] = new IosfGeneric(t)


  // better to generate these automatically from scheme
  // and put them in a companion object, etc.
  object IosfReadExtractor {
    def unapply(a : Array[Byte]) : Option[IosfRegReadReq] = {
      val candidate = IosfRegReadReq(a)
      if (candidate.opcode != IOSF.RegRead) None
      else Some(candidate)
    }
  }
  object IosfBlkWriteExtractor {
    def unapply(a : Array[Byte]) : Option[IosfRegBlkWriteReqHdr] = {
      if (a.size != 16) return None
      val candidate = IosfRegBlkWriteReqHdr(a)
      if (candidate.opcode != IOSF.RegBlkWrite) None
      else Some(candidate)
    }
  }
  object IosfRegWriteExtractor {
    def unapply(a : Array[Byte]) : Option[IosfRegWriteReq] = {
      if (a.size != 24) return None
      val candidate = IosfRegWriteReq(a)
      if (candidate.opcode != IOSF.RegWrite) {
        println("Rejecting, opcode is " + candidate.opcode)
        println("Rejecting, byte array is " + a.toList )
        println("Rejecting data is " + candidate.data.toHexString)
        println("Rejecting address is " + candidate.address.toHexString)

        println("Rejecting, operation is: " + candidate)

        None
      }
      else Some(candidate)
    }
  }

  val csrSpace = new scala.collection.mutable.HashMap[Int, Byte]


  def processWriteBlk(iosf : IosfRegBlkWriteReqHdr)(implicit is: DataInputStream, os: DataOutputStream): Unit = {
    val addr = getAddress(iosf)
    println("Processing block write @" + addr.toHexString + " of "  + iosf.ndw + " words")
    val array = Array.ofDim[Byte](iosf.ndw.toInt * 4)
    is.readFully(array)
    println(" Data is: " + array.toList.map(f => f"$f%x"))
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
    val addr = iosf.addr1 << 16 | iosf.addr0
    val data = is.readIosfRegBlkData()
    val response = makeResponse(iosf)
    val hdr = FmModelMessageHdr(3 * 4 + 2*4, 2.shortValue(), FmModelMsgType.Iosf, 0x0.shortValue, 0.shortValue)
    os.writeFmModelMessageHdr(hdr)
    os.writeIosfRegCompNoData(response)
    os.flush()
  }

  def processReadReg(iosf : IosfRegReadReq)(implicit is: DataInputStream, os: DataOutputStream): Unit = {
    val addr = getAddress(iosf)
    println("Processing read of 0x" + addr.toHexString)
    val data = csrSpace.getOrElse(addr.toInt, 0xdeadbeef.toByte)
    //val array = Array.ofDim[Byte](2 * 4)
    //array(0) = data
    val theArray = Array.ofDim[Byte](8)
    (0 until 8).map( x => theArray(x) = csrSpace.getOrElse((addr + x).toInt, 0))
    os.writeFmModelMessageHdr( FmModelMessageHdr(3 * 4 + 2 * 4 + 2 * 4, 2.shortValue(), FmModelMsgType.Iosf, 0x0.shortValue, 0.shortValue))
    val response = makeReadResponse(iosf)
    os.writeIosfRegCompDataHdr(response)
    (0 until 8).map(x => os.writeByte(theArray(x)))

    os.flush()
    println("Wrote the response back " + theArray.toIndexedSeq.map(f => f"$f%x"))
  }

  def processIosf(implicit is : DataInputStream, os : DataOutputStream, toGo  : Int) = {
    val array = Array.ofDim[Byte](128 / 8)

    is.readFully(array)
    array match {
      case IosfBlkWriteExtractor(writeReg) => processWriteBlk(writeReg)
      case IosfReadExtractor(readReg) => processReadReg(readReg)
      case _ => {
        // todo -- pull in the other 64 bits
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

  def processCommandQuit() = {
    println("Received quit operation!")
  }

  def processMessage(implicit is : DataInputStream, os : DataOutputStream) = {

    val hdr : FmModelMessageHdr = is.readFmModelMessageHdr()

    println ("Processing message with hdr" + hdr)
    implicit val toGo = hdr.Msglength - 12

    hdr.Type match  {
      case FmModelMsgType.Packet => Unit
      case FmModelMsgType.Mgmt => Unit
      case FmModelMsgType.Iosf => processIosf
      case FmModelMsgType.CommandQuit => assert(false)
      case _ =>
    }
  }

  import java.net._
  def main(args : Array[String]) : Unit = {
    println("hello world")
    val server = new ServerSocket(0) // 0 means pick any available port


    val port = server.getLocalPort()
    val myaddress = server.getInetAddress.getHostName
    val hostname = InetAddress.getLocalHost().getHostAddress()
    val descText = "0:" + hostname + ":" + port
    println("Scala White Model Server for Madison Bay Switch Chip")
    println("Socket port open at:" +  descText)
    val psFile = new FileWriter("models.packetServer")
    psFile.write(descText)
    psFile.write("\n")
    psFile.close()

    var done = false
    while (!done) {
      val s : Socket = server.accept()
      s.setTcpNoDelay(true)
      println("Accepted new connection:" + s)
      implicit val is = new DataInputStream(new BufferedInputStream(s.getInputStream))
      implicit val os = new DataOutputStream(new BufferedOutputStream(s.getOutputStream))

      try {
        while (true) processMessage
      } catch {
        case eof: EOFException => {
          println("Termination of IO from client without shutdown command" + s.getInetAddress().getHostName())
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
}
