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
  object IosfBlkWriteExtractor {
    def unapply(a : Array[Byte]) : Option[IosfRegBlkWriteReqHdr] = {
      if (a.size != 16) return None
      val candidate = IosfRegBlkWriteReqHdr(a)
      if (candidate.opcode != IOSF.RegWrite) None
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

  def processWriteBlk(iosf : IosfRegBlkWriteReqHdr): Unit = {

  }

  def makeResponse(req: IosfRegWriteReq) : IosfRegCompNoData =
    new IosfRegCompNoData(
      sai = 1,
      dest = req.source,
      source = req.dest,
      tag = req.tag,
      rsp = 0,
      rsvd0 = 0)


  def processWriteReg(iosf : IosfRegWriteReq)(implicit is: DataInputStream, os: DataOutputStream): Unit = {
    val addr = iosf.addr1 << 16 | iosf.addr0
    val data = is.readIosfRegBlkData()
    val response = makeResponse(iosf)
    val hdr = FmModelMessageHdr(20, 2.shortValue(), FmModelMsgType.Iosf, 0x0.shortValue, 0.shortValue)
    os.writeFmModelMessageHdr(hdr)
    os.writeIosfRegCompNoData(response)

  }

  def processIosf(implicit is : DataInputStream, os : DataOutputStream, toGo  : Int) = {
    val array = Array.ofDim[Byte](toGo)

    is.readFully(array)
    array match {
      case IosfBlkWriteExtractor(writeReg) => processWriteBlk(writeReg)
      case IosfRegWriteExtractor(writeReg) => processWriteReg(writeReg)

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
    val server = new ServerSocket(15000)


    val port = server.getLocalPort()
    val myaddress = server.getInetAddress
    val descText = "0:localhost:" + port
    println("Socket port open at:" +  descText)
    val psFile = new FileWriter("models.packetServer")
    psFile.write(descText)
    psFile.write("\n")
    psFile.close()

    val s = server.accept()
    println("Accepted connection:" + s)
    implicit val is = new DataInputStream(s.getInputStream)
    implicit val os = new DataOutputStream(s.getOutputStream)
    while (true) processMessage
    os.flush
    os.close

  }
}
