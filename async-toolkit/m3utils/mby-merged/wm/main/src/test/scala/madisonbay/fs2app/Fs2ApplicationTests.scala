package madisonbay.fs2app

import com.intel.cg.hpfd.csr.testData.AtAddressingExample
import com.intel.cg.hpfd.csr.testData.optics._
import com.intel.cg.hpfd.csr.testData.common._
import com.intel.cg.hpfd.madisonbay.Memory._
import com.intel.cg.hpfd.madisonbay.PrimitiveTypes._
import fs2.Sink

import madisonbay.csr._
import madisonbay.tcp._
import madisonbay.tcp.iosf._
import madisonbay.fs2app.algebra.messages._

import monocle.Optional
import scalaz.{ MonadError, StateT }
import cats.effect.IO

import org.scalatest.compatible.Assertion
import org.scalatest.{FlatSpec, Matchers}

import shapeless.{HNil, ::}

class Fs2ApplicationTests extends FlatSpec
    with Requests
    with Matchers
    with AtAddressingExample {

  val headerDec = ByteArrayDecoder[FmModelMessageHdr]

  val topMap = AddressMap(Address at 0.bytes)
  val optics = AddressMap.genOpticsLookup(topMap, Optional.id)

  def registerReadAssertion(expectedRegisterValue: Long)(implicit me: MonadError[IO,Throwable]): StateT[IO,Array[Byte],Unit] =
    for {
      _      <- headerDec.decode[IO]
      _      <- ByteArrayDecoder[IosfRegCompDataHdr].decode[IO]
      regVal <- ByteArrayDecoder[U64].decode[IO]
      _ = regVal shouldEqual expectedRegisterValue
    } yield ()

  it should "pass EgressSocketInfo request message to publisher stream" in {
    val csrContext = CsrContext(topMap, optics)

    val hostname = "lorem-ipsum"
    val port = 33.toShort

    new IOTestContext[AddressMap] {
      override def payload: Array[Byte] = ByteArrayEncoder[SetEgressInfoRequest].encode(
        SetEgressInfoRequest(port, hostname)
      )
      override def expectedResponseSink: Sink[IO,Byte] =
        _.chunks
          .map(_.toArray)
          .map(_ shouldBe empty)
          .map(_ => ())

      override def publisherSocketCallback(esi: EgressSocketInfo): Assertion =
        esi shouldEqual EgressSocketInfo(hostname, port)

      Fs2Application
        .fs2Program[CsrContext[AddressMap],IO](csrContext)
        .unsafeRunSync
    }
  }

  it should "read correct register value from the given address" in {
    val lens = AddressMap._regFileB composeLens
               RegisterFileB._r composeLens Register._value
    val expectedRegisterValue = 12345L
    val regAddress = topMap.regFileB.r.range.pos
    val csrContext = CsrContext(lens.set(expectedRegisterValue)(topMap), optics)

    new IOTestContext[AddressMap] {
      override def payload: Array[Byte] =
        ByteArrayEncoder[ReadRegisterRequest].encode(ReadRegisterRequest(regAddress))
      override def expectedResponseSink: Sink[IO,Byte] =
        _.chunks
          .map(_.toArray)
          .evalMap(registerReadAssertion(expectedRegisterValue).eval)

      Fs2Application
        .fs2Program[CsrContext[AddressMap],IO](csrContext)
        .unsafeRunSync
    }
  }

  it should "write-block and read sequentially" in {
    val expectedRegisterValue1 = 12345L
    val expectedRegisterValue2 = 67890L
    val initialAddress = topMap.regFileB.range.pos

    val csrContext = CsrContext(topMap, optics)

    new IOTestContext[AddressMap] {

      // send 3 requests and expect 3 responses
      override def payload: Array[Byte] =
        ByteArrayEncoder[
          WriteBlkRegisterRequest :: ReadRegisterRequest :: ReadRegisterRequest :: HNil
        ].encode(
          WriteBlkRegisterRequest(initialAddress, expectedRegisterValue1, expectedRegisterValue2) ::
          ReadRegisterRequest(initialAddress) ::
          ReadRegisterRequest(initialAddress + 8.bytes) ::
          HNil
        )

      override def expectedResponseSink: Sink[IO,Byte] = {
        in => {
          val assertion = for {
            _ <- headerDec.decode[IO]
            _ <- ByteArrayDecoder[IosfRegCompNoData].decode[IO]
            _ <- registerReadAssertion(expectedRegisterValue1)
            _ <- registerReadAssertion(expectedRegisterValue2)
          } yield ()

          val result = in.compile.toList.map(_.toArray).flatMap(assertion.eval)

          in.evalMap(_ => result)
        }
      }

      Fs2Application
        .fs2Program[CsrContext[AddressMap],IO](csrContext)
        .unsafeRunSync
    }
  }

}
