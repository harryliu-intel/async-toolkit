package com.intel.cg.hpfd.madisonbay

import org.scalacheck.{Arbitrary, Gen}


sealed trait Trilean
case object True extends Trilean
case object False extends Trilean
case object Unknown extends Trilean

object Trilean {
  val tri2long: PartialFunction[Trilean, Long] = {
    case False => 0L
    case True => 1L
    case Unknown => 2L
  }

  val long2tri: PartialFunction[Long, Trilean] = {
    case 0L => False
    case 1L => True
    case 2L => Unknown
  }

  implicit val encodeTrilean: Encode[Trilean] = Encode.simple[Trilean](2)(tri2long)(long2tri)

  val genTrilean: Gen[Trilean] = Gen.choose(0, 2).map {
    case 0L => False
    case 1L => True
    case 2L => Unknown
  }

  implicit lazy val arbTrilean: Arbitrary[Trilean] = Arbitrary(genTrilean)
}
