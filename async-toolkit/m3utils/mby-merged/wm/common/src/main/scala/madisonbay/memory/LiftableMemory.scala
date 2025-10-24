// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.memory

import scala.reflect.api.Universe
import scala.reflect.macros.blackbox
import scala.reflect.runtime.{universe => runtimeUniverse}


/** Lifting and unlifting memory-related types. */
trait LiftableMemoryImpl {
  val universe: Universe
  import universe._

  lazy val asMemoryUnitsSym: TypeSymbol = symbolOf[asMemoryUnits]
  lazy val BitsCom = symbolOf[Bits].companion
  lazy val BytesCom = symbolOf[Bytes].companion

  implicit lazy val liftBits = Liftable[Bits] { b =>
    q"(new $asMemoryUnitsSym(${b.toLong})).bits"
  }
  implicit lazy val unliftBits = Unliftable[Bits] {
    case q"${value: Long}.bits" => value.bits
    case q"${value: Int}.bits" => value.toLong.bits
    case q"$sym(${value: Long})" if sym == BitsCom => value.bits
    case q"$sym(${value: Int})" if sym == BitsCom => value.toLong.bits
  }

  implicit lazy val liftBytes = Liftable[Bytes] { b =>
    q"(new $asMemoryUnitsSym(${b.value})).bytes"
  }
  implicit lazy val unliftBytes = Unliftable[Bytes] {
    case q"${value: Long}.bytes" => value.bytes
    case q"${value: Int}.bytes" => value.toLong.bytes
    case q"$sym(${value: Long})" if sym == BytesCom => value.bytes
    case q"$sym(${value: Int})" if sym == BytesCom => value.toLong.bytes
  }

  implicit lazy val unliftMemoryUnit = Unliftable[MemoryUnit] {
    case q"${bits: Bits}" => bits
    case q"${bytes: Bytes}" => bytes
  }
  implicit lazy val unliftFullBytes = Unliftable[FullBytes] {
    case q"${bytes: Bytes}" => bytes
  }

  lazy val AlignmentCom = symbolOf[Alignment].companion
  implicit lazy val liftAlignment = Liftable[Alignment] { a =>
    q"$AlignmentCom(${a.toBytes})"
  }
  implicit lazy val unliftAlignment = Unliftable[Alignment] {
    case q"$sym(${fbytes: FullBytes})" if sym == AlignmentCom => Alignment(fbytes.toBytes)
    case q"${fbytes: FullBytes}.toAlignment" => fbytes.toAlignment
  }
}

object RuntimeLiftableMemory extends LiftableMemoryImpl {
  type U = runtimeUniverse.type
  val universe: U = runtimeUniverse
}
trait LiftableMemory extends LiftableMemoryImpl {
  val c: blackbox.Context  // whitebox's one is blackbox's one's subtype
  type U = c.universe.type
  val universe: U = c.universe
}
