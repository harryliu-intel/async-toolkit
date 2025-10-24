// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//scalastyle:off
package madisonbay.tcp

// TODO : seems to be unused - to be removed?
object FmSocketType extends Enumeration {

  val Closed = Value(0, "Closed")
  val Tcp = Value(1, "Tcp")
  val Udp = Value(2, "Udp")
  val Pipe = Value(3, "Pipe")
  val Max = Value(4, "Max")
}
