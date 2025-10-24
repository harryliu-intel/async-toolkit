// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.iface

object UriConstants {

  case class UriParameter(key: String, value: Option[String])

  object UriParameter {
    val KeyPath     = "path"
  }

  object UriSegment {
    val Css         = "css"
    val Img         = "img"

    val Address     = "address"

    val App         = "app"

    val RxPpe       = "rx_ppe"
    val Pipe        = "pipe"
    val Parser      = "parser"
    val Programmer  = "programmer"
    val Mapper      = "mapper"
  }

}
