// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay
package fs2app
package algebra

import fs2.Stream
import fs2.io.tcp.Socket

trait ServerSocket[F[_]] {
  def create: Stream[F,Socket[F]]
}

object ServerSocket {
  def apply[F[_]](implicit ss: ServerSocket[F]): ServerSocket[F] = ss
}
