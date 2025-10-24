// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay
package fs2app
package algebra

import fs2.Stream
import fs2.io.tcp.Socket
import messages.EgressSocketInfo

trait PublisherSocket[F[_]] {
  def create(egressSocketInfo: EgressSocketInfo): Stream[F,Socket[F]]
}

object PublisherSocket {
  def apply[F[_]](implicit ps: PublisherSocket[F]): PublisherSocket[F] = ps
}

