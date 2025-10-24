// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.fs2app.http

import fs2.Stream

trait MbyHttpServer[F[_]] {
  def create: Stream[F, Stream[F, Unit]]
}

object MbyHttpServer {
  def apply[F[_]](implicit hp: MbyHttpServer[F]): MbyHttpServer[F] = hp
}
