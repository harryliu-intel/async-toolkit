// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.fs2app.http

import fs2.{RaiseThrowable, Stream}
import spinoco.fs2.http.HttpResponse
import spinoco.protocol.http._

trait UriDispatcher[F[_]] {
  def processRequest(request: HttpRequestHeader, body: Stream[F,Byte])(implicit rt: RaiseThrowable[F]): HttpResponse[F]
}

object UriDispatcher {
  def apply[F[_]](implicit ud: UriDispatcher[F]): UriDispatcher[F] = ud
}
