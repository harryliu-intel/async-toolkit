// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.iface.rest

import madisonbay.iface.model.CsrModel
import spinoco.protocol.http.HttpStatusCode

case class RestResponse(
                         uriSupported: Boolean,
                         error: Boolean,
                         response: String,
                         httpStatusCode: HttpStatusCode,
                         updatedCsr: Option[CsrModel]
                       )

