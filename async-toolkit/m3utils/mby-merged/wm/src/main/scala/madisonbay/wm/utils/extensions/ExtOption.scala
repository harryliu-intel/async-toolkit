// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.wm.utils.extensions

object ExtOption {

  def ifThenOpt[A](condition: Boolean)(action: A): Option[A] = if (condition) { Some(action) } else { None }

}
