package com.intel.cg.hpfd.madisonbay.wm.utils.extensions

object ExtOption {

  def ifThenOpt[A](condition: Boolean)(action: A): Option[A] = if (condition) { Some(action) } else { None }

}
