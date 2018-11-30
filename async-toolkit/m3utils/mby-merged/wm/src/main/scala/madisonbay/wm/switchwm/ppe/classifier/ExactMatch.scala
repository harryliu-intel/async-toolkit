package madisonbay.wm.switchwm.ppe.classifier

import madisonbay.csr.all._
import madisonbay.wm.switchwm.ppe.mapper.output.ClassifierKeys

case class Actions(value: Vector[Int])

final class ExactMatch(
  val lookup: List[em_hash_lookup_r],
  val cgrpEmMap: mby_ppe_cgrp_em_map,
  val shmMap: mby_shm_map,
  val classifierKeys: ClassifierKeys,
  val profile: Byte,
  val group: Byte
) extends Function0[Actions] {

  override def apply: Actions = {
    Actions(Vector.empty)
  }
}
