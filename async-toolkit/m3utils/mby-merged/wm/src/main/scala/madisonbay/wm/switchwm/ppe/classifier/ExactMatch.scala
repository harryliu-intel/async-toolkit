package madisonbay.wm.switchwm.ppe.classifier

import madisonbay.wm.utils.extensions.UIntegers._
import madisonbay.csr.all._
import madisonbay.tcp.ByteArrayEncoder // TODO: tcp attacks pipeline logic xD
import madisonbay.wm.switchwm.ppe.mapper.output.ClassifierKeys
import madisonbay.wm.utils.defs.FlexibleConstantContainer
import madisonbay.BitVector
import shapeless.tag
import shapeless.tag.@@

object types {
  trait ProfileTag
  trait HashNumTag

  type Profile = Byte @@ ProfileTag
  type HashNum = Int @@ HashNumTag
}
import types._

object ExactMatchMode extends Enumeration {
  type ExactMatchMode = Value

  val Split = Value(0) // MBY_CGRP_HASH_ENTRY_MODE_32B
  val NonSplit = Value(1) // MBY_CGRP_HASH_ENTRY_MODE_64B
}
object ClassifierGroup extends Enumeration {
  type ClassifierGroup = Value

  val GroupA = Value(0)
  val GroupB = Value(1)
}
case class EmHash(idx: Short, more: Short)
case class Actions(value: Vector[Int])
case class KeyMaskConfig(
  keyMaskSel: em_key_sel1_r.KEY_MASK_SEL,
  key32Mask: em_key_sel1_r.KEY32_MASK,
  key16Mask: em_key_sel1_r.KEY16_MASK,
  key8Mask: em_key_sel0_r.KEY8_MASK
)
object KeyMaskConfig {
  def apply(map: mby_ppe_cgrp_em_map, hashNum: HashNum, profile: Profile): KeyMaskConfig = {
    val emKeySel1 = map.KEY_SEL1(hashNum).KEY_SEL1(profile)
    KeyMaskConfig(
      emKeySel1.KEY_MASK_SEL,
      emKeySel1.KEY32_MASK,
      emKeySel1.KEY16_MASK,
      map.KEY_SEL0(hashNum).KEY_SEL0(profile).KEY8_MASK
    )
  }
}

final class ExactMatch(
  val lookup: List[em_hash_lookup_r],
  val cgrpEmMap: mby_ppe_cgrp_em_map,
  val shmMap: mby_shm_map,
  val classifierKeys: ClassifierKeys,
  val profile: Profile,
  val group: ClassifierGroup.ClassifierGroup
) extends Function0[Actions] {

  private def maskClassifierKeys(keyMaskConfig: KeyMaskConfig): ClassifierKeys = {
    val key16Mask = BitVector(keyMaskConfig.key16Mask())
    val key32Mask = BitVector(keyMaskConfig.key32Mask())
    val key8Mask = BitVector(keyMaskConfig.key8Mask())

    ClassifierKeys(
      classifierKeys.key32.map {
        case elem @ (key32, _) if key32Mask.extract[Boolean](key32.index) => elem
        case (key32, _) => (key32, 0)
      },
      classifierKeys.key16.map {
        case elem @ (key16, _) if key16Mask.extract[Boolean](key16.index) => elem
        case (key16, _) => (key16, 0.toShort)
      },
      classifierKeys.key8.zipWithIndex.map {
        case (key8, idx) if key8Mask.extract[Boolean](idx) => key8
        case _ => 0.toByte
      }
    )
  }

  // TODO: I hope it works well... no tests for now :/
  private def performKeyCompaction(cKeys: ClassifierKeys, keyMaskConfig: KeyMaskConfig): Array[Byte] = {
    // TODO: From where can I get those values?
    val MBY_CGRP_KEY8 = 64
    val MBY_CGRP_KEY16 = 32
    val MBY_CGRP_KEY32 = 16
    val MBY_CGRP_HASH_KEYS = (MBY_CGRP_KEY8 + MBY_CGRP_KEY16*2 + MBY_CGRP_KEY32*4)

    val key16Mask = BitVector(keyMaskConfig.key16Mask())
    val key32Mask = BitVector(keyMaskConfig.key32Mask())
    val key8Mask = BitVector(keyMaskConfig.key8Mask())

    def collectKeysToByteArray[K <: FlexibleConstantContainer[Int]#Element, V : ByteArrayEncoder](
      map: Map[K,V], mask: BitVector
    ): Array[Byte] = map.filterKeys(key => mask.extract[Boolean](key.index))
        .values.map(ByteArrayEncoder[V].encode)
        .map(_.reverse)
        .reduce(_ ++ _)

    val key32Arr = collectKeysToByteArray(cKeys.key32, key32Mask)
    val key16Arr = collectKeysToByteArray(cKeys.key16, key16Mask)
    val key8Arr = cKeys.key8.zipWithIndex.collect {
      case (key, idx) if key8Mask.extract[Boolean](idx) => key
    }.toArray

    // TODO: hmmm... '.take' ...
    // TODO: 'just to make sure that actual size does not exceed expected (hardcoded) one' :/
    (key32Arr ++ key16Arr ++ key8Arr)
      .padTo(MBY_CGRP_HASH_KEYS, 0.toByte)
      .take(MBY_CGRP_HASH_KEYS)
  }

  private def calculateHash(hashNum: HashNum, keys: Array[Byte], group: ClassifierGroup.ClassifierGroup): EmHash = {
    import madisonbay.wm.switchwm.crc._

    val crcFunction = if (hashNum == 0) mbyCrc32ByteSwap _ else mbyCrc32CByteSwap _
    // TODO: below mask can be done as (lookup.size - 1)
    val mask: Short = if (group.equals(ClassifierGroup.GroupA)) 0x7fff else 0x1fff
    val hash: Int = crcFunction(keys)

    EmHash(
      idx = (hash & mask).toShort,
      more = getUpper16From32(hash.toLong).toShort
    )
  }

  private def calculateLookupPtr(hashNum: HashNum, hashIdx: Short, hashCfg: em_hash_cfg_r): Short = {
    // This code is coproduct jealous
    val hashSize = if (hashNum == 0) hashCfg.HASH_SIZE_0() else hashCfg.HASH_SIZE_1()
    val lookupStartIdx: Short = (if (hashNum == 1) lookup.size/2 else 0).toShort

    val lookupBasePtr = if (hashNum == 0) hashCfg.BASE_PTR_0() else hashCfg.BASE_PTR_1()
    val bucketTableIdx = hashIdx % (1 << hashSize.toInt)

    (lookupStartIdx + lookupBasePtr + bucketTableIdx).toShort
  }

  override def apply: Actions = {

    val hashConfig: em_hash_cfg_r = cgrpEmMap.HASH_CFG(profile)
    val splitMode = ExactMatchMode(hashConfig.MODE().toInt)

    def loop(items: List[HashNum], results: Actions): Actions  = items match {
      case _ :: Nil if splitMode == ExactMatchMode.NonSplit => results
      case _ :: Nil if hashConfig.ENTRY_SIZE_1 == 0 => results
      case _ :: (remaining @ (_ :: Nil)) if hashConfig.ENTRY_SIZE_0 == 0 => loop(remaining, results)
      case hashNum :: _ =>
        val keyMaskConfig = KeyMaskConfig(cgrpEmMap, hashNum, profile)
        val maskedClassifierKeys = maskClassifierKeys(keyMaskConfig)
        val packagedKeys = performKeyCompaction(maskedClassifierKeys, keyMaskConfig)
        val hash = calculateHash(hashNum, packagedKeys, group)
        val _ = calculateLookupPtr(hashNum, hash.idx, hashConfig)
        results
      case _ => results
    }

    loop(
      List.range(0, cgrpEmMap.KEY_SEL0.length).map(tag[HashNumTag][Int]),
      Actions(Vector.empty)
    )
  }
}
