// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation


#include "mby_mapper.h"
#include "mby_clsfr_regs.h"
#include "mby_lpm.h"
#include "mby_wcm.h"
#include "mby_exactmatch.h"
#include "mby_classifier.h"
#include "mby_crc32.h"

/* Check precedence and set the action with the new value */
static inline void setAct
(
    mbyActionPrecVal * const action, // the current value
    fm_byte            const new_prec,
    fm_uint32          const new_value
)
{
    if ((new_prec >= action->prec) && (new_prec > 0))
    {
        action->prec = new_prec;
        action->val  = new_value;
    }
}

/* Action Set format: https://securewiki.ith.intel.com/x/XxDpKQ
 * Formerly called doAction()
 */
static void resolveActionSet
(
    fm_uint32              const action,
    mbyClassifierActions * const actions
)
{
    fm_byte prec   = FM_GET_FIELD        (action, MBY_FFU_ACTION, PREC);
    fm_byte encode = FM_GET_UNNAMED_FIELD(action, MBY_FFU_ACTION_l_ENTRYTYPE, 5);

    mbyClassifierActionEntryType entryType =
        (encode == 1)                ? MBY_FFU_ACTION_SET4_4B  :
        (encode == 2)                ? MBY_FFU_ACTION_SET8_1B  :
        (encode == 4)                ? MBY_FFU_ACTION_SET3_1B  :
        (((encode >> 3) & 0x3) == 1) ? MBY_FFU_ACTION_SET3_4B  :
        (((encode >> 4) & 0x1) == 1) ? MBY_FFU_ACTION_SET1_24B : MBY_FFU_ACTION_NOP;

    switch (entryType)
    {
        case MBY_FFU_ACTION_SET4_4B:
        {
            fm_byte   index  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET4_4B_INDEX);
            fm_byte   enable = FM_GET_FIELD(action, MBY_FFU_ACTION, SET4_4B_ENABLE);
            fm_uint32 value  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET4_4B_VALUE);
            for (fm_uint i = 0; i < 4; i++) {
                fm_uint j = index * 4 + i;
                if ((j < MBY_FFU_ACT4) && (enable & (1uL << i)))
                    setAct(&(actions->act4[j]), prec, ((value >> 4*i) & 0xF));
            }
            break;
        }

        case MBY_FFU_ACTION_SET8_1B:
        {
            fm_byte   index  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET8_1B_INDEX);
            fm_byte   enable = FM_GET_FIELD(action, MBY_FFU_ACTION, SET8_1B_ENABLE);
            fm_uint32 value  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET8_1B_VALUE);
            for (fm_uint i = 0; i < 8; i++) {
                fm_uint j = index * 8 + i;
                if ((j < MBY_FFU_ACT1) && (enable & (1uL << i)))
                    setAct(&(actions->act1[j]), prec, ((value >> i) & 0x1));
            }
            break;
        }

        case MBY_FFU_ACTION_SET3_1B:
        {
            fm_byte indexA  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_1B_INDEXA);
            fm_bool enableA = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_EA);
            fm_byte valueA  = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_VA);
            if (enableA && (indexA < MBY_FFU_ACT1))
                setAct(&(actions->act1[indexA]), prec, valueA);

            fm_byte indexB  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_1B_INDEXB);
            fm_bool enableB = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_EB);
            fm_byte valueB  = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_VB);
            if (enableB && (indexB < MBY_FFU_ACT1))
                setAct(&(actions->act1[indexB]), prec, valueB);

            fm_byte indexC  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_1B_INDEXC);
            fm_bool enableC = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_EC);
            fm_byte valueC  = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_VC);
            if (enableC && (indexC < MBY_FFU_ACT1))
                setAct(&(actions->act1[indexC]), prec, valueC);

            break;
        }

        case MBY_FFU_ACTION_SET3_4B:
        {
            fm_byte indexA = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_INDEXA);
            fm_byte valueA = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_VALUEA);
            if (indexA < MBY_FFU_ACT4)
                setAct(&(actions->act4[indexA]), prec, valueA);

            fm_byte indexB = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_INDEXB);
            fm_byte valueB = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_VALUEB);
            if (indexB < MBY_FFU_ACT4)
                setAct(&(actions->act4[indexB]), prec, valueB);

            fm_byte indexC = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_INDEXC);
            fm_byte valueC = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_VALUEC);
            if (indexC < MBY_FFU_ACT4)
                setAct(&(actions->act4[indexC]), prec, valueC);

            break;
        }

        case MBY_FFU_ACTION_SET1_24B:
        {
            fm_byte   index = FM_GET_FIELD(action, MBY_FFU_ACTION, SET1_24B_INDEX);
            fm_uint32 value = FM_GET_FIELD(action, MBY_FFU_ACTION, SET1_24B_VALUE);
            if(index < MBY_FFU_ACT24)
                setAct(&(actions->act24[index]), prec, value);
            break;
        }

        default:

        case MBY_FFU_ACTION_NOP:
            break;
    }
}

/* Remap Keys
 * Replaces keys at the end of each Classifier Group.
 */
static void remapKeys
(
    mbyClassifierActions  * const actions,
    mbyClassifierKeys     * const keys
)
{
    fm_uint32               remapAction;
    fm_uint16               value16;
    fm_byte                 value8;
    fm_byte                 mask;
    fm_int                  i;
    fm_byte                 key;
    fm_byte                 index[MBY_FFU_REMAP_ACTIONS];
    fm_uint                 keyIdx;
    fm_byte                 idx16;
    fm_uint                 set1_16_base;

    /* Resolve precedence between remap actions*/

    for (i = 0; i < MBY_FFU_REMAP_ACTIONS; i++)
    {
        index[i] = FM_GET_FIELD(actions->act24[MBY_FFU_ACTION_REMAP0+i].val,
                                MBY_FFU_REMAP,
                                SET1_16B_INDEX);
    }

    for (i = 0; i < MBY_FFU_REMAP_ACTIONS; i++)
    {
        if (actions->act24[MBY_FFU_ACTION_REMAP0+i].prec == 0)
        {
            actions->act24[MBY_FFU_ACTION_REMAP0+i].val = 0;
            continue;
        }
        remapAction = actions->act24[MBY_FFU_ACTION_REMAP0+i].val;
        set1_16_base = MBY_FFU_KEY32*4 + MBY_FFU_KEY16*2 + MBY_FFU_KEY8;
        /* SET1_16b */
        if(index[i] >= (set1_16_base))
        {
            idx16 = index[i] - set1_16_base;
            value16 = FM_GET_FIELD(remapAction,
                                   MBY_FFU_REMAP,
                                   SET1_16B_VALUE);

            if (idx16 < MBY_FFU_KEY16)
            {
                keys->key16[idx16] = value16;
            }
            else
            {
                /* set 16-bits of KEY32 */
                keyIdx = (idx16 - MBY_FFU_KEY16) >> 1;
                FM_SET_UNNAMED_FIELD(keys->key32[keyIdx],
                                     (idx16 % 2) * 16,
                                     16,
                                     value16);
            }
        }
        /* SET8_1b */
        else
        {
            mask = FM_GET_FIELD(remapAction,
                                MBY_FFU_REMAP,
                                SET8_1B_MASK);

            if (mask != 0)
            {
                value8 = FM_GET_FIELD(remapAction,
                                      MBY_FFU_REMAP,
                                      SET8_1B_VALUE);

                if (index[i] < MBY_FFU_KEY16*2)
                {
                    /* set 8-bits of KEY16 */
                    key = FM_GET_UNNAMED_FIELD(keys->key16[index[i] >> 1],
                                               (index[i] % 2) * 8,
                                               8);
                    key = ( key & ~mask ) | ( value8 & mask);

                    FM_SET_UNNAMED_FIELD(keys->key16[index[i] >> 1],
                                         (index[i] % 2) * 8,
                                         8,
                                         key);
                }
                else if (index[i] < (MBY_FFU_KEY16*2 + MBY_FFU_KEY8))
                {
                    /* set entire KEY8 */
                    key = keys->key8[index[i] - (MBY_FFU_KEY16*2)];

                    key = ( key & ~mask ) | ( value8 & mask);

                    keys->key8[index[i] - (MBY_FFU_KEY16*2)]  = key;
                }
                else
                {
                    /* set 8-bits of KEY32 */
                    keyIdx = (index[i] - (MBY_FFU_KEY16*2 +
                                MBY_FFU_KEY8)) >> 2;
                    key = FM_GET_UNNAMED_FIELD(keys->key32[keyIdx],
                                              (index[i] % 4) * 8,
                                              8);
                    key = ( key & ~mask ) | ( value8 & mask);

                    FM_SET_UNNAMED_FIELD(keys->key32[keyIdx],
                                        (index[i] % 4) * 8,
                                         8,
                                         key);
                }

            }

        }

        /* After remap action is applied to keys, zero out the remap action so that next group
         * doesnot take same remap action */
        actions->act24[MBY_FFU_ACTION_REMAP0+i].val = 0;
        actions->act24[MBY_FFU_ACTION_REMAP0+i].prec = 1;
    }

}   /* end remapKeys */

static void applyEntropyKeyMask
(
    mbyClassifierEntropyCfg         const entropy_cfg,
    mbyClassifierKeys               const keys,
    mbyClassifierKeys             * const hash_keys
)
{
    for (fm_uint i = 0; i < MBY_FFU_KEY32; i++)
        hash_keys->key32[i] = (FM_GET_UNNAMED_FIELD  (entropy_cfg.KEY32_MASK, i, 1)) ? keys.key32[i] : 0;

    for (fm_uint i = 0; i < MBY_FFU_KEY16; i++)
        hash_keys->key16[i] = (FM_GET_UNNAMED_FIELD  (entropy_cfg.KEY16_MASK, i, 1)) ? keys.key16[i] : 0;

    for (fm_uint i = 0; i < MBY_FFU_KEY8; i++)
        hash_keys->key8[i]  = (FM_GET_UNNAMED_FIELD64(entropy_cfg.KEY8_MASK,  i, 1)) ? keys.key8 [i] : 0;
}

static void populateMuxedAction
(

    mbyClassifierKeys           const keys,
    mbyClassifierActions        const actions,
    fm_byte                     const pri_profile,
    mbyClassifierMuxedAction  * const muxed_action
)
{
    fm_byte mpls_pop  = actions.act4[MBY_FFU_ACTION_MPLS_POP].val;
    fm_byte ecn_ctrl  = actions.act4[MBY_FFU_ACTION_ECN_CTRL].val;
    fm_byte tc_ctrl   = actions.act4[MBY_FFU_ACTION_TC_CTRL].val;
    fm_byte ttl_ctrl  = actions.act4[MBY_FFU_ACTION_TTL_CTRL].val;
    fm_byte dscp_ctrl = actions.act4[MBY_FFU_ACTION_DSCP_CTRL].val;

    // Update ECN:
    muxed_action->ecn         = 0;
    muxed_action->aqm_mark_en = 0;

    fm_byte exp  = 0;
    fm_byte dscp = 0;

    if (ecn_ctrl < 4) // FFU directly specifies ECN value:
    {
        muxed_action->ecn         = (ecn_ctrl == 2) ? 1 : ecn_ctrl;
        muxed_action->aqm_mark_en = (ecn_ctrl == 2) ? 1 : 0;
    }
    else if (ecn_ctrl < 16) // Define ECN source and marking rules:
    {
        switch (ecn_ctrl & 3) // ECN
        {
            case 0: muxed_action->ecn = FM_GET_UNNAMED_FIELD(keys.key8[MBY_FFU_KEY8_OUTER_DS], 0, 2); break;
            case 1: muxed_action->ecn = FM_GET_UNNAMED_FIELD(keys.key8[MBY_FFU_KEY8_INNER_DS], 0, 2); break;

            case 2: // ECN-CTRL[1:0] = 2 : ECN source is MPLS label 1, i.e. MPLS_MUX_EXP_DS[mpls_labels[0].exp].ecn
            {
                exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL1_1], 9, 3);
                muxed_action->ecn = 0; // FIXME!!!
                break;
            }
            case 3: // ECN_CTRL[1:0]=3: ECN source is MPLS label exposed after MPLS_POP
            {
                exp = (mpls_pop < 4) ? FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL1_1 + (mpls_pop * 2)], 9, 3) :
                      (mpls_pop < 6) ? FM_GET_UNNAMED_FIELD(keys.key8[MBY_FFU_KEY8_MPLS_LABEL5_2 + ((mpls_pop - 4) * 4)], 1, 3) : 0;
                muxed_action->ecn = 0; // FIXME!!!
                break;
            }

            default: break;
        }

        // ECN_CTRL[3:2]=3: mark in FFU if ingress ECN is 01 or 10 (aqm_mark_en=0):
        if ((((ecn_ctrl >> 2) & 3) == 3) && (muxed_action->ecn == 1 || muxed_action->ecn == 2))
            muxed_action->ecn = 3;

        // ECN marking:
        muxed_action->aqm_mark_en = (((ecn_ctrl >> 2) & 3) == 2);
    }

    // Update DSCP:
    muxed_action->dscp = 0;

    switch (dscp_ctrl)
    {
        case  0: muxed_action->dscp = (actions.act4[MBY_FFU_ACTION_DSCP_LOW ].val & 0xF) |
                                     ((actions.act4[MBY_FFU_ACTION_DSCP_HIGH].val & 0xF) << 4); break;

        case  4: muxed_action->dscp = FM_GET_UNNAMED_FIELD(keys.key8[MBY_FFU_KEY8_OUTER_DS], 2, 6); break;
        case  5: muxed_action->dscp = FM_GET_UNNAMED_FIELD(keys.key8[MBY_FFU_KEY8_INNER_DS], 2, 6); break;

        case  6: exp = (mpls_pop < 4) ? FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL1_1 + (mpls_pop * 2)], 9, 3) :
                       (mpls_pop < 6) ? FM_GET_UNNAMED_FIELD(keys.key8[MBY_FFU_KEY8_MPLS_LABEL5_2  + ((mpls_pop - 4) * 4)], 1, 3) : 0; break;

        case  8: exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL1_1], 9, 3); break;
        case  9: exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL2_1], 9, 3); break;
        case 10: exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL3_1], 9, 3); break;
        case 11: exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL4_1], 9, 3); break;

        default: break;
    }

    if (dscp_ctrl == 6 || ((dscp_ctrl >= 8) && (dscp_ctrl <= 11))) {
        muxed_action->dscp = 0; // FIXME!!!
    }

    // Update SWPRI:
    muxed_action->swpri = 0;

    switch (tc_ctrl)
    {
        case  0: muxed_action->swpri = actions.act4[MBY_FFU_ACTION_TC].val; break;

        case  4:
        {
            dscp = FM_GET_UNNAMED_FIELD(keys.key8[MBY_FFU_KEY8_OUTER_DS], 2, 6);
            muxed_action->swpri = 0; // FIXME!!!
            break;
        }
        case  5:
        {
            dscp = FM_GET_UNNAMED_FIELD(keys.key8[MBY_FFU_KEY8_INNER_DS], 2, 6);
            muxed_action->swpri = 0; // FIXME!!!
            break;
        }
    case  6: exp = (mpls_pop < 4) ? FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL1_1 + (mpls_pop * 2)],   9, 3) :
                   (mpls_pop < 6) ? FM_GET_UNNAMED_FIELD(keys.key8 [MBY_FFU_KEY8_MPLS_LABEL5_2 + ((mpls_pop - 4) * 4)], 1, 3) : 0; break;

        case  8: exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL1_1], 9, 3); break;
        case  9: exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL2_1], 9, 3); break;
        case 10: exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL3_1], 9, 3); break;
        case 11: exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL4_1], 9, 3); break;

        default: break;
    }

    if (tc_ctrl == 6 || (tc_ctrl >= 8 && tc_ctrl <= 11)) {
        muxed_action->swpri = 0; // FIXME!!!
    }

    // get TTL value based on TTL_CTRL Action:
    fm_byte ttl = 0;

    switch (ttl_ctrl)
    {
        case  0: ttl = keys.key8[MBY_FFU_KEY8_OUTER_TTL]; break;
        case  1: ttl = keys.key8[MBY_FFU_KEY8_INNER_TTL]; break;
        case  2: ttl = (mpls_pop < 4) ? FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL1_1 + (mpls_pop * 2)], 0, 8) :
                       (mpls_pop < 6) ? keys.key8[MBY_FFU_KEY8_MPLS_LABEL5_3 + ((mpls_pop - 4) * 4)] : 0; break;
        case  4: ttl = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL1_1], 0, 8); break;
        case  5: ttl = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL2_1], 0, 8); break;
        case  6: ttl = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL3_1], 0, 8); break;
        case  7: ttl = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL4_1], 0, 8); break;

        default: break;
    }

    muxed_action->ttl01 = 0;

    if (ttl == 0)
        FM_SET_UNNAMED_FIELD(muxed_action->ttl01, 0, 1, 1)
    else if (ttl == 1)
        FM_SET_UNNAMED_FIELD(muxed_action->ttl01, 1, 1, 1)

    muxed_action->ttl_ctrl = ttl_ctrl;

    muxed_action->vpri = (actions.act4[MBY_FFU_ACTION_VPRI_LOW].prec >= actions.act4[MBY_FFU_ACTION_VPRI_HIGH].prec) ?
        actions.act4[MBY_FFU_ACTION_VPRI_LOW].val : actions.act4[MBY_FFU_ACTION_VPRI_HIGH].val;

    muxed_action->route = (actions.act1[MBY_FFU_ACTION_NO_ROUTE].val == 0 && actions.act24[MBY_FFU_ACTION_FWD].val != 0);
}

static void populateEntropy
(
    mby_ppe_entropy_map        * const entropy_map,
    mbyClassifierKeys            const keys,
    mbyClassifierActions         const actions,
    fm_uint32                  * const ecmp_hash,
    fm_uint64                  * const mod_meta
)
{
    fm_byte   hash_profiles[2] = { 0 };
    fm_uint32 hash_values  [2] = { 0 };

    for (fm_uint hash_num = 0; hash_num < 2; hash_num++)
    {
        fm_byte val0 = (actions.act4[MBY_FFU_ACTION_HASH_PROFILE_ECMP_0 + (2 * hash_num)].val);
        fm_byte val1 = (actions.act4[MBY_FFU_ACTION_HASH_PROFILE_ECMP_1 + (2 * hash_num)].val & 0x3);
        fm_byte prof = (val1 << 4) | val0;

        hash_profiles[hash_num] = prof;

        // Get FFU_KEY_MASK register fields:
        mbyClassifierEntropyCfg entropy_cfg = mbyClsGetEntropyCfg(entropy_map, hash_num, prof);
        // Apply key mask on FFU keys:
        mbyClassifierKeys hash_keys;
        applyEntropyKeyMask(entropy_cfg, keys, &hash_keys);

        // Convert Keys into array of bytes:
        fm_byte hash_bytes[MBY_FFU_HASH_KEYS] = { 0 };
        mbyClsConvertKeysToBytes(hash_keys, hash_bytes);

        // Get hash value from CRC:
         hash_values[hash_num] = (hash_num == 0) ?
            mbyCrc32ByteSwap (hash_bytes, MBY_FFU_HASH_KEYS) : // HASH0: CRC-32 (Ethernet)
            mbyCrc32CByteSwap(hash_bytes, MBY_FFU_HASH_KEYS) ; // HASH1: CRC-32C (iSCSI)
    }


    // ECMP HASH for ARP_TABLE:
    *ecmp_hash = hash_values[0] & 0xFFFFFF;

    // Populate MOD_META for use by the Modifier:
    mbyEntropyMetaCfg meta_cfg = mbyClsGetEntropyMetaCfg(entropy_map, hash_profiles[1]);

    fm_uint64 mod_meta_l = 0; // local var

    // Apply Defaults to MOD_META:
    for (fm_uint i = 0; i < 6; i++)
    {
        fm_byte s = FM_GET_UNNAMED_FIELD(meta_cfg.BYTE_DEFAULTS, i*2, 2);
        switch (s)
        {
            case 0:
                FM_SET_UNNAMED_FIELD64(mod_meta_l, i*8,     4, actions.act4[MBY_FFU_ACTION_META0_LOW ].val);
                FM_SET_UNNAMED_FIELD64(mod_meta_l, i*8 + 4, 4, actions.act4[MBY_FFU_ACTION_META0_HIGH].val);
                break;
            case 1:
                FM_SET_UNNAMED_FIELD64(mod_meta_l, i*8,     4, actions.act4[MBY_FFU_ACTION_META1_LOW ].val);
                FM_SET_UNNAMED_FIELD64(mod_meta_l, i*8 + 4, 4, actions.act4[MBY_FFU_ACTION_META1_HIGH].val);
                break;
            case 2:
                FM_SET_UNNAMED_FIELD64(mod_meta_l, i*8,     4, actions.act4[MBY_FFU_ACTION_META2_LOW ].val);
                FM_SET_UNNAMED_FIELD64(mod_meta_l, i*8 + 4, 4, actions.act4[MBY_FFU_ACTION_META2_HIGH].val);
                break;
            case 3:
                FM_SET_UNNAMED_FIELD64(mod_meta_l, i*8,     8, 0);
                break;
        }
    }

    // Apply Hash to MOD_META:
    fm_uint64 meta_mask = 0;
    for (fm_uint i = 0; i < 48; i++)
        if ((i >= meta_cfg.HASH_START) && (i < (meta_cfg.HASH_START + meta_cfg.HASH_SIZE)))
            meta_mask |= (FM_LITERAL_U64(1) << i);

    fm_uint64 meta_hash = (((fm_uint64) hash_values[1]) << 32) | hash_values[1];

    *mod_meta = (mod_meta_l & ~meta_mask) | (meta_hash & meta_mask);
}

static fm_macaddr extractDmac
(
    fm_uint32 const ip_lo,
    fm_uint32 const ip_hi
)
{
    fm_macaddr mac_addr =  ((fm_macaddr)( ip_lo & 0xFFFFFF  )) |
                          (((fm_macaddr)( ip_hi & 0xFDFFFF00)) << 16) |
                          (((fm_macaddr)(~ip_hi & 0x2000000 )) << 16) ;
    return mac_addr;
}

static void transformActions
(
    mbyClassifierKeys          const keys,
    mbyClassifierActions       const actions,
    mbyClassifierMuxedAction   const muxed_action,
    fm_bool                    const ip_option[2],
    mbyParserInfo              const parser_info,
    fm_uint32                  const ecmp_hash,
    fm_uint64                  const mod_meta,
    fm_bool                  * const decap,
    fm_bool                  * const encap,
    fm_byte                  * const mod_idx,
    fm_uint16                * const l2_etype,
    fm_byte                  * const mpls_pop,
    fm_uint64                * const sglort,
    fm_uint64                * const idglort,
    fm_macaddr               * const l2_smac,
    fm_macaddr               * const l2_dmac,
    fm_macaddr               * const dmac_from_ipv6,
    fm_bool                  * const is_ipv4,
    fm_bool                  * const is_ipv6,
    fm_uint16                * const l3_length,
    fm_uint16                * const inner_l3_length,
    fm_uint16                * const outer_l3_length,
    fm_bool                  * const trap_ip_options,
    fm_bool                  * const drop_ttl,
    fm_bool                  * const trap_icmp,
    fm_bool                  * const trap_igmp,
    mbyClassifierFlags       * const ffu_flags,
    fm_uint32                * const ffu_route,
    fm_bool                  * const no_learn,
    fm_uint16                * const l2_ivid1,
    fm_byte                  * const qos_l2_vpri1,
    fm_byte                  * const ffu_trig,
    fm_uint32                * const policer_action
)
{
    *decap    = (actions.act24[MBY_FFU_ACTION_MOD_IDX ].val >> 1) & 0x1;
    *encap    =  actions.act24[MBY_FFU_ACTION_MOD_IDX ].val & 0x1;
    *mod_idx  = (actions.act24[MBY_FFU_ACTION_MOD_IDX ].val >> 2) & 0xFFFF;
    *mpls_pop =  actions.act4 [MBY_FFU_ACTION_MPLS_POP].val;

    *sglort = 0;
    FM_SET_UNNAMED_FIELD64(*sglort,  0,  8, keys.key8[(MBY_RE_KEYS_SGLORT - MBY_RE_KEYS_GENERAL_8B)*2 + 1]);
    FM_SET_UNNAMED_FIELD64(*sglort,  8, 16, keys.key8[(MBY_RE_KEYS_SGLORT - MBY_RE_KEYS_GENERAL_8B)*2    ]);

    *idglort = 0;
    FM_SET_UNNAMED_FIELD64(*idglort, 0,  8, keys.key8[(MBY_RE_KEYS_DGLORT - MBY_RE_KEYS_GENERAL_8B)*2 + 1]);
    FM_SET_UNNAMED_FIELD64(*idglort, 8, 16, keys.key8[(MBY_RE_KEYS_DGLORT - MBY_RE_KEYS_GENERAL_8B)*2    ]);

    *l2_smac = 0;
    fm_uint    l2_smac_off = (*decap) ? MBY_RE_KEYS_INNER_SMAC : MBY_RE_KEYS_OUTER_SMAC;
    for (fm_uint i = 0; i <= 2; i++)
        FM_SET_UNNAMED_FIELD64(*l2_smac, (16 * i), 16, keys.key16[l2_smac_off + (2 - i)])

    *l2_dmac  = 0;
    fm_uint   l2_dmac_off = (*decap) ? MBY_RE_KEYS_INNER_DMAC : MBY_RE_KEYS_OUTER_DMAC;
    for (fm_uint i = 0; i <= 2; i++)
        FM_SET_UNNAMED_FIELD64(*l2_dmac, (16 * i), 16, keys.key16[l2_dmac_off + (2 - i)])

    *l2_etype = (*decap) ? keys.key16[MBY_RE_KEYS_INNER_ETYPE] : keys.key16[MBY_RE_KEYS_OUTER_ETYPE];

    fm_uint16  ip_prot = (*decap) ? keys.key8[MBY_FFU_KEY8_INNER_PROT] : keys.key8[MBY_FFU_KEY8_OUTER_PROT];

    fm_uint    dmac_ipv6_off_lo = (*decap) ? MBY_FFU_KEY32_INNER_DIP_31_0  : MBY_FFU_KEY32_OUTER_DIP_31_0;
    fm_uint    dmac_ipv6_off_hi = (*decap) ? MBY_FFU_KEY32_INNER_DIP_63_32 : MBY_FFU_KEY32_OUTER_DIP_63_32;

    *dmac_from_ipv6 = extractDmac(keys.key32[dmac_ipv6_off_lo], keys.key32[dmac_ipv6_off_hi]);

    *is_ipv4 = (*decap) ? (parser_info.inr_l3_len && !parser_info.inr_l3_v6)
                        : (parser_info.otr_l3_len && !parser_info.otr_l3_v6);

    *is_ipv6 = (*decap) ? (parser_info.inr_l3_len &&  parser_info.inr_l3_v6)
                        : (parser_info.otr_l3_len &&  parser_info.otr_l3_v6);

    fm_uint32 l3_length_off = (*decap) ? MBY_FFU_KEY8_INNER_LEN : MBY_FFU_KEY8_OUTER_LEN;
    *l3_length = 0;
    FM_SET_UNNAMED_FIELD64(*l3_length, 0, 8, keys.key8[l3_length_off + 1]);
    FM_SET_UNNAMED_FIELD64(*l3_length, 8, 8, keys.key8[l3_length_off    ]);

    *trap_ip_options = ip_option[*decap];

    fm_bool is_ttl01 = (muxed_action.ttl01 & 1) || ((muxed_action.ttl01 >> 1) & 1);

    *drop_ttl = is_ttl01 && (*is_ipv4 || *is_ipv6);

    *trap_icmp = drop_ttl &&
        ((*is_ipv4 && (ip_prot == MBY_PROT_ICMPv4)) || (*is_ipv6 && (ip_prot == MBY_PROT_ICMPv6)));

    *trap_igmp = *is_ipv4 && (ip_prot == MBY_PROT_IGMP);

    *inner_l3_length = 0;
    fm_uint inner_index = (MBY_RE_KEYS_INNER_IP_LEN - MBY_RE_KEYS_GENERAL_8B) * 2 + 1;
    FM_SET_UNNAMED_FIELD64(*inner_l3_length, 0, 8, keys.key8[inner_index]);
    FM_SET_UNNAMED_FIELD64(*inner_l3_length, 8, 8, keys.key8[inner_index - 1]);

    *outer_l3_length = 0;
    fm_uint outer_index = (MBY_RE_KEYS_OUTER_IP_LEN - MBY_RE_KEYS_GENERAL_8B) * 2 + 1;
    FM_SET_UNNAMED_FIELD64(*outer_l3_length, 0, 8, keys.key8[outer_index]);
    FM_SET_UNNAMED_FIELD64(*outer_l3_length, 8, 8, keys.key8[outer_index - 1]);

    ffu_flags->drop         = actions.act1[MBY_FFU_ACTION_DROP     ].val;
    ffu_flags->trap         = actions.act1[MBY_FFU_ACTION_TRAP     ].val;
    ffu_flags->log          = actions.act1[MBY_FFU_ACTION_LOG      ].val;
    ffu_flags->no_route     = actions.act1[MBY_FFU_ACTION_NO_ROUTE ].val;
    ffu_flags->rx_mirror    = actions.act1[MBY_FFU_ACTION_RX_MIRROR].val;
    ffu_flags->capture_time = actions.act1[MBY_FFU_ACTION_CAPT_TIME].val;

    // FIXME cppcheck (error) Uninitialized struct member: ffu_flags.tx_tag
    for (fm_uint i = 0; i <= 1; i++)
        FM_SET_UNNAMED_FIELD(ffu_flags->tx_tag, i, 1, actions.act1[i+MBY_FFU_ACTION_TX_TAG0].val);

    *ffu_route = 0;
    FM_SET_UNNAMED_FIELD64(*ffu_route, 0, 22, FM_GET_UNNAMED_FIELD64(actions.act24[MBY_FFU_ACTION_FWD].val, 0, 22));

    *no_learn = FALSE;
    FM_SET_UNNAMED_FIELD(*no_learn, 0, 1, ~actions.act1[MBY_FFU_ACTION_LEARN].val);

    *l2_ivid1 = 0;
    if (*decap && actions.act4[MBY_FFU_ACTION_VID_LOW].prec <= 1)
        *l2_ivid1 = keys.key16[MBY_RE_KEYS_INNER_VLAN1];
    else
        for (fm_uint i = 0; i <= (MBY_FFU_ACTION_VID_HIGH - MBY_FFU_ACTION_VID_LOW); i++)
            FM_SET_UNNAMED_FIELD(*l2_ivid1, i * 4, 4, actions.act4[MBY_FFU_ACTION_VID_LOW+i].val);

    fm_bool qos_l2_vpri1_sel = *decap
        && (actions.act4[MBY_FFU_ACTION_VPRI_LOW ].val <= 1)
        && (actions.act4[MBY_FFU_ACTION_VPRI_HIGH].val <= 1);

    fm_byte qos_l2_vpri1_decap = (actions.act1[MBY_FFU_ACTION_COPY_OTR_VPRI].val)
           ? ((keys.key16[MBY_RE_KEYS_OUTER_VLAN1] >> 12) & 0xF)
           : ((keys.key16[MBY_RE_KEYS_INNER_VLAN1] >> 12) & 0xF);

    *qos_l2_vpri1 = (qos_l2_vpri1_sel) ? qos_l2_vpri1_decap : muxed_action.vpri;

    *ffu_trig = 0;
    for (fm_uint i = MBY_FFU_ACTION_TRIGGER0; i <= MBY_FFU_ACTION_TRIGGER7; i++)
        FM_SET_UNNAMED_FIELD(*ffu_trig, i - MBY_FFU_ACTION_TRIGGER0, 1, actions.act1[i].val);

    for (fm_uint i = MBY_FFU_ACTION_POLICER0; i <= MBY_FFU_ACTION_POLICER3; i++)
        policer_action[i] = actions.act24[i].val;
}

void Classifier
(
    mby_ppe_cgrp_a_map          * const cgrp_a_map,
    mby_ppe_cgrp_b_map          * const cgrp_b_map,
    mby_ppe_entropy_map         * const entropy_map,
    mby_shm_map                 * const shm_map, // shared memory (forwarding tables)
    mbyMapperToClassifier const * const in,
    mbyClassifierToHash         * const out
)
{
    // Read inputs from the Mapper:
    mbyClassifierActions  const actions_in        = in->FFU_ACTIONS;
    mbyClassifierKeys     const keys_in           = in->FFU_KEYS;
    fm_byte               const packet_profile_in = in->FFU_PROFILE;
    fm_bool       const * const ip_option         = in->IP_OPTION;
    mbyParserInfo         const parser_info       = in->PARSER_INFO;
    fm_byte               const pri_profile       = in->PRIORITY_PROFILE;

    fm_byte              packet_profile = packet_profile_in;
    mbyClassifierActions actions        = actions_in;
    mbyClassifierKeys    keys           = keys_in;


    // Exact match A (EM_A):
    fm_uint32 em_a_out[MBY_EM_MAX_ACTIONS_NUM] = { 0 };

    // TODO rename packet_profile to packet_profile
    mbyMatchExact(cgrp_a_map->A.EM_HASH_LOOKUP, &(cgrp_a_map->EM), shm_map, &keys, packet_profile, MBY_CLA_GROUP_A, em_a_out);

    for (fm_uint i = 0; i < MBY_EM_MAX_ACTIONS_NUM; ++i)
        resolveActionSet(em_a_out[i], &actions);

    // FIXME if EM_A_LPP.EN = 1 skip the LPM

    // Longest Prefix Match (LPM):
    fm_uint32 lpm_out[MBY_LPM_MAX_ACTIONS_NUM];

    // TODO is the packet_profile == 6-bit profile ID in the HAS?
    mbyMatchLpm(cgrp_a_map, shm_map, &keys, packet_profile, lpm_out);

    for (fm_uint i = 0; i < MBY_LPM_MAX_ACTIONS_NUM; ++i)
        resolveActionSet(lpm_out[i], &actions);

    // Remap takes effect between CGRP_A and CGRP_B
    remapKeys(&actions, &keys);

    // Update packet_profile based on profile action:
    for (fm_uint s = MBY_FFU_ACTION_SCENARIO0, i = 0; s <= MBY_FFU_ACTION_SCENARIO5; s++, i++) {
        if (actions.act1[s].prec != 0)
            FM_SET_UNNAMED_FIELD(packet_profile, i, 1, actions.act1[s].val & 1);
    }

    // Exact match B (EM_B):
    fm_uint32 em_b_out[MBY_EM_MAX_ACTIONS_NUM] = { 0 };

    mbyMatchExact(cgrp_b_map->B.EM_HASH_LOOKUP, &(cgrp_b_map->EM), shm_map, &keys, packet_profile, MBY_CLA_GROUP_B, em_b_out);

    for (fm_uint i = 0; i < MBY_EM_MAX_ACTIONS_NUM; ++i)
        resolveActionSet(em_b_out[i], &actions);

    // Wildcard Match (WCM):
    fm_uint32 wcm_out[MBY_WCM_MAX_ACTIONS_NUM] = { 0 };

    mbyMatchWildcard(cgrp_b_map, &keys, packet_profile, MBY_CLA_GROUP_B, wcm_out);

    for (fm_uint i = 0; i < MBY_WCM_MAX_ACTIONS_NUM; ++i)
        resolveActionSet(wcm_out[i], &actions);

    // Populate muxed_action:
    // TODO What are these? Add reference to MBY spec or remove
    mbyClassifierMuxedAction muxed_action;

    populateMuxedAction
    (

        keys,
        actions,
        pri_profile,
        &muxed_action
    );

    // Populate ecmp_hash and mod_meta:
    fm_uint32 ecmp_hash = 0;
    fm_uint64 mod_meta  = 0;

    populateEntropy
    (
        entropy_map,
        keys,
        actions,
        &ecmp_hash,
        &mod_meta
    );

    // Transform exisiting keys, actions, etc. into desired output:

    fm_bool                  decap           = FALSE;
    fm_bool                  encap           = FALSE;
    fm_byte                  mod_idx         = 0;
    fm_uint16                l2_etype        = 0;
    fm_byte                  mpls_pop        = 0;
    fm_uint64                sglort          = 0;
    fm_uint64                idglort         = 0;
    fm_macaddr               l2_smac         = 0;
    fm_macaddr               l2_dmac         = 0;
    fm_macaddr               dmac_from_ipv6  = 0;
    fm_bool                  is_ipv4         = FALSE;
    fm_bool                  is_ipv6         = FALSE;
    fm_uint16                l3_length       = 0;
    fm_uint16                inner_l3_length = 0;
    fm_uint16                outer_l3_length = 0;
    fm_bool                  trap_ip_options = FALSE;
    fm_bool                  drop_ttl        = FALSE;
    fm_bool                  trap_icmp       = FALSE;
    fm_bool                  trap_igmp       = FALSE;
    mbyClassifierFlags       ffu_flags       = { 0 };
    fm_uint32                ffu_route       = 0;
    fm_bool                  no_learn        = FALSE;
    fm_uint16                l2_ivid1        = 0;
    fm_byte                  qos_l2_vpri1    = 0;
    fm_byte                  ffu_trig        = 0;

    fm_uint32                policer_action[MBY_FFU_ACTION_POLICER3 + 1] = { 0 };

    transformActions
    (
        keys,
        actions,
        muxed_action,
        ip_option,
        parser_info,
        ecmp_hash,
        mod_meta,
        &decap,
        &encap,
        &mod_idx,
        &l2_etype,
        &mpls_pop,
        &sglort,
        &idglort,
        &l2_smac,
        &l2_dmac,
        &dmac_from_ipv6,
        &is_ipv4,
        &is_ipv6,
        &l3_length,
        &inner_l3_length,
        &outer_l3_length,
        &trap_ip_options,
        &drop_ttl,
        &trap_icmp,
        &trap_igmp,
        &ffu_flags,
        &ffu_route,
        &no_learn,
        &l2_ivid1,
        &qos_l2_vpri1,
        &ffu_trig,
        policer_action
    );

    // Write outputs:

    // Write outputs:
    out->AQM_MARK_EN      = muxed_action.aqm_mark_en;
    out->DECAP            = decap;
    out->DMAC_FROM_IPV6   = dmac_from_ipv6;
    out->DROP_TTL         = drop_ttl;
    out->ECN              = muxed_action.ecn;
    out->ENCAP            = encap;
    out->FFU_FLAGS        = ffu_flags;
    out->FFU_ROUTE        = ffu_route;
    out->FFU_TRIG         = ffu_trig;
    out->IDGLORT          = idglort;
    out->INNER_L3_LENGTH  = inner_l3_length;
    out->IS_IPV4          = is_ipv4;
    out->IS_IPV6          = is_ipv6;
    out->L2_DMAC          = l2_dmac;
    out->L2_ETYPE         = l2_etype;
    out->L2_IVID1         = l2_ivid1;
    out->L2_SMAC          = l2_smac;
    out->L34_HASH         = ecmp_hash;
    out->L3_LENGTH        = l3_length;
    out->MOD_IDX          = mod_idx;
    out->MPLS_POP         = mpls_pop;
    out->NO_LEARN         = no_learn;
    out->OUTER_L3_LENGTH  = outer_l3_length;
    out->PARSER_INFO      = parser_info;

    for (fm_uint i = MBY_FFU_ACTION_POLICER0; i <= MBY_FFU_ACTION_POLICER3; i++)
        out->POLICER_ACTION[i] = policer_action[i];

    out->QOS_L2_VPRI1     = qos_l2_vpri1;
    out->QOS_L3_DSCP      = muxed_action.dscp;
    out->QOS_SWPRI        = muxed_action.swpri;
    out->SGLORT           = sglort;
    out->TRAP_ICMP        = trap_icmp;
    out->TRAP_IGMP        = trap_igmp;
    out->TRAP_IP_OPTIONS  = trap_ip_options;
    out->TTL_CTRL         = muxed_action.ttl_ctrl;
    out->TX_TAG           = ffu_flags.tx_tag;

    // Pass thru:

    out->LEARN_MODE       = in->LEARN_MODE;
    out->L2_IDOMAIN       = in->L2_IDOMAIN;
    out->L3_IDOMAIN       = in->L3_IDOMAIN;
    out->PARITY_ERROR     = in->PARITY_ERROR;
    out->PARSER_ERROR     = in->PARSER_ERROR;
    out->PA_DROP          = in->PA_DROP;
    out->PA_L3LEN_ERR     = in->PA_L3LEN_ERR;
    out->RX_DATA          = in->RX_DATA;
    out->RX_LENGTH        = in->RX_LENGTH;
    out->RX_PORT          = in->RX_PORT;
    out->TRAFFIC_CLASS    = in->TRAFFIC_CLASS;
}
