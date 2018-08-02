// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_mapper.h"
#include "mby_classifier.h"

static void getTcamCfg
(
          fm_uint32              regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_byte                group,
    const fm_byte                slice,
    const fm_byte                scenario,    
    mbyClassifierTcamCfg * const tcam_cfg
)
{
    fm_uint32 ffu_tcam_cfg_vals[MBY_FFU_TCAM_CFG_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_FFU_TCAM_CFG(group, slice, scenario, 0), MBY_FFU_TCAM_CFG_WIDTH, ffu_tcam_cfg_vals);

    tcam_cfg->CHUNK_MASK    = FM_ARRAY_GET_FIELD(ffu_tcam_cfg_vals, MBY_FFU_TCAM_CFG, CHUNK_MASK);
    tcam_cfg->START_COMPARE = FM_ARRAY_GET_BIT  (ffu_tcam_cfg_vals, MBY_FFU_TCAM_CFG, START_COMPARE);
    tcam_cfg->START_SET     = FM_ARRAY_GET_BIT  (ffu_tcam_cfg_vals, MBY_FFU_TCAM_CFG, START_SET);
    tcam_cfg->SELECT_TOP    = FM_ARRAY_GET_FIELD(ffu_tcam_cfg_vals, MBY_FFU_TCAM_CFG, SELECT_TOP);
    tcam_cfg->SELECT0       = FM_ARRAY_GET_FIELD(ffu_tcam_cfg_vals, MBY_FFU_TCAM_CFG, SELECT0);
    tcam_cfg->SELECT1       = FM_ARRAY_GET_FIELD(ffu_tcam_cfg_vals, MBY_FFU_TCAM_CFG, SELECT1);
    tcam_cfg->SELECT2       = FM_ARRAY_GET_FIELD(ffu_tcam_cfg_vals, MBY_FFU_TCAM_CFG, SELECT2);
    tcam_cfg->SELECT3       = FM_ARRAY_GET_FIELD(ffu_tcam_cfg_vals, MBY_FFU_TCAM_CFG, SELECT3);
}

static void getTcamEntry
(
          fm_uint32                regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_byte                  group,
    const fm_byte                  slice,
    const fm_uint16                index,
    mbyClassifierTcamEntry * const tcam_entry
)
{
    fm_uint32 ffu_tcam_vals[MBY_FFU_TCAM_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_FFU_TCAM(group, slice, index, 0), MBY_FFU_TCAM_WIDTH, ffu_tcam_vals);

    tcam_entry->Key       = FM_ARRAY_GET_FIELD64(ffu_tcam_vals, MBY_FFU_TCAM, KEY) |
                            FM_ARRAY_GET_FIELD64(ffu_tcam_vals, MBY_FFU_TCAM, KEY_TOP) << 32;

    tcam_entry->KeyInvert = FM_ARRAY_GET_FIELD64(ffu_tcam_vals, MBY_FFU_TCAM, KEY_INVERT) |
                            FM_ARRAY_GET_FIELD64(ffu_tcam_vals, MBY_FFU_TCAM, KEY_TOP_INVERT) << 32;
}

static void selectKeyMask
(
    const mbyClassifierTcamCfg * const tcam_cfg,
    const mbyClassifierKeys    * const keys,
    mbyLookupInfo              * const lookup_info
)
{
    fm_byte mux[5] = { 0 };
    mux[0] = tcam_cfg->SELECT0;
    mux[1] = tcam_cfg->SELECT1;
    mux[2] = tcam_cfg->SELECT2;
    mux[3] = tcam_cfg->SELECT3;
    mux[4] = tcam_cfg->SELECT_TOP + MBY_FFU_N_KEY16;
    
    lookup_info->key       = 0;
    lookup_info->keyInvert = FM_LITERAL_U64(0xFFFFFFFFF);

    // Loop through the 5 bytes of key ({7:0}, {15:8}, {23,16}, {31:24}, {39:32})
    for (fm_uint i = 0; i < 5; i++)
    {
        // Set low bit:
        fm_uint lo_bit = 8 * i;

        // key8:
        fm_byte temp = 0;
        if ((mux[i] >= MBY_FFU_KEY8_BASE) && (mux[i] < (MBY_FFU_KEY8_BASE + MBY_FFU_N_KEY8))) 
            temp = keys->key8[mux[i] - MBY_FFU_KEY8_BASE];
        
        // key16 and key32 are invalid for SelectTop:
        if (i >= 4)
            continue;

        // key16:
        if (mux[i] < (MBY_FFU_KEY16_BASE + MBY_FFU_N_KEY16)) 
            temp = FM_GET_UNNAMED_FIELD(keys->key16[mux[i] - MBY_FFU_KEY16_BASE], lo_bit % 16, 8);

        // key32:
        if ((mux[i] >= MBY_FFU_KEY32_BASE) && (mux[i] < (MBY_FFU_KEY32_BASE + MBY_FFU_N_KEY32))) 
            temp = FM_GET_UNNAMED_FIELD(keys->key32[mux[i] - MBY_FFU_KEY32_BASE], lo_bit, 8);

        FM_SET_UNNAMED_FIELD64(lookup_info->key, lo_bit, 8, temp);
    }

    lookup_info->keyInvert = ~lookup_info->key;
}

static void lookUpTcam
(
    fm_uint32               regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_byte           group,
    const fm_byte           slice,
    const fm_uint16         chunk_mask,
    mbyLookupInfo   * const lookup_info
)
{
    for (fm_uint i = 0; i < MBY_FFU_TCAM_ENTRIES_0; i++)
        lookup_info->rawHits[i] = FALSE;
    
    fm_uint16 tcam_index = 0;

    while (tcam_index < MBY_FFU_TCAM_ENTRIES_0)
    {
        fm_byte tcam_chunk = tcam_index >> 6;

        if ((chunk_mask >> tcam_chunk) & 1)
        {    
            mbyClassifierTcamEntry tcam_entry;
            getTcamEntry(regs, group, slice, tcam_index, &tcam_entry);

            fm_uint64 cam_key_inv = tcam_entry.KeyInvert;
            fm_uint64 cam_key     = tcam_entry.Key;
            fm_uint64 mask        = cam_key ^ cam_key_inv;

            if (((cam_key & cam_key_inv) == 0) && ((lookup_info->key & mask) == (cam_key & mask)))
                lookup_info->rawHits[tcam_index] = TRUE;

            tcam_index++;
        }
        else
        {
            tcam_index += 64;
        }    
    }
}

static void lookUpTcamCascade
(
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_byte                   scenario,
    const mbyClassifierKeys * const keys,
    const fm_byte                   group,
    mbyClassifierHitInfo            tcam_hit_info[MBY_FFU_TCAM_CFG_ENTRIES_1]
)
{
    // Compute cascade width:
    fm_byte cascade_width[MBY_FFU_TCAM_CFG_ENTRIES_1] = { 0 };
    
    for (fm_uint slice = 0; slice < MBY_FFU_TCAM_CFG_ENTRIES_1; slice++)
    {
        mbyClassifierTcamCfg tcam_cfg;
        getTcamCfg(regs, group, slice, scenario, &tcam_cfg);
        if (tcam_cfg.START_COMPARE)
            cascade_width[slice]++;

        for (fm_uint i = slice + 1; i < MBY_FFU_TCAM_CFG_ENTRIES_1; i++)
        {
            mbyClassifierTcamCfg tcam_cfg1;
            getTcamCfg(regs, group, i, scenario, &tcam_cfg1);
            if (tcam_cfg1.START_COMPARE)
                break;
            cascade_width[slice]++;
        }
    }
    
    fm_bool fsc           = FALSE;
    fm_bool exclusion_set = FALSE;
    fm_bool set_hit       = FALSE;
   
    for (fm_uint slice = 0; slice < MBY_FFU_TCAM_CFG_ENTRIES_1; slice++)
    {
        if (cascade_width[slice] == 0)
            continue;

        mbyClassifierTcamCfg tcam_cfg;
        getTcamCfg(regs, group, slice, scenario, &tcam_cfg);

        // Start Exclusion Set
        if (tcam_cfg.START_SET) {    
            exclusion_set = TRUE;
            set_hit       = FALSE;
        } 

        fm_bool hits[MBY_FFU_TCAM_ENTRIES_0] = { FALSE };

        for (fm_uint i = slice; i < slice + cascade_width[slice]; i++)
        {    
            mbyClassifierTcamCfg tcam_cfg1;
            getTcamCfg(regs, group, i, scenario, &tcam_cfg1);
            
            // Compute TCAM key:
            mbyLookupInfo lookup_info;
            selectKeyMask(&tcam_cfg1, keys, &lookup_info);

            // Look up in the TCAM and update raw hits with results of lookup:
            lookUpTcam(regs, group, i, tcam_cfg1.CHUNK_MASK, &lookup_info);

            for (fm_uint j = 0; j < MBY_FFU_TCAM_ENTRIES_0; j++)
                hits[j] = (fsc || tcam_cfg1.START_COMPARE || hits[j]) && lookup_info.rawHits[j];

            fsc = (i == slice) && (tcam_cfg1.START_COMPARE == 1) && (tcam_cfg1.CHUNK_MASK == 0);
        }

        for (fm_int j = MBY_FFU_TCAM_ENTRIES_0 - 1; j >= 0; j--)
        {
            if (hits[j] && !set_hit) {
                // introduce slice_info to fix klocwork error
                fm_uint slice_info = slice + cascade_width[slice]-1;
                tcam_hit_info[slice_info].hitIndexValid = TRUE;
                tcam_hit_info[slice_info].hitIndex = j;
                if (exclusion_set)
                    set_hit = TRUE;
                break;
            }      
        }
    }
}

static inline void setPrec(mbyPrecVal *old, fm_byte prec, fm_uint32 value)
{
    if ((prec >= old->prec) && (prec > 0))
    {
        old->prec = prec;
        old->val  = value;
    }
}

static void doAction
(
          fm_uint32              regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32              action,
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
                if ((j < MBY_FFU_N_ACT4) && (enable & (1uL << i)))
                    setPrec(&(actions->act4[j]), prec, ((value >> 4*i) & 0xF));
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
                if ((j < MBY_FFU_N_ACT1) && (enable & (1uL << i)))
                    setPrec(&(actions->act1[j]), prec, ((value >> i) & 0x1));
            }
            break;
        }
            
        case MBY_FFU_ACTION_SET3_1B:
        {
            fm_byte indexA  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_1B_INDEXA);                 
            fm_bool enableA = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_EA);
            fm_byte valueA  = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_VA);
            if (enableA && (indexA < MBY_FFU_N_ACT1))
                setPrec(&(actions->act1[indexA]), prec, valueA);
            
            fm_byte indexB  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_1B_INDEXB);
            fm_bool enableB = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_EB);
            fm_byte valueB  = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_VB);
            if (enableB && (indexB < MBY_FFU_N_ACT1))
                setPrec(&(actions->act1[indexB]), prec, valueB);
            
            fm_byte indexC  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_1B_INDEXC);                 
            fm_bool enableC = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_EC);
            fm_byte valueC  = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_VC);
            if (enableC && (indexC < MBY_FFU_N_ACT1))
                setPrec(&(actions->act1[indexC]), prec, valueC);

            break;
        }
    
        case MBY_FFU_ACTION_SET3_4B:
        {
            fm_byte indexA = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_INDEXA); 
            fm_byte valueA = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_VALUEA);
            if (indexA < MBY_FFU_N_ACT4)
                setPrec(&(actions->act4[indexA]), prec, valueA);

            fm_byte indexB = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_INDEXB); 
            fm_byte valueB = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_VALUEB);
            if (indexB < MBY_FFU_N_ACT4)
                setPrec(&(actions->act4[indexB]), prec, valueB);

            fm_byte indexC = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_INDEXC);               
            fm_byte valueC = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_VALUEC);
            if (indexC < MBY_FFU_N_ACT4)
                setPrec(&(actions->act4[indexC]), prec, valueC);

            break;
        }

        case MBY_FFU_ACTION_SET1_24B:
        {
            fm_byte   index = FM_GET_FIELD(action, MBY_FFU_ACTION, SET1_24B_INDEX);
            fm_uint32 value = FM_GET_FIELD(action, MBY_FFU_ACTION, SET1_24B_VALUE);
            if(index < MBY_FFU_N_ACT24)
                setPrec(&(actions->act24[index]), prec, value);
            break;
        }

        default:  

        case MBY_FFU_ACTION_NOP: 
            break;
    }
}

static void resolveActions
(
    fm_uint32                    regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_byte                scenario,
    const fm_byte                group,
    mbyClassifierHitInfo         tcam_hit_info[MBY_FFU_TCAM_CFG_ENTRIES_1],
    mbyClassifierActions * const actions
)
{
    fm_uint32 ffu_action_cfg_vals[MBY_FFU_ACTION_CFG_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_FFU_ACTION_CFG(group, scenario, 0), MBY_FFU_ACTION_CFG_WIDTH, ffu_action_cfg_vals);
  
    for (fm_uint ram_num = 0; ram_num < MBY_FFU_ACTION_ENTRIES_1; ram_num++)
    {
        fm_bool enable = FM_ARRAY_GET_UNNAMED_FIELD(ffu_action_cfg_vals, ram_num + (MBY_FFU_ACTION_ENTRIES_1 * 4), 1);
        if (!enable)
            continue; // skip action RAM if disabled

        fm_uint slice = FM_ARRAY_GET_UNNAMED_FIELD(ffu_action_cfg_vals, ram_num * 4, 4);

        if (tcam_hit_info[slice].hitIndexValid)
        {
            fm_uint hit_index = tcam_hit_info[slice].hitIndex;

            fm_uint32 ffu_action_vals[MBY_FFU_ACTION_WIDTH] = { 0 };
            mbyModelReadCSRMult(regs, MBY_FFU_ACTION(group, ram_num, hit_index, 0), MBY_FFU_ACTION_WIDTH, ffu_action_vals);
    
            for (fm_uint i = 0; i < MBY_FFU_ACTIONS_PER_ENTRY; i++) {
                fm_uint32 action = (i == 0)
                    ? FM_ARRAY_GET_FIELD(ffu_action_vals, MBY_FFU_ACTION, ACTION0)
                    : FM_ARRAY_GET_FIELD(ffu_action_vals, MBY_FFU_ACTION, ACTION1);
                doAction(regs, action, actions);
            }
        }
    }
}

void Classifier
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyMapperToClassifier * const in,
          mbyClassifierToHash   * const out
)
{
    // Init to defaults from the Mapper:
    fm_byte               scenario = in->FFU_SCENARIO;
    mbyClassifierKeys     keys     = in->FFU_KEYS;
    mbyClassifierActions  actions  = in->FFU_ACTIONS;

    // Use group = 0 for now <-- FIXME!!!!
    fm_byte group = 0;

    // Get hit index for each tcam slice:
    mbyClassifierHitInfo tcam_hit_info[MBY_FFU_TCAM_CFG_ENTRIES_1];
    lookUpTcamCascade(regs, scenario, &keys, group, tcam_hit_info);
        
    // Apply and resolve actions from action RAMs based on tcam hit index per slice:
    resolveActions(regs, scenario, group, tcam_hit_info, &actions);

#if 0
    FGHash(model, group);

    /* Remap subset of keys going into next group */
    Remap(model);
#endif

    // Update scenario based on scenario action:
    for (fm_uint s = MBY_FFU_ACTION_SCENARIO0, i = 0; s <= MBY_FFU_ACTION_SCENARIO5; s++, i++) {
        if (actions.act1[s].prec != 0) {    
            fm_byte val = actions.act1[s].val & 1;
            FM_SET_UNNAMED_FIELD(scenario, i, 1, val);
        }     
    }                

    // Assign outputs:
    out->FFU_SCENARIO = scenario;
    out->FFU_KEYS     = keys;
    out->FFU_ACTIONS  = actions;
}
