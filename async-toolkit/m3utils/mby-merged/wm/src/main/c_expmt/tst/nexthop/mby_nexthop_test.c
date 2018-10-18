#include <stdlib.h>
#include <stdio.h>

#include <mby_nexthop_test.h>
#include <mby_classifier.h>
#include <mby_pipeline.h>

#ifdef USE_NEW_CSRS
#include <mby_top_map.h>
#endif

#define COLOR_RED     "\x1b[31m"
#define COLOR_GREEN   "\x1b[32m"
#define COLOR_RESET   "\x1b[0m"

fm_uint tests = 0;
fm_uint fails = 0;
fm_uint passes = 0;

void test_pass(const char * name)
{
    printf(COLOR_GREEN "[pass]" COLOR_RESET " %s\n", name);
    passes++;
    tests++;
}

void test_fail(const char * name)
{
    printf(COLOR_RED   "[FAIL]" COLOR_RESET " %s\n", name );
    fails++;
    tests++;
}

static void nexthop_test_setup
(
#ifdef USE_NEW_CSRS
    mby_ppe_nexthop_map * const nexthop_map,
#else
    fm_uint32                   regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyHashToNextHop    * const hashToNextHop,
    mby_nh_test_data_in * const test_data_in
)
{
    if(test_data_in->glort_routed)
    {
        FM_SET_FIELD(hashToNextHop->FFU_ROUTE, MBY_FFU_ROUTE, DGLORT, test_data_in->dglort);
    }
    else
    {
        /* Set hashToNextHop. */
        FM_SET_BIT  (hashToNextHop->FFU_ROUTE, MBY_FFU_ROUTE, ARP_ROUTE, !test_data_in->glort_routed);
        FM_SET_FIELD(hashToNextHop->FFU_ROUTE, MBY_FFU_ROUTE, ARP_INDEX, test_data_in->arp_index);
        FM_SET_BIT  (hashToNextHop->FFU_ROUTE, MBY_FFU_ROUTE, GROUP_TYPE, test_data_in->group_type);
        FM_SET_FIELD(hashToNextHop->FFU_ROUTE, MBY_FFU_ROUTE, GROUP_SIZE, test_data_in->group_size);

        hashToNextHop->ARP_HASH[test_data_in->group_size] = test_data_in->arp_hash;
        hashToNextHop->RAW_HASH                           = test_data_in->raw_hash;

        fm_byte sel_hash = (test_data_in->group_type == 0) ?
                    test_data_in->arp_hash :
                    ((test_data_in->raw_hash << test_data_in->group_size) >> 12);
        fm_uint16 arp_tbl_idx  = (test_data_in->arp_index + sel_hash) & (MBY_ARP_TABLE_ENTRIES - 1);

        /* Set registers. */
#ifdef USE_NEW_CSRS
        nexthop_neighbors_table_0_r * const nh_table_0 = &(nexthop_map->NH_NEIGHBORS_0[arp_tbl_idx]);
        nexthop_neighbors_table_1_r * const nh_table_1 = &(nexthop_map->NH_NEIGHBORS_1[arp_tbl_idx]);

        nh_table_1->ENTRY_TYPE       = test_data_in->entry_type;
        nh_table_1->IPV6_ENTRY       = test_data_in->ipv6_entry;
        nh_table_1->EVID             = test_data_in->evid;
        nh_table_1->MTU_INDEX        = test_data_in->mtu_index;
        nh_table_1->MOD_IDX          = test_data_in->mod_idx;
        nh_table_1->L3_DOMAIN        = test_data_in->l3_domain;
        nh_table_1->L2_DOMAIN        = test_data_in->l2_domain;
        nh_table_1->UPDATE_L3_DOMAIN = test_data_in->update_l3_domain;
        nh_table_1->UPDATE_L2_DOMAIN = test_data_in->update_l2_domain;

        if (test_data_in->entry_type == MBY_ARP_TYPE_MAC) {
            if (test_data_in->ipv6_entry)
                hashToNextHop->DMAC_FROM_IPV6 = test_data_in->dmac;
            else
                nh_table_0->DST_MAC = test_data_in->dmac;
        } else {
            nh_table_1->MARK_ROUTED = test_data_in->mark_routed;
            nh_table_0->DGLORT      = test_data_in->dglort;
        }
#else
        fm_uint32 arp_table_regs[MBY_ARP_TABLE_WIDTH] = { 0 };

        FM_ARRAY_SET_BIT    (arp_table_regs, MBY_ARP_TABLE, ENTRY_TYPE, test_data_in->entry_type);
        FM_ARRAY_SET_BIT    (arp_table_regs, MBY_ARP_TABLE, IPV6_ENTRY, test_data_in->ipv6_entry);
        FM_ARRAY_SET_FIELD64(arp_table_regs, MBY_ARP_TABLE, EVID, test_data_in->evid);
        FM_ARRAY_SET_FIELD64(arp_table_regs, MBY_ARP_TABLE, MTU_INDEX, test_data_in->mtu_index);
        FM_ARRAY_SET_FIELD64(arp_table_regs, MBY_ARP_TABLE, MOD_IDX, test_data_in->mod_idx);
        FM_ARRAY_SET_FIELD64(arp_table_regs, MBY_ARP_TABLE, L3_DOMAIN, test_data_in->l3_domain);
        FM_ARRAY_SET_FIELD64(arp_table_regs, MBY_ARP_TABLE, L2_DOMAIN, test_data_in->l2_domain);
        FM_ARRAY_SET_BIT    (arp_table_regs, MBY_ARP_TABLE, UPDATE_L3_DOMAIN, test_data_in->update_l3_domain);
        FM_ARRAY_SET_BIT    (arp_table_regs, MBY_ARP_TABLE, UPDATE_L2_DOMAIN, test_data_in->update_l2_domain);

        if (test_data_in->entry_type == MBY_ARP_TYPE_MAC) {
            if (test_data_in->ipv6_entry)
                hashToNextHop->DMAC_FROM_IPV6 = test_data_in->dmac;
            else
                FM_ARRAY_SET_FIELD64(arp_table_regs, MBY_ARP_TABLE, DST_MAC, test_data_in->dmac);
        } else {
            FM_ARRAY_SET_FIELD64(arp_table_regs, MBY_ARP_ENTRY_GLORT, DGLORT, test_data_in->dglort);
            FM_ARRAY_SET_BIT    (arp_table_regs, MBY_ARP_ENTRY_GLORT, MARK_ROUTED, test_data_in->mark_routed);
        }

        mbyModelWriteCSRMult(regs, MBY_ARP_TABLE(arp_tbl_idx, 0), MBY_ARP_TABLE_WIDTH, arp_table_regs);
#endif
    }
}

static fm_bool nexthop_test_verify
(
#ifdef USE_NEW_CSRS
    mby_ppe_nexthop_map * const  nexthop,
#else
    fm_uint32                    regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    mby_nh_test_data_in  * const test_data_in,
    mbyNextHopToMaskGen  * const nexthopToMaskGen,
    mby_nh_test_data_out * const test_data_out
)
{
    if (test_data_in->glort_routed)
    {
        if (nexthopToMaskGen->IDGLORT != test_data_out->dglort)
            return FALSE;

        if (nexthopToMaskGen->MARK_ROUTED != FALSE)
            return FALSE;
    }
    else
    {
        if (test_data_in->entry_type == MBY_ARP_TYPE_MAC)
        {
            if (nexthopToMaskGen->L2_DMAC != test_data_out->dmac)
                return FALSE;
        }
        else
        {
            if (nexthopToMaskGen->IDGLORT != test_data_out->dglort)
                return FALSE;

            if (nexthopToMaskGen->MARK_ROUTED != test_data_out->mark_routed)
                return FALSE;
        }

        if (nexthopToMaskGen->L2_EDOMAIN != test_data_out->l2_domain)
            return FALSE;

        if (nexthopToMaskGen->L3_EDOMAIN != test_data_out->l3_domain)
            return FALSE;

        if (nexthopToMaskGen->L2_EVID1 != test_data_out->evid)
            return FALSE;

        if (nexthopToMaskGen->MTU_INDEX != test_data_out->mtu_idx)
            return FALSE;

        if (nexthopToMaskGen->MOD_IDX != test_data_out->mod_idx)
            return FALSE;

        if (nexthopToMaskGen->DECAP != test_data_out->decap)
            return FALSE;

        if (nexthopToMaskGen->ENCAP != test_data_out->encap)
            return FALSE;

#ifdef USE_NEW_CSRS
#else
        fm_uint64 arp_used_reg = 0;
        mbyModelReadCSR64(regs, MBY_ARP_USED((test_data_in->arp_tbl_idx >> 6), 0), &arp_used_reg);

        fm_uint64 used_value = FM_GET_FIELD64(arp_used_reg, MBY_ARP_USED, USED);
        if (used_value != test_data_out->arp_used)
            return FALSE;
#endif
    }

    return TRUE;
}
static void nexthop_run_test(nh_test_data * const test_data)
{
#ifdef USE_NEW_CSRS
    mby_ppe_nexthop_map nexthop_map;
#else
    fm_uint32 *         regs;
#endif
    mbyHashToNextHop    hashToNextHop = { 0 };
    mbyNextHopToMaskGen out  = { 0 };
    fm_bool             pass = FALSE;

#ifdef USE_NEW_CSRS
    nexthop_test_setup(&nexthop_map, &hashToNextHop, &(test_data->in));

    NextHop(&nexthop_map, &hashToNextHop, &out);

    pass = nexthop_test_verify(&nexthop_map, &(test_data->in), &out, &(test_data->out));

#else
    regs = calloc(MBY_REGISTER_ARRAY_SIZE, sizeof(fm_uint32));

    nexthop_test_setup(regs, &hashToNextHop, &(test_data->in));

    NextHop(regs, &hashToNextHop, &out);

    pass = nexthop_test_verify(regs, &(test_data->in), &out, &(test_data->out));

    free(regs);
#endif

    if (pass)
        test_pass(test_data->name);
    else
        test_fail(test_data->name);
}

int main()
{
    printf("--------------------------------------------------------------------------------\n");

    int err;

    fm_uint tests_num = sizeof(nexthop_tests) / sizeof(nh_test_data);

    for (fm_uint test_num = 0; test_num < tests_num; test_num++)
        nexthop_run_test(&nexthop_tests[test_num]);

    printf("--------------------------------------------------------------------------------\n");

    if (fails == 0)
        printf(COLOR_GREEN "[pass]");
    else
        printf(COLOR_RED   "[FAIL]");

    printf(" %2d/%2d - Nexthop tests\n" COLOR_RESET, passes, tests);

    printf("--------------------------------------------------------------------------------\n");

    int rv = (fails == 0) ? 0 : -1;
    return rv;
}
