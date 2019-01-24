#include <stdlib.h>
#include <stdio.h>

#include <mby_nexthop_test.h>
#include <mby_classifier.h>
#include <mby_pipeline.h>

#include <mby_top_map.h>
#include "tst_model_c_write.h"

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
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    mby_nh_test_data_in       const * const test_data_in,
    mbyHashToNextHop                * const hashToNextHop
)
{
    /* Set hashToNextHop. */
    hashToNextHop->ECMP_HASH = test_data_in->ecmp_hash;
    FM_SET_BIT  (hashToNextHop->FWD, MBY_FWD, SUBTYPE, !test_data_in->glort_routed);

    if(test_data_in->glort_routed)
    {
        FM_SET_FIELD(hashToNextHop->FWD, MBY_FWD_GLORT, DGLORT,          test_data_in->dglort);
        FM_SET_BIT  (hashToNextHop->FWD, MBY_FWD_GLORT, FORWARDED_TYPE, !test_data_in->normal_fwd);
        FM_SET_BIT  (hashToNextHop->FWD, MBY_FWD_GLORT, IS_FLOODSET,     test_data_in->is_floodset);
    }
    else
    {
        FM_SET_FIELD(hashToNextHop->FWD, MBY_FWD_ARP, ROUTE_INDEX, test_data_in->route_index);
        FM_SET_BIT  (hashToNextHop->FWD, MBY_FWD_ARP, ROUTE_TYPE,  test_data_in->route_type);

        fm_uint16 neighbor_idx = 0;

        if (test_data_in->route_type == MBY_NH_ROUTE_TYPE_SINGLE)
        {
            // Route Index is used to index directly into the neighbor table.
            neighbor_idx = test_data_in->route_index;
            /* Set registers. */
            setNextHopNeighborEntry(nexthop_w, neighbor_idx, &(test_data_in->nh_neighbor));
        }
        else if (test_data_in->route_type == MBY_NH_ROUTE_TYPE_GROUP)
        {
            // Route Index is used as an offset into the group table to retrieve the Group Type.
            fm_uint16 group_idx = test_data_in->route_index;
            /* Set registers. */
            setNextHopGroupEntry   (nexthop_w, group_idx,                     &(test_data_in->nh_group)   );
            setNextHopRouteEntry   (nexthop_w, test_data_in->nh_route_idx,    &(test_data_in->nh_route)   );
            setNextHopNeighborEntry(nexthop_w, test_data_in->nh_neighbor_idx, &(test_data_in->nh_neighbor));
        }

        if (test_data_in->nh_neighbor.ipv6_entry)
            hashToNextHop->DMAC_FROM_IPV6 = test_data_in->IPv6_dmac;
    }
}

static fm_bool nexthop_test_verify
(
    mby_ppe_nexthop_map  const * const nexthop_map,
    mby_nh_test_data_in  const * const test_data_in,
    mbyNextHopToMaskGen  const * const nexthopToMaskGen,
    mby_nh_test_data_out const * const test_data_out
)
{
    if (test_data_in->glort_routed)
    {
        if (nexthopToMaskGen->IDGLORT != test_data_out->dglort)
            return FALSE;

        if (nexthopToMaskGen->ROUTED != FALSE)
            return FALSE;
    }
    else
    {
        if (test_data_in->nh_neighbor.entry_type == MBY_NH_ENTRY_TYPE_IP_ROUTING)
        {
            if (nexthopToMaskGen->L2_DMAC != test_data_out->dmac)
                return FALSE;
        }
        else
        {
            if (nexthopToMaskGen->IDGLORT != test_data_out->dglort)
                return FALSE;
        }

        if (nexthopToMaskGen->ROUTED != test_data_out->routed)
            return FALSE;

        if (nexthopToMaskGen->L2_EDOMAIN != test_data_out->l2_domain)
            return FALSE;

        if (nexthopToMaskGen->L3_EDOMAIN != test_data_out->l3_domain)
            return FALSE;

        if (nexthopToMaskGen->L2_EVID1 != test_data_out->evid)
            return FALSE;
    }

    return TRUE;
}

static void nexthop_run_test(nh_test_data * const test_data)
{
    mby_ppe_nexthop_map       nexthop_map;
    mby_ppe_nexthop_map__addr nexthop_w;
    mbyHashToNextHop          hashToNextHop = { 0 };
    mbyNextHopToMaskGen       out  = { 0 };
    fm_bool                   pass = FALSE;

    /* Initialize register maps */
    mby_ppe_nexthop_map__init(&nexthop_map, &nexthop_w, mby_field_init_cb);
    /* Setup registers and structs */
    nexthop_test_setup(&nexthop_w, &(test_data->in), &hashToNextHop);
    /* Perform test */
    NextHop(&nexthop_map, &nexthop_w, &hashToNextHop, &out);
    /* Verify test */
    pass = nexthop_test_verify(&nexthop_map, &(test_data->in), &out, &(test_data->out));

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
