#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include <mby_mapper.h>
#include <mby_classifier.h>
#include <mby_parser.h>
#include <mby_pipeline.h>

#define COLOR_RED     "\x1b[31m"
#define COLOR_GREEN   "\x1b[32m"
#define COLOR_RESET   "\x1b[0m"

#define MAPPER_TEST_ASSERT(what, name) { \
        if (!(what)) { \
            printf("fail @ " name "\n"); \
            return 0; \
        } \
    }

static void set_key(mbyParserToMapper* inout, int key, fm_uint16 val)
{
    inout->PA_KEYS      [key] = val;
    inout->PA_KEYS_VALID[key] = 1;
}

typedef struct _mapper_test_packet_data
{
    uint32_t src_ip;
    uint32_t dst_ip;
    uint64_t src_mac;
    uint64_t dst_mac;
    uint16_t src_tcpudp_port;
    uint16_t dst_tcpudp_port;

} mapper_test_packet_data;

static void load_pa_keys(const mapper_test_packet_data* packet_data, mbyParserToMapper* inout)
{
    const unsigned char ip_hdr[] = {
        0x45, 0x00, 0x00,   62, // 62 is len
        0xAB, 0xCD, 0x00, 0x00, // ABCD = identification
        0x40, 0x06, 0xFF, 0xFF, // TTL = 0x40, protocol = TCP
        // ffs is checksum
        (packet_data->src_ip >> 24) & 0xFF,
        (packet_data->src_ip >> 16) & 0xFF,
        (packet_data->src_ip >>  8) & 0xFF,
         packet_data->src_ip        & 0xFF,
        (packet_data->dst_ip >> 24) & 0xFF,
        (packet_data->dst_ip >> 16) & 0xFF,
        (packet_data->dst_ip >>  8) & 0xFF,
         packet_data->dst_ip        & 0xFF
    };

    // TODO a nice function for these multi-keys?
    set_key(inout, MBY_PA_KEYS_OUTER_DMAC + 0, (packet_data->dst_mac >> 32) & 0xFFFF);
    set_key(inout, MBY_PA_KEYS_OUTER_DMAC + 1, (packet_data->dst_mac >> 16) & 0xFFFF);
    set_key(inout, MBY_PA_KEYS_OUTER_DMAC + 2, packet_data->dst_mac & 0xFFFF);

    set_key(inout, MBY_PA_KEYS_OUTER_SMAC + 0, (packet_data->src_mac >> 32) & 0xFFFF);
    set_key(inout, MBY_PA_KEYS_OUTER_SMAC + 1, (packet_data->src_mac >> 16) & 0xFFFF);
    set_key(inout, MBY_PA_KEYS_OUTER_SMAC + 2, packet_data->src_mac & 0xFFFF);

    set_key(inout, MBY_PA_KEYS_OUTER_ETYPE, 0x0800);

    set_key(inout, MBY_PA_KEYS_OUTER_L4SRC, packet_data->src_tcpudp_port);
    set_key(inout, MBY_PA_KEYS_OUTER_L4DST, packet_data->dst_tcpudp_port);

    for (size_t i = 0;i < sizeof(ip_hdr) / 2;++i)
    {
        set_key(inout, MBY_PA_KEYS_OUTER_IP_HEADER+i,
                        (ip_hdr[2*i] << 8) + ip_hdr[2*i+1]);
    }

    set_key(inout, MBY_PA_KEYS_OUTER_SIPDIP + 0, (packet_data->src_ip >> 16) & 0xFFFF);
    set_key(inout, MBY_PA_KEYS_OUTER_SIPDIP + 1, packet_data->src_ip & 0xFFFF);
    set_key(inout, MBY_PA_KEYS_OUTER_SIPDIP + 2, (packet_data->dst_ip >> 16) & 0xFFFF);
    set_key(inout, MBY_PA_KEYS_OUTER_SIPDIP + 3, packet_data->dst_ip & 0xFFFF);

    inout->PA_FLAGS[MBY_PA_FLAGS_OTR_L4_TCP_V] = 1;
    inout->PA_FLAGS[MBY_PA_FLAGS_OTR_L3_V] = 1;
    inout->PA_FLAGS[MBY_PA_FLAGS_OTR_L4_V] = 1;
}

static int check_against_keys(const mapper_test_packet_data* packet_data, const mbyMapperToClassifier* mapper_to_classifier)
{
    if (mapper_to_classifier->FFU_KEYS.key32[0] != packet_data->src_ip)
            printf("fail @ src IP -> key32[0]\n");

    if (mapper_to_classifier->FFU_KEYS.key32[1] != packet_data->dst_ip)
            printf("fail @ dst IP -> key32[1]\n");

    if (mapper_to_classifier->FFU_KEYS.key16[6] != ((packet_data->dst_mac >> 32) & 0xFFFF))
                    printf("fail @ dmac#0 -> key16[6]\n");
    if (mapper_to_classifier->FFU_KEYS.key16[7] != ((packet_data->dst_mac >> 32) & 0xFFFF))
                    printf("fail @ dmac#1 -> key16[7]\n");
    if (mapper_to_classifier->FFU_KEYS.key16[8] != (packet_data->dst_mac & 0xFFFF))
                    printf("fail @ dmac#2 -> key16[8]\n");

    if (mapper_to_classifier->FFU_KEYS.key16[9] != ((packet_data->src_mac >> 32) & 0xFFFF))
            printf("fail @ smac#0 -> key16[9]\n");
    if (mapper_to_classifier->FFU_KEYS.key16[10] != ((packet_data->src_mac >> 16) & 0xFFFF))
            printf("fail @ smac#1 -> key16[10]\n");
    if (mapper_to_classifier->FFU_KEYS.key16[11] != (packet_data->src_mac & 0xFFFF))
            printf("fail @ smac#2 -> key16[11]\n");

    if (mapper_to_classifier->FFU_KEYS.key16[16] != packet_data->src_tcpudp_port)
            printf("fail @ L4SRC -> key16[16]\n");
    if (mapper_to_classifier->FFU_KEYS.key16[17] != packet_data->dst_tcpudp_port)
            printf("fail @ L4DST -> key16[17]\n");

    return 1; // FIXME return error on err msg
}

typedef void(*run_on_simple_tcp_setup_fn)
(
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map * const mapper_map,
#else
    fm_uint32[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyParserToMapper*
);
typedef int(*run_on_simple_tcp_check_fn)(const mbyMapperToClassifier *);

static int run_on_simple_tcp(run_on_simple_tcp_setup_fn setup,
                             run_on_simple_tcp_check_fn check)
{
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map mapper_map = { 0 };
#else
    fm_uint32* regs = calloc(MBY_REGISTER_ARRAY_SIZE, sizeof(fm_uint32));
#endif
    int ret = 1;

    mbyParserToMapper in = {0} ;
    mbyMapperToClassifier mapper_to_classifier;

    mapper_test_packet_data simple_tcp;
    simple_tcp.src_ip = 0x01020304;
    simple_tcp.dst_ip = 0x05060708;
    simple_tcp.src_mac = 0x111111111111;
    simple_tcp.dst_mac = 0x222222222222;
    simple_tcp.src_tcpudp_port = 0xA123;
    simple_tcp.dst_tcpudp_port = 0xA456;

    load_pa_keys(&simple_tcp, &in);

#ifdef USE_NEW_CSRS
    setup(&mapper_map, &in);

    Mapper(&mapper_map, &in, &mapper_to_classifier);
#else
    setup(regs, &in);

    Mapper(regs, &in, &mapper_to_classifier);
#endif

    if (!check_against_keys(&simple_tcp, &mapper_to_classifier))
        ret = 0;

    if (!check(&mapper_to_classifier))
        ret = 0;

#ifndef USE_NEW_CSRS
    free(regs);
#endif

    return ret;
}

void pass(const char* name)
{
    printf(COLOR_GREEN "[pass]" COLOR_RESET " %s\n", name);
}

void fail(const char* name)
{
    printf(COLOR_RED   "[FAIL]" COLOR_RESET " %s\n", name );
}

#define SIMPLE_TCP_TEST(name, fails) {if (run_on_simple_tcp(simple_tcp_ ## name ## _test_setup, \
                simple_tcp_ ## name ## _test_check)) pass(#name); else {++fails; fail(#name);} }

static void simple_tcp_basic_test_setup
(
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map * const mapper_map,
#else
    fm_uint32 reg[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyParserToMapper* inout
)
{

}

static int simple_tcp_basic_test_check(const mbyMapperToClassifier* in)
{
    return 1;
}

static void simple_tcp_act24_default_test_setup
(
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map * const mapper_map,
#else
    fm_uint32 reg[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyParserToMapper* inout
)
{
    //act24[0]
    // lower
#ifdef USE_NEW_CSRS
    map_port_default_r * port_def = &(mapper_map->MAP_PORT_DEFAULT[0][0]);
    port_def->TARGET              = 96;
    port_def->VALUE               = 0xabcd;
#else
    fm_uint32 map_port_default_vals_4[MBY_MAP_PORT_DEFAULT_WIDTH] = {0};
    FM_ARRAY_SET_FIELD(map_port_default_vals_4, MBY_MAP_PORT_DEFAULT, TARGET, 96);
    FM_ARRAY_SET_FIELD(map_port_default_vals_4, MBY_MAP_PORT_DEFAULT, VALUE, 0xabcd);
    mbyModelWriteCSRMult(reg, MBY_MAP_PORT_DEFAULT(0, 0, 0), MBY_MAP_PORT_DEFAULT_WIDTH, map_port_default_vals_4);
#endif
    // upper
#ifdef USE_NEW_CSRS
    port_def         = &(mapper_map->MAP_PORT_DEFAULT[0][1]);
    port_def->TARGET = 112;
    port_def->VALUE  = 0xff;
#else
    fm_uint32 map_port_default_vals_5[MBY_MAP_PORT_DEFAULT_WIDTH] = { 0 };
    FM_ARRAY_SET_FIELD(map_port_default_vals_5, MBY_MAP_PORT_DEFAULT, TARGET, 112);
    FM_ARRAY_SET_FIELD(map_port_default_vals_5, MBY_MAP_PORT_DEFAULT, VALUE, 0xff);
    mbyModelWriteCSRMult(reg, MBY_MAP_PORT_DEFAULT(0, 1, 0), MBY_MAP_PORT_DEFAULT_WIDTH, map_port_default_vals_5);
#endif
}

static int simple_tcp_act24_default_test_check(const mbyMapperToClassifier* in)
{
    MAPPER_TEST_ASSERT(in->FFU_ACTIONS.act24[0].val == 0Xffabcd, "act24[0]");

    return 1;
}

static void simple_tcp_act4_default_test_setup
(
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map * const mapper_map,
#else
    fm_uint32 reg[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyParserToMapper* inout
)
{
#ifdef USE_NEW_CSRS
    map_port_default_r * port_def = &(mapper_map->MAP_PORT_DEFAULT[0][0]);
    port_def->TARGET              = 192;
    port_def->VALUE               = 0xb;
#else
    fm_uint32 map_port_default_vals_6[MBY_MAP_PORT_DEFAULT_WIDTH] = { 0 };
    FM_ARRAY_SET_FIELD(map_port_default_vals_6, MBY_MAP_PORT_DEFAULT, TARGET, 192);
    FM_ARRAY_SET_FIELD(map_port_default_vals_6, MBY_MAP_PORT_DEFAULT, VALUE, 0xb);
    mbyModelWriteCSRMult(reg, MBY_MAP_PORT_DEFAULT(0, 0, 0), MBY_MAP_PORT_DEFAULT_WIDTH, map_port_default_vals_6);
#endif
}

static int simple_tcp_act4_default_test_check(const mbyMapperToClassifier* in)
{
    MAPPER_TEST_ASSERT(in->FFU_ACTIONS.act4[0].val == 0Xb, "act4[0]");

    return 1;
}

static void simple_tcp_default0_test_setup
(
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map * const mapper_map,
#else
    fm_uint32 reg[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyParserToMapper* inout
)
{
#ifdef USE_NEW_CSRS
    map_port_default_r * port_def = &(mapper_map->MAP_PORT_DEFAULT[0][0]);
    port_def->TARGET              = 3;
    port_def->VALUE               = 0xba;
#else
    fm_uint32 map_port_default_vals_0[MBY_MAP_PORT_DEFAULT_WIDTH] = { 0 };
    FM_ARRAY_SET_FIELD(map_port_default_vals_0, MBY_MAP_PORT_DEFAULT, TARGET, 3);
    FM_ARRAY_SET_FIELD(map_port_default_vals_0, MBY_MAP_PORT_DEFAULT, VALUE, 0xBA);
    mbyModelWriteCSRMult(reg, MBY_MAP_PORT_DEFAULT(0/*port=0*/, 0/*i=0*/, 0), MBY_MAP_PORT_DEFAULT_WIDTH, map_port_default_vals_0);
#endif
}

static int simple_tcp_default0_test_check(const mbyMapperToClassifier* in)
{
    MAPPER_TEST_ASSERT(in->FFU_KEYS.key16[3] == 0xBA, "key16[3]");

    return 1;
}

static void simple_tcp_RE_KEYS_OUTER_VLAN1_test_setup
(
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map * const mapper_map,
#else
    fm_uint32 reg[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyParserToMapper* inout
)
{
#ifdef USE_NEW_CSRS
    map_port_default_r * port_def = &(mapper_map->MAP_PORT_DEFAULT[0][1]);
    port_def->TARGET              = MBY_RE_KEYS_OUTER_VLAN1;
    port_def->VALUE               = 0;

    port_def         = &(mapper_map->MAP_PORT_DEFAULT[0][2]);
    port_def->TARGET = MBY_RE_KEYS_OUTER_VLAN1;
    port_def->VALUE  = 0xfef;
#else
    fm_uint32 map_port_default_vals_1[MBY_MAP_PORT_DEFAULT_WIDTH] = { 0};
    FM_ARRAY_SET_FIELD(map_port_default_vals_1, MBY_MAP_PORT_DEFAULT, TARGET, MBY_RE_KEYS_OUTER_VLAN1);
    FM_ARRAY_SET_FIELD(map_port_default_vals_1, MBY_MAP_PORT_DEFAULT, VALUE, 0);
    mbyModelWriteCSRMult(reg, MBY_MAP_PORT_DEFAULT(0, 1, 0), MBY_MAP_PORT_DEFAULT_WIDTH, map_port_default_vals_1);
    fm_uint32 map_port_default_vals_2[MBY_MAP_PORT_DEFAULT_WIDTH] = { 0 };
    FM_ARRAY_SET_FIELD(map_port_default_vals_2, MBY_MAP_PORT_DEFAULT, TARGET, MBY_RE_KEYS_OUTER_VLAN1);
    FM_ARRAY_SET_FIELD(map_port_default_vals_2, MBY_MAP_PORT_DEFAULT, VALUE, 0xfef);
    mbyModelWriteCSRMult(reg, MBY_MAP_PORT_DEFAULT(0, 2, 0), MBY_MAP_PORT_DEFAULT_WIDTH, map_port_default_vals_2);
#endif
}

static int simple_tcp_RE_KEYS_OUTER_VLAN1_test_check(const mbyMapperToClassifier* in)
{
    MAPPER_TEST_ASSERT(in->FFU_KEYS.key16[MBY_RE_KEYS_OUTER_VLAN1] == 0xFEF, "key16[MBY_RE_KEYS_OUTER_VLAN1]");

    return 1;
}

static void simple_tcp_default_forced_test_setup
(
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map * const mapper_map,
#else
    fm_uint32 reg[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyParserToMapper* inout
)
{
#ifdef USE_NEW_CSRS
    map_port_default_r * port_def = &(mapper_map->MAP_PORT_DEFAULT[0][3]);
    port_def->TARGET              = 80;
    port_def->VALUE               = 0xdede;
#else
    fm_uint32 map_port_default_vals_3[MBY_MAP_PORT_DEFAULT_WIDTH] = { 0 };
    FM_ARRAY_SET_FIELD(map_port_default_vals_3, MBY_MAP_PORT_DEFAULT, TARGET, 80);
    FM_ARRAY_SET_FIELD(map_port_default_vals_3, MBY_MAP_PORT_DEFAULT, VALUE, 0xdede);
    mbyModelWriteCSRMult(reg, MBY_MAP_PORT_DEFAULT(0, 3, 0), MBY_MAP_PORT_DEFAULT_WIDTH, map_port_default_vals_3);
#endif
}

static int simple_tcp_default_forced_test_check(const mbyMapperToClassifier* in)
{
    MAPPER_TEST_ASSERT(in->FFU_KEYS.key16[12] == 0xdede, "key16[12]");

    return 1;
}

static void activate_profile_0
(
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map * const mapper_map
#else
    fm_uint32 reg[MBY_REGISTER_ARRAY_SIZE]
#endif
)
{
    // PROFILE
#ifdef USE_NEW_CSRS
    map_profile_action_r * prof_action = &(mapper_map->MAP_PROFILE_ACTION[0]);
    prof_action->PROFILE               = 3;
    prof_action->TRIG_VALID            = 1;
    prof_action->IP_OPTIONS_MASK       = 64;
    prof_action->PROFILE_VALID         = 1;
#else
    fm_uint32 profile_reg_data[MBY_MAP_PROFILE_ACTION_WIDTH] = {0};
    // set profile no to 3
    FM_ARRAY_SET_FIELD(profile_reg_data, MBY_MAP_PROFILE_ACTION, PROFILE, 3);
    FM_ARRAY_SET_BIT(profile_reg_data, MBY_MAP_PROFILE_ACTION, TRIG_VALID, 1);
    FM_ARRAY_SET_FIELD(profile_reg_data, MBY_MAP_PROFILE_ACTION, IP_OPTIONS_MASK, 64);
    // Set PROFILE_VALID to 1
    FM_ARRAY_SET_BIT(profile_reg_data, MBY_MAP_PROFILE_ACTION, PROFILE_VALID, 1);
    mbyModelWriteCSRMult(reg, MBY_MAP_PROFILE_ACTION(0, 0), MBY_MAP_PROFILE_ACTION_WIDTH, profile_reg_data);
#endif
}

static void simple_tcp_map_smac_test_setup
(
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map * const mapper_map,
#else
    fm_uint32 reg[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyParserToMapper* inout
)
{
    unsigned char mapped_mac = 0x7E;

#ifdef USE_NEW_CSRS
    map_mac_r * map_mac = &(mapper_map->MAP_MAC[0]);
    map_mac->MAC        = 0x111111111111;
    map_mac->VALID      = 2;
    map_mac->MAP_MAC    = mapped_mac;

    activate_profile_0(mapper_map);

    // We use MAP_REWRITE 0
    // 0 nibble, id 0
    map_rewrite_r * map_rewrite = &(mapper_map->MAP_REWRITE[0][0]);
    map_rewrite->SRC_ID         = SOURCE_MAP_OUTER_SMAC_L;
    // 1 nibble, id 0
    map_rewrite                 = &(mapper_map->MAP_REWRITE[0][1]);
    map_rewrite->SRC_ID         = SOURCE_MAP_OUTER_SMAC_H;
#else

    fm_uint32 reg_data[MBY_MAP_MAC_WIDTH] = {0};
    FM_ARRAY_SET_FIELD64(reg_data, MBY_MAP_MAC, MAC, 0x111111111111);
    FM_ARRAY_SET_FIELD(reg_data, MBY_MAP_MAC, VALID, 2);
    FM_ARRAY_SET_FIELD(reg_data, MBY_MAP_MAC, MAP_MAC, mapped_mac);
    mbyModelWriteCSRMult(reg, MBY_MAP_MAC(0, 0), MBY_MAP_MAC_WIDTH, reg_data);

    activate_profile_0(reg);
    // We use MAP_REWRITE 0
    fm_uint32 rewrite_reg_data_lo[MBY_MAP_REWRITE_WIDTH] = {0};
    FM_ARRAY_SET_FIELD(rewrite_reg_data_lo, MBY_MAP_REWRITE, SRC_ID, SOURCE_MAP_OUTER_SMAC_L);
    // 0 nibble, id 0
    mbyModelWriteCSRMult(reg, MBY_MAP_REWRITE(0, 0, 0), MBY_MAP_REWRITE_WIDTH, rewrite_reg_data_lo);
    fm_uint32 rewrite_reg_data_hi[MBY_MAP_REWRITE_WIDTH] = {0};
    FM_ARRAY_SET_FIELD(rewrite_reg_data_hi, MBY_MAP_REWRITE, SRC_ID, SOURCE_MAP_OUTER_SMAC_H);
    // 1 nibble, id 0
    mbyModelWriteCSRMult(reg, MBY_MAP_REWRITE(0, 1, 0), MBY_MAP_REWRITE_WIDTH, rewrite_reg_data_hi);
#endif
}

static int simple_tcp_map_smac_test_check(const mbyMapperToClassifier* in)
{
    MAPPER_TEST_ASSERT(in->FFU_SCENARIO       == 0x3,  "FFU_SCENARIO");
    MAPPER_TEST_ASSERT(in->IP_OPTION[0]       == TRUE, "IP_OPTION[0]");
    MAPPER_TEST_ASSERT(in->FFU_KEYS.key16[13] == 0x7e, "key16[13]");

    return 1;
}

static void simple_tcp_map_prot_test_setup
(
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map * const mapper_map,
#else
    fm_uint32 reg[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyParserToMapper* inout
)
{
#ifdef USE_NEW_CSRS
    map_prot_r * map_prot = &(mapper_map->MAP_PROT[0]);
    map_prot->PROT        = 0x6; // map TCP (6)
    map_prot->MAP_PROT    = 0x5; // to 101b

    activate_profile_0(mapper_map);

    // We use MAP_REWRITE 0
    // 0 nibble, id 0
    map_rewrite_r * map_rewrite = &(mapper_map->MAP_REWRITE[0][0]);
    map_rewrite->SRC_ID         = SOURCE_MAP_OUTER_PROT;
#else
    fm_uint32 map_prot_reg_data[MBY_MAP_PROT_WIDTH] = {0};
    FM_ARRAY_SET_FIELD(map_prot_reg_data, MBY_MAP_PROT, PROT, 0x6); // map TCP (6)
    FM_ARRAY_SET_FIELD(map_prot_reg_data, MBY_MAP_PROT, MAP_PROT, 0x5); // to 101b

    mbyModelWriteCSRMult(reg, MBY_MAP_PROT(0, 0), MBY_MAP_PROT_WIDTH, map_prot_reg_data);

    activate_profile_0(reg);
    // We use MAP_REWRITE 0
    fm_uint32 rewrite_reg_data[MBY_MAP_REWRITE_WIDTH] = {0};
    FM_ARRAY_SET_FIELD(rewrite_reg_data, MBY_MAP_REWRITE, SRC_ID, SOURCE_MAP_OUTER_PROT);
    // 0 nibble, id 0
    mbyModelWriteCSRMult(reg, MBY_MAP_REWRITE(0, 0, 0), MBY_MAP_REWRITE_WIDTH, rewrite_reg_data);
#endif
}

static int simple_tcp_map_prot_test_check(const mbyMapperToClassifier* in)
{
    MAPPER_TEST_ASSERT(in->FFU_KEYS.key16[13] == 0x5, "key16[13]");

    return 1;
}


static void simple_tcp_map_l4dst_test_setup
(
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map * const mapper_map,
#else
    fm_uint32 reg[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyParserToMapper* inout
)
{
#ifdef USE_NEW_CSRS
    simple_tcp_map_prot_test_setup(mapper_map, inout);

    // map L4 dst A456h of PROT 101, valid for _outer_ L4DST
    map_l4_dst_r * map_l4_dst = &(mapper_map->MAP_L4_DST[0]);
    map_l4_dst->L4_DST        = 0xA456;
    map_l4_dst->MAP_PROT      = 0x5;
    map_l4_dst->VALID         = 0x1;
    map_l4_dst->MAP_L4_DST    = 0x1234;

    // set up all 4 nibbles!
    for (fm_uint32 i = 0;i < 4;++i)
    {
        map_rewrite_r * map_rewrite = &(mapper_map->MAP_REWRITE[0][i]);
        map_rewrite->SRC_ID         = SOURCE_MAP_OUTER_L4_DST_H - i;
    }
#else
    simple_tcp_map_prot_test_setup(reg, inout);

    fm_uint16 map_l4_to = 0x1234;

    fm_uint32 map_l4_dst_data[MBY_MAP_L4_DST_WIDTH] = {0};
    // map L4 dst A456h of PROT 101, valid for _outer_ L4DST
    FM_ARRAY_SET_FIELD(map_l4_dst_data, MBY_MAP_L4_DST, L4_DST, 0xA456);
    FM_ARRAY_SET_FIELD(map_l4_dst_data, MBY_MAP_L4_DST, MAP_PROT, 0x5);
    FM_ARRAY_SET_FIELD(map_l4_dst_data, MBY_MAP_L4_DST, VALID, 0x1);
    FM_ARRAY_SET_FIELD(map_l4_dst_data, MBY_MAP_L4_DST, MAP_L4_DST, map_l4_to);
    mbyModelWriteCSRMult(reg, MBY_MAP_L4_DST(0, 0), MBY_MAP_L4_DST_WIDTH, map_l4_dst_data);

    // set up all 4 nibbles!
    for (fm_uint32 i = 0;i < 4;++i)
    {
            fm_uint32 rewrite_reg_data_[MBY_MAP_REWRITE_WIDTH] = {0};
            FM_ARRAY_SET_FIELD(rewrite_reg_data_, MBY_MAP_REWRITE, SRC_ID, SOURCE_MAP_OUTER_L4_DST_H - i);
            // 0 nibble, id 0
            mbyModelWriteCSRMult(reg, MBY_MAP_REWRITE(0, i, 0), MBY_MAP_REWRITE_WIDTH, rewrite_reg_data_);
    }
#endif
}

static int simple_tcp_map_l4dst_test_check(const mbyMapperToClassifier* in)
{
    MAPPER_TEST_ASSERT(in->FFU_KEYS.key16[13] == 0x1234, "key16[13]");

    return 1;
}


static void simple_tcp_PRIORITY_PROFILE_test_setup
(
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map * const mapper_map,
#else
    fm_uint32 reg[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyParserToMapper* inout
)
{
#ifdef USE_NEW_CSRS
    map_domain_profile_r * domain_profile = &(mapper_map->MAP_DOMAIN_PROFILE[0]);
    domain_profile->PRIORITY_PROFILE      = 0xa;
#else
    fm_uint32 domain_profile_reg_data[MBY_MAP_DOMAIN_PROFILE_WIDTH] = {0};

    FM_ARRAY_SET_FIELD(domain_profile_reg_data, MBY_MAP_DOMAIN_PROFILE, PRIORITY_PROFILE, 0xA);

    mbyModelWriteCSRMult(reg, MBY_MAP_DOMAIN_PROFILE(0, 0), MBY_MAP_DOMAIN_PROFILE_WIDTH, domain_profile_reg_data);
#endif
}

static int simple_tcp_PRIORITY_PROFILE_test_check(const mbyMapperToClassifier* in)
{
    MAPPER_TEST_ASSERT(in->PRIORITY_PROFILE == 0xA, "PRIORITY_PROFILE");

    MAPPER_TEST_ASSERT(in->NO_PRI_ENC, "NO_PRI_ENC");

    return 1;
}


static void simple_tcp_NO_PRI_ENC_test_setup
(
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map * const mapper_map,
#else
    fm_uint32 reg[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyParserToMapper* inout
)
{
#ifdef USE_NEW_CSRS
    map_domain_action0_r * domain_act0 = &(mapper_map->MAP_DOMAIN_ACTION0[0]);
    domain_act0->DEFAULT_PRI           = 0x1;
    domain_act0->PRI_SOURCE            = (TC_SOURCE_DSCP << 6);
#else
    fm_uint32 map_domain_action0_vals[MBY_MAP_DOMAIN_ACTION0_WIDTH] = { 0 };

    FM_ARRAY_SET_FIELD(map_domain_action0_vals, MBY_MAP_DOMAIN_ACTION0, DEFAULT_PRI, 1);
    FM_ARRAY_SET_FIELD(map_domain_action0_vals, MBY_MAP_DOMAIN_ACTION0, PRI_SOURCE, (TC_SOURCE_DSCP << 6));

    mbyModelWriteCSRMult(reg, MBY_MAP_DOMAIN_ACTION0(0, 0), MBY_MAP_DOMAIN_ACTION0_WIDTH, map_domain_action0_vals);
#endif
}

static int simple_tcp_NO_PRI_ENC_test_check(const mbyMapperToClassifier* in)
{
    MAPPER_TEST_ASSERT(!in->NO_PRI_ENC, "NO_PRI_ENC");

    return 1;
}

static void simple_tcp_LEARN_MODE_test_setup
(
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map * const mapper_map,
#else
    fm_uint32 reg[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyParserToMapper* inout
)
{
#ifdef USE_NEW_CSRS
    map_domain_action0_r * domain_act0 = &(mapper_map->MAP_DOMAIN_ACTION0[0]);
    domain_act0->LEARN_MODE            = TRUE;
#else
    fm_uint32 map_domain_action0_vals[MBY_MAP_DOMAIN_ACTION0_WIDTH] = { 0 };

    FM_ARRAY_SET_BIT(map_domain_action0_vals, MBY_MAP_DOMAIN_ACTION0, LEARN_MODE, TRUE);

    mbyModelWriteCSRMult(reg, MBY_MAP_DOMAIN_ACTION0(0, 0), MBY_MAP_DOMAIN_ACTION0_WIDTH, map_domain_action0_vals);
#endif
}

static int simple_tcp_LEARN_MODE_test_check(const mbyMapperToClassifier* in)
{
    MAPPER_TEST_ASSERT((in->LEARN_MODE == TRUE), "LEARN_MODE");

    return 1;
}

static void simple_tcp_VLAN_COUNTER_test_setup
(
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map * const mapper_map,
#else
    fm_uint32 reg[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyParserToMapper* inout
)
{
#ifdef USE_NEW_CSRS
    map_domain_action1_r * domain_act1 = &(mapper_map->MAP_DOMAIN_ACTION1[0]);
    domain_act1->VLAN_COUNTER          = 0x805;
#else
    fm_uint32 map_domain_action1_vals[MBY_MAP_DOMAIN_ACTION1_WIDTH] = { 0 };

    FM_ARRAY_SET_FIELD(map_domain_action1_vals, MBY_MAP_DOMAIN_ACTION1, VLAN_COUNTER, 0x805);

    mbyModelWriteCSRMult(reg, MBY_MAP_DOMAIN_ACTION1(0, 0), MBY_MAP_DOMAIN_ACTION1_WIDTH, map_domain_action1_vals);
#endif
}

static int simple_tcp_VLAN_COUNTER_test_check(const mbyMapperToClassifier* in)
{
    MAPPER_TEST_ASSERT(in->L2_IVLAN1_CNT == 0x805, "L2_IVLAN1_CNT");

    return 1;
}

int main()
{
    printf("--------------------------------------------------------------------------------\n");

    fm_uint tests = 0;
    fm_uint fails = 0;

    SIMPLE_TCP_TEST(basic,               fails); tests++;
    SIMPLE_TCP_TEST(act24_default,       fails); tests++;
    SIMPLE_TCP_TEST(act4_default,        fails); tests++;
    SIMPLE_TCP_TEST(default0,            fails); tests++;
    SIMPLE_TCP_TEST(RE_KEYS_OUTER_VLAN1, fails); tests++;
    SIMPLE_TCP_TEST(default_forced,      fails); tests++;
    SIMPLE_TCP_TEST(map_smac,            fails); tests++;
    SIMPLE_TCP_TEST(map_prot,            fails); tests++;
    SIMPLE_TCP_TEST(map_l4dst,           fails); tests++;
    SIMPLE_TCP_TEST(PRIORITY_PROFILE,    fails); tests++;
    SIMPLE_TCP_TEST(NO_PRI_ENC,          fails); tests++;
    SIMPLE_TCP_TEST(LEARN_MODE,          fails); tests++;
    SIMPLE_TCP_TEST(VLAN_COUNTER,        fails); tests++;

    fm_uint passes = (tests > fails) ? tests - fails : 0;

    printf("--------------------------------------------------------------------------------\n");

    if (fails == 0)
        printf(COLOR_GREEN "[pass]");
    else
        printf(COLOR_RED   "[FAIL]");

    printf(" %2d/%2d - Mapper tests\n" COLOR_RESET, passes, tests);

    printf("--------------------------------------------------------------------------------\n");

    int rv = (fails == 0) ? 0 : -1;
    return rv;
}
