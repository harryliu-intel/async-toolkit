#! /usr/bin/csh -f

set jar     = /usr/share/java/jnaerator-0.12-SNAPSHOT-20130727.jar
set runtime = BridJ
set arch    = linux_x64
#et mode    = Jar
#et mode    = StandaloneJar
set mode    = Directory
set library = Wm
set package = HlpWm
set full_package = java.HlpWm
set outdir  = src/java
set tmpdir  = tmp

set prefix  = ../../../../nd_ies-hlp_wm/ies

set defines_ = ( \
    _GNU_SOURCE \
    _FM_ARCH_x86_64 \
    INSTRUMENT_LOG_LEVEL=0 \
    DMPINJ_MODS \
)

set defines = ""
foreach define ($defines_)
    set defines = "${defines} -D${define}"
end

# echo "defines = $defines"
# exit

set includes_ = ( \
    . \
    ${prefix}/include \
    ${prefix}/include/platforms \
    ${prefix}/include/platforms/whiteModelLib \
    ${prefix}/include/platforms/whiteModelLib/kernel \
    ${prefix}/testing/include \
    ${prefix}/include/alos \
    ${prefix}/include/alos/linux \
    ${prefix}/include/std/intel \
    ${prefix}/applications/support/include \
    ${prefix}/applications/protocols/include \
    ${prefix}/examples/expmt/src \
    /usr/include \
    /usr/include/x86_64-linux-gnu \
)

set includes = ""
foreach i (${includes_})
    set includes = "${includes} -I${i}"
end

#echo $includes
#exit

set headers_ALL = ( \
\
    ${prefix}/include/fm_sdk.h \
    ${prefix}/include/alos/linux/fm_alos_sys.h \
    ${prefix}/include/std/intel/fm_std.h \
    ${prefix}/include/common/fm_common.h \
    ${prefix}/include/common/fm_errno.h \
    ${prefix}/include/common/fm_tree.h \
    ${prefix}/include/common/fm_dlist.h \
    ${prefix}/include/common/fm_bitarray.h \
    ${prefix}/include/common/fm_bitfield.h \
    ${prefix}/include/common/fm_crc32.h \
    ${prefix}/include/common/fm_plusargs.h \
    ${prefix}/include/common/fm_attr.h \
    ${prefix}/include/common/fm_string.h \
    ${prefix}/include/common/fm_md5.h \
    ${prefix}/include/common/fm_lock_prec.h \
    ${prefix}/include/common/fm_array.h \
    ${prefix}/include/common/fm_graycode.h \
    ${prefix}/include/common/fm_c11_annex_k.h \
    ${prefix}/include/platforms/whiteModelLib/platform_defines.h \
    ${prefix}/include/alos/fm_alos.h \
    ${prefix}/include/alos/fm_alos_logging.h \
    ${prefix}/include/alos/fm_alos_init.h \
    ${prefix}/include/alos/fm_alos_time.h \
    ${prefix}/include/alos/fm_alos_lock.h \
    ${prefix}/include/alos/fm_alos_rwlock.h \
    ${prefix}/include/alos/fm_alos_sem.h \
    ${prefix}/include/alos/fm_alos_event_queue.h \
    ${prefix}/include/alos/fm_alos_threads.h \
    ${prefix}/include/alos/fm_alos_alloc.h \
    ${prefix}/include/alos/fm_alos_dynamic_load.h \
    ${prefix}/include/alos/fm_alos_rand.h \
    ${prefix}/include/alos/fm_alos_debughash.h \
    ${prefix}/include/api/fm_api.h \
    ${prefix}/include/api/fm_api_common.h \
    ${prefix}/include/api/fm_api_init.h \
    ${prefix}/include/api/fm_api_regs.h \
    ${prefix}/include/api/fm_api_buffer.h \
    ${prefix}/include/api/fm_api_packet.h \
    ${prefix}/include/api/fm_api_routing.h \
    ${prefix}/include/api/fm_api_event_mac_maint.h \
    ${prefix}/include/api/fm_api_event_types.h \
    ${prefix}/include/api/fm_api_event_mgmt.h \
    ${prefix}/include/api/fm_api_attr.h \
    ${prefix}/include/api/fm_api_vlan.h \
    ${prefix}/include/api/fm_api_stp.h \
    ${prefix}/include/api/fm_api_addr.h \
    ${prefix}/include/api/fm_api_lag.h \
    ${prefix}/include/api/fm_api_multicast.h \
    ${prefix}/include/api/fm_api_storm.h \
    ${prefix}/include/api/fm_api_acl.h \
    ${prefix}/include/api/fm_api_mapper.h \
    ${prefix}/include/api/fm_api_stats.h \
    ${prefix}/include/api/fm_api_port.h \
    ${prefix}/include/api/fm_api_mirror.h \
    ${prefix}/include/api/fm_api_qos.h \
    ${prefix}/include/api/fm_api_ffu.h \
    ${prefix}/include/api/fm_api_pkt.h \
    ${prefix}/include/api/fm_api_policer.h \
    ${prefix}/include/api/fm_api_lbg.h \
    ${prefix}/include/api/fm_api_swag.h \
    ${prefix}/include/api/fm_api_stacking.h \
    ${prefix}/include/api/fm_api_sflow.h \
    ${prefix}/include/api/fm_api_trigger.h \
    ${prefix}/include/api/fm_api_crm.h \
    ${prefix}/include/api/fm_api_flow.h \
    ${prefix}/include/api/fm_api_replication.h \
    ${prefix}/include/api/fm_api_rbridge.h \
    ${prefix}/include/api/fm_api_vn.h \
    ${prefix}/include/debug/fm_debug.h \
    ${prefix}/include/fm_sdk_hlp_int.h \
    ${prefix}/include/fm_sdk_int.h \
    ${prefix}/include/alos/linux/fm_alos_sys_int.h \
    ${prefix}/include/alos/linux/fm_alos_int.h \
    ${prefix}/include/alos/linux/fm_alos_logging_int.h \
    ${prefix}/include/alos/linux/fm_alos_dyn_load_int.h \
    ${prefix}/include/alos/linux/fm_alos_init_int.h \
    ${prefix}/include/alos/linux/fm_alos_threads_int.h \
    ${prefix}/include/debug/fm_debug_int.h \
    ${prefix}/include/api/internal/fm_api_int.h \
    ${prefix}/include/api/internal/fm_api_common_int.h \
    ${prefix}/include/api/internal/fm_api_portmask.h \
    ${prefix}/include/api/internal/fm_api_lag_int.h \
    ${prefix}/include/api/internal/fm_api_lport_int.h \
    ${prefix}/include/api/internal/fm_api_addr_int.h \
    ${prefix}/include/api/internal/fm_api_cardinal_int.h \
    ${prefix}/include/api/internal/fm_api_event_mac_maint_int.h \
    ${prefix}/include/api/internal/fm_api_vlan_int.h \
    ${prefix}/include/api/internal/fm_api_stp_int.h \
    ${prefix}/include/api/internal/fm_api_routing_int.h \
    ${prefix}/include/api/internal/fm_api_mcast_groups_int.h \
    ${prefix}/include/api/internal/fm_api_lbg_int.h \
    ${prefix}/include/api/internal/fm_api_events_int.h \
    ${prefix}/include/api/internal/fm_api_stacking_int.h \
    ${prefix}/include/api/internal/fm_api_fibm_int.h \
    ${prefix}/include/api/internal/fm_api_acl_int.h \
    ${prefix}/include/api/internal/fm_api_mirror_int.h \
    ${prefix}/include/api/internal/fm_api_stat_int.h \
    ${prefix}/include/api/internal/fm_api_vn_int.h \
    ${prefix}/include/api/internal/fm_api_switch_int.h \
    ${prefix}/include/api/internal/fm_api_port_int.h \
    ${prefix}/include/api/internal/fm_api_init_int.h \
    ${prefix}/include/api/internal/fm_api_qos_int.h \
    ${prefix}/include/api/internal/fm_api_regs_cache_int.h \
    ${prefix}/include/api/internal/fm_api_root_int.h \
    ${prefix}/include/platforms/platform.h \
    ${prefix}/include/platforms/whiteModelLib/platform_types.h \
    ${prefix}/include/platforms/common/model/fm_model_message.h \
    ${prefix}/include/platforms/common/model/hlp/hlp_model.h \
    ${prefix}/include/platforms/platform_defaults.h \
    ${prefix}/include/platforms/platform_api.h \
    ${prefix}/include/platforms/platform_app.h \
    ${prefix}/include/api/internal/hlp/hlp_api_regs_int.h \
    ${prefix}/include/api/internal/hlp/hlp_api_hw_int.h \
    ${prefix}/include/api/internal/hlp/hlp_api_debug_int.h \
    ${prefix}/include/api/internal/hlp/hlp_api_port_int.h \
    ${prefix}/include/api/internal/hlp/hlp_api_vlan_int.h \
    ${prefix}/include/api/internal/hlp/hlp_api_switch_int.h \
    ${prefix}/include/platforms/common/model/hlp/hlp_model_types.h \
    ${prefix}/include/platforms/common/model/hlp/hlp_model_state.h \
#   ${prefix}/examples/expmt/src/msg_dump_inject.h \
    ${prefix}/examples/expmt/src/c/model_server.h \
)

# Temporary override:
set headers = ( \
\
    # ${prefix}/include/alos/fm_alos.h \
    # ${prefix}/include/alos/fm_alos_alloc.h \
    # ${prefix}/include/alos/fm_alos_debughash.h \
    # ${prefix}/include/alos/fm_alos_dynamic_load.h \
    # ${prefix}/include/alos/fm_alos_event_queue.h \
    # ${prefix}/include/alos/fm_alos_init.h \
    # ${prefix}/include/alos/fm_alos_lock.h \
    # ${prefix}/include/alos/fm_alos_logging.h \
    # ${prefix}/include/alos/fm_alos_rand.h \
    # ${prefix}/include/alos/fm_alos_rwlock.h \
    # ${prefix}/include/alos/fm_alos_sem.h \
    # ${prefix}/include/alos/fm_alos_threads.h \
    # ${prefix}/include/alos/fm_alos_time.h \
    # ${prefix}/include/alos/linux/fm_alos_dyn_load_int.h \
    # ${prefix}/include/alos/linux/fm_alos_init_int.h \
    # ${prefix}/include/alos/linux/fm_alos_int.h \
    # ${prefix}/include/alos/linux/fm_alos_logging_int.h \
    # ${prefix}/include/alos/linux/fm_alos_sys.h \
    # ${prefix}/include/alos/linux/fm_alos_sys_int.h \
    # ${prefix}/include/alos/linux/fm_alos_threads_int.h \
    # ${prefix}/include/api/fm_api.h \
    # ${prefix}/include/api/fm_api_acl.h \
    # ${prefix}/include/api/fm_api_addr.h \
    # ${prefix}/include/api/fm_api_attr.h \
    # ${prefix}/include/api/fm_api_buffer.h \
    # ${prefix}/include/api/fm_api_common.h \
    # ${prefix}/include/api/fm_api_crm.h \
    # ${prefix}/include/api/fm_api_event_mac_maint.h \
    # ${prefix}/include/api/fm_api_event_mgmt.h \
    # ${prefix}/include/api/fm_api_event_types.h \
    # ${prefix}/include/api/fm_api_ffu.h \
    # ${prefix}/include/api/fm_api_flow.h \
    # ${prefix}/include/api/fm_api_init.h \
    # ${prefix}/include/api/fm_api_lag.h \
    # ${prefix}/include/api/fm_api_lbg.h \
    # ${prefix}/include/api/fm_api_mapper.h \
    # ${prefix}/include/api/fm_api_mirror.h \
    # ${prefix}/include/api/fm_api_multicast.h \
    # ${prefix}/include/api/fm_api_packet.h \
    # ${prefix}/include/api/fm_api_pkt.h \
    # ${prefix}/include/api/fm_api_policer.h \
    # ${prefix}/include/api/fm_api_port.h \
    # ${prefix}/include/api/fm_api_qos.h \
    # ${prefix}/include/api/fm_api_rbridge.h \
    # ${prefix}/include/api/fm_api_regs.h \
    # ${prefix}/include/api/fm_api_replication.h \
    # ${prefix}/include/api/fm_api_routing.h \
    # ${prefix}/include/api/fm_api_sflow.h \
    # ${prefix}/include/api/fm_api_stacking.h \
    # ${prefix}/include/api/fm_api_stats.h \
    # ${prefix}/include/api/fm_api_storm.h \
    # ${prefix}/include/api/fm_api_stp.h \
    # ${prefix}/include/api/fm_api_swag.h \
    # ${prefix}/include/api/fm_api_trigger.h \
    # ${prefix}/include/api/fm_api_vlan.h \
    # ${prefix}/include/api/fm_api_vn.h \
    # ${prefix}/include/api/internal/fm_api_acl_int.h \
    # ${prefix}/include/api/internal/fm_api_addr_int.h \
    # ${prefix}/include/api/internal/fm_api_cardinal_int.h \
    # ${prefix}/include/api/internal/fm_api_common_int.h \
    # ${prefix}/include/api/internal/fm_api_event_mac_maint_int.h \
    # ${prefix}/include/api/internal/fm_api_events_int.h \
    # ${prefix}/include/api/internal/fm_api_fibm_int.h \
    # ${prefix}/include/api/internal/fm_api_init_int.h \
    # ${prefix}/include/api/internal/fm_api_int.h \
    # ${prefix}/include/api/internal/fm_api_lag_int.h \
    # ${prefix}/include/api/internal/fm_api_lbg_int.h \
    # ${prefix}/include/api/internal/fm_api_lport_int.h \
    # ${prefix}/include/api/internal/fm_api_mcast_groups_int.h \
    # ${prefix}/include/api/internal/fm_api_mirror_int.h \
    # ${prefix}/include/api/internal/fm_api_port_int.h \
    # ${prefix}/include/api/internal/fm_api_portmask.h \
    # ${prefix}/include/api/internal/fm_api_qos_int.h \
    # ${prefix}/include/api/internal/fm_api_regs_cache_int.h \
    # ${prefix}/include/api/internal/fm_api_root_int.h \
    # ${prefix}/include/api/internal/fm_api_routing_int.h \
    # ${prefix}/include/api/internal/fm_api_stacking_int.h \
    # ${prefix}/include/api/internal/fm_api_stat_int.h \
    # ${prefix}/include/api/internal/fm_api_stp_int.h \
    # ${prefix}/include/api/internal/fm_api_switch_int.h \
    # ${prefix}/include/api/internal/fm_api_vlan_int.h \
    # ${prefix}/include/api/internal/fm_api_vn_int.h \
    # ${prefix}/include/api/internal/hlp/hlp_api_debug_int.h \
    # ${prefix}/include/api/internal/hlp/hlp_api_hw_int.h \
    # ${prefix}/include/api/internal/hlp/hlp_api_port_int.h \
    # ${prefix}/include/api/internal/hlp/hlp_api_regs_int.h \
    # ${prefix}/include/api/internal/hlp/hlp_api_switch_int.h \
    # ${prefix}/include/api/internal/hlp/hlp_api_vlan_int.h \
    # ${prefix}/include/common/fm_array.h \
    # ${prefix}/include/common/fm_attr.h \
    # ${prefix}/include/common/fm_bitarray.h \
    # ${prefix}/include/common/fm_bitfield.h \
    # ${prefix}/include/common/fm_c11_annex_k.h \
    # ${prefix}/include/common/fm_common.h \
    # ${prefix}/include/common/fm_crc32.h \
    # ${prefix}/include/common/fm_dlist.h \
    # ${prefix}/include/common/fm_errno.h \
    # ${prefix}/include/common/fm_graycode.h \
    # ${prefix}/include/common/fm_lock_prec.h \
    # ${prefix}/include/common/fm_md5.h \
    # ${prefix}/include/common/fm_plusargs.h \
    # ${prefix}/include/common/fm_string.h \
    # ${prefix}/include/common/fm_tree.h \
    # ${prefix}/include/debug/fm_debug.h \
    # ${prefix}/include/debug/fm_debug_int.h \
    # ${prefix}/include/fm_sdk.h \
    # ${prefix}/include/fm_sdk_hlp_int.h \
    # ${prefix}/include/fm_sdk_int.h \
    # ${prefix}/include/platforms/platform.h \
    # ${prefix}/include/platforms/platform_api.h \
    # ${prefix}/include/platforms/platform_app.h \
    # ${prefix}/include/platforms/platform_defaults.h \
    # -------------------------------------------------------------------------------- \
\
    ${prefix}/examples/expmt/src/c/msg_dump_inject.h \
\
    ${prefix}/examples/expmt/src/c/model_server.h \
\
    ${prefix}/include/common/fm_errno.h \
\
    ${prefix}/include/platforms/whiteModelLib/platform_types.h \
\
    ${prefix}/include/platforms/common/model/hlp/hlp_model_state.h \
\
    # Note: hlp_model_types.h needs: \
    #     - fm_alos_time.h (struct fm_timestamp) \
    #     - fm_api_routing.h (struct fm_ipAddr) \
    #     - fm_alos_lock.h (struct fm_lock) \
    #     - hlp_api_regs_int.h (struct hlpMaTable) \
    ${prefix}/include/alos/fm_alos_time.h \
    ${prefix}/include/api/fm_api_routing.h \
    ${prefix}/include/alos/fm_alos_lock.h \
    ${prefix}/include/api/internal/hlp/hlp_api_regs_int.h \
    \
    ${prefix}/include/platforms/common/model/hlp/hlp_model_types.h \
\
    # Note: hlp_model.h needs fm_api_event_types.h (struct fm_eventPktRecv): \
    ${prefix}/include/api/fm_api_event_types.h \
    ${prefix}/include/platforms/common/model/hlp/hlp_model.h \
\
    # platform_defines.h defines macro FM_MODEL_MAX_TLV_SIZE: \
    ${prefix}/include/platforms/whiteModelLib/platform_defines.h \
\
    # fm_model_message.h defines structs fm_modelMessage, fm_modelSidebandData and fm_modelDataTlv: \
    ${prefix}/include/platforms/common/model/fm_model_message.h \
\
    # fm_std.h defines the standard fm data types: \
    ${prefix}/include/std/intel/fm_std.h \
)

# echo "headers = $headers"
# exit

rm -rf _jnaerator.* ${outdir}/${package} ${tmpdir}/*

/usr/bin/java \
    -jar     $jar \
    -runtime $runtime \
    -arch    $arch \
    -mode    $mode \
    -library $library \
    -package $package \
    -o       $outdir \
    -v \
    -nocpp \
    -noAuto \
    -noComp \
    $defines \
    $includes \
    $headers

mv _jnaerator.* $tmpdir

set num_java_files = `ls -l ${outdir}/${package}/*.java | wc -l`
#echo  "ls -l ${outdir}/${package}/*.java | wc -l"
echo ""
echo "num_java_files = $num_java_files"

echo ""
echo "Checking for conversion errors:"
egrep 'Conversion Error' ${outdir}/${package}/*.java
