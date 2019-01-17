
source $::env(MODEL_ROOT)/tools/upf/scripts/merge_munge/merge_upf_config.tcl

use_pst {
# --- Power states and PST
add_power_state "ss_vnn" -state "v_0p75" "-supply_expr {power == `{FULL_ON,0.75}} -simstate NORMAL"
add_power_state "ss_vnn" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vnn" -state "v_0p00" "-supply_expr {ground == `{FULL_ON,0.00}} -simstate NORMAL"

create_pst "vnn_island_paracctemp5_pd_pst" -supplies "ss_vnn.power ss_vnn.ground"
add_pst_state "p_0" -pst "vnn_island_paracctemp5_pd_pst" -state "v_0p75 v_0p00"
add_pst_state "p_1" -pst "vnn_island_paracctemp5_pd_pst" -state "v_off  v_0p00"
}
