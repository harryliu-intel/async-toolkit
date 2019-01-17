
source $::env(MODEL_ROOT)/tools/upf/scripts/merge_munge/merge_upf_config.tcl

use_pst {
    # --- Power states and PST
add_power_state "ss_v3p3" -state "v_3p30" "-supply_expr {power == `{FULL_ON,3.30}} -simstate NORMAL"
add_power_state "ss_v3p3" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vccfa_ehv" -state "v_1p80" "-supply_expr {power == `{FULL_ON,1.80}} -simstate NORMAL"
add_power_state "ss_vccfa_ehv" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vnn" -state "v_0p75" "-supply_expr {power == `{FULL_ON,0.75}} -simstate NORMAL"
add_power_state "ss_vnn" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vnn" -state "v_0p00" "-supply_expr {ground == `{FULL_ON,0.00}} -simstate NORMAL"
add_power_state "ss_vccsram" -state "v_0p85" "-supply_expr {power == `{FULL_ON,0.85}} -simstate NORMAL"
add_power_state "ss_vccsram" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vddq" -state "v_1p10" "-supply_expr {power == `{FULL_ON,1.10}} -simstate NORMAL"
add_power_state "ss_vddq" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vddqlp" -state "v_1p10" "-supply_expr {power == `{FULL_ON,1.10}} -simstate NORMAL"
add_power_state "ss_vddqlp" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vddrx_ephy" -state "v_1p15" "-supply_expr {power == `{FULL_ON,1.15}} -simstate NORMAL"
add_power_state "ss_vddrx_ephy" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vddtx_ephy" -state "v_1p15" "-supply_expr {power == `{FULL_ON,1.15}} -simstate NORMAL"
add_power_state "ss_vddtx_ephy" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vddck_ephy" -state "v_1p15" "-supply_expr {power == `{FULL_ON,1.15}} -simstate NORMAL"
add_power_state "ss_vddck_ephy" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vcc" -state "v_1p00" "-supply_expr {power == `{FULL_ON,1.00}} -simstate NORMAL"
add_power_state "ss_vcc" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"

add_power_state "ss_vccfa_pphy_ehv" -state "v_1p80" "-supply_expr {power == `{FULL_ON,1.80}} -simstate NORMAL"
add_power_state "ss_vccfa_pphy_ehv" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vccfa_dphy_ehv" -state "v_1p80" "-supply_expr {power == `{FULL_ON,1.80}} -simstate NORMAL"
add_power_state "ss_vccfa_dphy_ehv" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vccfa_fuse_ehv" -state "v_1p80" "-supply_expr {power == `{FULL_ON,1.80}} -simstate NORMAL"
add_power_state "ss_vccfa_fuse_ehv" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vccfa_pll_ehv" -state "v_1p80" "-supply_expr {power == `{FULL_ON,1.80}} -simstate NORMAL"
add_power_state "ss_vccfa_pll_ehv" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vccfa_lvds_ehv" -state "v_1p80" "-supply_expr {power == `{FULL_ON,1.80}} -simstate NORMAL"
add_power_state "ss_vccfa_lvds_ehv" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vccfa_xtal_ehv" -state "v_1p80" "-supply_expr {power == `{FULL_ON,1.80}} -simstate NORMAL"
add_power_state "ss_vccfa_xtal_ehv" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"

add_power_state "ss_vnnq" -state "v_0p75" "-supply_expr {power == `{FULL_ON,0.75}} -simstate NORMAL"
add_power_state "ss_vnnq" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vnnvptx_pphy" -state "v_0p75" "-supply_expr {power == `{FULL_ON,0.75}} -simstate NORMAL"
add_power_state "ss_vnnvptx_pphy" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vnnvpdig_dphy" -state "v_0p75" "-supply_expr {power == `{FULL_ON,0.75}} -simstate NORMAL"
add_power_state "ss_vnnvpdig_dphy" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vnnvp_pphy" -state "v_0p75" "-supply_expr {power == `{FULL_ON,0.75}} -simstate NORMAL"
add_power_state "ss_vnnvp_pphy" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vnn_ephy" -state "v_0p75" "-supply_expr {power == `{FULL_ON,0.75}} -simstate NORMAL"
add_power_state "ss_vnn_ephy" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"
add_power_state "ss_vccddr" -state "v_0p75" "-supply_expr {power == `{FULL_ON,0.75}} -simstate NORMAL"
add_power_state "ss_vccddr" -state "v_off" "-supply_expr {power == `{OFF}} -simstate CORRUPT"


### FULL CHIP vccfa_ehv*(1.8) SUPPLIES PST
create_pst "soc_vccfa_ehv_pd_pst" -supplies                         "ss_vccfa_ehv.power      ss_vccfa_pphy_ehv.power ss_vccfa_dphy_ehv.power ss_vccfa_fuse.power ss_vccfa_pll_ehv.power ss_vccfa_lvds.power ss_vccfa_xtal_ehv.power"
add_pst_state "p_v_1p80_all_on"  -pst  "soc_vccfa_ehv_pd_pst" -state "v_1p80                  v_1p80                  v_1p80                  v_1p80              v_1p80                 v_1p80              v_1p80"
add_pst_state "p_v_1p80_all_off" -pst "soc_vccfa_ehv_pd_pst"  -state "v_off                   v_off                   v_off                   v_off               v_off                  v_off               v_off"

### FULL CHIP VNN* SUPPLIES PST
create_pst "soc_vnn_pd_pst" -supplies                         "ss_vnn.power  ss_vnnq.power ss_vnnvptx_pphy.power ss_vnnvpdig_dphy.power ss_vnnvp_pphy.power ss_vnn_ephy.power ss_vccddr.power"
add_pst_state "p_v_0p75_all_on"  -pst  "soc_vnn_pd_pst" -state "v_0p75        v_0p75        v_ 0p75               v_0p75                 v_0p75              v_0p75            v_0p75"
add_pst_state "p_v_0p75_all_off"  -pst "soc_vnn_pd_pst" -state "v_off         v_off         v_off                 v_off                  v_off               v_off             v_off"

### FULL CHIP VDDQ* SUPPLIES PST
create_pst "soc_vddq_pd_pst" -supplies                       "ss_vddq.power  ss_vddqlp.power"
add_pst_state "p_vddq_all_on"  -pst  "soc_vddq_pd_pst" -state "v_1p10         v_1p10"
add_pst_state "p_vddq_all_off" -pst  "soc_vddq_pd_pst" -state "v_off          v_off"

### FULL CHIP VDD*_EPHY SUPPLIES PST
create_pst "soc_vdd_ephy_pd_pst" -supplies                        "ss_vddrx_ephy.power  ss_vddtx_ephy.power ss_vddck_ephy.power"
add_pst_state "p_v1p15_all_on"  -pst  "soc_vdd_ephy_pd_pst" -state "v_1p15               v_1p15              v_1p15"
add_pst_state "p_v1p15_all_off" -pst  "soc_vdd_ephy_pd_pst" -state "v_off                v_off               v_off"


### FULL CHIP PRIMARY SUPPLIES PST
create_pst "soc_pd_pst" -supplies                   "ss_v3p3.power ss_vccfa_ehv.power ss_vnn.power ss_vccsram.power ss_vddq.power ss_vddrx_ephy.power ss_vcc.power ss_vnn.ground"
add_pst_state "p_0_all_on"  -pst "soc_pd_pst" -state "v_3p30        v_1p80             v_0p75       v_0p85           v_1p10        v_1p15              v_1p00       v_0p00"
add_pst_state "p_1"         -pst "soc_pd_pst" -state "v_3p30        v_1p80             v_0p75       v_0p85           v_1p10        v_1p15              v_off        v_0p00"
add_pst_state "p_2"         -pst "soc_pd_pst" -state "v_3p30        v_1p80             v_0p75       v_0p85           v_1p10        v_off               v_off        v_0p00"
add_pst_state "p_3"         -pst "soc_pd_pst" -state "v_3p30        v_1p80             v_0p75       v_0p85           v_off         v_off               v_off        v_0p00"
add_pst_state "p_4"         -pst "soc_pd_pst" -state "v_3p30        v_1p80             v_0p75       v_off            v_off         v_off               v_off        v_0p00"
add_pst_state "p_5"         -pst "soc_pd_pst" -state "v_3p30        v_1p80             v_off        v_off            v_off         v_off               v_off        v_0p00"
add_pst_state "p_6"         -pst "soc_pd_pst" -state "v_3p30        v_off              v_off        v_off            v_off         v_off               v_off        v_0p00"
add_pst_state "p_7_all_off" -pst "soc_pd_pst" -state "v_off         v_off              v_off        v_off            v_off         v_off               v_off        v_0p00"
}
