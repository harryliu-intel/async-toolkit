* XA options
.OPTION CMIFLAG=1 CMIUSRFLAG=3 PDMI=1
.OPTION POST=fsdb PROBE=1
.OPTION XA_CMD="set_sim_level -level 6"
.OPTION XA_CMD="set_wildcard_rule -match* one"
.OPTION XA_CMD="set_message_option -limit 100"
.OPTION XA_CMD="enable_print_statement 1"
.OPTION XA_CMD="set_sim_case -case sensitive"

* parameters
* .TEMP temp
* .PARAM spice_temps=temp

* Performance corners
* PFFF, 0.75V,  125C and -40C
* TTTT, 0.815V, 125C and -40C
* PSSS, 0.88V,  125C and -40C

* Power corner
* TTTT, 0.915V, 105C

* for PRS environments
.param Vlo='vtrue*0.45'
.param Vhi='vtrue*0.55'
.param PrsCap=1e-13
.param PrsMaxRes=1e9
.param PrsMinRes=30
.param PrsDelay=6e-12
