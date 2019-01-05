# Madison Bay - C Functional Model

Browsable documentation for Madison Bay C Functional Model.

## Pipeline stages

Documentation of the Madison Bay RX and TX PPE.

Note: the link >> points to the struct used to exchange data between the blocks.

[RxPipeline](@ref RxPipeline):

* [>>](@ref mbyRxMacToParser)
[|Parser|](@ref Parser) [>>](@ref mbyParserToMapper)
[|Mapper|](@ref Mapper) [>>](@ref mbyMapperToClassifier)
[|Classifier|](@ref Classifier) [>>](@ref mbyClassifierToHash)
[|Hash|](@ref Hash) [>>](@ref mbyHashToNextHop)
[|NextHop|](@ref NextHop) [>>](@ref mbyNextHopToMaskGen)
[|MaskGen|](@ref MaskGen) [>>](@ref mbyMaskGenToTriggers)
[|Triggers|](@ref Triggers) [>>](@ref mbyTriggersToCongMgmt)
[|CongMgmt|](@ref CongMgmt) [>>](@ref mbyCongMgmtToRxStats)
[|RxStats|](@ref RxStats) [>>](@ref mbyRxStatsToRxOut)

[TxPipeline](@ref TxPipeline):

* [>>](@ref mbyTxInToModifier)
[|Modifier|](@ref Modifier) [>>](@ref mbyModifierToTxStats)
[|TxStats|](@ref TxStats) [>>](@ref mbyTxStatsToTxMac)


## C Client

Documentation of the [C client](@ref c_client) that can be used to:
* [Start](@ref wm_server_start)/[stop](@ref wm_server_stop) the functional model
* [Read](@ref wm_reg_read)/[write](@ref wm_reg_write) registers
* [Send](@ref wm_pkt_push)/[receive](@ref wm_pkt_get) [packets](@ref wm_pkt)


## Useful links

* [MBY FM Jira Wiki](https://securewiki.ith.intel.com/display/MBYFM/) 
* [MBY FM Register Doc](https://madisonbay.intel.com/mby-mby-x0-latest/target/GenRTL/regflow/mby/mby_reg_doc.html) 

## Organization

The **Data Structures** tab is organized by data structure name. Click a
highlighted name to see the reference page for that structure.

The **Files** tab is organized by directory and file name. Click a highlighted
file name to see the reference page for that file.

The **Search** box may be used to search for a matching symbol or file name.


