# Madison Bay - C Functional Model

Browsable documentation for Madison Bay C Functional Model.

## Pipeline stages

Documentation of the Madison Bay PPE:

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
* Work in progress

Note: the link >> points to the struct used to exchange data between the blocks

## C Client

Documentation of the [C client](@ref c_client) that can be used to:
* Start/stop the functional model
* Read/write registers
* Send/receive packets


## Useful links

* [MBY FM Jira Wiki](https://securewiki.ith.intel.com/display/MBYFM/) 

## Organization

The **Data Structures** tab is organized by data structure name. Click a
highlighted name to see the reference page for that structure.

The **Files** tab is organized by directory and file name. Click a highlighted
file name to see the reference page for that file.

The **Search** box may be used to search for a matching symbol or file name.


