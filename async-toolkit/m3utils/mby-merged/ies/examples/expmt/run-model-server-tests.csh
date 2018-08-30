#! /usr/bin/csh -f

# List of test cases using a single processor as client:
set files_1 = ( \
    LBGConfig_hlp-model_583673831 \
    MACTableApi_hlp-model_1422577372 \
    MACTableApi_hlp-model+noEvents_241318615 \
    MACTableApi_hlp-model+nonBlocking_254769544 \
    SampleAddrFlush_hlp-model+no-lttng_616988304 \
#   AclActionRam_hlp-model \
#    AclActions_hlp-model+CPM-enabled+exactMatchTable0 \
#    AclActions_hlp-model+CPM-enabled+exactMatchTable1 \
#    AclActions_hlp-model+CPM-enabled \
#    AclApiFunctions_hlp-model \
#    AclChain_hlp-model+emt02tcam1+no_scenario_change \
#    AclChain_hlp-model+emt12tcam2+no_scenario_change \
#    AclChain_hlp-model+emt2emt+no_scenario_change \
#    AclChain_hlp-model+no_scenario_change \
#    AclChain_hlp-model+tcam02emt1+no_scenario_change \
#    AclChain_hlp-model+tcam12emt2+change_scenario \
#    AclChain_hlp-model+tcam12emt2+no_scenario_change \
#    AclCondFragFrame_hlp-model \
#    AclConditionsMasks_hlp-model+random_mask \
#    AclConditionsMasks_hlp-model \
#    AclConditions_hlp-model+CPM-enabled+exactMatchTable0 \
#    AclConditions_hlp-model+CPM-enabled+exactMatchTable1 \
#    AclConditions_hlp-model+CPM-enabled \
#    AclDeepInspectionL2_hlp-model \
#    AclDeepInspectionL3_hlp-model+ipv6 \
#    AclDeepInspectionL3_hlp-model \
#    AclDynamicTableResizing_hlp-model+ffuGroup \
#    AclDynamicTableResizing_v2_hlp-model \
#    AclEMTFunctional_hlp-model+exactMatchTable0 \
#    AclEMTFunctional_hlp-model+exactMatchTable1 \
#    AclExactMatchTableLimits_hlp-model+exact_match_0+negative_tcs \
#    AclExactMatchTableLimits_hlp-model+exact_match_0 \
#    AclExactMatchTableLimits_hlp-model+exact_match_1+negative_tcs \
#    AclExactMatchTableLimits_hlp-model+exact_match_1 \
#    AclFfuRulePrecedence_hlp-model \
#    AclLimits_hlp-model+ffuGroup \
#    AclMatchOnModifiedData_hlp-model \
#    AclModifyRule_hlp-model+ffuGroup \
#    AclMultipleApi_hlp-model \
#    AclMultipleBasic_hlp-model \
#    AclMultipleChunk_hlp-model \
#    AclMultiplePrec_hlp-model \
#    AclRawMetadataMatch_hlp-model+CPM-enabled \
#    AclRemapEMT_hlp-model+manual_remap \
#    AclRemapEMT_hlp-model \
#    AclRemapUpdRule_hlp-model \
#    AclRemap_hlp-model+ffuGroup0+CPM-enabled+manual_remap \
#    AclRemap_hlp-model+ffuGroup0+CPM-enabled \
#    AclResourcePrecedence_hlp-model \
#    AclRulePrecedence_hlp-model \
#    AclScenarioMatch_hlp-model+ffuGroup+CPM-enabled \
#    AclScenario_hlp-model+ffuGroup+CPM-enabled \
#    AclSetGet_hlp-model \
#    AclSetMetadataExt_hlp-model+CPM-enabled \
#    AclSliceSharingMixed_hlp-model+add_to_active_instance+ffuGroup0 \
#    AclSliceSharingMixed_hlp-model+ffuGroup0+random_growing \
#    AclSliceSharingMixed_hlp-model+multiple_instances+mi_order_1_2_NO_INSTANCE+ffuGroup0 \
#    AclSliceSharingMixed_hlp-model+multiple_instances+mi_order_1_NO_INSTANCE_2+ffuGroup0 \
#    AclSliceSharingMixed_hlp-model+multiple_instances+mi_order_NO_INSTANCE_2_1+ffuGroup0 \
#    AclSliceSharingMixed_hlp-model+no_instance_first+ffuGroup2 \
#    AclSliceSharingMixed_hlp-model+no_instance_last+ffuGroup1 \
#    AclSliceSharing_hlp-model+basic+ffuGroup0 \
#    AclSliceSharing_hlp-model+basic+ffuGroup1 \
#    AclSliceSharing_hlp-model+basic+ffuGroup2 \
#    AclSliceSharing_hlp-model+ffuGroup0+random_growing \
#    AclSliceSharing_hlp-model+ffuGroup0+zero_min_entries \
#    AclTableMasks_hlp-model+random_mask \
#    AclTableMasks_hlp-model \
#    AclTablePrecedence_hlp-model+cliWrapper-create \
#    AclTablePrecedence_hlp-model+cliWrapper-destroy \
#    AclTablePrecedence_hlp-model+cliWrapper \
#    AclTablePrecedence_hlp-model \
#    ActiveQueueManagement_hlp-model \
#    AllDSIqueues_hlp-model \
#    BasicEvent_hlp-model \
#    BasicFlooding_hlp-model+tagging \
#    BasicFlooding_hlp-model \
#    BasicForwarding_hlp-model+tagging \
#    BasicForwarding_hlp-model \
#    BasicInit_hlp-model+loggingScreen+testingLibLoggingScreen \
#    BasicInit_hlp-model+no-lttng \
#    BasicInit_hlp-model+testingLibLoggingLttng \
#    BasicInit_hlp-model \
#    BasicL2D_hlp-model \
#    BasicMACSec_hlp-model \
#    BasicMirror_hlp-model+sample-rate \
#    BasicMirror_hlp-model \
#    BasicPolicers_hlp-model \
#    BasicPortStatistics_hlp-model \
#    BasicVlan_hlp-model \
#    CacheApi_hlp-model \
#    ChangePortMode_hlp-model \
#    CounterPolicerOverflow_hlp-model \
#    DMConditions_hlp-model \
#    DMLimits_hlp-model \
#    DMNonDisruptive_hlp-model \
#    DMPrecedence_hlp-model \
#    DMProfiles_hlp-model \
#    DMVsiFloodset_hlp-model \
#    DbgDumpStatus_hlp-model \
#    EventStress_hlp-model \
#    EventsThreshold_hlp-model \
#    FFULogAction_hlp-model \
#    FFUMirrorAction_hlp-model+mirror-precedence \
#    FFUMirrorAction_hlp-model \
#    Flooding_hlp-model \
#    Flush_hlp-model \
#    Forwarding_hlp-model+VLAN2 \
#    Forwarding_hlp-model \
#    FullCounterBank_hlp-model+partialCntVal \
#    FuncSwitchAttr_hlp-model \
#    FunctionalPortMode_hlp-model \
#    FunctionalPortPhyType_hlp-model+init-port-mode-down \
#    FunctionalPortPhyType_hlp-model+init-port-mode-up \
#    GeneralPortStatistics_hlp-model+badSize \
#    GeneralPortStatistics_hlp-model+mcastFlooding \
#    GeneralPortStatistics_hlp-model+testDrops \
#    GeneralPortStatistics_hlp-model+ucastFlooding \
#    GetExtendedStatus_hlp-model \
#    GetPortSpeed_hlp-model \
#    HashApi_hlp-model \
#    IPSecEgressFlow_hlp-model \
#    IPSecIngressFlow_hlp-model \
#    Jitter_hlp-model \
#    L2DeepInspection_hlp-model \
#    LBGApi_hlp-model \
#    LBGConfig_hlp-model \
#    LBGMappedDSI_hlp-model \
#    LBGMappedMode_hlp-model \
#    LBGRemap_hlp-model \
#    Lacp_hlp-model \
#    LagAddRemove_hlp-model+upDown \
#    LagAddRemove_hlp-model \
#    LagApi_hlp-model \
#    LagCreateDeleteStorm_hlp-model \
#    LagHashingDi_hlp-model+ipv4 \
#    LagHashingDi_hlp-model+ipv6 \
#    LagHashingDi_hlp-model+non-ip \
#    LagHashing_hlp-model \
#    LagIterators_hlp-model \
#    LagLearning_hlp-model \
#    LearnAgeExtended_hlp-model \
#    LearnAge_hlp-model+defaultL2D \
#    LearnAge_hlp-model+l2dLearningDisabled+portLearningSetFirst \
#    LearnAge_hlp-model+l2dLearningDisabled \
#    LearnAge_hlp-model+noEvents \
#    LearnAge_hlp-model+nonBlocking \
#    LearnAge_hlp-model+portLearningDisabled+defaultL2D \
#    LearnAge_hlp-model+portLearningDisabled+l2dLearningDisabled \
#    LearnAge_hlp-model+portLearningDisabled+portLearningSetFirst \
#    LearnAge_hlp-model+portLearningDisabled \
#    LearnAge_hlp-model+sharedLearning \
#    LearnAge_hlp-model \
#    MACSecApi_hlp-model \
#    MACSecTagging_hlp-model \
#    MACTableApi_hlp-model+noEvents \
#    MACTableApi_hlp-model+nonBlocking \
#    MACTableApi_hlp-model \
#    MapperApi_hlp-model \
#    McastApiFunctions_hlp-model \
#    McastL2_hlp-model+mcastGroups+cliWrapper \
#    McastL2_hlp-model+mcastGroups \
#    McastL2_hlp-model+mcastListeners+cliWrapper \
#    McastL2_hlp-model+mcastListeners \
#    McastL2_hlp-model+useVsiQ+cliWrapper \
#    McastL2_hlp-model+useVsiQ \
#    McastL2sharing_hlp-model \
#    MirrorAttributes_hlp-model \
#    MirrorPrecedence_hlp-model \
#    MirrorProfiles_hlp-model \
#    MirrorVlanFiltering_hlp-model \
#    MirroringApi_hlp-model \
#    MultipleLBGs_hlp-model \
#    MultipleTriggers_hlp-model \
#    PauseGen_hlp-model \
#    PauseProcess_hlp-model \
#    PolicerApi_hlp-model \
#    PolicerBank_hlp-model \
#    PortApi_hlp-model \
#    PortAttributeFunctional_hlp-model \
#    PortDSCPAttribute_hlp-model \
#    PortMapping_hlp-model \
#    PortSetsApi_hlp-model \
#    PortVLANAttributes_hlp-model \
#    PriorityMap_hlp-model \
#    PrioritySelection_hlp-model+Aqm \
#    PrioritySelection_hlp-model+CongestionDomainMapping \
#    PrioritySelection_hlp-model+ExpAndDscpMapping+skipKnownIssues \
#    PrioritySelection_hlp-model+ExpAndDscpMapping \
#    PrioritySelection_hlp-model+NisPin \
#    PrioritySelection_hlp-model+RxMapping+defaultTC \
#    PrioritySelection_hlp-model+RxMapping \
#    PrioritySelection_hlp-model+RxSia \
#    PrioritySelection_hlp-model+Wred \
#    ProfileManagement_hlp-model \
#    QoSAttributes_hlp-model \
#    QoSPortAttributes_hlp-model+skipKnownIssues \
#    QoSPortAttributes_hlp-model \
#    ReLearning_hlp-model \
#    SampleACL2_hlp-model+no-lttng \
#    SampleACL_hlp-model+no-lttng \
#    SampleADK_hlp-model+no-lttng \
#    SampleAddr2_hlp-model+no-lttng \
#    SampleAddr_hlp-model+no-lttng \
#    SampleBasic_hlp-model+no-lttng \
#    SampleDI_hlp-model+no-lttng \
#    SampleLAG_hlp-model+no-lttng \
#    SampleLM_hlp-model+no-lttng \
#    SampleMirror_hlp-model+no-lttng \
#    SampleTrigger_hlp-model+no-lttng \
#    SampleVLAN_hlp-model+no-lttng \
#    ScenarioMapperActions_hlp-model+CPM-enabled \
#    ScenarioMapperBasic_hlp-model+CPM-enabled \
#    ScenarioMapperNegative_hlp-model \
#    Scheduler_hlp-model \
#    SetGetInitAttributes_hlp-model \
#    SetGetPortAttribute_hlp-model+all \
#    SetGetPortAttribute_hlp-model+nis \
#    SetGetPortAttribute_hlp-model+qat \
#    SetGetPortAttribute_hlp-model \
#    SetGetPortMode_hlp-model \
#    SetGetPortPhyType_hlp-model \
#    Shaper_hlp-model \
#    SwitchAttributes_hlp-model+skipKnownIssues \
#    SwitchAttributes_hlp-model \
#    SwitchStateEvent_hlp-model \
#    SwitchState_hlp-model \
#    TriggerActions_hlp-model \
#    TriggerApi_hlp-model \
#    TriggerConditions_hlp-model \
#    TriggerHAMask_hlp-model \
#    TriggerPrecedence_hlp-model \
#    VlanApiFunctions_hlp-model \
#    VlanEthType_hlp-model \
#    VlanNonIBV_hlp-model \
#    VlanPassthru_hlp-model \
#    VlanPortAdmit_hlp-model \
#    VlanReflect_hlp-model \
#    VlanSnake_hlp-model \
#    VlanTagging_hlp-model \
#    VsiApiFunctions_hlp-model \
)

# List of test cases using two processors as clients:
set files_2 = ( \
    AclFfuRulePrecedence_hlp-model+testOnSecondaryProc \
    BasicFlooding_hlp-model+tagging+testOnSecondaryProc \
    BasicMultiProcess_hlp-model+2processes \
    DMLimits_hlp-model+testOnSecondaryProc \
    IPSecEgressFlow_hlp-model+testOnSecondaryProc \
    IPSecIngressFlow_hlp-model+testOnSecondaryProc \
    MACTableApi_hlp-model+testOnSecondaryProc \
    VlanTagging_hlp-model+testOnSecondaryProc \
)

# List of test cases using 16 processors as clients:
set files_16 = ( \
    BasicMultiProcess_hlp-model+16processes \
)

# List of test cases using 17 processors as clients:
set files_17 = ( \
    BasicMultiProcess_hlp-model+17processes \
)

foreach f ($files_1)
    echo "--------------------------------------------------------------------------------"
    echo "Processing file $f ..."
    echo "--------------------------------------------------------------------------------"
    /usr/bin/time --format='Ran for %e seconds.' ./bin/model_server -d 0 -clt 1 -inj dat/${f}
end

#foreach f ($files_2)
#    echo "--------------------------------------------------------------------------------"
#    echo "Processing file $f ..."
#    echo "--------------------------------------------------------------------------------"
#    /usr/bin/time --format='Ran for %e seconds.' ./bin/model_server -d 0 -clt 2 -inj dat/${f}
#end
#
#foreach f ($files_16)
#    echo "--------------------------------------------------------------------------------"
#    echo "Processing file $f ..."
#    echo "--------------------------------------------------------------------------------"
#    /usr/bin/time --format='Ran for %e seconds.' ./bin/model_server -d 0 -clt 16 -inj dat/${f}
#end
#
#foreach f ($files_17)
#    echo "--------------------------------------------------------------------------------"
#    echo "Processing file $f ..."
#    echo "--------------------------------------------------------------------------------"
#    /usr/bin/time --format='Ran for %e seconds.' ./bin/model_server -d 0 -clt 17 -inj dat/${f}
#end

echo "--------------------------------------------------------------------------------"
