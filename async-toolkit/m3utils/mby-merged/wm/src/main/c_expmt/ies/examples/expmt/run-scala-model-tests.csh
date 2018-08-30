#! /usr/bin/csh -f

# List of test cases using a single processor as client:

set files_1 = ( \
    AclActionRam_hlp-model_1949652429 \
    AclActions_hlp-model+CPM-enabled_170035993 \
    AclActions_hlp-model+CPM-enabled+exactMatchTable0_1119553901 \
#    AclActions_hlp-model+CPM-enabled+exactMatchTable0+testOnSecondaryProc_2140820035 \
    AclActions_hlp-model+CPM-enabled+exactMatchTable1_1692219031 \
#    AclActions_hlp-model+CPM-enabled+exactMatchTable1+testOnSecondaryProc_1083071590 \
    AclApiFunctions_hlp-model_946796505 \
    AclBasic_hlp-model_866852121 \
    AclChain_hlp-model+emt02tcam1+no_scenario_change_1637379742 \
#    AclChain_hlp-model+emt02tcam1+no_scenario_change+testOnSecondaryProc_187072651 \
    AclChain_hlp-model+emt12tcam2+no_scenario_change_833774717 \
#    AclChain_hlp-model+emt12tcam2+no_scenario_change+testOnSecondaryProc_1507214943 \
    AclChain_hlp-model+emt2emt+no_scenario_change_1021045424 \
#    AclChain_hlp-model+emt2emt+no_scenario_change+testOnSecondaryProc_1895226388 \
    AclChain_hlp-model+no_scenario_change_1182450599 \
#    AclChain_hlp-model+no_scenario_change+testOnSecondaryProc_1136344908 \
    AclChain_hlp-model+tcam02emt1+no_scenario_change_724680316 \
#    AclChain_hlp-model+tcam02emt1+no_scenario_change+testOnSecondaryProc_1699895244 \
    AclChain_hlp-model+tcam12emt2+change_scenario_1278050772 \
#    AclChain_hlp-model+tcam12emt2+change_scenario+testOnSecondaryProc_614986926 \
    AclChain_hlp-model+tcam12emt2+no_scenario_change_1220740604 \
#    AclChain_hlp-model+tcam12emt2+no_scenario_change+testOnSecondaryProc_215378904 \
    AclCondFragFrame_hlp-model_1947080068 \
    AclCondFragFrame_hlp-model_341198927 \
    AclConditions_hlp-model+CPM-enabled_288188791 \
    AclConditions_hlp-model+CPM-enabled+exactMatchTable0_2138310995 \
#    AclConditions_hlp-model+CPM-enabled+exactMatchTable0+testOnSecondaryProc_526441953 \
    AclConditions_hlp-model+CPM-enabled+exactMatchTable1_1052639720 \
#    AclConditions_hlp-model+CPM-enabled+exactMatchTable1+testOnSecondaryProc_1469346515 \
    AclConditionsMasks_hlp-model_23190514 \
    AclConditionsMasks_hlp-model+random_mask_627154046 \
    AclDeepInspectionL2_hlp-model_1044866778 \
    AclDeepInspectionL3_hlp-model_966418912 \
    AclDeepInspectionL3_hlp-model+ipv6_1873335895 \
    AclDynamicTableResizing_hlp-model+ffuGroup_1147156020 \
    AclDynamicTableResizing_hlp-model+ffuGroup_1409753746 \
    AclDynamicTableResizing_hlp-model+ffuGroup_1691285938 \
    AclDynamicTableResizing_hlp-model+ffuGroup_1818462921 \
    AclDynamicTableResizing_hlp-model+ffuGroup_332924263 \
    AclDynamicTableResizing_hlp-model+ffuGroup_374394295 \
    AclDynamicTableResizing_hlp-model+ffuGroup_830806117 \
    AclDynamicTableResizing_hlp-model+ffuGroup_900295999 \
    AclDynamicTableResizing_v2_hlp-model_1172020937 \
    AclEMTFunctional_hlp-model+exactMatchTable0_594385021 \
#    AclEMTFunctional_hlp-model+exactMatchTable0+testOnSecondaryProc_1937775093 \
    AclEMTFunctional_hlp-model+exactMatchTable1_669401360 \
#    AclEMTFunctional_hlp-model+exactMatchTable1+testOnSecondaryProc_1759629407 \
    AclExactMatchTableLimits_hlp-model+exact_match_0_992288164 \
    AclExactMatchTableLimits_hlp-model+exact_match_0+negative_tcs_408441463 \
    AclExactMatchTableLimits_hlp-model+exact_match_0+negative_tcs+testOnSecondaryProc_1695639604 \
    AclExactMatchTableLimits_hlp-model+exact_match_0+testOnSecondaryProc_1202737004 \
    AclExactMatchTableLimits_hlp-model+exact_match_1_1706752426 \
    AclExactMatchTableLimits_hlp-model+exact_match_1+negative_tcs_1400827460 \
    AclExactMatchTableLimits_hlp-model+exact_match_1+negative_tcs+testOnSecondaryProc_119236608 \
    AclExactMatchTableLimits_hlp-model+exact_match_1+testOnSecondaryProc_66343073 \
    AclFfuRulePrecedence_hlp-model_1347549860 \
#    AclFfuRulePrecedence_hlp-model+testOnSecondaryProc_1129265565 \
    AclLimits_hlp-model+ffuGroup_146872321 \
    AclMatchOnModifiedData_hlp-model_777688486 \
    AclModifyRule_hlp-model+ffuGroup_318620494 \
    AclMultipleApi_hlp-model_565547256 \
    AclMultipleApi_hlp-model+testOnSecondaryProc_886972802 \
    AclMultipleBasic_hlp-model_13506979 \
    AclMultipleBasic_hlp-model_2002373522 \
    AclMultipleBasic_hlp-model+testOnSecondaryProc_1426519351 \
#    AclMultipleBasic_hlp-model+testOnSecondaryProc_607511805 \
    AclMultipleChunk_hlp-model_1431599520 \
    AclMultipleChunk_hlp-model_492588465 \
#    AclMultipleChunk_hlp-model+testOnSecondaryProc_567056507 \
#    AclMultipleChunk_hlp-model+testOnSecondaryProc_997181011 \
    AclMultiplePrec_hlp-model_498223792 \
    AclMultiplePrec_hlp-model_70505952 \
#    AclMultiplePrec_hlp-model+testOnSecondaryProc_196353280 \
#    AclMultiplePrec_hlp-model+testOnSecondaryProc_267890606 \
    AclRawMetadataMatch_hlp-model+CPM-enabled_531283786 \
    AclRemapEMT_hlp-model_1524431761 \
    AclRemapEMT_hlp-model+manual_remap_23865888 \
    AclRemapEMT_hlp-model+manual_remap+testOnSecondaryProc_585078274 \
    AclRemapEMT_hlp-model+testOnSecondaryProc_773225264 \
    AclRemap_hlp-model+ffuGroup0+CPM-enabled_1110644588 \
    AclRemap_hlp-model+ffuGroup0+CPM-enabled+manual_remap_1147284976 \
    AclRemapUpdRule_hlp-model_136228265 \
    AclResourcePrecedence_hlp-model_1568953648 \
#    AclResourcePrecedence_hlp-model+testOnSecondaryProc_806785859 \
    AclRulePrecedence_hlp-model_2072638551 \
    AclScenario_hlp-model+ffuGroup+CPM-enabled_2127341964 \
    AclScenarioMatch_hlp-model+ffuGroup+CPM-enabled_202902758 \
    AclScenarioMatch_hlp-model+ffuGroup+CPM-enabled_756219171 \
    AclSetGet_hlp-model_1451598158 \
    AclSetMetadataExt_hlp-model+CPM-enabled_1142065835 \
    AclSetMetadataExt_hlp-model+CPM-enabled_757621275 \
    AclSliceSharing_hlp-model+basic+ffuGroup0_1943814314 \
    AclSliceSharing_hlp-model+basic+ffuGroup1_1798846070 \
    AclSliceSharing_hlp-model+basic+ffuGroup2_1703408439 \
    AclSliceSharing_hlp-model+ffuGroup0+random_growing_1237511178 \
#    AclSliceSharing_hlp-model+ffuGroup0+random_growing+testOnSecondaryProc_1291509484 \
    AclSliceSharingMixed_hlp-model+add_to_active_instance+ffuGroup0_1991929610 \
    AclSliceSharingMixed_hlp-model+ffuGroup0+random_growing_1973013396 \
#    AclSliceSharingMixed_hlp-model+ffuGroup0+random_growing+testOnSecondaryProc_346661450 \
    AclSliceSharingMixed_hlp-model+multiple_instances+mi_order_1_2_NO_INSTANCE+ffuGroup0_1597578789 \
    AclSliceSharingMixed_hlp-model+multiple_instances+mi_order_1_NO_INSTANCE_2+ffuGroup0_382791833 \
    AclSliceSharingMixed_hlp-model+multiple_instances+mi_order_NO_INSTANCE_2_1+ffuGroup0_42831362 \
    AclSliceSharingMixed_hlp-model+no_instance_first+ffuGroup2_1921813499 \
    AclSliceSharingMixed_hlp-model+no_instance_last+ffuGroup1_359513462 \
    AclTableMasks_hlp-model_1375887473 \
    AclTableMasks_hlp-model+random_mask_821105952 \
    AclTablePrecedence_hlp-model_1008361871 \
    AclTablePrecedence_hlp-model_1824116500 \
    AclTablePrecedence_hlp-model_1922344039 \
    AclTablePrecedence_hlp-model_528857755 \
    AclTablePrecedence_hlp-model_853784747 \
    AclTablePrecedence_hlp-model+cliWrapper_2023355444 \
    AclTablePrecedence_hlp-model+cliWrapper-create_1344881134 \
    AclTablePrecedence_hlp-model+cliWrapper-destroy_74125289 \
    ActiveQueueManagement_hlp-model+AqmApiTest_380244516 \
    ActiveQueueManagement_hlp-model+testOnSecondaryProc_427415407 \
    AllDSIqueues_hlp-model_982415792 \
    AllDSIqueues_hlp-model+testOnSecondaryProc_25610294 \
    BasicEvent_hlp-model_1464437299 \
    BasicFlooding_hlp-model_308853684 \
    BasicFlooding_hlp-model+tagging_727417981 \
#    BasicFlooding_hlp-model+tagging+testOnSecondaryProc_1640260505 \
    BasicForwarding_hlp-model_1074916173 \
    BasicForwarding_hlp-model+tagging_1625639198 \
    BasicInit_hlp-model_1192001908 \
    BasicInit_hlp-model_1451427270 \
    BasicInit_hlp-model_1828576193 \
    BasicInit_hlp-model_812284459 \
    BasicInit_hlp-model+loggingScreen+testingLibLoggingScreen_1778517328 \
    BasicInit_hlp-model+MEMORY_DEBUG_CALLER_1283730306 \
    BasicInit_hlp-model+MEMORY_DEBUG_CALLER_DBG_FULL_CALLER_DEPTH_311251966 \
    BasicInit_hlp-model+no-lttng_1811954131 \
    BasicInit_hlp-model+testingLibLoggingLttng_780738690 \
    BasicL2D_hlp-model_1537439240 \
    BasicMACSec_hlp-model_1367648349 \
#    BasicMACSec_hlp-model+testOnSecondaryProc_527062561 \
    BasicMirror_hlp-model_142837077 \
    BasicMirror_hlp-model+sample-rate_460828072 \
#    BasicMultiProcess_hlp-model+16processes_539660953 \
#    BasicMultiProcess_hlp-model+17processes_197640518 \
#    BasicMultiProcess_hlp-model+2processes_174366338 \
    BasicPolicers_hlp-model_1899514257 \
    BasicPortStatistics_hlp-model_185735064 \
    BasicVlan_hlp-model_1833235764 \
    CacheApi_hlp-model_1833835909 \
    ChangePortMode_hlp-model_883491736 \
    ChangePortMode_hlp-model+testOnSecondaryProc_672045009 \
    CounterPolicerOverflow_hlp-model_2026870711 \
    CounterPolicerOverflow_hlp-model+testOnSecondaryProc_2005027625 \
    DbgDumpStatus_hlp-model_1475146893 \
    DbgDumpStatus_hlp-model+testOnSecondaryProc_1810694859 \
    DMConditions_hlp-model_1230737601 \
    DMFrameTrapping_hlp-model_1331129069 \
    DMLimits_hlp-model_1368511578 \
    DMLimits_hlp-model+testOnSecondaryProc_576478866 \
    DMNonDisruptive_hlp-model_1845732876 \
    DMPrecedence_hlp-model_24731935 \
    DMProfiles_hlp-model_267636793 \
#    DMProfiles_hlp-model+testOnSecondaryProc_1842334160 \
    DMVsiFloodset_hlp-model_685000105 \
#    DMVsiFloodset_hlp-model+testOnSecondaryProc_1926284211 \
    EventsThreshold_hlp-model_471956517 \
    EventStress_hlp-model_1506217137 \
    FFULogAction_hlp-model_611355502 \
    FFUMirrorAction_hlp-model_1226735981 \
    FFUMirrorAction_hlp-model_1699781447 \
    FFUMirrorAction_hlp-model+mirror-precedence_258216629 \
    FFUMirrorAction_hlp-model+mirror-precedence_482176537 \
    Flooding_hlp-model_314466927 \
#    Flush_hlp-model+testOnSecondaryProc_211351238 \
    Forwarding_hlp-model_1667494618 \
    Forwarding_hlp-model+VLAN2_1024269347 \
    FullCounterBank_hlp-model+partialCntVal_1770545778 \
    FuncSwitchAttr_hlp-model_1017377538 \
#    FuncSwitchAttr_hlp-model+testOnSecondaryProc_1809695486 \
    FunctionalPortMode_hlp-model_1214769451 \
#    FunctionalPortMode_hlp-model+testOnSecondaryProc_1093292257 \
    FunctionalPortPhyType_hlp-model_2060098725 \
#    FunctionalPortPhyType_hlp-model+testOnSecondaryProc_111653652 \
#    FunctionalPortPhyType_hlp-model+testOnSecondaryProc_896998102 \
    GeneralPortStatistics_hlp-model+badSize_1277608655 \
    GeneralPortStatistics_hlp-model+mcastFlooding_756396994 \
#    GeneralPortStatistics_hlp-model+mcastFlooding+testOnSecondaryProc_2029815999 \
    GeneralPortStatistics_hlp-model+testDrops_1974077534 \
    GeneralPortStatistics_hlp-model+ucastFlooding_1500047020 \
    GetExtendedStatus_hlp-model_1364374019 \
    GetExtendedStatus_hlp-model+testOnSecondaryProc_1428038326 \
    HashApi_hlp-model_1018107463 \
    I2C_MDIO_TestPhyAttributes_hlp-model_1911033617 \
    I2C_ReadWrite_hlp-model_198009507 \
    I2C_TestPhyAttributes_hlp-model_256559213 \
    IPSecEgressFlow_hlp-model_528670322 \
#    IPSecEgressFlow_hlp-model+testOnSecondaryProc_1529917288 \
    IPSecIngressFlow_hlp-model_1462659810 \
#    IPSecIngressFlow_hlp-model+testOnSecondaryProc_1535578442 \
    Jitter_hlp-model_89033392 \
    Jitter_hlp-model+testOnSecondaryProc_1813270852 \
    L2DeepInspection_hlp-model_1418195656 \
    Lacp_hlp-model_2109674109 \
    LagAddRemove_hlp-model_1368050513 \
    LagAddRemove_hlp-model+upDown_1286615867 \
    LagApi_hlp-model_1465480758 \
    LagCreateDeleteStorm_hlp-model_820908632 \
    LagHashingDi_hlp-model+ipv4_1532723142 \
    LagHashingDi_hlp-model+ipv6_360063296 \
    LagHashingDi_hlp-model+non-ip_859203147 \
    LagHashing_hlp-model_1356103359 \
    LagIterators_hlp-model_446062819 \
    LagLearning_hlp-model_2040549761 \
    LAGoverMACsec_hlp-model_674181737 \
#    LAGoverMACsec_hlp-model+testOnSecondaryProc_453233085 \
    LBGApi_hlp-model_1654596387 \
    LBGConfig_hlp-model_583673831 \
    LBGMappedDSI_hlp-model_1474947713 \
    LBGMappedMode_hlp-model_1491693595 \
    LBGRemap_hlp-model_2110300056 \
    LearnAgeAttributes_hlp-model_536486374 \
    LearnAgeExtended_hlp-model_1454134577 \
#    LearnAgeExtended_hlp-model+testOnSecondaryProc_1911508947 \
    LearnAge_hlp-model_1585790731 \
    LearnAge_hlp-model+defaultL2D_1492579699 \
#    LearnAge_hlp-model+defaultL2D+testOnSecondaryProc_988622250 \
    LearnAge_hlp-model+l2dLearningDisabled_1117347689 \
    LearnAge_hlp-model+l2dLearningDisabled+portLearningSetFirst_128705353 \
    LearnAge_hlp-model+l2dLearningDisabled+portLearningSetFirst+testOnSecondaryProc_489157569 \
    LearnAge_hlp-model+l2dLearningDisabled+testOnSecondaryProc_1657833834 \
    LearnAge_hlp-model+noEvents_185471219 \
#    LearnAge_hlp-model+noEvents+testOnSecondaryProc_1782276228 \
    LearnAge_hlp-model+nonBlocking_887801885 \
#    LearnAge_hlp-model+nonBlocking+testOnSecondaryProc_1761921715 \
    LearnAge_hlp-model+portLearningDisabled_691725804 \
    LearnAge_hlp-model+portLearningDisabled+defaultL2D_1679359188 \
    LearnAge_hlp-model+portLearningDisabled+defaultL2D+testOnSecondaryProc_1791472701 \
    LearnAge_hlp-model+portLearningDisabled+l2dLearningDisabled_1845234611 \
    LearnAge_hlp-model+portLearningDisabled+l2dLearningDisabled+testOnSecondaryProc_670523117 \
    LearnAge_hlp-model+portLearningDisabled+portLearningSetFirst_249463522 \
    LearnAge_hlp-model+portLearningDisabled+portLearningSetFirst+testOnSecondaryProc_1450767362 \
    LearnAge_hlp-model+portLearningDisabled+testOnSecondaryProc_834201101 \
    LearnAge_hlp-model+sharedLearning_171118604 \
#    LearnAge_hlp-model+sharedLearning+testOnSecondaryProc_1412905473 \
    LearnAgeVLAN_hlp-model_854369315 \
    LM_RX_Drain_Mode_hlp-model_1116378165 \
    MACSecApi_hlp-model_1188591786 \
    MACSecApi_hlp-model+testOnSecondaryProc_849169277 \
    MACSecEvents_hlp-model_1541205062 \
    MACSecRegs_hlp-model_1455445751 \
    MACSecRegs_hlp-model+validateTwice_709188212 \
    MACSecRegs_hlp-model+validateTwice+readRegs_892300877 \
    MACSecTagging_hlp-model_92817295 \
#    MACSecTagging_hlp-model+testOnSecondaryProc_656525787 \
#    MACTableApi_hlp-model_1422577372 \
#    MACTableApi_hlp-model+noEvents_241318615 \
#    MACTableApi_hlp-model+nonBlocking_254769544 \
    MACTableApi_hlp-model+testOnSecondaryProc_671156925 \
    MapperApi_hlp-model_1664477527 \
    McastApiFunctions_hlp-model_2105416358 \
    McastApiFunctions_hlp-model+testOnSecondaryProc_1327941777 \
    McastL2_hlp-model+mcastGroups_1800877416 \
    McastL2_hlp-model+mcastGroups+cliWrapper_650976437 \
    McastL2_hlp-model+mcastGroups+cliWrapper+testOnSecondaryProc_12687141 \
#    McastL2_hlp-model+mcastGroups+testOnSecondaryProc_766617007 \
    McastL2_hlp-model+mcastListeners_799233574 \
    McastL2_hlp-model+mcastListeners+cliWrapper_459667971 \
#    McastL2_hlp-model+mcastListeners+cliWrapper+testOnSecondaryProc_1119604142 \
#    McastL2_hlp-model+mcastListeners+testOnSecondaryProc_287234899 \
    McastL2_hlp-model+useVsiQ_1133514853 \
    McastL2_hlp-model+useVsiQ+cliWrapper_642963726 \
#    McastL2_hlp-model+useVsiQ+cliWrapper+testOnSecondaryProc_1289699116 \
#    McastL2_hlp-model+useVsiQ+testOnSecondaryProc_162590146 \
    McastL2LagListeners_hlp-model_1006668667 \
    McastL2sharing_hlp-model_1670253039 \
#    McastL2sharing_hlp-model+testOnSecondaryProc_649357848 \
    MDIO_ReadWrite_hlp-model_1761349699 \
    MirrorAttributes_hlp-model_1112705440 \
    MirroringApi_hlp-model_1547997857 \
    MirrorPrecedence_hlp-model_1268580938 \
    MirrorProfiles_hlp-model_1212138497 \
    MirrorVlanFiltering_hlp-model_583920919 \
    MultipleLBGs_hlp-model_129943381 \
    MultipleTriggers_hlp-model_1779594030 \
    PauseGen_hlp-model_689029943 \
    PauseGen_hlp-model+testOnSecondaryProc_1961900732 \
    PauseProcess_hlp-model_1902551414 \
    PauseProcess_hlp-model+testOnSecondaryProc_489789044 \
    PolicerApi_hlp-model_1000132507 \
    PolicerApi_hlp-model+testOnSecondaryProc_1152696600 \
    PolicerBank_hlp-model_1789917547 \
    PolicerBank_hlp-model+testOnSecondaryProc_1615774187 \
    PortApi_hlp-model_1377390878 \
    PortApi_hlp-model+testOnSecondaryProc_600962771 \
    PortAttributeFunctional_hlp-model_134050483 \
    PortDSCPAttribute_hlp-model_193395240 \
#    PortDSCPAttribute_hlp-model+testOnSecondaryProc_1984399896 \
    PortMapping_hlp-model_1134046699 \
    PortSetsApi_hlp-model_469565042 \
    PortVLANAttributes_hlp-model_669890612 \
#    PortVLANAttributes_hlp-model+testOnSecondaryProc_262537488 \
    PriorityMap_hlp-model_1627211281 \
    PrioritySelection_hlp-model+Aqm_221298623 \
    PrioritySelection_hlp-model+Aqm+testOnSecondaryProc_324270605 \
    PrioritySelection_hlp-model+CongestionDomainMapping_61019393 \
    PrioritySelection_hlp-model+ExpAndDscpMapping_1686200085 \
    PrioritySelection_hlp-model+ExpAndDscpMapping+skipKnownIssues_1026672952 \
#    PrioritySelection_hlp-model+ExpAndDscpMapping+skipKnownIssues+testOnSecondaryProc_16698024 \
#    PrioritySelection_hlp-model+ExpAndDscpMapping+testOnSecondaryProc_606822765 \
    PrioritySelection_hlp-model+NisPin_987370559 \
    PrioritySelection_hlp-model+NisPin+testOnSecondaryProc_1528082705 \
    PrioritySelection_hlp-model+RxMapping_789810700 \
    PrioritySelection_hlp-model+RxMapping+defaultTC_1009568638 \
    PrioritySelection_hlp-model+RxSia_1080939232 \
    PrioritySelection_hlp-model+RxSia+testOnSecondaryProc_1455050220 \
    PrioritySelection_hlp-model+Wred_240149417 \
    PrioritySelection_hlp-model+Wred+testOnSecondaryProc_304655584 \
    ProfileManagement_hlp-model_1437230750 \
    QoSAttributes_hlp-model_1459000266 \
    QoSPortAttributes_hlp-model_1446597202 \
    QoSPortAttributes_hlp-model+skipKnownIssues_635493338 \
    ReadWriteRMNReg_hlp-model_580743771 \
    ReLearning_hlp-model_2046360746 \
    Reset_hlp-model_1618151076 \
    SampleACL2_hlp-model+no-lttng_1406711404 \
    SampleACL_hlp-model+no-lttng_701604634 \
    SampleAddr2_hlp-model+no-lttng_1796903726 \
#    SampleAddrFlush_hlp-model+no-lttng_616988304 \
    SampleAddr_hlp-model+no-lttng_2076328838 \
    SampleADK_hlp-model+no-lttng_131822435 \
    SampleBasic_hlp-model+no-lttng_181014492 \
    SampleDI_hlp-model+no-lttng_24469863 \
    SampleLAG_hlp-model+no-lttng_1250172626 \
    SampleLM_hlp-model+no-lttng_668394337 \
    SampleMACSec_hlp-model+no-lttng_1781301184 \
    SampleMirror_hlp-model+no-lttng_1063032747 \
    SampleTrigger_hlp-model+no-lttng_1328879655 \
    SampleTrigger_hlp-model+no-lttng_2084847442 \
    SampleVLAN_hlp-model+no-lttng_1679982901 \
    ScenarioMapperActions_hlp-model+CPM-enabled_1678651936 \
    ScenarioMapperActions_hlp-model+CPM-enabled_719947537 \
    ScenarioMapperBasic_hlp-model+CPM-enabled_1247194937 \
    ScenarioMapperBasic_hlp-model+CPM-enabled_2039741876 \
    ScenarioMapperBasic_hlp-model+CPM-enabled_916010931 \
    ScenarioMapperConditions_hlp-model+CPM-enabled_1022097534 \
    ScenarioMapperNegative_hlp-model_1935818981 \
    Scheduler_hlp-model_1235430005 \
    Scheduler_hlp-model+testOnSecondaryProc_1374040977 \
    SetGetInitAttributes_hlp-model_1573241292 \
    SetGetPortAttribute_hlp-model_2064969945 \
    SetGetPortAttribute_hlp-model+all_894863498 \
    SetGetPortAttribute_hlp-model+nis_848509761 \
    SetGetPortAttribute_hlp-model+qat_95343187 \
    SetGetPortMode_hlp-model_1712993618 \
    Shaper_hlp-model_1303652535 \
    Shaper_hlp-model+testOnSecondaryProc_1170158432 \
    StatePolling_hlp-model_1429397288 \
    SwitchAttributes_hlp-model_907858630 \
    SwitchAttributes_hlp-model+skipKnownIssues_339334769 \
    SwitchAttributesMAC_hlp-model_1269808655 \
    SwitchStateEvent_hlp-model_879395416 \
    SwitchState_hlp-model_1482379197 \
    SwitchState_hlp-model_722946507 \
    SystemPropertiesApi_hlp-model_1981239359 \
    TestInitConfig_hlp-model_178683778 \
    TestPhyAttributes_hlp-model_1907284528 \
    TestPortPhyType_hlp-model_206247257 \
    TriggerActions_hlp-model_1817517481 \
    TriggerApi_hlp-model_460925278 \
    TriggerConditions_hlp-model_1888342967 \
    TriggerHAMask_hlp-model_1883142205 \
    TriggerPrecedence_hlp-model_1020254899 \
    VlanApiFunctions_hlp-model_221830043 \
    VlanEthType_hlp-model_1906514368 \
    VlanEthType_hlp-model+testOnSecondaryProc_737558444 \
    VlanNonIBV_hlp-model_950894140 \
    VlanPassthru_hlp-model_829083322 \
    VlanPortAdmit_hlp-model_333944968 \
    VlanPortAdmit_hlp-model+testOnSecondaryProc_1331568447 \
    VlanReflect_hlp-model_1682477918 \
    VlanSnake_hlp-model_535599103 \
    VlanTagging_hlp-model_999159022 \
#    VlanTagging_hlp-model+testOnSecondaryProc_217641152 \
    VsiApiFunctions_hlp-model_1596291726 \
)

# List of test cases using two processors as clients:
#set files_2 = ( \
#    AclFfuRulePrecedence_hlp-model+testOnSecondaryProc \
#    BasicFlooding_hlp-model+tagging+testOnSecondaryProc \
#    BasicMultiProcess_hlp-model+2processes \
#    DMLimits_hlp-model+testOnSecondaryProc \
#    IPSecEgressFlow_hlp-model+testOnSecondaryProc \
#    IPSecIngressFlow_hlp-model+testOnSecondaryProc \
#    MACTableApi_hlp-model+testOnSecondaryProc \
#    VlanTagging_hlp-model+testOnSecondaryProc \
#)

# List of test cases using 16 processors as clients:
#set files_16 = ( \
#    BasicMultiProcess_hlp-model+16processes \
#)

# List of test cases using 17 processors as clients:
#set files_17 = ( \
#    BasicMultiProcess_hlp-model+17processes \
#)

foreach f ($files_1)
    echo "--------------------------------------------------------------------------------"
    echo "Processing file $f ..."
    echo "--------------------------------------------------------------------------------"
    /usr/bin/time --format='Ran for %e seconds.' make -f Makefile.scala run FILE=${f}
end

#foreach f ($files_2)
#    echo "--------------------------------------------------------------------------------"
#    echo "Processing file $f ..."
#    echo "--------------------------------------------------------------------------------"
#    /usr/bin/time --format='Ran for %e seconds.' ./bin/model_server -d 0 -clt 2 -inj ${f}
#end
#
#foreach f ($files_16)
#    echo "--------------------------------------------------------------------------------"
#    echo "Processing file $f ..."
#    echo "--------------------------------------------------------------------------------"
#    /usr/bin/time --format='Ran for %e seconds.' ./bin/model_server -d 0 -clt 16 -inj ${f}
#end
#
#foreach f ($files_17)
#    echo "--------------------------------------------------------------------------------"
#    echo "Processing file $f ..."
#    echo "--------------------------------------------------------------------------------"
#    /usr/bin/time --format='Ran for %e seconds.' ./bin/model_server -d 0 -clt 17 -inj ${f}
#end

echo "--------------------------------------------------------------------------------"
