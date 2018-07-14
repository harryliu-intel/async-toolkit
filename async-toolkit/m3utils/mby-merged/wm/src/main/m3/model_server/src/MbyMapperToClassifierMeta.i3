INTERFACE MbyMapperToClassifierMeta;
IMPORT Metadata;
IMPORT MbyTypes;
IMPORT MbyFfuTypes;

TYPE
  T = Metadata.T OBJECT
    paError : BOOLEAN;
    ffuKeys : MbyFfuTypes.Keys;
    ffuActions : MbyFfuTypes.Actions;
    ffuScenario : MbyFfuTypes.Scenario;
    ffuVrid : MbyFfuTypes.Vrid;
    ipOption : ARRAY [0..2-1] OF BOOLEAN;
    priorityProfile : MbyFfuTypes.PriorityProfile;
    noPriEnc : BOOLEAN;
    learnMode : BOOLEAN;
    l2Ivlan1CntIndex : MbyTypes.L2IvlanCntIndex;
  END;

CONST Brand = "MbyMapperToClassifierMeta";

END MbyMapperToClassifierMeta.
