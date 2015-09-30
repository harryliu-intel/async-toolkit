INTERFACE PRSClass;
IMPORT PRS, TimingModel;

TYPE 
  Private = PRS.Public OBJECT
    tm : TimingModel.T;
  END;

REVEAL PRS.T <: Private;

END PRSClass.
  
