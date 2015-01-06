; Copyright 2004 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/rrc/cad/external-tools-integration/cadence/virtuoso/skill/layout/lefdef/apr_lefdef.il#1 $
; $DateTime: 2013/06/18 14:33:02 $
; $Author: pankala $

(defvar DefLibList list("vendor.avago.abstracts" "chip.rrc.apr_partitions" "lib.synchronous.conversion.a2s_hs" "lib.synchronous.conversion.s2a_hs" "lib.synchronous.conversion.a2s_ls" "lib.synchronous.conversion.s2a_ls"))

(defvar APRList list(
                           "chip__rrc__fabric__fc__ffu__FFU_SLICE_4__1000" 
                                  "chip__rrc__fabric__fc__ffu__EACL__1000" 
                                    "chip__rrc__fabric__fc__PARSING__1000" 
                                        "chip__rrc__fabric__fc__FFU__1000" 
                                       "chip__rrc__fabric__fc__POST__1000" 
                                       "chip__rrc__fabric__fc__TAIL__1000" 
                              "chip__rrc__fabric__fc__FRAME_CONTROL__1000" 
                     "chip__rrc__fabric__modify__mod_sync__MOD_SYNC__1000" 
                           "chip__rrc__fabric__scheduler__SCHEDULER__1000" 
                            "chip__rrc__fabric__array__ARRAY_SUBSEG__1000" 
                                             "chip__rrc__mgmt__MGMT__1000" 
                                         "chip__rrc__port__epl__EPL__1000" 
                                 "chip__rrc__port__mgmt__PORTS_MGMT__1000" 
                                                                     "pep" 
                                                        "AVAGO_TAP_S28_02" 
                            "chip__rrc__hosts__visa_pads__VISA_PADS__1000" 
                            "chip__rrc__hosts__pcie_host__PCIE_HOST__1000" 
                            "chip__rrc__hosts__pcie_mgmt__PCIE_MGMT__1000" 
                       "chip__rrc__te__tunnel_engine__TUNNEL_ENGINE__1000" 
                   "chip__rrc__mgmt__linear_bus__FABRIC_MGMT_IN_OUT__1000" 
                            "chip__rrc__fabric__clock__FABRIC_CLOCK__1000" 
                                "chip__rrc__port__clock__EPL_SYSCLK__1000" 
                              "chip__rrc__hosts__clock__PCIE_SYSCLK__1000" 
                          "chip__rrc__util__ring_osc__RING_OSC_CTRL__1000" 
                               "chip__rrc__util__xclk_mux__XCLK_MUX__1000" 
))

(defun defLocation ( module )
 (let  ( defLoc spar_dir dfII_dir libLoc dfIILoc
 )

  spar_dir = strcat( (userRootDir) "spar" )
  dfII_dir = ( ConfigFileGetValue TheCDSConfigTable "DFII_DIR" )
;  case( module
;    (       "chip__rrc__fabric__fc__ffu__FFU_SLICE_4__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/fc/ffu/FFU_SLICE_4/1000/"       module ".def") libLoc="chip.rrc.fabric.fc.ffu"
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/fabric/fc/ffu/"          module ))
;    (              "chip__rrc__fabric__fc__ffu__EACL__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/fc/ffu/EACL/1000/"              module ".def") libLoc="chip.rrc.fabric.fc.ffu"
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/fabric/fc/ffu/"          module ))
;    (                "chip__rrc__fabric__fc__PARSING__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/fc/PARSING/1000/"               module ".def") libLoc="chip.rrc.fabric.fc"
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/fabric/fc/"              module ))
;    (                    "chip__rrc__fabric__fc__FFU__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/fc/FFU/1000/"                   module ".def") libLoc="chip.rrc.fabric.fc"
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/fabric/fc/"              module ))
;    (                   "chip__rrc__fabric__fc__POST__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/fc/POST/1000/"                  module ".def") libLoc="chip.rrc.fabric.fc"
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/fabric/fc/"              module ))
;    (                   "chip__rrc__fabric__fc__TAIL__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/fc/TAIL/1000/"                  module ".def") libLoc="chip.rrc.fabric.fc"
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/fabric/fc/"              module ))
;    (          "chip__rrc__fabric__fc__FRAME_CONTROL__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/fc/FRAME_CONTROL/1000/"         module ".def") libLoc="chip.rrc.fabric.fc"
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/fabric/fc/"              module ))
;    ( "chip__rrc__fabric__modify__mod_sync__MOD_SYNC__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/modify/mod_sync/MOD_SYNC/1000/" module ".def") libLoc="chip.rrc.fabric.modify.mod_sync"
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/fabric/modify/mod_sync/" module ))
;    (       "chip__rrc__fabric__scheduler__SCHEDULER__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/scheduler/SCHEDULER/1000/"      module ".def") libLoc="chip.rrc.fabric.scheduler"
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/fabric/scheduler/"       module ))
;    (       "chip__rrc__fabric__clock__FABRIC_CLOCK__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/clock/FABRIC_CLOCK/1000/"        module ".def") libLoc="chip.rrc.fabric.clock"
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/fabric/clock/"       module ))
;    (           "chip__rrc__port__clock__EPL_SYSCLK__1000" defLoc=strcat(spar_dir "/chip/rrc/port/clock/EPL_SYSCLK/1000/"            module ".def") libLoc="chip.rrc.port.clock"
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/port/clock/"       module ))          
;    (       "chip__rrc__hosts__clock__PCIE_SYSCLK__1000" defLoc=strcat(spar_dir "/chip/rrc/hosts/clock/PCIE_SYSCLK/1000/"            module ".def") libLoc="chip.rrc.hosts.clock"
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/hosts/clock/"       module ))
;    (        "chip__rrc__fabric__array__ARRAY_SUBSEG__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/array/ARRAY_SUBSEG/1000/"       module ".def") libLoc="chip.rrc.fabric.array"
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/fabric/array/"           module ))
;    (                         "chip__rrc__mgmt__MGMT__1000" defLoc=strcat(spar_dir "/chip/rrc/mgmt/MGMT/1000/"                       module ".def") libLoc="chip.rrc.mgmt"
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/mgmt/"                   module ))
;    (                     "chip__rrc__port__epl__EPL__1000" defLoc=strcat(spar_dir "/chip/rrc/port/epl/EPL/1000/"                    module ".def") libLoc="chip.rrc.port.epl"
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/port/epl/"               module ))
;    (             "chip__rrc__port__mgmt__PORTS_MGMT__1000" defLoc=strcat(spar_dir "/chip/rrc/port/mgmt/PORTS_MGMT/1000/"            module ".def") libLoc="chip.rrc.port.mgmt" 
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/port/mgmt/"              module ))
;    (                                                 "pep" defLoc=strcat(spar_dir "/chip/rrc/hosts/pep/"                            module ".def") libLoc="chip.rrc.hosts" 
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/hosts/"                  module ))
;    (                                              "pep_hm" defLoc=strcat(spar_dir "/chip/rrc/hosts/pep_hm/"                         module ".def") libLoc="chip.rrc.hosts" 
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/hosts/"                  module ))
;    (                                             "pep_pcw" defLoc=strcat(spar_dir "/chip/rrc/hosts/pep_pcw/"                        module ".def") libLoc="chip.rrc.hosts" 
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/hosts/"                  module ))
;    (        "chip__rrc__hosts__visa_pads__VISA_PADS__1000" defLoc=strcat(spar_dir "/chip/rrc/hosts/visa_pads/VISA_PADS/1000/"       module ".def") libLoc="chip.rrc.hosts.visa_pads" 
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/hosts/visa_pads/"        module ))
;    (        "chip__rrc__hosts__pcie_host__PCIE_HOST__1000" defLoc=strcat(spar_dir "/chip/rrc/hosts/pcie_host/PCIE_HOST/1000/"       module ".def") libLoc="chip.rrc.hosts.pcie_host" 
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/hosts/pcie_host/"        module ))
;    (        "chip__rrc__hosts__pcie_mgmt__PCIE_MGMT__1000" defLoc=strcat(spar_dir "/chip/rrc/hosts/pcie_mgmt/PCIE_MGMT/1000/"       module ".def") libLoc="chip.rrc.hosts.pcie_mgmt" 
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/hosts/pcie_mgmt/"        module ))
;    (   "chip__rrc__te__tunnel_engine__TUNNEL_ENGINE__1000" defLoc=strcat(spar_dir "/chip/rrc/te/tunnel_engine/TUNNEL_ENGINE/1000/"  module ".def") libLoc="chip.rrc.te.tunnel_engine" 
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/te/tunnel_engine/"       module ))
;    ( "chip__rrc__mgmt__linear_bus__FABRIC_MGMT_IN_OUT__1000" defLoc=strcat(spar_dir "/chip/rrc/mgmt/linear_bus/FABRIC_MGMT_IN_OUT/1000/" module ".def") libLoc="chip.rrc.mgmt.linear_bus"
;                                                           dfIILoc=strcat(dfII_dir "/chip/rrc/mgmt/linear_bus/"        module ))
;

  case( module

    (         "chip__rrc__fabric__fc__ffu__FFU_SLICE_4__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/fc/ffu/FFU_SLICE_4/1000/"            module ".def") ) 
    (                "chip__rrc__fabric__fc__ffu__EACL__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/fc/ffu/EACL/1000/"                   module ".def") )
    (                  "chip__rrc__fabric__fc__PARSING__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/fc/PARSING/1000/"                    module ".def") )
    (                      "chip__rrc__fabric__fc__FFU__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/fc/FFU/1000/"                        module ".def") )
    (                     "chip__rrc__fabric__fc__POST__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/fc/POST/1000/"                       module ".def") )
    (                     "chip__rrc__fabric__fc__TAIL__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/fc/TAIL/1000/"                       module ".def") )
    (            "chip__rrc__fabric__fc__FRAME_CONTROL__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/fc/FRAME_CONTROL/1000/"              module ".def") )
    (   "chip__rrc__fabric__modify__mod_sync__MOD_SYNC__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/modify/mod_sync/MOD_SYNC/1000/"      module ".def") )
    (         "chip__rrc__fabric__scheduler__SCHEDULER__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/scheduler/SCHEDULER/1000/"           module ".def") )
    (          "chip__rrc__fabric__clock__FABRIC_CLOCK__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/clock/FABRIC_CLOCK/1000/"            module ".def") )
    (              "chip__rrc__port__clock__EPL_SYSCLK__1000" defLoc=strcat(spar_dir "/chip/rrc/port/clock/EPL_SYSCLK/1000/"                module ".def") )
    (            "chip__rrc__hosts__clock__PCIE_SYSCLK__1000" defLoc=strcat(spar_dir "/chip/rrc/hosts/clock/PCIE_SYSCLK/1000/"              module ".def") )
    (          "chip__rrc__fabric__array__ARRAY_SUBSEG__1000" defLoc=strcat(spar_dir "/chip/rrc/fabric/array/ARRAY_SUBSEG/1000/"            module ".def") )
    (                           "chip__rrc__mgmt__MGMT__1000" defLoc=strcat(spar_dir "/chip/rrc/mgmt/MGMT/1000/"                            module ".def") )
    (                       "chip__rrc__port__epl__EPL__1000" defLoc=strcat(spar_dir "/chip/rrc/port/epl/EPL/1000/"                         module ".def") )
    (               "chip__rrc__port__mgmt__PORTS_MGMT__1000" defLoc=strcat(spar_dir "/chip/rrc/port/mgmt/PORTS_MGMT/1000/"                 module ".def") )
    (                                                   "pep" defLoc=strcat(spar_dir "/chip/rrc/hosts/pep/"                                 module ".def") )
    (                                      "AVAGO_TAP_S28_02" defLoc=strcat(spar_dir "/vendor/avago/tap/AVAGO_TAP_S28_02/"                  module ".def") )
    (                                                "pep_hm" defLoc=strcat(spar_dir "/chip/rrc/hosts/pep_hm/"                              module ".def") )
    (                                               "pep_pcw" defLoc=strcat(spar_dir "/chip/rrc/hosts/pep_pcw/"                             module ".def") )
    (          "chip__rrc__hosts__visa_pads__VISA_PADS__1000" defLoc=strcat(spar_dir "/chip/rrc/hosts/visa_pads/VISA_PADS/1000/"            module ".def") )
    (          "chip__rrc__hosts__pcie_host__PCIE_HOST__1000" defLoc=strcat(spar_dir "/chip/rrc/hosts/pcie_host/PCIE_HOST/1000/"            module ".def") )
    (          "chip__rrc__hosts__pcie_mgmt__PCIE_MGMT__1000" defLoc=strcat(spar_dir "/chip/rrc/hosts/pcie_mgmt/PCIE_MGMT/1000/"            module ".def") )
    (     "chip__rrc__te__tunnel_engine__TUNNEL_ENGINE__1000" defLoc=strcat(spar_dir "/chip/rrc/te/tunnel_engine/TUNNEL_ENGINE/1000/"       module ".def") )
    ( "chip__rrc__mgmt__linear_bus__FABRIC_MGMT_IN_OUT__1000" defLoc=strcat(spar_dir "/chip/rrc/mgmt/linear_bus/FABRIC_MGMT_IN_OUT/1000/"   module ".def") )
    (        "chip__rrc__util__ring_osc__RING_OSC_CTRL__1000" defLoc=strcat(spar_dir "/chip/rrc/util/ring_osc/RING_OSC_CTRL/1000/"          module ".def") )
    (             "chip__rrc__util__xclk_mux__XCLK_MUX__1000" defLoc=strcat(spar_dir "/chip/rrc/util/xclk_mux/XCLK_MUX/1000/"               module ".def") )

  )

  libLoc = "chip.rrc.apr_partitions"
  dfIILoc = strcat(dfII_dir "/chip/rrc/apr_partitions/" module)

  list(defLoc libLoc dfIILoc)
 )
)

(defun exportCDCStream ( cdcName ) 
 (let ( 
      root_dir   temp_lef     cdcType   
      temp_dir   lef_in       cdcSpeed  
      spar_dir   dfII_dir     cdcOrient 
      mem_lef    currCV       cdcSize   
                              cdcLib    
     )

      parts     = parseString( cdcName "_" )
      cdcType   = nth( 0 parts )
      cdcSpeed  = nth( 1 parts )
      cdcOrient = nth( 2 parts )
      cdcSize   = nth( 3 parts )
      cdcLib    = strcat( "lib.synchronous.conversion." cdcType "_" cdcSpeed )
      root_dir  = (userRootDir) ; "/nfs/site/disks/wdisk.83/pankala1/rrc_1/"
      temp_dir  = ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) ; strcat(root_dir "temp/")
      spar_dir  = strcat(root_dir "spar/lib/synchronous/conversion/" cdcType "_" cdcSpeed "/")

      P4File( strcat(dfII_dir "abstract/master.tag")             "edit" )
      P4File( strcat(dfII_dir "abstract/layout.oa")              "edit" )
      P4File( strcat(dfII_dir "abstract/thumbnail_128x128.png")  "edit" )
      P4File( strcat(dfII_dir "abstract/master.tag")             "lock" )
      P4File( strcat(dfII_dir "abstract/layout.oa")              "lock" )
      P4File( strcat(dfII_dir "abstract/thumbnail_128x128.png")  "lock" )
 )
)

(defun importAvagoMem ( memName @key (lef nil) )
 (let ( 
      root_dir   temp_lef     
      temp_dir   lef_in      
      spar_dir   dfII_dir     
      mem_lef                
)

      
      root_dir  = (userRootDir) ; "/nfs/site/disks/wdisk.83/pankala1/rrc_1/"
      temp_dir  = ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) ; strcat(root_dir "temp/")
      spar_dir  = strcat(root_dir "spar/vendor/avago/mem/")
      dfII_dir  = sprintf(nil "%sdfII/vendor/avago/abstracts/%s/" root_dir memName)
      mem_lef   = sprintf(nil "%s%s/%s.lef" spar_dir memName memName)
      temp_lef  = sprintf(nil "%s/%s.lef" temp_dir memName)
      
      when( !lef
      sh(strcat("cp " mem_lef " " temp_dir))
      sh(strcat("chmod +w " temp_lef))
      sh(strcat("perl -pi.bak -e 's/metal/M/g;' " temp_lef))
      )

      P4File( strcat(dfII_dir "abstract/master.tag")             "edit" )
      P4File( strcat(dfII_dir "abstract/layout.oa")              "edit" )
      P4File( strcat(dfII_dir "abstract/thumbnail_128x128.png")  "edit" )
      P4File( strcat(dfII_dir "abstract/master.tag")             "lock" )
      P4File( strcat(dfII_dir "abstract/layout.oa")              "lock" )
      P4File( strcat(dfII_dir "abstract/thumbnail_128x128.png")  "lock" )
      currCV = ddGetObj("vendor.avago.abstracts" memName "abstract" )
      when( currCV!=nil ddDeleteObj(currCV) )
      
      if( lef
       then 
        lef_in = ldtrLefReadOA(lef "vendor.avago.abstracts")
       else
        lef_in = ldtrLefReadOA(temp_lef "vendor.avago.abstracts")
       )

      currCV = nrOpenCellViewWritable("vendor.avago.abstracts" memName "abstract")
       when(currCV 
         foreach(s currCV~>shapes when(s~>theLabel  dbDeleteObject(s)))
         foreach(s currCV~>shapes when(s~>net~>name MidPinsCreateLabel(currCV s)))
          dbSave(currCV)
         dbClose(currCV)
        )
      
      when(lef_in 
         P4File( strcat(dfII_dir "abstract/master.tag")             "add" )
         P4File( strcat(dfII_dir "abstract/layout.oa")              "add" )
         P4File( strcat(dfII_dir "abstract/thumbnail_128x128.png")  "add" )
        )
 )
)


(defun importCDCAbstract ( cdcName )
 (let ( 
      root_dir   temp_lef     cdcType   
      temp_dir   lef_in       cdcSpeed  
      spar_dir   dfII_dir     cdcOrient 
      mem_lef    currCV       cdcSize   
                              cdcLib    

)

      parts     = parseString( cdcName "_" )
      cdcType   = nth( 0 parts )
      cdcSpeed  = nth( 1 parts )
      cdcOrient = nth( 2 parts )
      cdcSize   = nth( 3 parts )
      cdcLib    = strcat( "lib.synchronous.conversion." cdcType "_" cdcSpeed )
      root_dir  = (userRootDir) ; "/nfs/site/disks/wdisk.83/pankala1/rrc_1/"
      temp_dir  = ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) ; strcat(root_dir "temp/")
;     spar_dir  = strcat(root_dir "spar/lib/synchronous/conversion/def" cdcType "_" cdcSpeed "/")
      dfII_dir  = sprintf(nil "%sdfII/lib/synchronous/conversion/%s_%s/%s/" root_dir cdcType cdcSpeed cdcName )
      mem_lef   = strcat(root_dir "spar/lib/synchronous/conversion/def/" cdcName ".lef") ; sprintf(nil "%s/%s.lef" spar_dir cdcName)

      P4File( strcat(dfII_dir "abstract/master.tag")             "edit" )
      P4File( strcat(dfII_dir "abstract/layout.oa")              "edit" )
      P4File( strcat(dfII_dir "abstract/thumbnail_128x128.png")  "edit" )
      P4File( strcat(dfII_dir "abstract/master.tag")             "lock" )
      P4File( strcat(dfII_dir "abstract/layout.oa")              "lock" )
      P4File( strcat(dfII_dir "abstract/thumbnail_128x128.png")  "lock" )
      currCV = ddGetObj( cdcLib cdcName "abstract" )
      when( currCV!=nil ddDeleteObj(currCV) )

      lef_in = ldtrLefReadOA( mem_lef cdcLib )
      currCV = nrOpenCellViewWritable( cdcLib cdcName "abstract" )
 
       when(currCV 
         foreach(s currCV~>shapes when(s~>theLabel  dbDeleteObject(s)))
         foreach(s currCV~>shapes when(s~>net~>name MidPinsCreateLabel(currCV s)))
          dbSave(currCV)
         dbClose(currCV)
        )
      
      when(lef_in 
         P4File( strcat(dfII_dir "abstract/master.tag")             "add" )
         P4File( strcat(dfII_dir "abstract/layout.oa")              "add" )
         P4File( strcat(dfII_dir "abstract/thumbnail_128x128.png")  "add" )
        )
 )
)

(defun recurseCDC ( memList )
 (let (
      infl
      mem
 )

      inFl = infile(memList)
      
      while( gets(mem inFl)
       printf("%s" car(parseString(mem "\n")))
       importCDCAbstract(car(parseString(mem "\n")))
      )

 )
)


(defun userRootDir ()
 (let ( parts root I ) 

  parts = parseString(( ConfigFileGetValue TheCDSConfigTable "DFII_DIR" ) "/")
  root = "/"
  for(I 0 length(parts)-2 root = strcat(root nth(I parts) "/") )

  root
 )
)

(defun extractMem ( defFile )
 (let ( )
  

      sh(strcat("rm " defFile ".mem"))
      sh(strcat("sort " defFile " | grep mem28 | awk '{print $3}' > " defFile ".mem"))
      

 )
)

(defun recurseMem ( memList )
 (let (
      infl
      mem
 )

      inFl = infile(memList)
      
      while( gets(mem inFl)
       printf("%s" car(parseString(mem "\n")))
       importAvagoMem(car(parseString(mem "\n")))
      )

 )
)

(defun recurseLefs ( memList )
 (let (
      infl
      mem
 )

      inFl = infile(memList)
      
      while( gets(mem inFl)
       printf("%s" car(parseString(mem "\n")))
       ldtrLefReadOA(car(parseString(mem "\n")) "vendor.avago.abstracts")
      )

 )
)

(defun preICCDef ( defFile )
  (let ( )
      sh(strcat("perl -pi.bak -e 's/\\./_/g; s/\\//\\./g; s/\\\\//g;' " defFile))
 )
)

(defun preICCDefVia ( defFile )
  (let ( )
      sh(strcat("perl -pi.bak -e 's/VIA910_1cut/M10_M9/g; s/VIA89_1cut/M9_M8/g; s/VIA78_LONG_H/M8_M7/g; s/VIA67_LONG_H/M7_M6/g; s/VIA56_LONG_V/M6_M5/g; s/VIA45_LONG_H/M5_M4/g; s/VIA34_LONG_H/M4_M3/g; s/VIA23_LONG_H/M3_M2/g; s/VIA12_LONG_H/M2_M1/g; ' " defFile))
 )
)

(defun preICCDefCus ( defFile cellName )
  (let ( )

  case( cellName
   ( "chip__rrc__port__mgmt__PORTS_MGMT__1000"
      sh(strcat("perl -pi.bak -e 's/pad2pm\\\\\\[eth_refclk\\\\\\]/pad2pm\\\\\\[0\\\\\\]/g; ' " defFile))
    )
   ( "chip__rrc__mgmt__MGMT__1000"
      sh(strcat("perl -pi.bak -e 's/pad2lsm\\\\\\[pcie_refclk\\\\\\]/pad2lsm\\\\\\[0\\\\\\]/g; ' " defFile))
      sh(strcat("perl -pi.bak -e 's/pad2lsm\\\\\\[ieee1588_refclk\\\\\\]/pad2lsm\\\\\\[1\\\\\\]/g; ' " defFile))
    )
   ( "pep"
      sh(strcat("perl -pi.bak -e 's/i_systime\\\\\\[clock1\\\\\\]/i_systime\\\\\\[0\\\\\\]/g; ' " defFile))
      sh(strcat("perl -pi.bak -e 's/i_systime\\\\\\[clock0\\\\\\]/i_systime\\\\\\[1\\\\\\]/g; ' " defFile))
      sh(strcat("perl -pi.bak -e 's/i_systime\\\\\\[plus1\\\\\\]/i_systime\\\\\\[2\\\\\\]/g;  ' " defFile))
      sh(strcat("perl -pi.bak -e 's/i_systime\\\\\\[minus1\\\\\\]/i_systime\\\\\\[3\\\\\\]/g; ' " defFile))
    )
   ( t  t )
    )
 )
)

(defun postICCDef ( defFile )
  (let ( )
      sh(strcat("perl -pi.bak -e 's/</\\[/g; s/>/\\]/g; s/\\[/\\\\[/g; s/\\]/\\\\]/g; ' " defFile))
 )
)

(defun postICCDefVia ( defFile )
  (let ( )
      sh(strcat("perl -pi.bak -e 's/M10_M9/VIA910_1cut/g; s/M9_M8/VIA89_1cut/g; s/M8_M7/VIA78_LONG_H/g; s/M7_M6/VIA67_LONG_H/g; s/M6_M5/VIA56_LONG_V/g; s/M5_M4/VIA45_LONG_H/g; s/M4_M3/VIA34_LONG_H/g; s/M3_M2/VIA23_LONG_H/g; s/M2_M1/VIA12_LONG_H/g; ' " defFile))
 )
)

(defun postICCDefCus ( defFile cellName )
  (let ( )

  case( cellName
   ( "chip__rrc__port__mgmt__PORTS_MGMT__1000"
      sh(strcat("perl -pi.bak -e 's/pad2pm\\\\\\[0\\\\\\]/pad2pm\\\\\\[eth_refclk\\\\\\]/g; ' " defFile))
    )
   ( "chip__rrc__mgmt__MGMT__1000"
      sh(strcat("perl -pi.bak -e 's/pad2lsm\\\\\\[0\\\\\\]/pad2lsm\\\\\\[pcie_refclk\\\\\\]/g; ' " defFile))
      sh(strcat("perl -pi.bak -e 's/pad2lsm\\\\\\[1\\\\\\]/pad2lsm\\\\\\[ieee1588_refclk\\\\\\]/g; ' " defFile))
    )
   ( "pep"
      sh(strcat("perl -pi.bak -e 's/i_systime\\\\\\[0\\\\\\]/i_systime\\\\\\[clock1\\\\\\]/g; ' " defFile))
      sh(strcat("perl -pi.bak -e 's/i_systime\\\\\\[1\\\\\\]/i_systime\\\\\\[clock0\\\\\\]/g; ' " defFile))
      sh(strcat("perl -pi.bak -e 's/i_systime\\\\\\[2\\\\\\]/i_systime\\\\\\[plus1\\\\\\]/g;  ' " defFile))
      sh(strcat("perl -pi.bak -e 's/i_systime\\\\\\[3\\\\\\]/i_systime\\\\\\[minus1\\\\\\]/g; ' " defFile))
    )
   ( t  t )
    )
 )
)


(defun postDef ( defFile cellName )
  (let ( )
       postICCDef( defFile )
    postICCDefVia( defFile )
    postICCDefCus( defFile cellName )
 )
)

(defun readICCDef ( cellName )
  (let ( defFile temp_dir
         libName  libLoc  
                  dfIILoc 
         viewName delCV newCV s PinLabel
        )

       println(cellName)
       defFile = nth(0 defLocation(cellName))
       libLoc  = nth(1 defLocation(cellName))
       dfIILoc = nth(2 defLocation(cellName))
       temp_dir  = ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) ; strcat(root_dir "temp/")
       temp_def  = strcat(temp_dir "/" cellName ".def")
       println(strcat("cp " defFile " " temp_dir))
       sh(strcat("cp " defFile " " temp_dir))
       sh(strcat("chmod +w " temp_def))
       preICCDefCus(temp_def cellName)
          preICCDef(temp_def)
       preICCDefVia(temp_def)

       delCV=ddGetObj("scratch.icc_imports" cellName "import")
       when( delCV!=nil ddDeleteObj(delCV))

       status = (ldtrDefReadOA
                  temp_def ; t_fileName
                  "scratch.icc_imports" ; t_libName
                  (getShellEnvVar "DFII_DIR") ; [t_libPath libPath]
                  cellName    ; [t_cellName cellName]
                  "import"    ; [t_viewName viewName]
                  "tsmc28" ; [t_techName techName]
                  "floorplan abstract"  ; [t_viewNameList viewNameList]
                  DefLibList ; [t_masterLibs masterLibs]
                  nil ; [ shared nil]
                  nil ; [ noRouting nil]
                  (strcat temp_def ".log") ; [ t_logName logName]
                  nil ; [ useCustomVias nil]
                  t   ; [ overwrite nil]
                  nil ; [ createModHier nil]
                  ; [ t_commentChar commentChar]
                  ; [ t_templateFileName templateFileName]
                  )

      when(status
           delCV  = nrOpenCellViewWritable("scratch.icc_imports" cellName "import")
           foreach(s delCV~>shapes when(s~>theLabel  dbDeleteObject(s)))
           foreach(s delCV~>shapes when(s~>net~>name PinLabel=MidPinsCreateLabel(delCV s) PinLabel~>parent=s))
           dbSave(delCV)
           fp_dir = strcat(dfIILoc "/floorplan/")
           P4File(strcat(fp_dir "layout.oa"             ) "edit")
           P4File(strcat(fp_dir "master.tag"            ) "edit")
           P4File(strcat(fp_dir "thumbnail_128x128.png" ) "edit")
           P4File(strcat(fp_dir "layout.oa"             ) "lock")
           P4File(strcat(fp_dir "master.tag"            ) "lock")
           P4File(strcat(fp_dir "thumbnail_128x128.png" ) "lock")
           P4File(strcat(fp_dir "layout.oa"             ) "add")
           P4File(strcat(fp_dir "master.tag"            ) "add")
           P4File(strcat(fp_dir "thumbnail_128x128.png" ) "add")
           newCV  = dbCopyCellView(delCV libLoc cellName "floorplan" nil nil t)
           dbClose(delCV)
           dbClose(newCV)
          )

 )
)

(defun P4AprCell ( @key (CV       (geGetEditCellView)) 
                        (CMD      "edit"             )
                   )

(let (  
        dfIILoc cellName
        fp_dir  
) 

           cellName = CV->cellName
           dfIILoc  = nth(2 defLocation(cellName))
           fp_dir   = strcat(dfIILoc "/floorplan/")
           P4File(strcat(fp_dir "layout.oa"             ) CMD)
           P4File(strcat(fp_dir "master.tag"            ) CMD)
           P4File(strcat(fp_dir "thumbnail_128x128.png" ) CMD)    

        when(CMD == "edit"
           P4File(strcat(fp_dir "layout.oa"             ) "lock")
           P4File(strcat(fp_dir "master.tag"            ) "lock")
           P4File(strcat(fp_dir "thumbnail_128x128.png" ) "lock")    
            )    

 )
)

(defun P4Cell ( @key (CV       (geGetEditCellView)) 
                     (CMD      "edit"             )
                   )

(let (  
        dfIILoc cellName
        fp_dir  p4_fp_dir  
) 

           cellName  = CV->cellName
           dfIILoc   = ( ConfigFileGetValue TheCDSConfigTable "DFII_DIR" )
           fp_dir    = NameGetViewDirFromDFIIDirAndCellNameAndView( dfIILoc cellName CV~>viewName )
           p4_fp_dir = buildString( parseString(fp_dir "#") "%23") 
           P4File(strcat(p4_fp_dir "/layout.oa"             ) CMD)
           P4File(strcat(p4_fp_dir "/master.tag"            ) CMD)
           P4File(strcat(p4_fp_dir "/thumbnail_128x128.png" ) CMD)    

        when(CMD == "edit"
           P4File(strcat(p4_fp_dir "/layout.oa"             ) "lock")
           P4File(strcat(p4_fp_dir "/master.tag"            ) "lock")
           P4File(strcat(p4_fp_dir "/thumbnail_128x128.png" ) "lock")    
            )    

 )
)

(defun recursePinLabels ( memList )
 (let (
      infl currCV s
      mem memName 
 )

      inFl = infile(memList)
      
      while( gets(mem inFl)
       memName = car(parseString(mem "\n"))
       printf("%s\n" memName)
       currCV = nrOpenCellViewWritable("vendor.avago.abstracts" memName "abstract")
       when(currCV 
         foreach(s currCV~>shapes when(s~>theLabel  dbDeleteObject(s)))
         foreach(s currCV~>shapes when(s~>net~>name MidPinsCreateLabel(currCV s)))
          dbSave(currCV)
         dbClose(currCV)
        )
      )

 )
)

(defun aprDefRefresh ()
 (let ( apr )
  foreach(apr APRList readICCDef(apr))
)
)

(defun P4File (filename action)
 (let (cmd)
  if( action != "submit"
    then
      sprintf(cmd "p4 -c %s %s %s" 
                  ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                  action
                  filename
              )
    else
      sprintf(cmd "p4 -c %s %s -d %s %s"
                  ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                  action
                  "\"Automated cable spec check-in.\""
                  filename
              )
     )

    println(cmd)
      shell(cmd)
t
 )
)

(defun setCustomNet (netName @key (Selected (geGetSelectedSet)) (CV (UIGetCellView)))
 (let ( n cNet )

   if( member(netName CV~>nets~>name) 
    then
     cNet = car( setof( n CV~>nets n~>name==netName ) )
    else
     cNet = dbCreateNet( CV netName )
     )

   foreach( n Selected n~>net=cNet )

t
 )
)
