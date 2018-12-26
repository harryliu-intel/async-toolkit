'''
# Tool mpgen: Memory Partition Generator
# Description: This tool creates the wrapper of the physical and
#              functional memories using generic interface. This
#              tool makes use of NGEN and MGM.
#              already run.
#Instructions:
#   1) Setup your nhdk enviroment tool, this script uses the
#      enviroment variable $MODEL_ROOT
#   2) Place the script under the folder of your partition. For example egr/
#   3) Create your logical files in a folder called mem/. For example egr/mem/
#   4) Run mgm for creating memories of your block.
#      mgm -c egr -cnfg $MODEL_ROOT/tools/mgm/config_file -includes $MODEL_ROOT/tools/mgm/blocks_inclusion -apr_struct $MODEL
#   5) This script will create (or overwrite) a folder called ngen_mem/ 
#   6) The script calls the make function inside ngen_mem/ directory
#   7) You can copy the generated RTL in ngen_mem/ngen/ via "make rtl copy" command.
#
#Author: Abisai Ramirez Perez abisai.ramirez.perez@intel.com
#Modified: Isaac Perez Andrade isaac.perez.andrade@intel.com -- 18/Dec/2018

'''
#TODO: review if reset is used in rf ff and sram and shells wrapper.
#TODO: Check if ngen_mem dir has been created

# ##################################################
# Imports
# ##################################################

import time
import os
import stat
import io
import sys
import re
import argparse
import glob
from shutil import copy2
import subprocess

# ##################################################
# Class
# ##################################################
class MyParser(argparse.ArgumentParser):
    def error(self, message):
        sys.stderr.write('error: %s\n' % message)
        self.print_help()
        sys.exit(2)

# ##################################################
# Functions
# ##################################################

    
# ##################################################
# get_args
# 
# ##################################################
def get_args():
    # Declare parser
    parser = argparse.ArgumentParser(description='Create memory files and wrappers using generic interfaces.')
    
    # Block name
    parser.add_argument('-b', '--block', type=str, nargs=1, required=True, help='Block name.')

    # Parse arguments
    args = parser.parse_args()

    # Store arguments in user-friendly variable names
    # Assign module name to either top or sub
    #if args.b == None :
    #    sys.exit('Error: Please provide name of top-level module or sub-module')
    #else:
    #    if args.top == None:
    #        module_name = args.sub[0]
    #    elif args.sub == None:
    #        module_name = args.top[0]
    module_name = args.block[0]

    return(module_name)

# ##################################################
# check_block_path
# 
# ##################################################
#def check_block_path(path,module_name):
#    isTop = True
#    if(os.path.isdir(path + "/" + module_name)):
#        path = path + "/" + module_name
#        isTop = False
#    elif not(path.endswith(module_name)):
#        sys.exit('Error: Please provide name of top-level module or sub-module')   
#    return([path,isTop])

def check_block_path(path,module_name):
    isTop = True
    if(os.path.isdir(path + "/" + module_name)):
        isTop = False
    elif not(path.endswith(module_name)):
        sys.exit('Error: Please provide name of top-level module or sub-module')   
    return([path,isTop])

# ##################################################
# get_logicals
# 
# ##################################################
def get_logicals(path,module_name):
    list_logicals = glob.glob(path + "/mem/*"+module_name+"*.logical")
    if len(list_logicals) == 0:
        sys.exit("Error: No logical files were found.")
    else:
        names = [logical.replace(path+"/mem/","").replace(".logical","") for logical in list_logicals]
    return (list_logicals, names)

# ##################################################
# create_if_include
# 
# ##################################################
def create_if_include(names, Ports, Insta, Lines,DataW):
    with io.open(inclPath,"w") as if_inst:
        L= len(names)
        for ii in range(L):
            If_include.append("mby_mem_"+Ports[ii]+"_if    #(.W_DATA("+DataW[ii]+"),    .W_DEEP("+Lines[ii]+"),   .W_INST("+Insta[ii]+") )    "+names[ii]+"_if();\n")
        if_inst.writelines(If_include)

# ##################################################
# createSigFile
# 
# ##################################################    
def createSigFile(names):
    L= len(names)
    with io.open(sig_Path, "w") as sigFile:
        for it in range(L):
            sig_Text.append(names[it]+"_if      interface\n;\n")
        sigFile.write("i_reset logic\n;\nreset_n logic\n\tno_pins\n;\n")
        sigFile.writelines(sig_Text) 

# ##################################################
# createSigFile
# 
# ##################################################    
def createSigFile(names):
    L= len(names)
    with io.open(sig_Path, "w") as sigFile:
        for it in range(L):
            sig_Text.append(names[it]+"_if      interface\n;\n")
        sigFile.write("i_reset logic\n;\nreset_n logic\n\tno_pins\n;\n")
        sigFile.writelines(sig_Text) 

# ##################################################
# parsingImplemReport
# 
# ##################################################
def parsingImplemReport(module_name,path,ff_Path,rf_Path,sr_Path):
    makeText = []
    with io.open(path, "r") as report, io.open(hierPath,"w") as hier, io.open(prePPath,"w") as preP:
        rdata=report.read(1000) # Reduce the amount of line read
        P_flops = r".+(FLOPS)"
        P_sram  = r".+(SRAM)"
        P_rf    = r".+(RF)"
        FF_Matched   = re.findall(P_flops,rdata) # Search for number of lines
        RF_Matched   = re.findall(P_rf,rdata) # Search for number of lines
        SR_Matched   = re.findall(P_sram,rdata) # Search for number of lines
        prePText =[ "#!/usr/bin/env perl \n",
                    "open(SF, \"$ENV{'MODEL_ROOT'}/target/mby/mgm_run/"+module_name+"/src/"+module_name+"_shells_wrapper.v\") || die \"can't open input file\n\";\n",
                    "open(TF, \">./"+module_name+"_shells_wrapper_inc.v\") || die \"can't open output file\n\";\n",
                    "\n",
                    "print TF \"`include        \\\""+module_name+"_mem.def\\\"\n\";\n",
                    "while (<SF>) {\n",
                    "   printf TF (\"%s\",$_);\n",
                    "}\n",
                    "close (TF);\n",
                    "close (SF);\n"
        ]
        HierText = ["//"+module_name+" block\n",
                    "\n",
                    "mby_"+module_name+"_gen_mem[\n",
                    module_name+"_shells_wrapper of "+module_name+"_shells_wrapper with "+module_name+"_shells_wrapper.map\n"
                        ]
        print("## Found memories in "+path+";")                        
        print("Number of FF mems:", len(FF_Matched))
        print("Number of RF mems:", len(RF_Matched))
        print("Number of SRAM mems:", len(SR_Matched), "\n")
        if (len(FF_Matched)>0):
            with io.open(ff_Path, "w") as ffMap:
                ffMap.write("1'b1 >>  car_raw_lan_power_good_with_byprst\n")
                HierText.append(module_name+"_ff_mems of "+module_name+"_ff_mems with "+module_name+"_ff_mems.map\n")
                makeText.append("	$(V2BBOX) -v $(MGM_RUN)/$(PAR)/src/mem_wrap/$(PAR)_ff_mems.v -p $(MGM_RUN)/$(PAR)/src,$(MGM_RUN)/rtl -o $(PAR)_ff_mems.v\n")
        if (len(RF_Matched)>0):
            with io.open(rf_Path, "w") as rfMap:
                rfMap.write("1'b1 >>  car_raw_lan_power_good_with_byprst\n")
                HierText.append(module_name+"_rf_mems of "+module_name+"_rf_mems with "+module_name+"_rf_mems.map\n")
                makeText.append("	$(V2BBOX) -v $(MGM_RUN)/$(PAR)/src/mem_wrap/$(PAR)_rf_mems.v -p $(MGM_RUN)/$(PAR)/src,$(MGM_RUN)/rtl -o $(PAR)_rf_mems.v\n")
        if (len(SR_Matched)>0):
            with io.open(sr_Path, "w") as srMap:
                srMap.write("1'b1 >>  car_raw_lan_power_good_with_byprst\n")
                HierText.append(module_name+"_sram_mems of "+module_name+"_sram_mems with "+module_name+"_sram_mems.map\n")
                makeText.append("	$(V2BBOX) -v $(MGM_RUN)/$(PAR)/src/mem_wrap/$(PAR)_sram_mems.v -p $(MGM_RUN)/$(PAR)/src,$(MGM_RUN)/rtl -o $(PAR)_sram_mems.v\n")
        HierText.append("]\n")
        hier.writelines(HierText)
        preP.writelines(prePText) 
    return makeText       



# ##################################################
# create_blockShellsWrapper
# 
# ##################################################          
def create_blockShellsWrapper(wrapper_name,names,Ports,Insta,WrRes):
    with io.open(wrapper_name,"w") as wrapperfile:
        L=len(names)
        for ii in range(L):
            for tt in range(int(Insta[ii])):
                if (int(Insta[ii]) > 1):
                    tempvar= "_"+str(tt)
                else:
                    tempvar= ""
                if (WrRes[ii]==0):
                    bweText = ""
                else:
                    bweText = "                                             >> "+names[ii]+tempvar+"_wr_bwe\n"

                WrapperText1r1w= [
                "1'b0                                         >> "+names[ii].upper()+tempvar+"_CFG_reg_sel\n",
                "1'b0                                         >> "+names[ii].upper()+tempvar+"_STATUS_reg_sel\n",
                names[ii]+"_if.rd_adr   "+"["+str(tt)+"]      >> "+names[ii]+tempvar+"_rd_adr\n",
                names[ii]+"_if.wr_adr   "+"["+str(tt)+"]      >> "+names[ii]+tempvar+"_wr_adr\n",
                names[ii]+"_if.rd_en    "+"["+str(tt)+"]      >> "+names[ii]+tempvar+"_rd_en\n",
                names[ii]+"_if.wr_data  "+"["+str(tt)+"]      >> "+names[ii]+tempvar+"_wr_data\n",
                names[ii]+"_if.wr_en    "+"["+str(tt)+"]      >> "+names[ii]+tempvar+"_wr_en\n",
                bweText, 
                names[ii]+"_if.rd_data  "+"["+str(tt)+"]      >> "+names[ii]+tempvar+"_rd_data\n",
                names[ii]+"_if.rd_valid "+"["+str(tt)+"]      >> "+names[ii]+tempvar+"_rd_valid\n",
                "                                             >> "+names[ii]+tempvar+"_ecc_uncor_err\n",
                "                                             >> "+names[ii]+tempvar+"_init_done\n",
                "1'b0                                         >> "+names[ii]+tempvar+"_mem_ls_enter\n",
                "\n"
                ]
                
                WrapperText1rw= [
                "1'b0                                         >> "+names[ii].upper()+tempvar+"_CFG_reg_sel\n",
                "1'b0                                         >> "+names[ii].upper()+tempvar+"_STATUS_reg_sel\n",
                names[ii]+"_if.adr      "+"["+str(tt)+"]      >> "+names[ii]+tempvar+"_adr\n",
                names[ii]+"_if.rd_en    "+"["+str(tt)+"]      >> "+names[ii]+tempvar+"_rd_en\n",
                names[ii]+"_if.wr_data  "+"["+str(tt)+"]      >> "+names[ii]+tempvar+"_wr_data\n",
                names[ii]+"_if.wr_en    "+"["+str(tt)+"]      >> "+names[ii]+tempvar+"_wr_en\n",
                bweText, 
                names[ii]+"_if.rd_data  "+"["+str(tt)+"]      >> "+names[ii]+tempvar+"_rd_data\n",
                names[ii]+"_if.rd_valid "+"["+str(tt)+"]      >> "+names[ii]+tempvar+"_rd_valid\n",
                "                                             >> "+names[ii]+tempvar+"_ecc_uncor_err\n",
                "                                             >> "+names[ii]+tempvar+"_init_done\n",
                "1'b0                                         >> "+names[ii]+tempvar+"_mem_ls_enter\n",
                "\n"
                ]

                if (Ports[ii]=="1r1w"):
                    tempText = WrapperText1r1w
                elif(Ports[ii]=="1rw"):
                    tempText = WrapperText1rw
                else:
                    tempText =""
                    print("Review logical file on RAM port")
                wrapperfile.writelines(tempText)
                tempText=""
        EndingText =[  
        "1'b0 >> "+module_name.upper()+"_ECC_COR_ERR_reg_sel\n",
        "1'b0 >> "+module_name.upper()+"_ECC_UNCOR_ERR_reg_sel\n",
        "\n",
        "1'b1 >> unified_regs_rd\n",
        "'h0  >> unified_regs_wr_data\n",
        "     >> "+module_name+"_ecc_int\n",
        "     >> "+module_name+"_init_done\n",
        "     >> unified_regs_ack\n",
        "     >> unified_regs_rd_data\n",
        "\n",
        "cclk    >> clk\n",
        "reset_n >> reset_n\n"
        ]
        wrapperfile.writelines(EndingText)


# ##################################################
# createIFBase_f
# 
# ##################################################          
def createIFBase_f(names,Ports):
    L= len(names)
    for ii in range(L):
        IfBaseName.append("mby_mem_"+Ports[ii]+"_if.mem       "+names[ii]+"_if,\n")

# ##################################################
# createBasefile
# 
# ##################################################          
def createBasefile(basefile_name,module_name):
    with io.open(basefile_name,"w") as basefile:
        textfile0 = [
        "// -- Author       : Autogenerated via MPGEN    \n",
        "// -- Project Name : MBY\n",
        "// -- Description  : "+module_name+" memory wrapper netlist.\n"
        "// -------------------------------------------------------------------\n",
        "ngen_mod_name\n",
        "\n"
        ]
        textfile1 = [
        "\n",
        "ngen_IO_list\n",
        "ngen_IO_end\n",
        "\n",
        "ngen_wires\n",
        "\n",
        "ngen_submodules\n",
        "\n",
        "ngen_endmodule\n"] # First part of script
        basefile.writelines(textfile0)
        basefile.writelines(IfBaseName)
        basefile.writelines(textfile1)

# ##################################################
# createMakefile
# 
# ##################################################
def createMakefile(makefile_name,module_name,makeText):
    with io.open(makefile_name,"w") as makefile:
        textfile=[ "BLOCK       = mby_$(PAR)_gen_mem\n",
        "DIFF        = $(if $(DISPLAY),tkdiff,diff -w)\n",
        "NGEN        = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/ngen_i/ngen_i.pl\n",
        "NGEN_FLAGS  = -fi=INTCNOPWR\n",
        "V2BBOX      = /p/hdk/rtl/proj_tools/sl2_tools/latest/scripts/v2bbox.pl\n",
        "\n",
        "V_DIRS      = -d=../rtl\n",
        "V_LIBS      =\n",
        "MGM_RUN     = $(MODEL_ROOT)/target/mby/mgm_run\n",
        "\n",
        ".PHONY: ngen pre top rtl copy clean diff\n",
        "\n",
        "ngen: clean pre top\n",
        "\n",
        "pre:\n",
        "	./pre_process.pl\n",
        "	$(V2BBOX) -v $(PAR)_shells_wrapper_inc.v -n $(PAR)_shells_wrapper -p $(MGM_RUN)/$(PAR)/src,$(MGM_RUN)/rtl -o $(PAR)_shells_wrapper.v\n"]
        textfile1=[
        "\n",
        "top: pre\n",
        "	@$(NGEN) $(NGEN_FLAGS) $(V_DIRS) $(V_LIBS) $(BLOCK)\n",
        "	rm -f *.v\n",
        "\n",
        "rtl copy:\n",
        "	cp ngen/$(BLOCK).sv ../rtl/$(BLOCK).sv\n",
        "\n",
        "clean:\n",
        "	@echo \"Cleaning $(BLOCK)\"\n",
        "	@rm -rf $(wildcard ngen *.v *.sv *.tmp)\n",
        "\n",
        "diff:\n",
        "	@echo \"Making a difference...\" # one byte at a time\n",
        "	$(DIFF) ../rtl/$(BLOCK).sv ngen/$(BLOCK).sv\n"
        ]
        makefile.write("PAR         = "+module_name+"\n")
        makefile.writelines(textfile)
        makefile.writelines(makeText)
        makefile.writelines(textfile1)



# ##################################################
# get_mem_data
# 
# ##################################################
def get_mem_data(list_logical,path): 
    #Strings to look for
    P_RamPorts      = r"Ram Ports.+=[\s\t]*(1r1w|1rw|2r2w)"
    P_Lines         = r"Ram Lines Number.+=[\s\t]*(\d{1,9})"
    P_DataW         = r"Ram Data Width.+=[\s\t]*(\d{1,9})"
    P_WrRes         = r"Ram Write Resolution.+=[\t\s]*(\d{1,9})"
    P_Insta         = r"Memory Instances.+=[\s\t]*(\d{1,9})"
    for logical_file in list_logical:
        print("Parsing " + logical_file.replace(path+"/mem/",""))
        with open(logical_file, "r") as logical:
            # Read whole logical file at once
            data=logical.read(1000) # Reduce the amount of line read
            # Search for strings
            PortsMatched   = re.search(P_RamPorts,data) # Search for ports types
            LinesMatched   = re.search(P_Lines,data) # Search for number of lines
            DataWMatched   = re.search(P_DataW,data) # Search for number of lines
            WrResMatched   = re.search(P_WrRes,data) # Search for number of lines
            InstaMatched   = re.search(P_Insta,data) # Search for number of lines
            
            if (WrResMatched is not None):
                if (WrResMatched.group(1)==DataWMatched.group(1)):
                    WrRes.append(0)
                else:
                    WrRes.append(1)
            else:
                    WrRes.append(0)
            Ports.append(PortsMatched.group(1))
            Lines.append(LinesMatched.group(1))
            DataW.append(DataWMatched.group(1))
            Insta.append(InstaMatched.group(1))
    return([Ports,Lines,DataW,Insta,WrRes])

# ##################################################
# copy_logicals
# 
# ##################################################
def copy_logicals(list_logicals,dst):
    # Create ngen output directory
    if not(os.path.isdir(dst)):
        try:      
            os.makedirs(dst)
        except OSError:  
            print ("## Creation of %s failed" % dst)
        else:  
            print ("## Successfully created %s" % dst)
    
    for src in list_logicals:
        print_info(("Copying %s to %s")%(src,dst)) 
        copy2(src, dst)

def print_info(msg):
    print("========================================")
    print(("========== %s")%(msg))
    print("========================================")


# #i################################################
# run_mgm
# 
# ##################################################
def run_mgm(module_name, path, list_logicals):
    # Define path for logical files 
    dst_path = path + "/" + module_name + "/mem/"
    # Copy logicals for this client
    copy_logicals(list_logicals,dst_path) 
    # Debug info
    print_info(("Calling mgm for %s")%(module_name)) 
    # mgm command to run
    cmd = ("mgm -c %s -cnfg $MODEL_ROOT/tools/mgm/config_file -includes $MODEL_ROOT/tools/mgm/blocks_inclusion -apr_struct $MODEL_ROOT/tools/mgm/apr_structure -csv $MODEL_ROOT/tools/mgm/mby_physical_params.csv -unique mby -clk $MODEL_ROOT/tools/mgm/clk_file -rprt_dir $MODEL_ROOT/target/mby/mgm_run/rtl -relative_path_from $MODEL_ROOT")%(module_name)
    # Run mgm
    subprocess.run(cmd, shell=True)
    # Delete temp logical files
    subprocess.call(("rm -rf %s")%(dst_path),shell=True)

# ##################################################
# main
# 
# ##################################################

# Get name to append to output files
module_name = get_args()

# Get current working directory
path=os.getcwd()

# Check if given block name exists
[path, isTop] = check_block_path(path,module_name)

# Get a list of logical files
[list_logicals, names] = get_logicals(path, module_name)

# Run mgm
run_mgm(module_name, path, list_logicals)

Ports=[]
Lines=[]
DataW=[]
WrRes=[]
Insta=[]

IfBaseName =[]
If_include =[]
MapWrapper =[]
sig_Text =[]

ngenPath=os.path.join(path,"ngen_mem")
makePath=os.path.join(ngenPath,"Makefile")
inclPath=os.path.join(ngenPath,"../rtl/mby_"+module_name+"_if_inst.sv")
basePath=os.path.join(ngenPath,"mby_"+module_name+"_gen_mem.base")
hierPath=os.path.join(ngenPath,"mby_"+module_name+"_gen_mem.hier")
sig_Path=os.path.join(ngenPath,"mby_"+module_name+"_gen_mem.sig")
prePPath=os.path.join(ngenPath,"pre_process.pl")
wrapPath=os.path.join(ngenPath,module_name+"_shells_wrapper.map")
ffPath=os.path.join(ngenPath,module_name+"_ff_mems.map")
srPath=os.path.join(ngenPath,module_name+"_sram_mems.map")
rfPath=os.path.join(ngenPath,module_name+"_rf_mems.map")
memoPath=os.path.join(path,"mem")
reportPath = os.path.join(os.environ['MODEL_ROOT'],"target/mby/mgm_run/"+module_name+"/reports/"+module_name+"_mem_imp.report")
print("## MGM PAR reports dir:",reportPath,"\n")

# Create ngen output directory
if not(os.path.isdir(ngenPath)):
    try:      
        os.makedirs(ngenPath)
    except OSError:  
        print ("## Creation of %s failed" % ngenPath)
    else:  
        print ("## Successfully created %s" % ngenPath)


[Ports, Lines, DataW, Insta, WrRes] = get_mem_data(list_logicals,path)


TextList=parsingImplemReport(module_name,reportPath,ffPath,rfPath,srPath)
st = os.stat(prePPath)
os.chmod(prePPath, st.st_mode | stat.S_IEXEC)
createIFBase_f(names,Ports)
createSigFile(names)
create_blockShellsWrapper(wrapPath,names, Ports, Insta,WrRes)
createMakefile(makePath, module_name,TextList)
time.sleep(0.1)
createBasefile(basePath, module_name) 
print("## Creating file:"+inclPath+" that could be included for memory interface instances\n")
create_if_include(names, Ports, Insta, Lines,DataW)
os.chdir(ngenPath)
print("## Running Make in ngen directory ##\n")
os.system("make")
