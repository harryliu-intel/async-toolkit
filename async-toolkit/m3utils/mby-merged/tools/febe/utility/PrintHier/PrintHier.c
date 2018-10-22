#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "mhpi_user.h"
#include "vpi_user.h"
#include "vhpi_user.h"
#define     MAX_NUM_ARGS    2

//---- Instance Callback Function ----
typedef void (*InstCB)(mhpiHandleT);

//---- Function Declarations ----
       void     PrintHier();
static void     GetArgs(char** args);
static void     FreeMem(char** args);
static void     TraverseHier(mhpiHandleT inpInstHdl,InstCB instCB);
static void     PrintInstInfo(mhpiHandleT inpInstHdl);
static char*    GetDeclFile1(mhpiHandleT scopeh);
static char*    GetDeclFile2(mhpiHandleT scopeh);
static int      GetDeclLine(mhpiHandleT scopeh);

//---- Output File ----
FILE*           hierLog;

//-----------------------------------------------------------------------------
void PrintHier(){
    char*               args[MAX_NUM_ARGS];
    mhpiHandleT         root;
    

    //---- Initialize MHPI Routines & set path delim ----
    mhpi_initialize('.');

    //---- Scan/Retrieve the args ----
    GetArgs(args);

    //---- Set start of rtl tree ----
    if(args[0] != NULL){
        if( !(root=mhpi_handle_by_name(args[0], NULL))){
            fprintf(stderr,"[PrintHier] ERROR: RTL path not found:'%s'\n", args[0]);
            FreeMem(args);
            return;
        }
    }
    else{
        fprintf(stderr,"Usage: PrintHier(rtlPath<,file>)\n");
        return;
    }
    
    //---- Open the output file( default is stdout ) ----
    if(args[1] != NULL){
        if( !(hierLog=fopen(args[1],"w"))) {
            fprintf(stderr,"[PrintHier] ERROR: Unable to open file:'%s'\n", args[1]);
            FreeMem(args);
            return;
        }
    }
    else{
        hierLog= stdout;
    }

    //---- Trace the hierarchy ----
    fprintf(hierLog,"#------ VCS Hierarchy Dump: %s ----\n", args[0]);
    TraverseHier(root,PrintInstInfo);

    //---- Close the output file ----
    if( hierLog!= stdout ){
        fclose(hierLog);
    }
    FreeMem(args);
}

//-----------------------------------------------------------------------------
static void GetArgs(char** args){
    vpiHandle           vpi_tf;
    vpiHandle           vpi_iter;
    vpiHandle           vpi_arg;
    struct t_vpi_value  vpi_val;
    int                 i;

    //---- Initialize the array ----
    for(i=0; i!=MAX_NUM_ARGS; i++){
        args[i]= NULL;
    }
    
    //---- Get tf Handle/Set String fmt ----
    vpi_tf= vpi_handle(vpiSysTfCall, NULL);
    vpi_val.format= vpiStringVal;

    //---- Scan for the args ----
    if( vpi_iter=vpi_iterate(vpiArgument,vpi_tf) ){
        i=0;
        while(vpi_arg=vpi_scan(vpi_iter) ){
            vpi_get_value(vpi_arg,&vpi_val);

    //----Lose leading spaces--vh 4-1-10
    while( strchr( vpi_val.value.str, ' ' ) ) {
      (vpi_val.value.str)++;
    }

            args[i]= (char*)malloc( strlen(vpi_val.value.str ) + 1 );
            strcpy(args[i], vpi_val.value.str);
    //fprintf( stderr, "Next string is '%s'\n", args[i] );
            i++;
        }
    }
}

//-----------------------------------------------------------------------------
static void FreeMem(char** args){
    int     i;

    //---- Free the arg strings ----
    for(i=0; i!=MAX_NUM_ARGS; i++){
        free( args[i] );
    }
}

//-----------------------------------------------------------------------------
static void PrintInstInfo(mhpiHandleT inpInstHdl){
  char fname1[500];
  static char* fname1ptr;
  char fname2[500];
  static char* fname2ptr;
  int  i = 0;
  int streq = 0;

  fname1ptr = GetDeclFile1(inpInstHdl);
  strcpy( fname1, fname1ptr );

  fname2ptr = GetDeclFile2(inpInstHdl);
  strcpy( fname2, fname2ptr );

  //  printf( "\nDEBUG: fname1ptr is '%s'\n", fname1ptr );
  //  printf( "DEBUG: fname2ptr is '%s'\n", fname2ptr );
  //  printf( "DEBUG: fname1 is '%s'\n", fname1 );
  //  printf( "DEBUG: fname2 is '%s'\n", fname2 );

  streq = 1;
  if( strlen( fname1 ) != strlen( fname2 ) ) {
streq = 0;
      }
  else {
    for( i=0; fname1[i] == '\n'; i++ ) {
      if( fname1[i] != fname2[i] ) {
streq = 0;
break;
      }
    }
  }
  //  printf( "\nDEBUG: streq is now %d\n", streq );

    fprintf(hierLog, "%s ", mhpi_get_str(mhpiFullNameP, inpInstHdl));
    fprintf(hierLog, "%s ", mhpi_get_str(mhpiModuleNameP, inpInstHdl));
    fprintf(hierLog, "%s", fname1 );
//    fprintf(hierLog, "%s %d", GetDeclFile(inpInstHdl), GetDeclLine(inpInstHdl));
    fprintf(hierLog, "\n");

    if( streq == 0 ) {
      fprintf(hierLog, "%s ", mhpi_get_str(mhpiFullNameP, inpInstHdl));
      fprintf(hierLog, "%s ", mhpi_get_str(mhpiModuleNameP, inpInstHdl));
      fprintf(hierLog, "%s", fname2 );
      fprintf(hierLog, "\n");
    }

}

//-----------------------------------------------------------------------------
static void TraverseHier(mhpiHandleT inpInstHdl, InstCB instCB){
    mhpiObjectKindT objkind;
    mhpiHandleT     scopeHdl= NULL;
    mhpiHandleT     scopeItrHdl;

    //---- Process this instance by calling the function ----
    
     objkind= mhpi_get(mhpiObjectKindP, inpInstHdl);
     if (objkind == mhpiInstanceK) {
        (instCB)(inpInstHdl);
     }

    //---- Scan for sub-instances ----
    scopeItrHdl= mhpi_iterator(mhpiInternalScopes, inpInstHdl);
    if(scopeItrHdl){
    while((scopeHdl=mhpi_scan(scopeItrHdl))) {
            objkind= mhpi_get(mhpiObjectKindP, scopeHdl);
            //printf("%s\n", mhpi_get_str(mhpiFullNameP, scopeHdl));
            //printf("%s\n", mhpi_get_str(mhpiObjectKindStrP, scopeHdl));
            if (objkind != mhpiInstanceK && objkind != mhpiForGenStmtK  && objkind != mhpiIfGenStmtK  && objkind != mhpiIfElseStmtK && objkind != mhpiGenerateStmtK ) {
            mhpi_release_handle(scopeHdl);
                continue;
            }
            TraverseHier(scopeHdl,instCB);
        mhpi_release_handle(scopeHdl);

    }
    mhpi_release_handle(scopeItrHdl);
    }
}

//-----------------------------------------------------------------------------
// for VHDL Entities
static char* GetDeclFile1(mhpiHandleT scopeh){
    char *fname = NULL;

    if (mhpi_get(mhpiPliP, scopeh) == mhpiVhpiPli){
        vhpiHandleT vhH = (vhpiHandleT) mhpi_get_vhpi_handle(scopeh);
        vhpiHandleT duH = vhpi_handle(vhpiDesignUnit, vhH);
        if (!duH)
            return 0;
        vhpiHandleT puH = vhpi_handle(vhpiPrimaryUnit, duH);
        if (!puH)
            return 0;
        /* use duH if architecture file/line is wanted */
        fname = vhpi_get_str(vhpiFileNameP, puH);
        vhpi_release_handle(duH);
        vhpi_release_handle(puH);

    }
    else{
        if (mhpi_get(mhpiPliP, scopeh) == mhpiVpiPli){
            vpiHandle vpH = (vpiHandle) mhpi_get_vpi_handle(scopeh);
            fname = vpi_get_str(vpiDefFile, vpH);
        }
    }

    return fname;
}

//-----------------------------------------------------------------------------
// for VHDL architectures
static char* GetDeclFile2(mhpiHandleT scopeh){
    char *fname = NULL;

    if (mhpi_get(mhpiPliP, scopeh) == mhpiVhpiPli){
        vhpiHandleT vhH = (vhpiHandleT) mhpi_get_vhpi_handle(scopeh);
        vhpiHandleT duH = vhpi_handle(vhpiDesignUnit, vhH);
        if (!duH)
            return 0;
        vhpiHandleT puH = vhpi_handle(vhpiPrimaryUnit, duH);
        if (!puH)
            return 0;
        /* use duH if architecture file/line is wanted */
        fname = vhpi_get_str(vhpiFileNameP, duH);
        vhpi_release_handle(duH);
        vhpi_release_handle(puH);

    }
    else{
        if (mhpi_get(mhpiPliP, scopeh) == mhpiVpiPli){
            vpiHandle vpH = (vpiHandle) mhpi_get_vpi_handle(scopeh);
            fname = vpi_get_str(vpiDefFile, vpH);
        }
    }

    return fname;
}

//-----------------------------------------------------------------------------
static int GetDeclLine(mhpiHandleT scopeh){
    int lnum = 0;

    if (mhpi_get(mhpiPliP, scopeh) == mhpiVhpiPli){
        vhpiHandleT vhH = (vhpiHandleT) mhpi_get_vhpi_handle(scopeh);
        vhpiHandleT duH = vhpi_handle(vhpiDesignUnit, vhH);
        if (!duH)
            return 0;
        vhpiHandleT puH = vhpi_handle(vhpiPrimaryUnit, duH);
        if (!puH)
            return 0;
        lnum = vhpi_get(vhpiStartLineNoP, puH); /* duH gives arch */
        vhpi_release_handle(duH);
        vhpi_release_handle(puH);

    }
    else{
        if (mhpi_get(mhpiPliP, scopeh) == mhpiVpiPli){
            vpiHandle vpH = (vpiHandle) mhpi_get_vpi_handle(scopeh);
            lnum = vpi_get(vpiDefLineNo, vpH);
        }
    }

    return lnum;
}

