%module liberty
%{
#include "si2dr_liberty.h"
%}


%include typemaps.i
%include "enumsimple.swg"
%javaconst(1);
%javaconst(0) SI2_ULONG_MAX;
%apply int *INOUT { si2drErrorT *err };
%apply int *INOUT { si2drValueTypeT *valtype };
%pragma(java) jniclassclassmodifiers="class";

%include "si2dr_liberty.h"
