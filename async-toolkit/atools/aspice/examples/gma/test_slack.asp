.include "util.asp";
.global _RESET;
BG a(A.d,A.e);
SLACK b(A.d,A.e,B.d,B.e);
CONV_CSP_TO_1of2 c(B.d,B.e,C.0,C.1,C.e);
CONV_1of2_TO_CSP d(C.0,C.1,C.e,D.d,D.e);
SLACK e(D.d,D.e,E.d,E.e);
BB f(E.d,E.e);
