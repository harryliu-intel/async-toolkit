.include "util.asp";
.global _RESET;

{
.level=0; /* env */
BG bgX(X.d,X.e);
CONV_CSP_TO_1of2 convA(X.d,X.e,A.0,A.1,A.e);
BB bbY(Y.d,Y.e);
CONV_1of2_TO_CSP convB(B.0,B.1,B.e,Y.d,Y.e);
}

{
.level=1; /* lower */
SLACK_1of2 lower.slack(A.0,A.1,A.e,B.0,B.1,B.e);
}

{
.level=2; /* upper */
CONV_1of2_TO_CSP lower.p2c(A.0,A.1,A.e,L.d,L.e);
SLACK upper.slack(L.d,L.e,R.d,R.e);
CONV_CSP_TO_1of2 lower.c2p(R.d,R.e,B.0,B.1,B.e);
}
