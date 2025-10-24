.include "util.asp";
.global _RESET;

{
.level=0; /* env */
BG4 bg(A.d,A.e);
BB  bb(B.d,B.e);
}

{
.level=1; /* lower */
SLACK lower.slack(A.d,A.e,B.d,B.e);
}

{
.level=2; /* upper */
SLACK upper.slack(A.d,A.e,B.d,B.e);
}
