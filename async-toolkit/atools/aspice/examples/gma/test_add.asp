.include "util.asp";
.global _RESET;

define ADD4(A.d,A.e,B.d,B.e,S.d,S.e) {
  /* csp { *[A?a, B?b; S!a+b] } */
  gma {
    _RESET==0 -> a=0, b=0, A.e=1, B.e=1, S.d=-1,
               pre_LOOP=1, post_LOOP=0,
               pre_A=0, mid_A=0, post_A=0,
               pre_B=0, mid_B=0, post_B=0,
               pre_S=0, mid_S=0

    /* compile the comma and receive */
    _RESET==1 && pre_LOOP -> instant pre_LOOP=0, pre_A=1, pre_B=1
    _RESET==1 && pre_A && A.d>=0 -> instant a=A.d, A.e=0, instant pre_A=0, mid_A=1
    _RESET==1 && pre_B && B.d>=0 -> instant b=B.d, B.e=0, instant pre_B=0, mid_B=1
    _RESET==1 && mid_A && A.d<0  -> A.e=1, instant mid_A=0, post_A=1
    _RESET==1 && mid_B && B.d<0  -> B.e=1, instant mid_B=0, post_B=1
    _RESET==1 && post_A && post_B -> instant post_A=0, instant post_B=0, pre_S=1

    /* compile the send and loop */
    _RESET==1 && pre_S &&  S.e -> S.d=POSMOD(a+b,16), instant pre_S=0, mid_S=1
    _RESET==1 && mid_S && !S.e -> S.d=-1, instant mid_S=0, pre_LOOP=1
  }
}

BG4 bgA(A.d,A.e);
BG4 bgB(B.d,B.e);
ADD4 fa(A.d,A.e,B.d,B.e,S.d,S.e);
BB bbS(S.d,S.e);
