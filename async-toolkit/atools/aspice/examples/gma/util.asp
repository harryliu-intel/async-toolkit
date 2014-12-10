define BG(R.d,R.e) {
  gma {
    _RESET==0 -> R.d=-1
    _RESET==1 &&  R.e -> after 200 R.d=RAND(1)
    _RESET==1 && !R.e -> after 200 R.d=-1
  }
}

define BG4(R.d,R.e) {
  gma {
    _RESET==0 -> R.d=-1
    _RESET==1 &&  R.e -> after 200 R.d=RAND(4)
    _RESET==1 && !R.e -> after 200 R.d=-1
  }
}

define BUF(L.d,L.e,R.d,R.e) {
  gma {
    _RESET==0 -> R.d=-1, L.e=1
    _RESET==1 &&  R.e && L.d>=0 -> after 200 R.d=L.d, after 700 L.e=0
    _RESET==1 && !R.e && L.d<0  -> after 200 R.d=-1,  after 700 L.e=1
  }
}

define BB(L.d,L.e) {
  gma {
    _RESET==0 -> L.e=1
    _RESET==1 && L.d>=0 -> after 300 L.e=0, instant stdout="BitBucketing " ++ HEX(L.d)
    _RESET==1 && L.d<0  -> after 300 L.e=1
  }
}

define CONV_CSP_TO_1of2(L.d,L.e,R.0,R.1,R.e) {
  gma {
    _RESET==0 -> R.0=0, R.1=0, L.e=1
    _RESET==1 &&  R.e && L.d>=0 && POSMOD(L.d,2)==0 -> after 200 R.0=1, after 300 L.e=0
    _RESET==1 &&  R.e && L.d>=0 && POSMOD(L.d,2)==1 -> after 200 R.1=1, after 300 L.e=0
    _RESET==1 && !R.e && L.d<0 && R.0 -> after 200 R.0=0, after 300 L.e=1
    _RESET==1 && !R.e && L.d<0 && R.1 -> after 200 R.1=0, after 300 L.e=1
  }
}

define CONV_1of2_TO_CSP(L.0,L.1,L.e,R.d,R.e) {
  gma {
    _RESET==0 -> R.d=-1, L.e=1
    _RESET==1 &&  R.e && L.0==1 -> after 200 R.d=0, after 300 L.e=0
    _RESET==1 &&  R.e && L.1==1 -> after 200 R.d=1, after 300 L.e=0
    _RESET==1 && !R.e && L.0==0 && L.1==0 && R.d>=0 -> after 200 R.d=-1, after 300 L.e=1
  }
} 

define SLACK(L.d,L.e,R.d,R.e) {
  BUF b0(L.d,L.e,X0.d,X0.e);
  BUF b1(X0.d,X0.e,X1.d,X1.e);
  BUF b2(X1.d,X1.e,X2.d,X2.e);
  BUF b3(X2.d,X2.e,R.d,R.e);
}

define BUF_1of2(L.0,L.1,L.e,R.0,R.1,R.e) {
  prs {
    _RESET & R.e & L.0 -> R.0+
    ~_RESET -> R.0-
    ~R.e & ~L.0 -> R.0-

    _RESET & R.e & L.1 -> R.1+
    ~_RESET -> R.1-
    ~R.e & ~L.1 -> R.1-

    ~_RESET -> L.e-
    R.0 -> L.e-
    R.1 -> L.e-
    _RESET & ~R.0 & ~R.1 -> L.e+
  }
}

define SLACK_1of2(L.0,L.1,L.e,R.0,R.1,R.e) {
  BUF_1of2 b0(L.0,L.1,L.e,X0.0,X0.1,X0.e);
  BUF_1of2 b1(X0.0,X0.1,X0.e,X1.0,X1.1,X1.e);
  BUF_1of2 b2(X1.0,X1.1,X1.e,X2.0,X2.1,X2.e);
  BUF_1of2 b3(X2.0,X2.1,X2.e,R.0,R.1,R.e);
}
