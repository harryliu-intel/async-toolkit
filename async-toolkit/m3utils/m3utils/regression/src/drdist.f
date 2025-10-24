C Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
C SPDX-License-Identifier: Apache-2.0

      PROGRAM drdist
c
c       This driver program was written to use the UTHLIB routines for
c               distribution and their inverses
c
C     .. Local Scalars ..
      REAL alpha,f,s,v,x,xb,xs
      INTEGER index,n1,n2
C     ..
C     .. External Functions ..
      REAL chi2,expan,fish,fishin,phi,phinv,studin
      EXTERNAL chi2,expan,fish,fishin,phi,phinv,studin
C     ..
   10 WRITE (*,FMT=
     +  '(''  Distribution calculator -- Enter the number '')')
      WRITE (*,FMT='(''   -1       will terminate '')')
      WRITE (*,FMT='(''    1       for inverse F '')')
      WRITE (*,FMT='(''    2       for F '')')
      WRITE (*,FMT='(''    3       for inverse Normal(0,1) '')')
      WRITE (*,FMT='(''    4       for Normal(0,1) '')')
      WRITE (*,FMT='(''    5       for inverse t '')')
      WRITE (*,FMT='(''    6       for t '')')
      WRITE (*,FMT='(''    7       for chisquare '')')
      READ (*,FMT=*) index
      IF (index.LT.1) GO TO 90
      GO TO (20,30,40,50,60,70,80) index
   20 WRITE (*,FMT=
     +  '('' Enter the alpha level followed by the 2 D.F. '')')
      READ (*,FMT=*) alpha,n1,n2
      f = fishin(alpha,n1,n2)
      WRITE (*,FMT=9000) alpha,n1,n2,f
 9000 FORMAT (' Finv(',f6.2,2i4,')= ',g15.4)
      GO TO 10
   30 WRITE (*,FMT='('' Enter the F-value followed by the 2 D.F. '')')
      READ (*,FMT=*) f,n1,n2
      alpha = 1. - fish(f,n1,n2)
      WRITE (*,FMT=9010) f,n1,n2,alpha
 9010 FORMAT (' F(',g15.4,2i4,')= ',g15.5)
      GO TO 10
   40 WRITE (*,FMT='('' Enter the alpha,mean,std.dev. '')')
      READ (*,FMT=*) alpha,xb,s
      x = phinv(alpha)*s + xb
      WRITE (*,FMT=9020) alpha,xb,s,x
 9020 FORMAT (' PHIinv(',f6.4,2g15.4,')= ',g15.4)
      GO TO 10
   50 WRITE (*,FMT='('' Enter the value, then mean and std. dev. '')')
      READ (*,FMT=*) x,xb,s
      xs = (x-xb)/s
      alpha = phi(xs)
      WRITE (*,FMT=9030) x,xb,s,alpha
 9030 FORMAT (' PHI(',3g15.4,')= ',g15.4)
      GO TO 10
   60 WRITE (*,FMT='('' Enter the p-value and degress of freedom '')')
      WRITE (*,FMT='('' The result will be the one-sided t '')')
      READ (*,FMT=*) alpha,n1
      x = studin(n1,alpha)
      WRITE (*,FMT=9040) alpha,n1,x
 9040 FORMAT ('  t( ',f7.4,' , ',i5,' )= ',g15.4)
      GO TO 10
   70 WRITE (*,FMT='('' Enter the t-value and degrees of freedom '')')
      READ (*,FMT=*) x,n1
      alpha = expan(n1,x)
      WRITE (*,FMT=9040) alpha,n1,x
      GO TO 10
   80 WRITE (*,FMT='('' Enter the chi-square value, then the D.F. '')')
      READ (*,FMT=*) x,v
      alpha = 1. - chi2(x,v)
      WRITE (*,FMT=9050) x,v,alpha
 9050 FORMAT (' chisquare( ',g15.7,',',f7.3,' ) = ',g15.4)
      GO TO 10
   90 STOP
      END
      REAL FUNCTION fishin(alpha,n1,n2)
C-----------------------------------------------------------------------
C CALCULATES THE INVERSE ÒFÓ VALUE GIVEN THE CONFIDENCE COEFFICIENT
C   ALPHA AND THE DEGREES OF FREEDOM(N).
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      REAL alpha
      INTEGER n1,n2
C     ..
C     .. Scalars in Common ..
      REAL bigd,bigda,err,glg,smd,smg
      INTEGER n1m,n2m,num
C     ..
C     .. Local Scalars ..
      REAL alp1,denom,f,g,h,x,x1,x2,xx,y,y1,y2,yy,z
      INTEGER i,ib1,ib2,ib3,ic,ict,ii,ish,isiz,j,mm,nd,nd1,nn,numbin
C     ..
C     .. External Functions ..
      REAL fish,phi,phinv
      EXTERNAL fish,phi,phinv
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,alog,exp,mod,sqrt
C     ..
C     .. Common blocks ..
      COMMON /fishcom/n1m,n2m,bigda,smd,bigd,num,smg,glg,err
C     ..
C     .. Statement Functions ..
      REAL a2,a3
C     ..
C     .. Data statements ..
c    2     100,100,500,1.0E-300,1.0E+300,20,1.0E-300,1.0E+300,1.0E-6/
      DATA n1m,n2m,bigda,smd,bigd,num,smg,glg,err/100,100,500,1.0E-30,
     +     1.0E+30,20,1.0E-30,1.0E+30,1.0E-6/
      DATA numbin/50/
C     ..
C     .. Statement Function definitions ..
      a2(xx,mm,nn) = (xx-nn/ (nn-2.))/
     +               (nn/ (nn-2.)*sqrt(2.* (mm+nn-2.)/ (mm* (nn-4.))))
      a3(xx,mm,nn) = (xx** (1./3.)* (1.-2./ (9.*nn))- (1.-2./ (9.*mm)))/
     +               sqrt(2./ (9.*mm)+ (2./ (9.*nn))*xx** (2./3.))
C     ..
      y1 = n1
      y2 = n2
C-----------------------------------------------------------------------
C  ADJUST FOR DEGREES OF FREEDOM EQUAL TO 1
C-----------------------------------------------------------------------
      IF (n1.EQ.1) y1 = 2
      IF (n2.EQ.1) y2 = 2
C-----------------------------------------------------------------------
C CALL PHINV TO GET INVERSE NORMAL VALUE OF 1.-ALPHA
C-----------------------------------------------------------------------
      x = phinv(1.-alpha)
C-----------------------------------------------------------------------
C  COMPUTE LAMDA VALUE
C-----------------------------------------------------------------------
      y = (x**2-3.)/6.
      ic = 0
C-----------------------------------------------------------------------
C  COMPUTE THE INITIAL APPROXIMATION TO THE INVERSE 'F' FUNCTION
C-----------------------------------------------------------------------
      y1 = 1./ (y1-1.)
      y2 = 1./ (y2-1.)
      h = 2./ (y1+y2)
C     WRITE(6,921) X,Y,Y1,Y2,H
 9000 FORMAT (' INITIAL VALUES X=',G16.9,' Y=',G16.9,/,' Y1=',G16.9,
     +       ' Y2=',G16.9,' H=',G16.9)
      x = x*sqrt(h+y)/h - (y1-y2)* (y+5./6.-2./ (3.*h))
      x = exp(2.*x)
      isiz = 1
      IF (n1.GT.n1m .OR. n2.GT.n2m) isiz = 2
C-----------------------------------------------------------------------
C  COMPUTE THE CONSTANT TO THE 'F' DISTRIBUTION,TESTING FOR N1 AND/OR N2
C   ODD OR EVEN.
C-----------------------------------------------------------------------
      g = 1.
      ib1 = 2
      IF (mod(n1,2).EQ.0) GO TO 10
      g = 1.7724539
      ib1 = 1
   10 ib2 = 2
      IF (mod(n2,2).EQ.0) GO TO 20
      g = g*1.7724539
      ib2 = 1
   20 ib3 = 2
      IF (mod(n1+n2,2).EQ.0) GO TO 30
      g = g/1.7724539
      ib3 = 1
   30 IF ((ib1+ib2).NE.2) g = 2.*g
      IF ((n1+n2).LE.3) GO TO 60
      nd = n1 + n2 - 2 - ib3
      nd1 = nd + 1
      ish = ((n1-ib3)/2)*2
C     WRITE(6,922) X,G,IB1,IB2,IB3,ND,ISH
 9010 FORMAT (' X=',G16.9,' G=',G16.9,5X,5I5)
      IF (nd.LT.0) GO TO 50
      DO 40 ii = 1,nd1,2
          i = ii - 1
          IF ((ib1+i).LE. (n1-2)) g = g* (ib1+i)
          j = i - ish + ib2
          IF ((j.GT.0) .AND. (j.LE. (n2-2))) g = g*j
          IF (g.LT.smg .OR. g.GT.glg) GO TO 100
          g = g/ (ib3+i)
   40 CONTINUE
   50 CONTINUE
C     WRITE (6,916)  G
 9020 FORMAT (' $$$$$$$G=',E15.7,'$$$$$$$')
C-----------------------------------------------------------------------
C  COMPUTE THE VALUE OF FISHIN
C-----------------------------------------------------------------------
      GO TO (60,70) isiz
   60 y2 = n2/ (n2+n1*x)
      y1 = 1. - y2
      y = 1. + (g* (1.-alpha-fish(x,n1,n2)))/sqrt(y1**n1*y2**n2)
C     F=FISH  (X,N1,N2)
C     WRITE(6,917) Y,X,F
 9030 FORMAT (' Y=',G16.9,' X=',G16.9,' F=',G16.9,' Z=',G16.9)
      GO TO 80
   70 CONTINUE
C-----------------------------------------------------------------------
C         N1 GT 100 OR N2 GT 100, USE Z APPROXIMATION FOR Q RATHER
C      THAN FISH.
C-----------------------------------------------------------------------
      y2 = n2/ (n2+n1*x)
      y1 = 1. - y2
      z = a3(x,n1,n2)
      f = phi(z)
C     WRITE(6,917) Y,X,F,Z
      denom = n1/2.*alog(y1) + n2/2.*alog(y2)
C     WRITE(6,918) DENOM
 9040 FORMAT (' D=',G16.9)
      IF (abs(denom).GE.bigda) GO TO 100
      denom = exp(denom)
C     IF (DENOM.LT.1.0E-75.OR.DENOM.GT.1.0E75) WRITE (6,902)
      WRITE (6,FMT=9050) denom
 9050 FORMAT (' DENOM=',G16.9)
      IF (denom.LT.smd .OR. denom.GT.bigd) WRITE (6,FMT=9060)
 9060 FORMAT (' DENOM OUT OF RANGE')
      y = 1. + (g* (1.-alpha-f))/denom
      yy = g* (1.-alpha-f)/denom
   80 CONTINUE
      fishin = x*y
 9070 FORMAT (' FISHIN=',G16.9)
C-----------------------------------------------------------------------
C  IF FISHIN IS NEGATIVE, RESET FISHIN TO .5*LAST APPROXIMATION(X).
C-----------------------------------------------------------------------
      IF (y.LT.0.) fishin = .5*x
C-----------------------------------------------------------------------
C  IF THE ABSOLUTE VALUE OF THE DIFFERENCE IS LESS THAN .5E-6, RETURN
C-----------------------------------------------------------------------
      IF (abs(x/fishin-1.).LT.err) GO TO 90
C-----------------------------------------------------------------------
C  IF THE RELATIVE VALUE OF THE DIFFERENCE IS LESS THAN .5E-6, RETURN
C-----------------------------------------------------------------------
      IF (abs(x-fishin).LT. (.5E-6)) GO TO 90
      ic = ic + 1
      IF (ic.GT.num) RETURN
C-----------------------------------------------------------------------
C  SET THE APPROXIMATION EQUAL TO FISHIN AND CONTINUE TO ITERATE
C-----------------------------------------------------------------------
      x = fishin
      GO TO (60,70) isiz
   90 RETURN
  100 alp1 = 1. - alpha
      IF (n1.LT.500 .OR. n2.LT.500) GO TO 220
C                  WRITE(6,923) I,G
 9080 FORMAT (' GSMALL ** I=',I5,' G=',G16.9)
      IF (alpha.LT..5) GO TO 140
  110 CONTINUE
      ict = 0
  120 ict = ict + 1
      IF (ict.GT.numbin) GO TO 130
      z = a2(x,n1,n2)
      f = phi(z)
      IF (f.LT.alp1) GO TO 130
      x = x - .1
      GO TO 120
  130 x1 = x
      x2 = 1.
      GO TO 180
  140 CONTINUE
      ict = 0
  150 ict = ict + 1
      IF (ict.GT.numbin) GO TO 170
      z = a2(x,n1,n2)
  160 f = phi(z)
      IF (f.GT.alp1) GO TO 170
      x = x + 1.
      GO TO 150
  170 x1 = 1.
      x2 = x
  180 x = (x1+x2)*.5
      z = a2(x,n1,n2)
      f = phi(z)
      IF (abs(x-x1)-err) 220,220,190
  190 IF (f-alp1) 200,220,210
  200 x1 = x
      GO TO 180
  210 x2 = x
      GO TO 180
  220 fishin = x
      RETURN
      END
      SUBROUTINE overfl(i)
C     .. Scalar Arguments ..
      INTEGER i
C     ..
      i = 1
      RETURN
      END
      REAL FUNCTION cbrt(x)
C     .. Scalar Arguments ..
      REAL x
C     ..
      cbrt = x** (1./3.)
      RETURN
      END
      REAL FUNCTION fish(f,n1,n2)
C     .. Scalar Arguments ..
      REAL f
      INTEGER n1,n2
C     ..
C     .. Local Scalars ..
      REAL d1,d2,dn,dt,h,x,y
      INTEGER i,ii,m,m1
      LOGICAL e1,e2,e3
C     ..
C     .. External Functions ..
      REAL phi
      EXTERNAL phi
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC acos,mod,sqrt
C     ..
      IF (n1.GE.100 .AND. n2.GE.100) GO TO 90
C-----------------------------------------------------------------------
C     INITIALIZATION AND SETTING OF LOGICAL SWITCHES  TO .TRUE. IF
C     THE DEGREES OF FREEDOM ARE EVEN
C-----------------------------------------------------------------------
      e1 = .false.
      e2 = .false.
      e3 = .false.
      IF (mod(n1,2).EQ.0) e1 = .true.
      IF (mod(n2,2).EQ.0) e2 = .true.
      x = n2/ (n2+n1*f)
      IF (.NOT. (e1.OR.e2)) GO TO 50
      IF (e1 .AND. .NOT.e2) GO TO 20
      IF (.NOT.e1 .AND. e2) GO TO 10
      IF (n1.LE.n2) GO TO 20
C-----------------------------------------------------------------------
C   INITIALIZATION FOR SECOND DEGREE OF FREEDOM EVEN  AND LESS THAN
C      FIRST DEGREE OF FREEDOM IF IT TOO IS EVEN
C-----------------------------------------------------------------------
   10 i = n1
      n1 = n2
      n2 = i
      x = 1.0 - x
      e3 = .true.
C-----------------------------------------------------------------------
C     INITIALIZATION FOR FIRST DEGREE OF FREEDOM EVEN AND LESS THAN
C   SECOND DEGREE OF FREEDOM IF IT IS EVEN
C-----------------------------------------------------------------------
   20 y = 1.0 - x
C-----------------------------------------------------------------------
C     CALCULATION OF PROBABILITY FOR AT LEAST ONE DEGREE OF FREEDOM EVEN
C-----------------------------------------------------------------------
      fish = 0.0
      h = sqrt(x**n2)
      m = n1/2 - 1
C     DO  3 I=0,M
      m1 = m + 1
      DO 30 ii = 1,m1
          i = ii - 1
          fish = fish + h
          h = (h*y* (n2+2.*i))/ (2.* (i+1.))
   30 CONTINUE
      IF (e3) GO TO 40
C-----------------------------------------------------------------------
C       ADJUST CALCULATED PROBABILITY IF ITS ONES COMPLEMENT WAS
C    CALCULATED ORIGINALLY
C-----------------------------------------------------------------------
      fish = 1.0 - fish
      RETURN
   40 i = n1
      n1 = n2
      n2 = i
      RETURN
C-----------------------------------------------------------------------
C     CALCULATION OF THE PROBABILITY FOR BOTH DEGREES OF FREEDOM ODD
C-----------------------------------------------------------------------
   50 y = 1.0 - x
      h = .63661977*sqrt(x*y)
      fish = .63661977*acos(sqrt(x))
      IF (n2.EQ.1) GO TO 70
      m = n2 - 2
      DO 60 i = 1,m,2
          fish = fish + h
          h = h*x* (i+1)/ (i+2)
   60 CONTINUE
   70 IF (n1.EQ.1) RETURN
      h = h*n2
      m = n1 - 2
      DO 80 i = 1,m,2
          fish = fish - h
          h = h*y* (n2+i)/ (i+2)
   80 CONTINUE
      RETURN
   90 d1 = n1
      d2 = n2
      dt = (d1/d2)*f
      dn = sqrt((2.*d2-1.)*dt) - sqrt(2.*d1-1.)
      x = dn/sqrt(1.+dt)
      fish = phi(x)
      RETURN
      END
      REAL FUNCTION phinv(p)
C     .. Scalar Arguments ..
      REAL p
C     ..
C     .. Local Scalars ..
      REAL php,pt,t3,t4p,t5p,xt,z
      INTEGER i,k
C     ..
C     .. External Functions ..
      REAL phi
      EXTERNAL phi
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,alog,exp,sqrt
C     ..
      IF (p.EQ.1.0) GO TO 80
      IF (p.EQ.0.0) GO TO 90
      IF (p.GT.1.0) GO TO 100
      IF (p.LT.0.0) GO TO 100
      k = 1
      IF (p.GT.0.5) GO TO 40
   10 t3 = sqrt(-2.0*alog(p))
      t4p = 2.515517 + .802853*t3 + .010328*t3*t3
      t5p = 1.0 + 1.432788*t3 + .189269*t3*t3 + .001308*t3*t3*t3
      xt = t3 - t4p/t5p
      xt = -xt
   20 DO 30 i = 1,100
          php = exp(-0.5*xt*xt)
          pt = phi(xt)
          IF (abs(p-pt).LT.p*4.0E-8) GO TO 50
          z = (p-pt)*2.50662827/php
          xt = xt + z
   30 CONTINUE
      GO TO 50
   40 p = 1.0 - p
      k = 2
      GO TO 10
   50 GO TO (60,70) k
   60 phinv = xt
      RETURN
   70 phinv = -xt
      p = 1.0 - p
      RETURN
   80 phinv = 1.0E+38
      RETURN
   90 phinv = -1.0E+38
      RETURN
  100 WRITE (6,FMT=9000) p
 9000 FORMAT ('0',5X,'ARGUMENT NOT A PROBABILITY = ',5X,E14.7)
      RETURN
      END
      REAL FUNCTION studin(n,p)
C     .. Scalar Arguments ..
      REAL p
      INTEGER n
C     ..
C     .. Local Scalars ..
      REAL alf,base,coef,delta,eps,fac,t,var,vari,void,xn,xpan,yalf,
     +     yexpan,ypan
      INTEGER i
      LOGICAL swh
C     ..
C     .. External Functions ..
      REAL expan,phinv
      EXTERNAL expan,phinv
C     ..
C     .. External Subroutines ..
      EXTERNAL gamma
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,sqrt
C     ..
      eps = 1.0E-4
      xn = n
      swh = .false.
      t = phinv(p)
      DO 20 i = 1,25
          IF (swh) GO TO 10
          alf = (xn+1.0)/2
          CALL gamma(alf,xpan,void)
          IF (xpan.EQ.0.) GO TO 30
          yalf = xn/2.0
          CALL gamma(yalf,yexpan,void)
          IF (yexpan.EQ.0.) GO TO 30
          ypan = 1.0/yexpan
          fac = sqrt(xn*3.1415926)
          coef = xpan*ypan/fac
   10     base = (1.0+t*t/xn)** (-alf)
          vari = coef*base
          swh = .true.
          var = expan(n,t)
          delta = (p-var)/vari
          t = t + delta
          IF (abs(delta)-eps) 30,30,20
   20 CONTINUE
   30 studin = t
      RETURN
      END
      SUBROUTINE cdfvec(n,p)
C-----------------------------------------------------------------------
C  V(K) IS THE CUMULATIVE DISTRIBUTION FUNCTIONAL VALUE OF THE BINOMIAL
C  DISTRIBUTION FOR PARAMETERS N AND P AT THE VALUE K
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      REAL p
      INTEGER n
C     ..
C     .. Arrays in Common ..
      REAL v(1000)
C     ..
C     .. Local Scalars ..
      REAL a,b,c,d,e,x
      INTEGER j,k,kk,l,m,nn
C     ..
C     .. External Functions ..
      REAL phi,zot
      EXTERNAL phi,zot
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC alog,exp,sqrt
C     ..
C     .. Common blocks ..
      COMMON /blek/v
C     ..
      b = alog(1.0-p)
      c = n*b
      v(1) = exp(c)
      nn = n + 1
      v(nn) = 1.00
      IF (n.GT.1000) GO TO 30
      a = alog(p)
      e = zot(n)
      m = n*p
      l = m + 1
      DO 10 k = 2,m
          j = n - k + 1
          d = e - zot(k-1) - zot(j) + (k-1)*a + j*b
          v(k) = v(k-1) + exp(d)
   10 CONTINUE
C     DO 2 K=N,L,-1
      DO 20 kk = l,n
          k = n - kk + l
          j = n - k
          d = e - zot(k) - zot(j) + k*a + j*b
          v(k) = v(k+1) - exp(d)
   20 CONTINUE
      RETURN
   30 x = sqrt(n*p* (1.0-p))
      l = n - 1
      DO 40 k = 1,l
          v(k+1) = phi((k-n*p)/x)
   40 CONTINUE
      RETURN
      END
      REAL FUNCTION phi(x)
C-----------------------------------------------------------------------
C
C  PHI CALCULATES THE AREA UNDER THE NORMAL CURVE
C  A TRANSFORMATION AND J-FRACTION ARE USED ( SEE METHOD )
C
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      REAL x
C     ..
C     .. Local Scalars ..
      REAL t,y,z
      LOGICAL upper
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,exp
C     ..
      IF (x.LT.-13.27) GO TO 30
      IF (x.GT.8.5) GO TO 40
      IF (x.NE.0.0) GO TO 10
      phi = 0.50
      RETURN
   10 upper = x .GT. 0.0
      z = abs(x)
      y = 5.6418953027302E-1*exp(-z*z/2.E0)
      z = z/1.4142135623731E0
      t = 0.0E0
      IF (abs(y/z).GT.0.0) t = y/ (z-6.9183675618730E-6+
     +                         5.0025350900390E-1/ (z+
     +                         1.2386797611409E-2+7.7267300865878E-1/
     +                         (z-4.3263982143053E0+
     +                         7.3456287718055E1/ (z+1.5040871364290E1+
     +                         6.20862456572356E0/ (z+8.8971612130791E0+
     +                         4.9182171845874E1/ (z-2.5108230069509E0-
     +                         2.8225972942737E0/ (z-9.7597917308472E-1+
     +                         2.4244213526837E1/ (z+4.8008570125081E0+
     +                         4.9227853919002E-1/ (z+7.6621170927661E0+
     +                         5.0285619125788E1/ (z-
     +                         4.6529284984655E0))))))))))
      t = t/2.E0
      IF (upper) GO TO 20
      phi = t
      RETURN
   20 phi = 1.0E0 - t
      RETURN
   30 phi = 0.0
      RETURN
   40 phi = 1.0
      RETURN
      END
      REAL FUNCTION expan(n,t)
C     .. Scalar Arguments ..
      REAL t
      INTEGER n
C     ..
C     .. Local Scalars ..
      REAL a,an,r,su,th,v,w
      INTEGER i,j
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC atan,cos,mod,sin,sqrt
C     ..
      an = n
      th = atan(t/sqrt(an))
      IF (n.EQ.1) GO TO 70
      j = n - 2
      IF (mod(n,2).EQ.0) GO TO 40
   10 w = cos(th)
      su = w
      IF (n.EQ.3) GO TO 30
      r = 0.
      v = 1.
      DO 20 i = 3,j,2
          r = r + 2.0
          v = v + 2.0
          w = w* (r/v)* (cos(th))**2
          su = su + w
   20 CONTINUE
   30 a = 2./3.1415927* (th+sin(th)*su)
      GO TO 80
   40 w = 1.
      su = 1.
      IF (n.EQ.2) GO TO 60
      r = -1.
      v = 0.
      DO 50 i = 2,j,2
          r = r + 2.
          v = v + 2.
          w = w*r/v* (cos(th))**2
          su = w + su
   50 CONTINUE
   60 a = sin(th)*su
      GO TO 80
   70 a = 2./3.1415927*th
   80 CONTINUE
      a = 0.5*a + 0.5
      expan = a
      RETURN
      END
      REAL FUNCTION pois(n,xmu)
C     .. Scalar Arguments ..
      REAL xmu
      INTEGER n
C     ..
C     .. Local Scalars ..
      REAL s,w,x,z
      INTEGER i
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC alog,exp
C     ..
      s = 0.0
      pois = 1.0
      IF (n.EQ.0) GO TO 20
      x = alog(xmu)
      DO 10 i = 1,n
          w = i
          z = w*x
          s = s + alog(w)
          pois = pois + exp(z-s)
   10 CONTINUE
   20 pois = exp(-xmu)*pois
      RETURN
      END
      REAL FUNCTION bin(n,p,m)
C     .. Scalar Arguments ..
      REAL p
      INTEGER m,n
C     ..
C     .. Local Scalars ..
      REAL c,d,e,x,y
      INTEGER i,ii,j,nn
C     ..
C     .. External Functions ..
      REAL phi,zot
      EXTERNAL phi,zot
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC alog,exp,sqrt
C     ..
      IF (n.LE.1000) GO TO 10
      x = sqrt(n*p* (1.0-p))
      bin = phi((m-n*p+0.5)/x) - phi((-n*p-0.5)/x)
      RETURN
   10 bin = 1.0
      IF (m.EQ.n) RETURN
      x = alog(p)
      y = alog(1.0-p)
      e = zot(n)
      nn = n*p
      IF (m.GT.nn) GO TO 30
      c = n*y
      bin = exp(c)
      IF (m.EQ.0) RETURN
      DO 20 i = 1,m
          d = e - zot(i) - zot(n-i) + i*x + (n-i)*y
          bin = bin + exp(d)
   20 CONTINUE
      RETURN
   30 j = m + 1
C     DO 4 I=N,J,-1
      DO 40 ii = j,n
          i = n - ii + j
          d = e - zot(i) - zot(n-i) + i*x + (n-i)*y
          bin = bin - exp(d)
   40 CONTINUE
      RETURN
      END
      REAL FUNCTION hytric(k,n,n1,nr)
C     .. Scalar Arguments ..
      INTEGER k,n,n1,nr
C     ..
C     .. Local Scalars ..
      REAL a,b,sum
      INTEGER j,k1,kl
C     ..
C     .. External Functions ..
      REAL fctrlg
      EXTERNAL fctrlg
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC exp,max0,min0
C     ..
      IF (k.LT.min0(nr,n1)) GO TO 20
   10 hytric = 1.0
      RETURN
   20 kl = max0(0,n1+nr-n)
      hytric = 0.0
      IF (k.LT.kl) RETURN
      a = fctrlg(nr)
      b = fctrlg(n-nr) + fctrlg(n1) + fctrlg(n-n1) - fctrlg(n)
      sum = 0.0
      k1 = k + 1
      DO 30 j = 1,k1
          IF (j-1-kl.LT.0) GO TO 30
          sum = sum + exp(a+b-fctrlg(j-1)-fctrlg(nr-j+1)-fctrlg(n1-j+1)-
     +          fctrlg(n-nr-n1+j-1))
   30 CONTINUE
      hytric = sum
      IF (hytric-1.0) 40,40,10
   40 RETURN
      END
      REAL FUNCTION fctrlg(n)
C     .. Scalar Arguments ..
      INTEGER n
C     ..
C     .. Local Scalars ..
      REAL a,af,afl
C     ..
C     .. External Subroutines ..
      EXTERNAL gamma
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC alog
C     ..
      a = n
      fctrlg = 0.0
      IF (n.EQ.0) RETURN
      CALL gamma(a,af,afl)
      fctrlg = afl + alog(a)
      RETURN
      END
      REAL FUNCTION betinc(x,a,b)
C     .. Scalar Arguments ..
      REAL a,b,x
C     ..
C     .. Local Scalars ..
      REAL a1,a2,a3,a4,a5,a6,a7,a8,c,d,f,p,v,y
      INTEGER i,n1,n2,n3
C     ..
C     .. Local Arrays ..
      REAL t(3)
C     ..
C     .. External Functions ..
      REAL cbrt,chi2,phi
      EXTERNAL cbrt,chi2,phi
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC sqrt
C     ..
C     .. Data statements ..
      DATA a1,a2,a3,a4,a5,a6,a7,a8/-.577191652,.988205891,-.897056937,
     +     .918206857,-.756704078,.482199394,-.193527818,.035868343/
C     ..
      IF (x.GT.0. .AND. x.LT.1.) GO TO 20
   10 betinc = x
      RETURN
   20 IF ((a+b).LT.130.) GO TO 40
      IF ((a+b-1.)* (1.-x).LT..8) GO TO 30
      c = cbrt(b*x)
      d = cbrt(a* (1.-x))
      betinc = phi((c* (3.- (1./ (3.*b)))-d* (3.- (1./ (3.*a))))/
     +         sqrt((c*c/b)+ (d*d/a)))
      RETURN
   30 i = 2.*b
      v = 2.*b
C     BETINC=1.-CHI2((1.-X)*((3.-X)*(A+B-1.)-B+1.),I,$99)
      betinc = 1. - chi2((1.-x)* ((3.-x)* (a+b-1.)-b+1.),v)
      RETURN
   40 IF (x.LT..5) GO TO 50
      y = 1.0 - x
      c = b
      d = a
      GO TO 60
   50 c = a
      d = b
      y = x
   60 n1 = c
      n2 = d
      n3 = c + d
      t(1) = c - n1
      t(2) = d - n2
      t(3) = c + d - n3
      p = 1.0
      DO 70 i = 1,n3
          IF (i.LE.n1) p = p/ (t(1)+i)
          IF (i.LT.n2) p = p/ (t(2)+i)
          IF (i.LT.n3) p = p* (t(3)+i)
   70 CONTINUE
      IF (n2.EQ.0) p = p*d
      IF (n3.EQ.0) p = p/ (c+d)
      DO 80 i = 1,3
          f = t(i)
          t(i) = 1.0 + f* (a1+f* (a2+f* (a3+f* (a4+f* (a5+f* (a6+f* (a7+
     +           f* (a8))))))))
   80 CONTINUE
      p = p* (y**c)* ((1.-y)**d)*t(3)/ (t(1)*t(2))
      i = 0
      betinc = 0.
      f = c + 1.
      t(1) = c + d
   90 betinc = betinc + p
      p = p*y* ((t(1)+i)/ (f+i))
      i = i + 1
      IF ((p/betinc).GT.5.E-6) GO TO 90
      IF (.NOT.x.LT..5) betinc = 1. - betinc
      RETURN
      END
      REAL FUNCTION gamin(x,a)
C     .. Scalar Arguments ..
      REAL a,x
C     ..
C     .. Local Scalars ..
      REAL a1,a2,a3,a4,a5,a6,a7,a8,f,p
      INTEGER i,n
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC alog,exp
C     ..
C     .. Data statements ..
      DATA a1,a2,a3,a4,a5,a6,a7,a8/-.577191652,.988205891,-.897056937,
     +     .918206857,-.756704078,.482199394,-.193527818,.035868343/
C     ..
      IF (x.GT.0.) GO TO 10
      gamin = x
      RETURN
   10 n = a
      f = a - n
C-----------------------------------------------------------------------
C     CALCULATION OF COMPLETE GAMMA FUNCTION
C-----------------------------------------------------------------------
      p = exp(-x+f*alog(x))/ (1.+f* (a1+f* (a2+f* (a3+f* (a4+f* (a5+
     +    f* (a6+f* (a7+f*a8))))))))
      IF (n.EQ.0) GO TO 30
      DO 20 i = 1,n
          p = p*x/ (f+i)
   20 CONTINUE
   30 gamin = p
      f = a + 1.
C-----------------------------------------------------------------------
C     CALCULATION OF APPROXIMATING SUM TO INCOMPLETE GAMMA FUNCTIONS
C-----------------------------------------------------------------------
   40 p = p*x/f
      IF ((p/gamin).LT.5.E-8) GO TO 50
      gamin = gamin + p
      f = f + 1.0
      GO TO 40
   50 RETURN
      END
      SUBROUTINE gamma(alpha,expan,yexpan)
C----------------------------------------------------------------------
C  COMPUTES GAMMA(A)  AND  LOG(GAMMA(A))
C  EXPAN = GAMMA(A), YEXPAN = LOG(GAMMA(A))
C----------------------------------------------------------------------
C     .. Scalar Arguments ..
      REAL alpha,expan,yexpan
C     ..
C     .. Local Scalars ..
      REAL a1,a2,a3,a5,float,frac,gl,yi,z
      INTEGER i,k,num
C     ..
C     .. Local Arrays ..
      REAL c(26)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC alog,exp
C     ..
C     .. Data statements ..
      DATA (c(i),i=1,26)/1.0,5.7721566E-01,-6.5587807E-01,
     +     -4.2002635E-02,1.6653861E-01,-4.2197734E-02,-9.6219715E-03,
     +     7.2189432E-03,-1.1651675E-03,-2.1524167E-04,1.2805028E-04,
     +     -2.0134854E-05,-1.2504934E-06,1.1330272E-06,-2.0563384E-07,
     +     6.1160950E-09,5.0020075E-09,-1.1812746E-09,1.043427E-10,
     +     7.7823E-12,-3.6968E-12,5.100E-13,-2.06E-14,-5.4E-15,1.4E-15,
     +     1.0E-16/
C     ..
      IF (alpha.GT.8.0) GO TO 60
      num = alpha
      float = num
      frac = alpha - float
      IF (frac.LE.1.E-08) GO TO 40
      yexpan = 0.
      DO 10 i = 1,26
          yexpan = yexpan + c(i)*frac**i
   10 CONTINUE
      gl = alog(yexpan)
      IF (num.EQ.0) GO TO 30
      DO 20 k = 1,num
          z = frac + k - 1.0
          gl = gl - alog(z)
   20 CONTINUE
   30 yexpan = -gl
      expan = 0.
      IF (gl.GT.-650.) expan = exp(-gl)
      RETURN
   40 gl = 0.
      num = num - 1
      DO 50 i = 1,num
          yi = i
          gl = gl - alog(yi)
   50 CONTINUE
      GO TO 30
   60 a1 = 12.0*alpha
      a2 = alpha*alpha
      a3 = 30.*a1*a2
      a5 = 105.*a1*a2*a2
      gl = - (alpha-0.5)*alog(alpha) + alpha - 1.0/a1 + 1.0/a3 -
     +     1.0/a5 - .91893853
      GO TO 30
      END
      REAL FUNCTION zap(h)
C-----------------------------------------------------------------------
C   ZAP = GAMMA(H)
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      REAL h
C     ..
C     .. Local Scalars ..
      REAL ex
C     ..
C     .. External Subroutines ..
      EXTERNAL gamma
C     ..
      CALL gamma(h,zap,ex)
      RETURN
      END
      REAL FUNCTION zot(n)
C-----------------------------------------------------------------------
C   ZOT = ALOG( N FACTORIAL )
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      INTEGER n
C     ..
C     .. Local Scalars ..
      REAL al,ex
C     ..
C     .. External Subroutines ..
      EXTERNAL gamma
C     ..
      al = n + 1
      CALL gamma(al,ex,zot)
      RETURN
      END
      REAL FUNCTION chi2(x,v)
C-----------------------------------------------------------------------
C  CHI2(X,V) = TO A VALUE OF THE CHI-SQUARE DISTRIBUTION FUNCTION AT THE
C  POINT X, WITH V DEGREES OF FREEDOM
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      REAL v,x
C     ..
C     .. Local Scalars ..
      REAL a,b,c,d,e,g,p,s,v1,y,z
      INTEGER i,in,j,k,n
C     ..
C     .. External Functions ..
      REAL cbrt,phi,zip
      EXTERNAL cbrt,phi,zip
C     ..
C     .. External Subroutines ..
      EXTERNAL overfl
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,alog,exp,float,ifix,mod,sqrt
C     ..
      chi2 = 0.0
      IF (x.LT.0.0) RETURN
      v1 = ifix(v)
      IF (abs(v-v1).LT.1.0E-8) GO TO 50
   10 CONTINUE
      a = (v/2.0)* (-0.69314718+alog(x)) - x/2.0 - zip((v+2.0)/2.0)
      IF (abs(a).GT.88.028) GO TO 40
      b = alog(x)
      s = 0.0
      d = 0.0
      DO 20 j = 1,1000
          s = s + alog(v+2*j)
          c = j*b - s
          IF (abs(c).GT.88.028) GO TO 40
          e = a + c
          IF (exp(e).LT.1.E-08) GO TO 30
          d = d + exp(c)
   20 CONTINUE
   30 chi2 = exp(a)* (1.0+d)
      RETURN
   40 y = 0.22222222/v
      z = (cbrt(x/v)- (1.0-y))/sqrt(y)
      chi2 = phi(z)
      RETURN
   50 n = v
      IF (n.EQ.1) GO TO 10
      IF (n.EQ.2) GO TO 140
      IF (x.LT.v) GO TO 100
      a = -.918938533 - (x/2.0)
      z = exp(a)
   60 IF (mod(n,2).EQ.0) GO TO 80
      p = 1.0 - phi(sqrt(x))
      b = alog(x)
      k = (n-1)/2
      s = 0.0
      d = 0.0
      i = 0
      DO 70 j = 1,k
          s = s + alog(float(2*j-1))
          c = (j-.5)*b - s
          IF (abs(c).GT.88.028) GO TO 40
          d = d + exp(c)
          CALL overfl(i)
          IF (i.EQ.1) GO TO 40
   70 CONTINUE
      chi2 = 1.0 - (2.0*p+2.0*z*d)
      RETURN
   80 k = (n-2)/2
      b = alog(x)
      s = 0.0
      d = 0.0
      i = 0
      DO 90 j = 1,k
          s = s + alog(float(2*j))
          c = j*b - s
          IF (abs(c).GT.88.028) GO TO 40
          d = d + exp(c)
          CALL overfl(i)
          IF (i.EQ.1) GO TO 40
   90 CONTINUE
      chi2 = 1.0 - (2.50662827*z* (1.0+d))
      IF (chi2.LT.0.0) GO TO 40
      RETURN
  100 IF (mod(n,2).EQ.0) GO TO 110
      g = sqrt(x)
      chi2 = 2.0*phi(g) - 1.0
      g = g/1.25331414
      in = 3
      GO TO 120
  110 in = 4
      g = x/2.0
      IF (g.GT.88.028) GO TO 40
      chi2 = 1.0 - exp(-g)
  120 IF (n.LT.3) RETURN
      g = g*exp(-x/2.0)
      DO 130 i = in,n,2
          chi2 = chi2 - g
          g = g*x/i
  130 CONTINUE
      RETURN
  140 chi2 = 1.00 - exp(-x/2.0)
      RETURN
      END
      REAL FUNCTION zip(a)
C-----------------------------------------------------------------------
C   ZIP = ALOG( GAMMA(A) )
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      REAL a
C     ..
C     .. Local Scalars ..
      REAL dum
C     ..
C     .. External Subroutines ..
      EXTERNAL gamma
C     ..
      CALL gamma(a,dum,zip)
      RETURN
      END
