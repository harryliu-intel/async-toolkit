/*** a distributed RC wire with 3 internal nodes ***/
define rc(a,b)(r,c)
  {
  res(a,x0)(r/4)
  res(x0,x1)(r/4)
  res(x1,x2)(r/4)
  res(x2,b)(r/4)
  cap(x0)(c/3)
  cap(x1)(c/3)
  cap(x2)(c/3)
  }

/*** an inductor ***/
define inductor(A,B)(L)
  {
  .C=1e-8; /*** try to keep x on similar scale as Voltage ***/
  cap(x)(C);
  source
    {
    B-A -> x
    C*x/L -> A
    -C*x/L -> B
    }
  }

/*** a distributed LC wire with 3 internal nodes ***/
define lc(a,b)(l,c)
  {
  inductor(a,x0)(l/4)
  inductor(x0,x1)(l/4)
  inductor(x1,x2)(l/4)
  inductor(x2,b)(l/4)
  cap(x0)(c/3)
  cap(x1)(c/3)
  cap(x2)(c/3)
  }
