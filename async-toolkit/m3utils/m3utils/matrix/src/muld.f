c
c     $Id$
c
      subroutine muld(a, b, p, ar, ac, bc)
c
c     Modula-3/C array indexing M(COLS,ROWS)
c
      integer ar, ac, bc
      double precision a(ac,ar), b(bc,ac), p(bc,ar)

      do 200 row = 1, ar
         do 150 col = 1, bc
            elem = 0.0
            do 100 term = 1, ac
               elem = elem + a(term,row) * b(col,term)
 100        continue
            p(col,row) = element
 150     continue
 200  continue
      end
