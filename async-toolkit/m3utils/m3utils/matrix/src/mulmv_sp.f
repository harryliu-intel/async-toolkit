c
c     $Id: mulmv.f.tmpl,v 1.2 2008/02/02 11:32:48 mika Exp $
c
      subroutine mulmv_sp(a, b, c, rows, cols)
      implicit none
      
      integer rows, cols, i, j
      real a(cols,rows), b(cols), c(rows)

      do 200 i=1,rows
         c(i) = 0.0
         do 100 j=1,cols
            c(i) = c(i) + a(j,i)*b(j)
 100     continue
 200  continue
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
