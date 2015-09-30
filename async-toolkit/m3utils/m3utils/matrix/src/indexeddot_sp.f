c
c     $Id: indexeddot.f.tmpl,v 1.4 2008/02/04 00:10:46 mika Exp $
c	
      subroutine indexeddot_sp(v, idx, n, w, res)
      implicit none
      integer n, idx(n), i
      real v(*), w(n), res
      
      res = 0.0

      do 100 i=1,n

c      	note Modula-3 array indexing here.. idx(i)+1 gives the Fortran index

         res = res + v(idx(i)+1) * w(i)
 100  continue
      end
	
