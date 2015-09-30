c
c     $Id: delta.f.tmpl,v 1.3 2008/02/02 13:30:43 mika Exp $
c	
      subroutine delta_sp(v, d, n)
      implicit none
      integer  i, n
      real v(n+1), d(n)
      
      do 100 i=1,n
         d(i) = v(i+1)-v(i)
 100  continue
      end
	
