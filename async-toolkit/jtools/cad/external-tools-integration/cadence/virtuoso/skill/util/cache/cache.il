; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun CacheCreate (  )
  ( makeTable `FunctionCache )
)

(defun CacheClear ( FunctionCacheTable ) 
  ( foreach FunSymbol FunctionCacheTable 
    ( remprop FunSymbol `CacheTable )
  )
)

(defun CacheGetValue ( FunctionCacheTable FunSymbol @rest Args )
  ( setarray FunctionCacheTable FunSymbol t )
  ( let (
	 ( Function ( getd FunSymbol ) )
         ( CacheTable ( get FunSymbol `CacheTable ) )
        )
    ( cond (
	    ( null CacheTable )
	    ( setq CacheTable ( makeTable FunSymbol ) )
	    ( putprop FunSymbol CacheTable `CacheTable )
	   )
    )    
    ( let (
	   ( CachedValue ( arrayref CacheTable Args ) )
	  )
      ( let (
	     ( Value 
	       ( cond (
		       ( boundp `CachedValue ) 
		        CachedValue
		      )
		      (
		       ( apply Function Args )		      
		      )
	       )
	     )
	    )
	( setarray CacheTable Args Value )	      
	Value
      )
    )
  )
)
