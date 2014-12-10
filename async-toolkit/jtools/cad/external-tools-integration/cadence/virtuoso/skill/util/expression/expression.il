; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$



(defun ExpressionReplaceSymbolsWithValues ( Expression Symbols )
 "Replaces all symbols in Expression that match a Symbol name in Symbols.  Alternatively, if Symbols is t, it replaces all of the symbols.  If Symbols is nil, it replaces none of them."
  (cond (
         ( null Expression )
         nil )
        (
         ( listp Expression )
         ( cons 
           ( ExpressionReplaceSymbolsWithValues ( car Expression ) Symbols )
           ( ExpressionReplaceSymbolsWithValues ( cdr Expression ) Symbols ) ) )
        (
         ( symbolp Expression )
         (if ( equal Symbols t )
             ( symeval Expression )
           (let ( 
                 ( Symbol
                   ( exists Symbol Symbols
                            ( equal Symbol  Expression ) ) ) )
             (if Symbol
                 ( symeval ( car Symbol ) )
               Expression ) ) ) )
        ( 
         t
         Expression ) ) )
