; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun PropGetPropForPropName ( Props PropName )
  ( car ( exists Prop Props
                 ( equal ( getq Prop name ) PropName ) ) ) )

(defun PropGetPropValueForPropName ( Props PropName )
  ( getq ( PropGetPropForPropName Props PropName ) value ) )

(defun PropGetParameters ( DBId )
  ( PropGetPropValueForPropName
    ( getq DBId prop )
    "parameters" ) )

(defun PropGetPropValueFromListOfProps ( PropsList ParameterName )
  (let (
        ( Value nil ) )
    (while ( and PropsList 
                 ( null Value ) )
      ( setq Value 
             ( PropGetPropValueForPropName
               ( car PropsList )
               ParameterName ) )
      ( setq PropsList ( cdr PropsList ) ) )
    Value ) )

(defun PropGetPropertyListFromProps ( Props )
  ( PropGetPropertyListFromListOfProps
    ( list Props ) ) )

(defun PropGetPropertyListFromListOfProps ( PropsList )
  (let (
        ( PropTable ( makeTable `foo nil ) ) )
    ( foreach Props PropsList
              ( foreach Prop Props
                        ( setarray PropTable
                                   ( list 
                                     ( getq Prop name )
                                     ( getq Prop valueType ) )
                                   ( getq Prop value ) ) ) )
    ( mapcar 
      (lambda ( x )
        ( list ( car ( car x ) )
               ( cadr ( car x ) )
               ( cadr x ) ) )
      ( tableToList PropTable ) ) ) )

(defun PropGetPropTripleFromPropertyList ( PropertyList PropertyName )
  ( car ( last ( setof Triple PropertyList 
                       ( equal ( car Triple ) PropertyName ) ) ) ) )

(defun PropGetPropValueFromPropertyList ( PropertyList PropertyName )
  ( nth 2 
        ( PropGetPropTripleFromPropertyList PropertyList PropertyName ) ) )
