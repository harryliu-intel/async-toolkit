# $Id$
# $DateTime$
# $Author$

import string
from com.avlsi.file.cdl.util.rename.CDLNameInterface import *

class CastNames2CadenceNames( CDLNameInterface ):
  "Converts cast names to cadence names."
  
  def renameCell( self, name ):
    badName = 0
    inParens = 0
    curlyCount = 0
    
    result=u""
    
    nameLength = len( name )
    i = 0
    while ( ( i < nameLength ) &
            ( not badName ) ):
      char = name[i]
      if ( char == u"." ):
        badName = inParens
        result = result + u"."
      elif ( char == u"," ):
        badName = not inParens
        result = result + u"_"
      elif ( char == u"(" ):
        badName = inParens
        inParens = 1
        result = result + u"-L"
      elif ( char == u")" ):
        badName = not inParens
        inParens = 0
        result = result + u"-R"
      elif ( char == u"{" ):
        badName = not inParens
        curlyCount = curlyCount + 1
        result = result + u"-L"
      elif ( char == u"}" ):
        badName = not ( inParens & ( curlyCount > 0 ) )
        curlyCount = curlyCount - 1
        result = result + u"-R"
      elif ( char == u"-" ):
        badName = ( ( not inParens ) |
                    ( i >= ( nameLength - 1 ) ) |
                    ( string.find(string.digits, name[ i + 1 ] ) == -1 ) )
        result = result + u"-"
      elif ( char == u"_" ):
        result = result + u"_"
      elif ( string.find( string.digits + string.ascii_letters, char ) != -1 ):
        result = result + char
      else:
        raise ValueError, "'%s' in \"%s\" can not be translated" % ( char, name )
      i = i + 1
    if ( badName ):
      raise ValueError, "\"%s\" can not be translated into a cadence name." % name
    return result
    
  def renameNode( self, name ):
    return self.translate( name )
    
  def renameDevice( self, name ):
    return self.translate( name )
    
  def renameSubCellInstance( self, name ):
    return self.translate( name )
  
  def renameTransistorModel( self, oldModelName ):
    return oldModelName
    
  def translate( self, name ):
    badName = 0
    inParens = 0
    inBrackets = 0
    
    nameLength = len( name )
    i = 0
   
    result = u""
    
    while ( ( i < nameLength ) &
            ( not badName ) ):
      char = name[i]
      if ( char == u"!" ):
        result = result + u"!"
      elif ( char == u"." ):
        result = result + u"."
      elif ( char == u"," ):
        badName = ( not inBrackets ) | inParens
        result = result + u"][";
      elif ( char == u"[" ):
        badName = inBrackets | inParens;
        inBrackets = 1
        result = result + u"["
      elif ( char == u"]" ):
        badName = ( not inBrackets ) | inParens
        inBrackets = 0
        result = result + u"]"
      elif ( char == u"-" ):
        badName = not ( inBrackets | inParens )
        result = result + u"-"
      elif ( char == u"_" ):
        result = result + u"_"
      elif ( char == u"#" ):
        badName = inBrackets | inParens
        result = result + u"#"
      elif ( string.find(string.digits + string.ascii_letters, char ) != -1 ):
      	result = result + char
      else:
        raise ValueError, "'%s' in \"%s\" can not be translated" % ( char, name )
      i = i + 1
    if ( badName ):
      raise ValueError, "\"%s\" can not be translated into a cadence name." % name
    return result
