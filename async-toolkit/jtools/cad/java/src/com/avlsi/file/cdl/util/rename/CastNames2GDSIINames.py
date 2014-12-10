# $Id$
# $DateTime$
# $Author$

import string
from com.avlsi.file.cdl.util.rename.CDLNameInterface import *

class CastNames2GDSIINames( CDLNameInterface ):
  "Converts cast names to gds II names."
  def translate( self, name ):
    result = u""
    for char in name:
      if ( char == u"." ):
        result = result + u"_D_"
      elif ( char == u"," ):
        result = result + u"_C_"
      elif ( char == u"[" ):
        result = result + u"_l_"
      elif ( char == u"]" ):
        result = result + u"_r_"
      elif ( char == u"(" ):
        result = result + u"_L_"
      elif ( char == u")" ):
        result = result + u"_R_"
      elif ( char == u"-" ):
        result = result + u"_M_"
      elif ( char == u"_" ):
        result = result + u"_U_"
      elif ( char == u"#" ):
        result = result + u"_H_"
      elif ( string.find(string.digits + string.ascii_letters, char ) != -1 ):
        result = result + char;
      else:
        hexstring = u"%00x" % ord( char );
        if ( len( hexstring ) == 2 ):
          result = result + u"_" + hexstring + u"_"
        else:
          raise ValueError, "The code point of %s is greater than 255." % char
    return result

  def renameCell( self, name ):
    return self.translate( name )
  def renameNode( self, name ):
    return self.translate( name )
  def renameDevice( self, name ):
    return self.translate( name )
  def renameSubCellInstance( self, name ):
    return self.translate( name )
  def renameTransistorModel( self, oldModelName ):
    return oldModelName
