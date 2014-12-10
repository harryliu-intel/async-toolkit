import string

from com.avlsi.file.cdl.util.rename.CDLNameInterface import *

class CadenceNames2CastNames( CDLNameInterface ):
    "Converts cadence names to cast names."

    def __init__(self):
        self.curlyCount = 0
        
    def left(self):
        if ( self.curlyCount == 0 ):
            result = u'('
        else:
            result = u'{'
        self.curlyCount = self.curlyCount + 1
        return result

    def right(self):
        if (self.curlyCount <= 0):
            raise CDLRenameException,  "Unmatched parenthesis!"
        elif (self.curlyCount == 1):
            result = u')'
        else:
            result = u'}'
        self.curlyCount = self.curlyCount - 1
        return result

    def renameCell(self, name):
        badName = 0
        sb = ""
        self.curlyCount = 0

        nameLength = len( name )

        i = 0
        while ( ( i < nameLength ) and
                ( not badName ) ):
            c = name[i]
            if ( c == u'.' ):
                badName = self.curlyCount != 0
                sb = sb + u'.'
            elif ( c == u'-' ):
                if (i + 1 < nameLength):
                    d = name[i+1]
                    if (d == u'L'):
                        i = i + 1
                        sb = sb + self.left()
                    elif (d == u'R'):
                        i = i + 1
                        sb = sb + self.right()
                    else:
                        sb = sb + u'-'
                else:
                    sb = sb + '-'
            elif ( c == u'_' ):
                if (self.curlyCount > 0):
                    sb = sb + u','
                else:
                    sb = sb + u'_'
            else:
                if ( string.find( string.digits + string.ascii_letters, c ) != -1 ):
                     sb = sb + c
                else:
                    errorMessage = "Unable to translate: " + c
                    raise CDLRenameException, errorMessage
            i = i + 1
            
        if ( badName or self.curlyCount != 0 ):
            errorMessage = "\"" + name + "\" is an invalid cell name."
            raise CDLRenameException, errorMessage
        return sb

    def renameNode(self, name):
        return self.translate(name)

    def renameDevice(self, name):
        return self.translate(name)

    def renameSubCellInstance(self, name):
        return self.translate(name)

    def renameTransistorModel(self, name):
        if ( name[0] in ( "n", "N" ) ):
            return u"n"
        else:
            return u"p"

    def translate(self, s):
        sb = ""
        nameLength = len(s)
        i = 0
        while ( i < nameLength ):
            c = s[i]
            if ( c in ( u'!' u'.' u'[' u'(' u')' u'_' ) ):
                sb = sb + c
            elif ( c == u']' ):
                if (i + 1 < len(s) and s[i+1] == u'['):
                    sb = sb + u','
                    i = i + 1
                else:
                    sb = sb + u']'
            elif ( c == u'-' ):
                if (i + 1 < nameLength and s[i+1]  == u'H'):
                    sb = sb + u'#'
                    i = i + 1
                else:
                    sb = sb + u'-'
            else:
                if ( string.find( string.digits + string.ascii_letters, c ) != -1 ):
                     sb = sb + c
                else:
                    errorMessage = "Unable to translate: " + c
                    raise CDLRenameException, errorMessage
            i = i + 1
        return sb

