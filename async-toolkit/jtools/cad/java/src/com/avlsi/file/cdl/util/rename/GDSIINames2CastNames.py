import string

from com.avlsi.file.cdl.util.rename.CDLNameInterface import *

class GDSIINames2CastNames( CDLNameInterface ):
    "Converts gds2 names to cast names."

    def renameCell(self, name):
        return self.translate(name)

    def renameNode(self, name):
        return self.translate(name)

    def renameDevice(self, name):
        return self.translate(name)

    def renameSubCellInstance(self, name):
        return self.translate(name)

    def renameTransistorModel(self, name):
        if ( name[0] in ( "n", "N" ) ):
            return "n"
        else:
            return "p"

    def translate(self, s):
        sb = ""
        nameLength = len(s)
        i = 0
        while ( i < nameLength ):
            c = s[i]
            if ( c == u'_' ):
                usb = ""
                while ( i + 1 < nameLength ):
                    i = i + 1
                    uc = s[i];
                    if ( uc == u'_' ):
                        break
                    usb = usb + uc
                sb = sb + self.translateEscaped(usb)
            else:
                sb = sb + c
            i = i + 1 
        return sb

    def translateEscaped(self, str):
        if(len(str) == 1 ):
            c = str[0]
            if ( c == u'D' ):
                return u'.'
            elif ( c == u'C' ):
                return u','
            elif ( c == u'l' ):
                return u'['
            elif ( c == u'r' ):
                return u']'
            elif ( c == u'L' ):
                return u'('
            elif ( c == u'R' ):
                return u')'
            elif ( c == u'M' ):
                return u'-'
            elif ( c == u'U' ):
                return u'_'
            elif ( c == u'H' ):
                return u'#'
            else:
                errorMessage =  "_" + str + "_ is and invalid escape construct"
                raise CDLRenameException, errorMessage
        elif(len(str) == 0 ):
            errorMessage =  "_" + str + "_ is and invalid escape construct"
            raise CDLRenameException, errorMessage
        else:
            charVal = int(str, 16)
            if (charVal < 256 ):
                return chr(charVal)
            else:
                errorMessage =  "_" + str + "_ is and invalid escape construct"
                raise CDLRenameException, errorMessage
            
