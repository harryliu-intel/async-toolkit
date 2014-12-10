#!/usr/bin/python
import sys
import getopt

from com.avlsi.file.cdl.util.rename.IdentityNames import *
from com.avlsi.file.cdl.util.rename.CompositeNames import *
from com.avlsi.file.cdl.util.rename.CastNames2CadenceNames import *
from com.avlsi.file.cdl.util.rename.CastNames2GDSIINames import *
from com.avlsi.file.cdl.util.rename.CadenceNames2CastNames import *
from com.avlsi.file.cdl.util.rename.GDSIINames2CastNames import *
from com.avlsi.file.cdl.util.rename.CDLNameInterface import CDLRenameException

import string

ArgError = "Error parsing arguments"

def usage():
    print "Usage: " + sys.argv[0]
    print '   --type=[cell|node|instance|model]'
    print '   --from=[cast,cadence,gds2]'
    print '   --to=[cast,cadence,gds2]'


NamesDict = { "cast" :
              { "cast" : IdentityNames(),
                "cadence" : CastNames2CadenceNames(),
                "gds2" : CastNames2GDSIINames() },
              "cadence" :
              { "cast" : CadenceNames2CastNames(),
                "cadence" : IdentityNames(),
                "gds2" : CompositeNames( CadenceNames2CastNames(), CastNames2GDSIINames() ) },
              "gds2" :
              { "cast" : GDSIINames2CastNames(),
                "cadence" : CompositeNames( GDSIINames2CastNames(), CastNames2CadenceNames() ),
                "gds2" : IdentityNames() }
              }

def main(argv):
    try:                                
        opts, args = getopt.getopt(argv, "", ["type=", "from=", "to=" ])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    
    renameType = ""
    fromCode = ""
    toCode = ""
    try:
        for key, val in opts:
            if (key == "--type"):
                renameType = val
            elif (key == "--from"):
                fromCode = val
            elif (key == "--to"):
                toCode = val
            else:
                raise ArgError, key
        if ( not ( renameType in ("cell", "node", "instance" "device" "model" ) ) or
             not ( fromCode in ("cast", "gds2", "cadence" ) ) or
             not ( toCode in ("cast", "gds2", "cadence" ) ) ):
            raise ArgError, "missing parameter"
    except ArgError, message:
        print ArgError + ": " +  message
        usage()
        sys.exit(2)

    
    Names = NamesDict[fromCode][toCode]

    if( renameType == "cell" ):
        renameFunc = Names.renameCell
    elif( renameType == "node" ):
        renameFunc = Names.renameNode
    elif( renameType == "instance" ):
        renameFunc = Names.renameSubCellInstance
    elif( renameType == "device" ):
        renameFunc = Names.renameDevice
    elif( renameType == "model" ):
        renameFunc = Names.renameTransistorModel

    while 1:
        try:
            line = raw_input()
        except EOFError:
            break
        names =  string.splitfields( line, " " )
        newnames = map( renameFunc, names )
        newline = string.joinfields( newnames, " " )
        print newline
                    
if __name__ == "__main__":
    main(sys.argv[1:])

    
