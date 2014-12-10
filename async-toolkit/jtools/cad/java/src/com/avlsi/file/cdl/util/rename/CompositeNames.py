from com.avlsi.file.cdl.util.rename.CDLNameInterface import *

class CompositeNames( CDLNameInterface ):
    
    def __init__(self,a,b):
        self.names1 = a
        self.names2 = b
        
    def renameCell(self, name):
        return self.names2.renameCell( self.names1.renameCell(name) )

    def renameNode(self, name):
        return self.names2.renameNode( self.names1.renameNode(name) )

    def renameDevice(self, name):
        return self.names2.renameDevice( self.names1.renameDevice(name) )

    def renameSubCellInstance(self, name):
        return self.names2.renameSubCellInstance( self.names1.renameSubCellInstance(name) )

    def renameTransistorModel(self, name):
        return self.names2.renameTransistorModel( self.names1.renameTransistorModel(name) )
