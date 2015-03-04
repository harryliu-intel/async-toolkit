2 0
java com.avlsi.tools.sigscan.AnalogTrace
java com.avlsi.tools.sigscan.Attribute
java com.avlsi.tools.sigscan.AttributeValue
java com.avlsi.tools.sigscan.BooleanAttribute
java com.avlsi.tools.sigscan.ChannelSigscan
java com.avlsi.tools.sigscan.DebugOpts
java com.avlsi.tools.sigscan.GrowingArray
java com.avlsi.tools.sigscan.IntAttribute
java com.avlsi.tools.sigscan.LoggedDouble
java com.avlsi.tools.sigscan.LoggedInt
java com.avlsi.tools.sigscan.LoggedLogicArray
java com.avlsi.tools.sigscan.LoggedLong
java com.avlsi.tools.sigscan.LoggedString
java com.avlsi.tools.sigscan.LoggedVar
java com.avlsi.tools.sigscan.LoggingObject
java com.avlsi.tools.sigscan.LongAttribute
java com.avlsi.tools.sigscan.NativeHandle
java com.avlsi.tools.sigscan.NodeLogger
java com.avlsi.tools.sigscan.Sigscan
java com.avlsi.tools.sigscan.SigscanException
java com.avlsi.tools.sigscan.StringAttribute
java com.avlsi.tools.sigscan.Transaction
java com.avlsi.tools.sigscan.TransactionFiber
java com.avlsi.tools.sigscan.TransactionType
$arch/lib/libSigscan.so	              libSigscan.so 775
ifarch i686
$arch/lib/libdwapi.so                 transrecord/lib/libdwapi.so 775
$arch/lib/libsdi2-sst2-gcc_4.1.so     transrecord/lib/libsdi2-sst2-gcc_4.1.so 775 p
$arch/lib/libdcapi.so                 transrecord/lib/libdcapi.so 775
endif
ifarch x86_64
$arch/lib/libdwapi.so                 transrecord/lib64/libdwapi.so 775 p
$arch/lib/libsdi2-sst2-gcc_4.1.so     transrecord/lib64/libsdi2-sst2-gcc_4.1.so 775 p
$arch/lib/libdcapi.so                 transrecord/lib64/libdcapi.so 775 p
endif
