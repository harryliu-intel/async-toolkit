import java.math.BigInteger;
import java.util.LinkedList;
import java.util.Random;
import com.avlsi.csp.csp2java.runtime.*;
import com.avlsi.tools.cosim.DeviceParameters;
import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.sigscan.LoggedLogicArray;
import com.avlsi.tools.sigscan.LoggedLong;
import com.avlsi.tools.sigscan.LoggedString;
import com.avlsi.tools.tsim.AbstractDevice;
import com.avlsi.tools.tsim.Arbiter;
import com.avlsi.tools.tsim.Arbiter.Action;
import com.avlsi.tools.tsim.Arbiter.Alternative;
import com.avlsi.tools.tsim.Arbiter.Linkage;
import com.avlsi.tools.tsim.Arbiter.NullaryPredicate;
import com.avlsi.tools.tsim.Arbiter.Term;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.EmptyWaitSetException;
import com.avlsi.tools.tsim.Wait;
import com.avlsi.tools.tsim.WaitFactory;
import com.avlsi.tools.tsim.Waitable;
import com.avlsi.tools.tsim.WideNode;
import com.avlsi.util.container.Pair;
import com.avlsi.fast.metaparameters.*;
import com.avlsi.csp.grammar.*;

public final class CSP2ClassTemp_45_collatz_46_STARTSPLIT_40_2_41_ extends CspRuntimeAbstractDevice {

// instance variables
private int outputOffset = 0;
private int inputOffset = 0;
private int nodeOffset = 0;
// declare channel vars
// outputPortDecl: (port-definition L in (channel standard.channel.bd(61) 1 2305843009213693952 f ((port-definition C out (structure standard.channel.bdc ((port-definition q out (node #f 1)) (port-definition a in (node #f 1))))) (port-definition D out (node #t 61)))))
public final ChannelInput _L;
// outputPortDecl: (port-definition R out (array (range 0 1) (channel standard.channel.bd(61) 1 2305843009213693952 f ((port-definition C out (structure standard.channel.bdc ((port-definition q out (node #f 1)) (port-definition a in (node #f 1))))) (port-definition D out (node #t 61))))))
public final CspChannelOutArray1 _R;
// outputPortDecl: (port-definition Vdd in (node #f 1))
public final CspNode _Vdd;
// outputPortDecl: (port-definition GND in (node #f 1))
public final CspNode _GND;
// outputPortDecl: (port-definition _RESET in (node #f 1))
public final CspNode __RESET;
// outputPortDecl: (port-definition START in (node #f 1))
public final CspNode _START;
// outputPortDecl: (port-definition DLY in (node #f 1))
public final CspNode _DLY;
// outputPortDecl: (port-definition CAPTURE in (node #f 1))
public final CspNode _CAPTURE;
// outputPortDecl: (port-definition PASSTHRU in (node #f 1))
public final CspNode _PASSTHRU;
// outputPortDecl: (port-definition INJECT in (node #f 1))
public final CspNode _INJECT;
// probFilter.process...strictVars
// linked arbitration nodes
// arbiterVisitor declareLinkages
// constructors
public CSP2ClassTemp_45_collatz_46_STARTSPLIT_40_2_41_(final DeviceParameters deviceParams, final ChannelInput[] in, final ChannelOutput[] out, final WideNode[] nodes
) throws InterruptedException {
super(deviceParams.getName(), in, out, nodes, deviceParams.isOutputSuppressed(), deviceParams.getArbitrationMode());
_srandom(new CspInteger(new BigInteger(Long.toString(deviceParams.getSeed()))));
setDigitalTau(deviceParams.getDigitalTau());
this._L = in[inputOffset++];
this._R = new CspChannelOutArray1(0, 1, "standard.channel.bd(61)", out, outputOffset);
outputOffset += 2;
this._Vdd = new CspNode(wideNodes[nodeOffset++]);
this._GND = new CspNode(wideNodes[nodeOffset++]);
this.__RESET = new CspNode(wideNodes[nodeOffset++]);
this._START = new CspNode(wideNodes[nodeOffset++]);
this._DLY = new CspNode(wideNodes[nodeOffset++]);
this._CAPTURE = new CspNode(wideNodes[nodeOffset++]);
this._PASSTHRU = new CspNode(wideNodes[nodeOffset++]);
this._INJECT = new CspNode(wideNodes[nodeOffset++]);
}


// function decls
// structure decls
// initStmt = (sequence (var ((decl (id level) (integer #t #t () ()) none 10_2))) (var ((decl (id SYM_NONE) (integer #t #t () ()) none 10_0))) (var ((decl (id ULVT_A_TRANSISTOR) (integer #t #t () ()) none 10_9))) (var ((decl (id ALWAYS_HIGH) (integer #t #t () ()) none 10_1))) (var ((decl (id real_time) (boolean #t) none 10_0))) (var ((decl (id SYM_SKIP_FIRST) (integer #t #t () ()) none 10_2))) (var ((decl (id INIT_HIGH) (integer #t #t () ()) none 10_1))) (var ((decl (id ALWAYS_LOW) (integer #t #t () ()) none 10_0))) (var ((decl (id HDC_TRANSISTOR) (integer #t #t () ()) none 10_4))) (var ((decl (id SYM_PARTIAL) (integer #t #t () ()) none 10_1))) (var ((decl (id PROCESS) (integer #t #t () ()) none 10_1278))) (var ((decl (id SLACKER_SIGNOFF) (boolean #t) none 10_-1))) (var ((decl (id LVT_A_TRANSISTOR) (integer #t #t () ()) none 10_7))) (var ((decl (id NONCONST) (integer #t #t () ()) none 10_3))) (var ((decl (id HVT_TRANSISTOR) (integer #t #t () ()) none 10_3))) (var ((decl (id ULVT_TRANSISTOR) (integer #t #t () ()) none 10_5))) (var ((decl (id LVT_TRANSISTOR) (integer #t #t () ()) none 10_2))) (var ((decl (id SVT_A_TRANSISTOR) (integer #t #t () ()) none 10_6))) (var ((decl (id SlackerSkipProteusSubcells) (boolean #t) none 10_0))) (var ((decl (id NO_STATICIZER) (integer #t #t () ()) none 10_-1))) (var ((decl (id IDLE_UNKNOWN) (integer #t #t () ()) none 10_2))) (var ((decl (id FULL_STATICIZER) (integer #t #t () ()) none 10_1))) (var ((decl (id INIT_LOW) (integer #t #t () ()) none 10_0))) (var ((decl (id DEFAULT_UP_DELAY) (integer #t #t () ()) none 10_100))) (var ((decl (id INIT_RANDOM) (integer #t #t () ()) none 10_-1))) (var ((decl (id FAST_REPEATER_BP) (integer #t #t () ()) none 10_45))) (var ((decl (id SYM_FULL_X) (integer #t #t () ()) none 10_5))) (var ((decl (id SVT_TRANSISTOR) (integer #t #t () ()) none 10_1))) (var ((decl (id DEFAULT_DN_DELAY) (integer #t #t () ()) none 10_100))) (var ((decl (id WEAK_STATICIZER) (integer #t #t () ()) none 10_0))) (var ((decl (id COMB_STATICIZER) (integer #t #t () ()) none 10_2))) (var ((decl (id FULL_COMB_STATICIZER) (integer #t #t () ()) none 10_3))) (var ((decl (id HVT_A_TRANSISTOR) (integer #t #t () ()) none 10_8))) (var ((decl (id GATED) (integer #t #t () ()) none 10_1))) (var ((decl (id ALWAYS_ON) (integer #t #t () ()) none 10_0))) (var ((decl (id SYM_TRUNK) (integer #t #t () ()) none 10_4))) (var ((decl (id scan_coverage_model) (boolean #t) none 10_0))) (var ((decl (id SYM_FULL) (integer #t #t () ()) none 10_3))) (var ((decl (id AUTO_STATICIZER) (integer #t #t () ()) none 10_4))) (var ((decl (id IDLE_1) (integer #t #t () ()) none 10_1))) (var ((decl (id FULL_AUTO_STATICIZER) (integer #t #t () ()) none 10_5))) (var ((decl (id IDLE_0) (integer #t #t () ()) none 10_0))) (var ((decl (id METASTABLE_LEAF_ROUTED) (boolean #t) none 10_-1))) (var ((decl (id BEST_REPEATER_BP) (integer #t #t () ()) none 10_60))) (var ((decl (id printstep) (integer #t #t () ()) none 10_4))) (var ((decl (id STATUSW) (integer #t #t () ()) none 10_312))) (var ((decl (id W) (integer #t #t () ()) none 10_61))) (var ((decl (id INTERVAL) (integer #t #t () ()) none 10_10000))) (var ((decl (id Billion) (integer #t #t () ()) none 10_1000000000))))

// initStmt = (sequence (var ((decl (id level) (integer #t #t () ()) none 10_2))) (var ((decl (id real_time) (boolean #t) none 10_0))) (var ((decl (id SLACKER_SIGNOFF) (boolean #t) none 10_-1))) (var ((decl (id SlackerSkipProteusSubcells) (boolean #t) none 10_0))) (var ((decl (id scan_coverage_model) (boolean #t) none 10_0))) (var ((decl (id METASTABLE_LEAF_ROUTED) (boolean #t) none 10_-1))))

// static vars
static final CspInteger _level = new CspInteger(new CspInteger((byte)2).toBigInteger());static final CspInteger _real_time = new CspBoolean(new CspInteger((byte)0).toBigInteger());static final CspInteger _SLACKER_SIGNOFF = new CspBoolean(new CspInteger((byte)-1).toBigInteger());static final CspInteger _SlackerSkipProteusSubcells = new CspBoolean(new CspInteger((byte)0).toBigInteger());static final CspInteger _scan_coverage_model = new CspBoolean(new CspInteger((byte)0).toBigInteger());static final CspInteger _METASTABLE_LEAF_ROUTED = new CspBoolean(new CspInteger((byte)-1).toBigInteger());
// static initializers
private static void staticInitializer() {
}
static {
  staticInitializer();
}

protected void cleanup() {
}

public void go() throws InterruptedException {
try{
enterFrame(null);
final CspInteger _i = new LoggedCspInteger("" + "i", "empty_parse_position 0:0-0:0", BigInteger.ZERO);{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 482:4-482:41");
label0:
for (;;) {
int trueIdx1 = -1;
if (new CspInteger((byte)-1).booleanValue()) {
trueIdx1 = 0;
}
switch (trueIdx1) {
case -2:
break label0;
case -1:
break label0;
case 0:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 482:7-482:10");
_i.setValue((new CspInteger(receive(_L).getValue())));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 482:12-482:38");
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 482:12-482:18");
send(_R.get(new CspInteger((byte)0), "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast", 482, 14), _i.toBigInteger());
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 482:20-482:38");
send(_R.get(new CspInteger((byte)1), "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast", 482, 22), (_i.add((new CspInteger((byte)2).pow(_level.intValue())))).toBigInteger());
/* ^ Should have been parallel. */
}
break;
}
}
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 482:4-482:41");
yield();
}
}
waitForever();
}catch(NonDeterminismException e){
outerr(e.toString());
}catch(CspNode.UnstableException e){
printUnstableException();
}catch(CspArrayBoundsException e){
handleBoundsException(e, null);
}finally{
leaveFrame();
}
}

// generate resetNodes()
// loopVars
public class LoggedCspInteger extends CspInteger {
private final LoggedLong logged;
public LoggedCspInteger(String varName) {
this(varName, BigInteger.ZERO);
}
public LoggedCspInteger(String varName, BigInteger val) {
super(val);
CspRuntimeAbstractDevice parent = getSelf();
logged = parent.getSigscan() == null ? null : new LoggedLong(getLoggingScope(), varName, parent.getSigscan(), parent.getDebugOpts(), true);
if (logged != null) logged.set(val.longValue(), getTime());
}
public LoggedCspInteger(String varName, String parsePos, BigInteger val) {
this(varName, val);
topFrame().addVariable(varName, parsePos, this);
}
protected void setValue(final BigInteger val) {
super.setValue(val);
if (logged != null) logged.set(val.longValue(), getTime());
}
}
public LoggedCspArray.ElementConstructor getElementConstructor(final int width, final boolean isSigned) {
return new LoggedCspArray.ElementConstructor() {
public CspValue create(CspRuntimeAbstractDevice parent, String varName) {
return new LoggedFiniteCspInteger(varName, BigInteger.ZERO, width, isSigned);
}
};
}
public class LoggedFiniteCspInteger extends FiniteCspInteger {
private final LoggedLogicArray logged;
public LoggedFiniteCspInteger(String varName, BigInteger val, int width, boolean isSigned) {
super(val, width, isSigned);
CspRuntimeAbstractDevice parent = getSelf();
logged = parent.getSigscan() == null ? null : new LoggedLogicArray(getLoggingScope(), varName, 0, width - 1, parent.getSigscan(), parent.getDebugOpts(), true);
if (logged != null) logged.set(getLogicArray(), getTime());
}
public LoggedFiniteCspInteger(String varName, String parsePos, BigInteger val, int width, boolean isSigned) {
this(varName, val, width, isSigned);
topFrame().addVariable(varName, parsePos, this);
}
protected void setValue(final BigInteger val) {
super.setValue(val);
if (logged != null) logged.set(getLogicArray(), getTime());
}
}
public class LoggedCspBoolean extends CspBoolean {
private final LoggedLong logged;
public LoggedCspBoolean(String varName) {
this(varName, BigInteger.ZERO);
}
public LoggedCspBoolean(String varName, BigInteger val) {
super(val);
CspRuntimeAbstractDevice parent = getSelf();
logged = parent.getSigscan() == null ? null : new LoggedLong(getLoggingScope(), varName, parent.getSigscan(), parent.getDebugOpts(), true);
if (logged != null) logged.set(val.longValue(), getTime());
}
public LoggedCspBoolean(String varName, String parsePos, BigInteger val) {
this(varName, val);
topFrame().addVariable(varName, parsePos, this);
}
protected void setValue(final BigInteger val) {
super.setValue(val);
if (logged != null) logged.set(val.longValue(), getTime());
}
}
public class LoggedCspString extends CspString {
private final LoggedString logged;
public LoggedCspString(String varName) {
this(varName, new CspString());
}
public LoggedCspString(String varName, CspString val) {
super(val.toString());
CspRuntimeAbstractDevice parent = getSelf();
logged = parent.getSigscan() == null ? null : new LoggedString(getLoggingScope(), varName, parent.getSigscan(), parent.getDebugOpts(), true);
if (logged != null) logged.set(val.toString(), getTime());
}
public LoggedCspString(String varName, String parsePos, CspString val) {
this(varName, val);
topFrame().addVariable(varName, parsePos, this);
}
public void setValue(final CspString val) {
super.setValue(val);
if (logged != null) logged.set(val.toString(), getTime());
}
}
}
