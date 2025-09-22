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

public final class CSP2ClassTemp_40_first_46_PROC_40_false_41_ extends CspRuntimeAbstractDevice {

// instance variables
private int outputOffset = 0;
private int inputOffset = 0;
private int nodeOffset = 0;
// declare channel vars
// outputPortDecl: (port-definition L in (channel standard.channel.bd(32) 1 4294967296 f ((port-definition C out (structure standard.channel.bdc ((port-definition q out (node #f 1)) (port-definition a in (node #f 1))))) (port-definition D out (node #t 32)))))
public final ChannelInput _L;
// outputPortDecl: (port-definition R out (channel standard.channel.bd(32) 1 4294967296 f ((port-definition C out (structure standard.channel.bdc ((port-definition q out (node #f 1)) (port-definition a in (node #f 1))))) (port-definition D out (node #t 32)))))
public final ChannelOutput _R;
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
public CSP2ClassTemp_40_first_46_PROC_40_false_41_(final DeviceParameters deviceParams, final ChannelInput[] in, final ChannelOutput[] out, final WideNode[] nodes
) throws InterruptedException {
super(deviceParams.getName(), in, out, nodes, deviceParams.isOutputSuppressed(), deviceParams.getArbitrationMode());
_srandom(new CspInteger(new BigInteger(Long.toString(deviceParams.getSeed()))));
setDigitalTau(deviceParams.getDigitalTau());
this._L = in[inputOffset++];
this._R = out[outputOffset++];
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
// initStmt = (sequence (var ((decl (id DOSTART) (boolean #t) none 10_0))) (var ((decl (id SYM_NONE) (integer #t #t () ()) none 10_0))) (var ((decl (id ULVT_A_TRANSISTOR) (integer #t #t () ()) none 10_9))) (var ((decl (id ALWAYS_HIGH) (integer #t #t () ()) none 10_1))) (var ((decl (id real_time) (boolean #t) none 10_0))) (var ((decl (id SYM_SKIP_FIRST) (integer #t #t () ()) none 10_2))) (var ((decl (id INIT_HIGH) (integer #t #t () ()) none 10_1))) (var ((decl (id ALWAYS_LOW) (integer #t #t () ()) none 10_0))) (var ((decl (id HDC_TRANSISTOR) (integer #t #t () ()) none 10_4))) (var ((decl (id SYM_PARTIAL) (integer #t #t () ()) none 10_1))) (var ((decl (id PROCESS) (integer #t #t () ()) none 10_1278))) (var ((decl (id SLACKER_SIGNOFF) (boolean #t) none 10_-1))) (var ((decl (id LVT_A_TRANSISTOR) (integer #t #t () ()) none 10_7))) (var ((decl (id NONCONST) (integer #t #t () ()) none 10_3))) (var ((decl (id HVT_TRANSISTOR) (integer #t #t () ()) none 10_3))) (var ((decl (id ULVT_TRANSISTOR) (integer #t #t () ()) none 10_5))) (var ((decl (id LVT_TRANSISTOR) (integer #t #t () ()) none 10_2))) (var ((decl (id SVT_A_TRANSISTOR) (integer #t #t () ()) none 10_6))) (var ((decl (id SlackerSkipProteusSubcells) (boolean #t) none 10_0))) (var ((decl (id NO_STATICIZER) (integer #t #t () ()) none 10_-1))) (var ((decl (id IDLE_UNKNOWN) (integer #t #t () ()) none 10_2))) (var ((decl (id FULL_STATICIZER) (integer #t #t () ()) none 10_1))) (var ((decl (id INIT_LOW) (integer #t #t () ()) none 10_0))) (var ((decl (id DEFAULT_UP_DELAY) (integer #t #t () ()) none 10_100))) (var ((decl (id INIT_RANDOM) (integer #t #t () ()) none 10_-1))) (var ((decl (id FAST_REPEATER_BP) (integer #t #t () ()) none 10_45))) (var ((decl (id SYM_FULL_X) (integer #t #t () ()) none 10_5))) (var ((decl (id SVT_TRANSISTOR) (integer #t #t () ()) none 10_1))) (var ((decl (id DEFAULT_DN_DELAY) (integer #t #t () ()) none 10_100))) (var ((decl (id WEAK_STATICIZER) (integer #t #t () ()) none 10_0))) (var ((decl (id COMB_STATICIZER) (integer #t #t () ()) none 10_2))) (var ((decl (id FULL_COMB_STATICIZER) (integer #t #t () ()) none 10_3))) (var ((decl (id HVT_A_TRANSISTOR) (integer #t #t () ()) none 10_8))) (var ((decl (id GATED) (integer #t #t () ()) none 10_1))) (var ((decl (id ALWAYS_ON) (integer #t #t () ()) none 10_0))) (var ((decl (id SYM_TRUNK) (integer #t #t () ()) none 10_4))) (var ((decl (id scan_coverage_model) (boolean #t) none 10_0))) (var ((decl (id SYM_FULL) (integer #t #t () ()) none 10_3))) (var ((decl (id AUTO_STATICIZER) (integer #t #t () ()) none 10_4))) (var ((decl (id IDLE_1) (integer #t #t () ()) none 10_1))) (var ((decl (id FULL_AUTO_STATICIZER) (integer #t #t () ()) none 10_5))) (var ((decl (id IDLE_0) (integer #t #t () ()) none 10_0))) (var ((decl (id METASTABLE_LEAF_ROUTED) (boolean #t) none 10_-1))) (var ((decl (id BEST_REPEATER_BP) (integer #t #t () ()) none 10_60))) (var ((decl (id WIDTH) (integer #t #t () ()) none 10_32))) (var ((decl (id N) (integer #t #t () ()) none 10_4096))))

// initStmt = (sequence (var ((decl (id DOSTART) (boolean #t) none 10_0))) (var ((decl (id real_time) (boolean #t) none 10_0))) (var ((decl (id SLACKER_SIGNOFF) (boolean #t) none 10_-1))) (var ((decl (id SlackerSkipProteusSubcells) (boolean #t) none 10_0))) (var ((decl (id scan_coverage_model) (boolean #t) none 10_0))) (var ((decl (id METASTABLE_LEAF_ROUTED) (boolean #t) none 10_-1))) (var ((decl (id WIDTH) (integer #t #t () ()) none 10_32))))

// static vars
static final CspInteger _DOSTART = new CspBoolean(new CspInteger((byte)0).toBigInteger());static final CspInteger _real_time = new CspBoolean(new CspInteger((byte)0).toBigInteger());static final CspInteger _SLACKER_SIGNOFF = new CspBoolean(new CspInteger((byte)-1).toBigInteger());static final CspInteger _SlackerSkipProteusSubcells = new CspBoolean(new CspInteger((byte)0).toBigInteger());static final CspInteger _scan_coverage_model = new CspBoolean(new CspInteger((byte)0).toBigInteger());static final CspInteger _METASTABLE_LEAF_ROUTED = new CspBoolean(new CspInteger((byte)-1).toBigInteger());static final CspInteger _WIDTH = new CspInteger(new CspInteger((byte)32).toBigInteger());
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
final CspInteger _y = new LoggedCspInteger("" + "y", "empty_parse_position 0:0-0:0", BigInteger.ZERO);final CspInteger _z = new LoggedCspInteger("" + "z", "empty_parse_position 0:0-0:0", BigInteger.ZERO);{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 12:3-12:15");
final CspInteger _i = new LoggedFiniteCspInteger("" + "i", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 12:14-12:15", BigInteger.ZERO, (_WIDTH).intValue(), false);setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 13:3-13:46");
{
Wait w0 = null;
label1:
for (;;) {
int trueIdx2 = -1;
if (_DOSTART.booleanValue()) {
trueIdx2 = 0;
}
if ((_DOSTART.not()).booleanValue()) {
if (trueIdx2 == -1)
trueIdx2 = 1;
else {
throw new NonDeterminismException("Error -- Multiple guards " + trueIdx2 + " and " + 1 + " true for deterministic repetition or selection at " + "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast:13:3-13:46");
}
}
switch (trueIdx2) {
case -1:
if (w0 == null)
w0 = WaitFactory.newWait(in, out, getNodesWithDifferentValue(true), getNodesWithDifferentValue(false));
final Pair/*<Waitable,Long>*/ p0 = select2(w0);
updateTime(((Long)p0.getSecond()).longValue());
final Waitable wa0 = (Waitable) p0.getFirst();
Arbiter.removeFromWait(w0, wa0);
break;
case 0:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 13:16-13:19");
_i.setValue(new CspInteger((byte)0));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 13:21-13:24");
send(_R, new CspInteger((byte)0).toBigInteger());
}
break label1;
}
case 1:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 13:40-13:44");
/* skip */
}
break label1;
}
}
}
w0 = null;
}
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 15:3-24:5");
label3:
for (;;) {
int trueIdx4 = -1;
if (new CspInteger((byte)-1).booleanValue()) {
trueIdx4 = 0;
}
switch (trueIdx4) {
case -2:
break label3;
case -1:
break label3;
case 0:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 15:6-15:11");
final CspInteger _x = new LoggedCspInteger("" + "x", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 15:10-15:11", BigInteger.ZERO);setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 16:6-16:9");
_x.setValue((new CspInteger(receive(_L).getValue())));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 18:6-18:77");
{
Wait w5 = null;
label6:
for (;;) {
int trueIdx7 = -1;
if (_DOSTART.booleanValue()) {
trueIdx7 = 0;
}
if ((_DOSTART.not()).booleanValue()) {
if (trueIdx7 == -1)
trueIdx7 = 1;
else {
throw new NonDeterminismException("Error -- Multiple guards " + trueIdx7 + " and " + 1 + " true for deterministic repetition or selection at " + "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast:18:6-18:77");
}
}
switch (trueIdx7) {
case -1:
if (w5 == null)
w5 = WaitFactory.newWait(in, out, getNodesWithDifferentValue(true), getNodesWithDifferentValue(false));
final Pair/*<Waitable,Long>*/ p5 = select2(w5);
updateTime(((Long)p5.getSecond()).longValue());
final Waitable wa5 = (Waitable) p5.getFirst();
Arbiter.removeFromWait(w5, wa5);
break;
case 0:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 18:19-18:50");
_print(new CspString("i = ").add(new CspString(_i.toString())).add(new CspString(" x = ")).add(new CspString(_x.toString())));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 18:52-18:55");
_i.setValue(new CspInteger((byte)1), Fold.ADD_FUNCTION);
}
break label6;
}
case 1:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 18:71-18:75");
/* skip */
}
break label6;
}
}
}
w5 = null;
}
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 20:6-20:16");
_y.setValue((_x.add(new CspInteger((byte)13))));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 21:6-21:34");
_z.setValue(((_y.multiply(new CspInteger(new BigInteger("16807")))).remainder(new CspInteger(new BigInteger("2147483647")))));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 23:6-23:9");
send(_R, _z.toBigInteger());
}
break;
}
}
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/first.cast 15:3-24:5");
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
