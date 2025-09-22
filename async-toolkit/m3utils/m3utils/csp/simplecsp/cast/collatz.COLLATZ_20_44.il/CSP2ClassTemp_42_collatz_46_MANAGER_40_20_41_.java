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

public final class CSP2ClassTemp_42_collatz_46_MANAGER_40_20_41_ extends CspRuntimeAbstractDevice {

// instance variables
private int outputOffset = 0;
private int inputOffset = 0;
private int nodeOffset = 0;
// declare channel vars
// outputPortDecl: (port-definition STRT out (channel standard.channel.bd(61) 1 2305843009213693952 f ((port-definition C out (structure standard.channel.bdc ((port-definition q out (node #f 1)) (port-definition a in (node #f 1))))) (port-definition D out (node #t 61)))))
public final ChannelOutput _STRT;
// outputPortDecl: (port-definition STATUS in (channel standard.channel.bd(312) 1 8343699359066055009355553539724812947666814540455674882605631280555545803830627148527195652096 f ((port-definition C out (structure standard.channel.bdc ((port-definition q out (node #f 1)) (port-definition a in (node #f 1))))) (port-definition D out (node #t 312)))))
public final ChannelInput _STATUS;
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
public CSP2ClassTemp_42_collatz_46_MANAGER_40_20_41_(final DeviceParameters deviceParams, final ChannelInput[] in, final ChannelOutput[] out, final WideNode[] nodes
) throws InterruptedException {
super(deviceParams.getName(), in, out, nodes, deviceParams.isOutputSuppressed(), deviceParams.getArbitrationMode());
_srandom(new CspInteger(new BigInteger(Long.toString(deviceParams.getSeed()))));
setDigitalTau(deviceParams.getDigitalTau());
this._STRT = out[outputOffset++];
this._STATUS = in[inputOffset++];
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
public class _longest_so_far_0 implements CspCloneableValue, CspStructure {
public CspInteger _num;
public CspInteger _length;

public _longest_so_far_0() {
this(true);
}
public _longest_so_far_0(final boolean init) {
if (init) {
 _num = new FiniteCspInteger(BigInteger.ZERO, (_W).intValue(), false); _length = new FiniteCspInteger(BigInteger.ZERO, (_W).intValue(), false);}
}
public _longest_so_far_0(final CspInteger _num,
final CspInteger _length) {
this();
this._num.setValue(_num);
this._length.setValue(_length);
}
public _longest_so_far_0(_longest_so_far_0 other) {
setValue(other);
}
public void setValue(CspValue other) {
setValue((_longest_so_far_0) other);
}
public void setValue(CspValue value, Fold.BinaryFunction modifier) {
throw new UnsupportedOperationException();
}
public void setValue(_longest_so_far_0 other) {
this._num.setValue(other._num);
this._length.setValue(other._length);
}
public CspCloneableValue duplicate() {
_longest_so_far_0 result = new 
_longest_so_far_0(false);
result._num = (CspInteger) this._num.duplicate();
result._length = (CspInteger) this._length.duplicate();
return result;
}
public int pack(CspInteger packed, int start) {
start = ((Packable) _length).pack(packed, start);
start = ((Packable) _num).pack(packed, start);
return start;
}
public int unpack(CspInteger packed, int start) {
start = ((Packable) _length).unpack(packed, start);
start = ((Packable) _num).unpack(packed, start);
return start;
}
}
public class _Loggedlongest_so_far_0 extends _longest_so_far_0 {
public _Loggedlongest_so_far_0(final String varName) {
super(false);
 _num = new LoggedFiniteCspInteger((varName + ".") + "num", BigInteger.ZERO, (_W).intValue(), false); _length = new LoggedFiniteCspInteger((varName + ".") + "length", BigInteger.ZERO, (_W).intValue(), false);}
public _Loggedlongest_so_far_0(final String varName, final String parsePos) {
this(varName);
topFrame().addVariable(varName, parsePos, this);
}
public _Loggedlongest_so_far_0(final String varName, _longest_so_far_0 val) {
this(varName);
setValue(val);
}
public _Loggedlongest_so_far_0(final String varName, final String parsePos, _longest_so_far_0 val) {
this(varName, val);
topFrame().addVariable(varName, parsePos, this);
}
}
public class _status_1 implements CspCloneableValue, CspStructure {
public CspInteger _tok;
public CspInteger _id;
public CspInteger _ops;
public _longest_so_far_0 _l;

public _status_1() {
this(true);
}
public _status_1(final boolean init) {
if (init) {
 _tok = new CspBoolean(BigInteger.ZERO); _id = new FiniteCspInteger(BigInteger.ZERO, (_W).intValue(), true); _ops = new FiniteCspInteger(BigInteger.ZERO, (new CspInteger(new BigInteger("128"))).intValue(), false); _l = new _longest_so_far_0();}
}
public _status_1(final CspInteger _tok,
final CspInteger _id,
final CspInteger _ops,
final _longest_so_far_0 _l) {
this();
this._tok.setValue(_tok);
this._id.setValue(_id);
this._ops.setValue(_ops);
this._l.setValue(_l);
}
public _status_1(_status_1 other) {
setValue(other);
}
public void setValue(CspValue other) {
setValue((_status_1) other);
}
public void setValue(CspValue value, Fold.BinaryFunction modifier) {
throw new UnsupportedOperationException();
}
public void setValue(_status_1 other) {
this._tok.setValue(other._tok);
this._id.setValue(other._id);
this._ops.setValue(other._ops);
this._l.setValue(other._l);
}
public CspCloneableValue duplicate() {
_status_1 result = new 
_status_1(false);
result._tok = (CspInteger) this._tok.duplicate();
result._id = (CspInteger) this._id.duplicate();
result._ops = (CspInteger) this._ops.duplicate();
result._l = (_longest_so_far_0) this._l.duplicate();
return result;
}
public int pack(CspInteger packed, int start) {
start = ((Packable) _l).pack(packed, start);
start = ((Packable) _ops).pack(packed, start);
start = ((Packable) _id).pack(packed, start);
start = ((Packable) _tok).pack(packed, start);
return start;
}
public int unpack(CspInteger packed, int start) {
start = ((Packable) _l).unpack(packed, start);
start = ((Packable) _ops).unpack(packed, start);
start = ((Packable) _id).unpack(packed, start);
start = ((Packable) _tok).unpack(packed, start);
return start;
}
}
public class _Loggedstatus_1 extends _status_1 {
public _Loggedstatus_1(final String varName) {
super(false);
 _tok = new LoggedCspBoolean((varName + ".") + "tok", BigInteger.ZERO); _id = new LoggedFiniteCspInteger((varName + ".") + "id", BigInteger.ZERO, (_W).intValue(), true); _ops = new LoggedFiniteCspInteger((varName + ".") + "ops", BigInteger.ZERO, (new CspInteger(new BigInteger("128"))).intValue(), false); _l = new _Loggedlongest_so_far_0((varName + ".") + "l");}
public _Loggedstatus_1(final String varName, final String parsePos) {
this(varName);
topFrame().addVariable(varName, parsePos, this);
}
public _Loggedstatus_1(final String varName, _status_1 val) {
this(varName);
setValue(val);
}
public _Loggedstatus_1(final String varName, final String parsePos, _status_1 val) {
this(varName, val);
topFrame().addVariable(varName, parsePos, this);
}
}
// initStmt = (sequence (var ((decl (id NWORKERS) (integer #t #t () ()) none 10_20))) (var ((decl (id SYM_NONE) (integer #t #t () ()) none 10_0))) (var ((decl (id ULVT_A_TRANSISTOR) (integer #t #t () ()) none 10_9))) (var ((decl (id ALWAYS_HIGH) (integer #t #t () ()) none 10_1))) (var ((decl (id real_time) (boolean #t) none 10_0))) (var ((decl (id SYM_SKIP_FIRST) (integer #t #t () ()) none 10_2))) (var ((decl (id INIT_HIGH) (integer #t #t () ()) none 10_1))) (var ((decl (id ALWAYS_LOW) (integer #t #t () ()) none 10_0))) (var ((decl (id HDC_TRANSISTOR) (integer #t #t () ()) none 10_4))) (var ((decl (id SYM_PARTIAL) (integer #t #t () ()) none 10_1))) (var ((decl (id PROCESS) (integer #t #t () ()) none 10_1278))) (var ((decl (id SLACKER_SIGNOFF) (boolean #t) none 10_-1))) (var ((decl (id LVT_A_TRANSISTOR) (integer #t #t () ()) none 10_7))) (var ((decl (id NONCONST) (integer #t #t () ()) none 10_3))) (var ((decl (id HVT_TRANSISTOR) (integer #t #t () ()) none 10_3))) (var ((decl (id ULVT_TRANSISTOR) (integer #t #t () ()) none 10_5))) (var ((decl (id LVT_TRANSISTOR) (integer #t #t () ()) none 10_2))) (var ((decl (id SVT_A_TRANSISTOR) (integer #t #t () ()) none 10_6))) (var ((decl (id SlackerSkipProteusSubcells) (boolean #t) none 10_0))) (var ((decl (id NO_STATICIZER) (integer #t #t () ()) none 10_-1))) (var ((decl (id IDLE_UNKNOWN) (integer #t #t () ()) none 10_2))) (var ((decl (id FULL_STATICIZER) (integer #t #t () ()) none 10_1))) (var ((decl (id INIT_LOW) (integer #t #t () ()) none 10_0))) (var ((decl (id DEFAULT_UP_DELAY) (integer #t #t () ()) none 10_100))) (var ((decl (id INIT_RANDOM) (integer #t #t () ()) none 10_-1))) (var ((decl (id FAST_REPEATER_BP) (integer #t #t () ()) none 10_45))) (var ((decl (id SYM_FULL_X) (integer #t #t () ()) none 10_5))) (var ((decl (id SVT_TRANSISTOR) (integer #t #t () ()) none 10_1))) (var ((decl (id DEFAULT_DN_DELAY) (integer #t #t () ()) none 10_100))) (var ((decl (id WEAK_STATICIZER) (integer #t #t () ()) none 10_0))) (var ((decl (id COMB_STATICIZER) (integer #t #t () ()) none 10_2))) (var ((decl (id FULL_COMB_STATICIZER) (integer #t #t () ()) none 10_3))) (var ((decl (id HVT_A_TRANSISTOR) (integer #t #t () ()) none 10_8))) (var ((decl (id GATED) (integer #t #t () ()) none 10_1))) (var ((decl (id ALWAYS_ON) (integer #t #t () ()) none 10_0))) (var ((decl (id SYM_TRUNK) (integer #t #t () ()) none 10_4))) (var ((decl (id scan_coverage_model) (boolean #t) none 10_0))) (var ((decl (id SYM_FULL) (integer #t #t () ()) none 10_3))) (var ((decl (id AUTO_STATICIZER) (integer #t #t () ()) none 10_4))) (var ((decl (id IDLE_1) (integer #t #t () ()) none 10_1))) (var ((decl (id FULL_AUTO_STATICIZER) (integer #t #t () ()) none 10_5))) (var ((decl (id IDLE_0) (integer #t #t () ()) none 10_0))) (var ((decl (id METASTABLE_LEAF_ROUTED) (boolean #t) none 10_-1))) (var ((decl (id BEST_REPEATER_BP) (integer #t #t () ()) none 10_60))) (var ((decl (id printstep) (integer #t #t () ()) none 10_4))) (var ((decl (id STATUSW) (integer #t #t () ()) none 10_312))) (var ((decl (id W) (integer #t #t () ()) none 10_61))) (var ((decl (id INTERVAL) (integer #t #t () ()) none 10_10000))) (var ((decl (id Billion) (integer #t #t () ()) none 10_1000000000))))

// initStmt = (sequence (var ((decl (id NWORKERS) (integer #t #t () ()) none 10_20))) (var ((decl (id real_time) (boolean #t) none 10_0))) (var ((decl (id SLACKER_SIGNOFF) (boolean #t) none 10_-1))) (var ((decl (id SlackerSkipProteusSubcells) (boolean #t) none 10_0))) (var ((decl (id scan_coverage_model) (boolean #t) none 10_0))) (var ((decl (id METASTABLE_LEAF_ROUTED) (boolean #t) none 10_-1))) (var ((decl (id printstep) (integer #t #t () ()) none 10_4))) (var ((decl (id W) (integer #t #t () ()) none 10_61))) (var ((decl (id Billion) (integer #t #t () ()) none 10_1000000000))))

// static vars
static final CspInteger _NWORKERS = new CspInteger(new CspInteger((byte)20).toBigInteger());static final CspInteger _real_time = new CspBoolean(new CspInteger((byte)0).toBigInteger());static final CspInteger _SLACKER_SIGNOFF = new CspBoolean(new CspInteger((byte)-1).toBigInteger());static final CspInteger _SlackerSkipProteusSubcells = new CspBoolean(new CspInteger((byte)0).toBigInteger());static final CspInteger _scan_coverage_model = new CspBoolean(new CspInteger((byte)0).toBigInteger());static final CspInteger _METASTABLE_LEAF_ROUTED = new CspBoolean(new CspInteger((byte)-1).toBigInteger());static final CspInteger _printstep = new CspInteger(new CspInteger((byte)4).toBigInteger());static final CspInteger _W = new CspInteger(new CspInteger((byte)61).toBigInteger());static final CspInteger _Billion = new CspInteger(new CspInteger(new BigInteger("1000000000")).toBigInteger());
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
final CspInteger _x = new LoggedCspInteger("" + "x", "empty_parse_position 0:0-0:0", BigInteger.ZERO);final CspInteger _lastprint = new LoggedCspInteger("" + "lastprint", "empty_parse_position 0:0-0:0", BigInteger.ZERO);{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 257:4-257:11");
final CspInteger _ops = new LoggedCspInteger("" + "ops", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 257:8-257:11", BigInteger.ZERO);setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 259:4-259:20");
final CspInteger _bigiter = new LoggedFiniteCspInteger("" + "bigiter", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 259:11-259:18", new CspInteger((byte)0).toBigInteger(), (_W).intValue(), false);setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 260:4-260:35");
final CspInteger _outstanding = new LoggedFiniteCspInteger("" + "outstanding", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 260:24-260:35", BigInteger.ZERO, (_log2(RuntimeUtils.copyInt(_NWORKERS))).intValue(), false);setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 261:4-261:10");
send(_STRT, new CspInteger((byte)0).toBigInteger());
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 261:12-261:34");
_outstanding.setValue(_NWORKERS);
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 261:36-261:45");
_bigiter.setValue(new CspInteger((byte)1), Fold.ADD_FUNCTION);
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 263:4-263:15");
final _status_1 _save = new _Loggedstatus_1("" + "save", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 263:11-263:15");setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 265:4-265:26");
final CspInteger _start = new LoggedCspInteger("" + "start", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 265:8-265:13", _walltime().toBigInteger());setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 266:4-266:21");
final CspInteger _last = new LoggedCspInteger("" + "last", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 266:8-266:12", _start.toBigInteger());setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 268:4-307:6");
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
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 268:7-268:15");
_x.setValue((new CspInteger(receive(_STATUS).getValue())));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 270:7-270:15");
final _status_1 _s = new _Loggedstatus_1("" + "s", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 270:14-270:15");setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 271:7-271:27");
final CspInteger _doprint = new LoggedCspBoolean("" + "doprint", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 271:12-271:19", new CspInteger((byte)0).toBigInteger());setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 273:7-273:18");
unpack(_s, _x)
;
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 275:7-283:8");
{
Wait w2 = null;
label3:
for (;;) {
int trueIdx4 = -1;
if (_s._tok.booleanValue()) {
trueIdx4 = 0;
}
switch (trueIdx4) {
case -1:
/* skip */
break label3;
case 0:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 276:9-276:22");
_outstanding.setValue(new CspInteger((byte)1), Fold.SUBTRACT_FUNCTION);
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 278:9-282:11");
{
Wait w5 = null;
label6:
for (;;) {
int trueIdx7 = -1;
if (((_outstanding.compareTo(new CspInteger((byte)0)))== 0 ? CspInteger.TRUE : CspInteger.FALSE).booleanValue()) {
trueIdx7 = 0;
}
switch (trueIdx7) {
case -1:
/* skip */
break label6;
case 0:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 280:7-280:63");
_print(new CspString("phase ").add(new CspString(_bigiter.toString())).add(new CspString(" complete, re-launching...")));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 281:12-281:24");
send(_STRT, _bigiter.toBigInteger());
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 281:27-281:47");
_outstanding.setValue(_NWORKERS);
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 281:50-281:59");
_bigiter.setValue(new CspInteger((byte)1), Fold.ADD_FUNCTION);
}
break label6;
}
}
}
w5 = null;
}
}
break label3;
}
}
}
w2 = null;
}
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 285:7-285:66");
{
Wait w8 = null;
label9:
for (;;) {
int trueIdx10 = -1;
if (((_s._l._num.compareTo(_save._l._num))!= 0 ? CspInteger.TRUE : CspInteger.FALSE).booleanValue()) {
trueIdx10 = 0;
}
switch (trueIdx10) {
case -1:
/* skip */
break label9;
case 0:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 285:39-285:47");
_save.setValue(_s);
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 285:50-285:64");
_doprint.setValue(new CspInteger((byte)-1));
}
break label9;
}
}
}
w8 = null;
}
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 291:7-291:20");
_ops.setValue(_s._ops, Fold.ADD_FUNCTION);
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 293:7-293:33");
final CspInteger _now = new LoggedCspInteger("" + "now", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 293:11-293:14", _walltime().toBigInteger());setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 294:7-294:34");
final CspInteger _runtime = new LoggedCspInteger("" + "runtime", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 294:11-294:18", (_now.subtract(_start)).toBigInteger());setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 295:7-295:44");
final CspInteger _runtime_s = new LoggedCspInteger("" + "runtime_s", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 295:11-295:20", ((_runtime.divide(_Billion)).add(new CspInteger((byte)1))).toBigInteger());setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 297:7-297:57");
_doprint.setValue(((_now.compareTo((_lastprint.add((_printstep.multiply(_Billion))))))> 0 ? CspInteger.TRUE : CspInteger.FALSE), Fold.OR_FUNCTION);
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 299:7-305:8");
{
Wait w11 = null;
label12:
for (;;) {
int trueIdx13 = -1;
if (_doprint.booleanValue()) {
trueIdx13 = 0;
}
switch (trueIdx13) {
case -1:
/* skip */
break label12;
case 0:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 301:10-302:107");
_print(new CspString("num = ").add(new CspString(_save._l._num.toString())).add(new CspString(" ; chain = ")).add(new CspString(_save._l._length.toString())).add(new CspString(" : total steps = ")).add(new CspString(_ops.toString())).add(new CspString(" wall = ")).add(new CspString(_runtime_s.toString())).add(new CspString(" ksteps/s = ")).add(new CspString(((_ops.divide(_runtime_s)).divide(new CspInteger(new BigInteger("1000")))).toString())));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 304:10-304:25");
_lastprint.setValue(_now);
}
break label12;
}
}
}
w11 = null;
}
}
break;
}
}
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 268:4-307:6");
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
