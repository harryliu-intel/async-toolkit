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

public final class CSP2ClassTemp_49_collatz_46_WORKER_40_20_44_44_41_ extends CspRuntimeAbstractDevice {

// instance variables
private int outputOffset = 0;
private int inputOffset = 0;
private int nodeOffset = 0;
// declare channel vars
// outputPortDecl: (port-definition STRT in (channel standard.channel.bd(61) 1 2305843009213693952 f ((port-definition C out (structure standard.channel.bdc ((port-definition q out (node #f 1)) (port-definition a in (node #f 1))))) (port-definition D out (node #t 61)))))
public final ChannelInput _STRT;
// outputPortDecl: (port-definition STATUS out (channel standard.channel.bd(312) 1 8343699359066055009355553539724812947666814540455674882605631280555545803830627148527195652096 f ((port-definition C out (structure standard.channel.bdc ((port-definition q out (node #f 1)) (port-definition a in (node #f 1))))) (port-definition D out (node #t 312)))))
public final ChannelOutput _STATUS;
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
public CSP2ClassTemp_49_collatz_46_WORKER_40_20_44_44_41_(final DeviceParameters deviceParams, final ChannelInput[] in, final ChannelOutput[] out, final WideNode[] nodes
) throws InterruptedException {
super(deviceParams.getName(), in, out, nodes, deviceParams.isOutputSuppressed(), deviceParams.getArbitrationMode());
_srandom(new CspInteger(new BigInteger(Long.toString(deviceParams.getSeed()))));
setDigitalTau(deviceParams.getDigitalTau());
this._STRT = in[inputOffset++];
this._STATUS = out[outputOffset++];
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
public CspInteger _even_0_real(final CspInteger _n) throws InterruptedException {
final CspInteger _even = new LoggedCspBoolean("" + "even", BigInteger.ZERO);{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 74:8-74:28");
_even.setValue((((_n.and(new CspInteger((byte)1))).compareTo(new CspInteger((byte)0)))== 0 ? CspInteger.TRUE : CspInteger.FALSE));
}
return _even;
}
public CspInteger _even_0(final CspInteger _n) throws InterruptedException {
try {
enterFrame("even");
return _even_0_real( _n);
} finally { leaveFrame(); } 
}
public CspInteger _odd_1_real(final CspInteger _n) throws InterruptedException {
final CspInteger _odd = new LoggedCspBoolean("" + "odd", BigInteger.ZERO);{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 77:8-77:22");
_odd.setValue((_even_0(RuntimeUtils.copyInt(_n)).not()));
}
return _odd;
}
public CspInteger _odd_1(final CspInteger _n) throws InterruptedException {
try {
enterFrame("odd");
return _odd_1_real( _n);
} finally { leaveFrame(); } 
}
public void _sendStatus_2_real(final CspInteger _tok,
final CspInteger _id,
final CspInteger _len,
final CspInteger _num,
final CspInteger _steps) throws InterruptedException {
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 50:7-50:15");
final _status_3 _s = new _Loggedstatus_3("" + "s", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 50:14-50:15");setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 51:7-51:25");
_s._tok.setValue(_tok);
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 52:7-52:24");
_s._id.setValue(_id);
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 53:7-53:25");
_s._l._length.setValue(_len);
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 54:7-54:25");
_s._l._num.setValue(_num);
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 55:7-55:27");
_s._ops.setValue(_steps);
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 57:7-57:22");
final CspInteger _p = new LoggedCspInteger("" + "p", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 57:11-57:12", pack(_s).toBigInteger());setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 64:7-64:15");
final _status_3 _t = new _Loggedstatus_3("" + "t", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 64:14-64:15");setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 65:7-65:19");
unpack(_t, _p)
;
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 69:7-69:16");
send(_STATUS, _p.toBigInteger());
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 70:7-70:16");
_steps.setValue(new CspInteger((byte)0));
}
}
public void _sendStatus_2(final CspInteger _tok,
final CspInteger _id,
final CspInteger _len,
final CspInteger _num,
final CspInteger _steps) throws InterruptedException {
try {
enterFrame("sendStatus");
_sendStatus_2_real( _tok,
 _id,
 _len,
 _num,
 _steps);
} finally { leaveFrame(); } 
}
// structure decls
public class _longest_so_far_4 implements CspCloneableValue, CspStructure {
public CspInteger _num;
public CspInteger _length;

public _longest_so_far_4() {
this(true);
}
public _longest_so_far_4(final boolean init) {
if (init) {
 _num = new FiniteCspInteger(BigInteger.ZERO, (_W).intValue(), false); _length = new FiniteCspInteger(BigInteger.ZERO, (_W).intValue(), false);}
}
public _longest_so_far_4(final CspInteger _num,
final CspInteger _length) {
this();
this._num.setValue(_num);
this._length.setValue(_length);
}
public _longest_so_far_4(_longest_so_far_4 other) {
setValue(other);
}
public void setValue(CspValue other) {
setValue((_longest_so_far_4) other);
}
public void setValue(CspValue value, Fold.BinaryFunction modifier) {
throw new UnsupportedOperationException();
}
public void setValue(_longest_so_far_4 other) {
this._num.setValue(other._num);
this._length.setValue(other._length);
}
public CspCloneableValue duplicate() {
_longest_so_far_4 result = new 
_longest_so_far_4(false);
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
public class _Loggedlongest_so_far_4 extends _longest_so_far_4 {
public _Loggedlongest_so_far_4(final String varName) {
super(false);
 _num = new LoggedFiniteCspInteger((varName + ".") + "num", BigInteger.ZERO, (_W).intValue(), false); _length = new LoggedFiniteCspInteger((varName + ".") + "length", BigInteger.ZERO, (_W).intValue(), false);}
public _Loggedlongest_so_far_4(final String varName, final String parsePos) {
this(varName);
topFrame().addVariable(varName, parsePos, this);
}
public _Loggedlongest_so_far_4(final String varName, _longest_so_far_4 val) {
this(varName);
setValue(val);
}
public _Loggedlongest_so_far_4(final String varName, final String parsePos, _longest_so_far_4 val) {
this(varName, val);
topFrame().addVariable(varName, parsePos, this);
}
}
public class _status_3 implements CspCloneableValue, CspStructure {
public CspInteger _tok;
public CspInteger _id;
public CspInteger _ops;
public _longest_so_far_4 _l;

public _status_3() {
this(true);
}
public _status_3(final boolean init) {
if (init) {
 _tok = new CspBoolean(BigInteger.ZERO); _id = new FiniteCspInteger(BigInteger.ZERO, (_W).intValue(), true); _ops = new FiniteCspInteger(BigInteger.ZERO, (new CspInteger(new BigInteger("128"))).intValue(), false); _l = new _longest_so_far_4();}
}
public _status_3(final CspInteger _tok,
final CspInteger _id,
final CspInteger _ops,
final _longest_so_far_4 _l) {
this();
this._tok.setValue(_tok);
this._id.setValue(_id);
this._ops.setValue(_ops);
this._l.setValue(_l);
}
public _status_3(_status_3 other) {
setValue(other);
}
public void setValue(CspValue other) {
setValue((_status_3) other);
}
public void setValue(CspValue value, Fold.BinaryFunction modifier) {
throw new UnsupportedOperationException();
}
public void setValue(_status_3 other) {
this._tok.setValue(other._tok);
this._id.setValue(other._id);
this._ops.setValue(other._ops);
this._l.setValue(other._l);
}
public CspCloneableValue duplicate() {
_status_3 result = new 
_status_3(false);
result._tok = (CspInteger) this._tok.duplicate();
result._id = (CspInteger) this._id.duplicate();
result._ops = (CspInteger) this._ops.duplicate();
result._l = (_longest_so_far_4) this._l.duplicate();
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
public class _Loggedstatus_3 extends _status_3 {
public _Loggedstatus_3(final String varName) {
super(false);
 _tok = new LoggedCspBoolean((varName + ".") + "tok", BigInteger.ZERO); _id = new LoggedFiniteCspInteger((varName + ".") + "id", BigInteger.ZERO, (_W).intValue(), true); _ops = new LoggedFiniteCspInteger((varName + ".") + "ops", BigInteger.ZERO, (new CspInteger(new BigInteger("128"))).intValue(), false); _l = new _Loggedlongest_so_far_4((varName + ".") + "l");}
public _Loggedstatus_3(final String varName, final String parsePos) {
this(varName);
topFrame().addVariable(varName, parsePos, this);
}
public _Loggedstatus_3(final String varName, _status_3 val) {
this(varName);
setValue(val);
}
public _Loggedstatus_3(final String varName, final String parsePos, _status_3 val) {
this(varName, val);
topFrame().addVariable(varName, parsePos, this);
}
}
// initStmt = (sequence (var ((decl (id UPDATEINTERVAL) (integer #t #t () ()) none 10_44))) (var ((decl (id NWORKERS) (integer #t #t () ()) none 10_20))) (var ((decl (id SYM_NONE) (integer #t #t () ()) none 10_0))) (var ((decl (id ULVT_A_TRANSISTOR) (integer #t #t () ()) none 10_9))) (var ((decl (id ALWAYS_HIGH) (integer #t #t () ()) none 10_1))) (var ((decl (id real_time) (boolean #t) none 10_0))) (var ((decl (id SYM_SKIP_FIRST) (integer #t #t () ()) none 10_2))) (var ((decl (id INIT_HIGH) (integer #t #t () ()) none 10_1))) (var ((decl (id ALWAYS_LOW) (integer #t #t () ()) none 10_0))) (var ((decl (id HDC_TRANSISTOR) (integer #t #t () ()) none 10_4))) (var ((decl (id SYM_PARTIAL) (integer #t #t () ()) none 10_1))) (var ((decl (id PROCESS) (integer #t #t () ()) none 10_1278))) (var ((decl (id SLACKER_SIGNOFF) (boolean #t) none 10_-1))) (var ((decl (id LVT_A_TRANSISTOR) (integer #t #t () ()) none 10_7))) (var ((decl (id NONCONST) (integer #t #t () ()) none 10_3))) (var ((decl (id HVT_TRANSISTOR) (integer #t #t () ()) none 10_3))) (var ((decl (id ULVT_TRANSISTOR) (integer #t #t () ()) none 10_5))) (var ((decl (id LVT_TRANSISTOR) (integer #t #t () ()) none 10_2))) (var ((decl (id SVT_A_TRANSISTOR) (integer #t #t () ()) none 10_6))) (var ((decl (id SlackerSkipProteusSubcells) (boolean #t) none 10_0))) (var ((decl (id NO_STATICIZER) (integer #t #t () ()) none 10_-1))) (var ((decl (id IDLE_UNKNOWN) (integer #t #t () ()) none 10_2))) (var ((decl (id FULL_STATICIZER) (integer #t #t () ()) none 10_1))) (var ((decl (id INIT_LOW) (integer #t #t () ()) none 10_0))) (var ((decl (id DEFAULT_UP_DELAY) (integer #t #t () ()) none 10_100))) (var ((decl (id INIT_RANDOM) (integer #t #t () ()) none 10_-1))) (var ((decl (id FAST_REPEATER_BP) (integer #t #t () ()) none 10_45))) (var ((decl (id SYM_FULL_X) (integer #t #t () ()) none 10_5))) (var ((decl (id SVT_TRANSISTOR) (integer #t #t () ()) none 10_1))) (var ((decl (id DEFAULT_DN_DELAY) (integer #t #t () ()) none 10_100))) (var ((decl (id WEAK_STATICIZER) (integer #t #t () ()) none 10_0))) (var ((decl (id COMB_STATICIZER) (integer #t #t () ()) none 10_2))) (var ((decl (id FULL_COMB_STATICIZER) (integer #t #t () ()) none 10_3))) (var ((decl (id HVT_A_TRANSISTOR) (integer #t #t () ()) none 10_8))) (var ((decl (id GATED) (integer #t #t () ()) none 10_1))) (var ((decl (id ALWAYS_ON) (integer #t #t () ()) none 10_0))) (var ((decl (id SYM_TRUNK) (integer #t #t () ()) none 10_4))) (var ((decl (id scan_coverage_model) (boolean #t) none 10_0))) (var ((decl (id SYM_FULL) (integer #t #t () ()) none 10_3))) (var ((decl (id AUTO_STATICIZER) (integer #t #t () ()) none 10_4))) (var ((decl (id IDLE_1) (integer #t #t () ()) none 10_1))) (var ((decl (id FULL_AUTO_STATICIZER) (integer #t #t () ()) none 10_5))) (var ((decl (id IDLE_0) (integer #t #t () ()) none 10_0))) (var ((decl (id METASTABLE_LEAF_ROUTED) (boolean #t) none 10_-1))) (var ((decl (id BEST_REPEATER_BP) (integer #t #t () ()) none 10_60))) (var ((decl (id printstep) (integer #t #t () ()) none 10_4))) (var ((decl (id STATUSW) (integer #t #t () ()) none 10_312))) (var ((decl (id W) (integer #t #t () ()) none 10_61))) (var ((decl (id INTERVAL) (integer #t #t () ()) none 10_10000))) (var ((decl (id Billion) (integer #t #t () ()) none 10_1000000000))))

// initStmt = (sequence (var ((decl (id UPDATEINTERVAL) (integer #t #t () ()) none 10_44))) (var ((decl (id NWORKERS) (integer #t #t () ()) none 10_20))) (var ((decl (id real_time) (boolean #t) none 10_0))) (var ((decl (id SLACKER_SIGNOFF) (boolean #t) none 10_-1))) (var ((decl (id SlackerSkipProteusSubcells) (boolean #t) none 10_0))) (var ((decl (id scan_coverage_model) (boolean #t) none 10_0))) (var ((decl (id METASTABLE_LEAF_ROUTED) (boolean #t) none 10_-1))) (var ((decl (id W) (integer #t #t () ()) none 10_61))))

// static vars
static final CspInteger _UPDATEINTERVAL = new CspInteger(new CspInteger((byte)44).toBigInteger());static final CspInteger _NWORKERS = new CspInteger(new CspInteger((byte)20).toBigInteger());static final CspInteger _real_time = new CspBoolean(new CspInteger((byte)0).toBigInteger());static final CspInteger _SLACKER_SIGNOFF = new CspBoolean(new CspInteger((byte)-1).toBigInteger());static final CspInteger _SlackerSkipProteusSubcells = new CspBoolean(new CspInteger((byte)0).toBigInteger());static final CspInteger _scan_coverage_model = new CspBoolean(new CspInteger((byte)0).toBigInteger());static final CspInteger _METASTABLE_LEAF_ROUTED = new CspBoolean(new CspInteger((byte)-1).toBigInteger());static final CspInteger _W = new CspInteger(new CspInteger((byte)61).toBigInteger());
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
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 157:4-157:29");
final CspInteger _assert_inrange = new LoggedCspBoolean("" + "assert_inrange", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 157:9-157:23", new CspInteger((byte)0).toBigInteger());setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 159:4-159:27");
final CspInteger _i = new LoggedFiniteCspInteger("" + "i", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 159:26-159:27", BigInteger.ZERO, ((_W.subtract(_log2(RuntimeUtils.copyInt(_NWORKERS))))).intValue(), false);setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 160:4-160:21");
final CspInteger _steps = new LoggedFiniteCspInteger("" + "steps", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 160:11-160:16", new CspInteger((byte)0).toBigInteger(), (_W).intValue(), false);setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 161:4-161:21");
final CspInteger _maxlen = new LoggedFiniteCspInteger("" + "maxlen", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 161:11-161:17", new CspInteger((byte)0).toBigInteger(), (_W).intValue(), false);setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 163:4-163:15");
final CspInteger _id = new LoggedFiniteCspInteger("" + "id", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 163:13-163:15", BigInteger.ZERO, ((_W.divide(new CspInteger((byte)2)))).intValue(), false);setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 165:4-165:11");
_id.setValue((new CspInteger(receive(_STRT).getValue())));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 166:4-166:24");
_print(new CspString("id = ").add(new CspString(_id.toString())));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 168:4-208:5");
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
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 169:6-169:36");
final CspInteger _idx = new LoggedFiniteCspInteger("" + "idx", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 169:13-169:16", ((_i.multiply(_NWORKERS)).add(_id)).toBigInteger(), (_W).intValue(), false);setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 170:6-170:30");
final CspInteger _num = new LoggedFiniteCspInteger("" + "num", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 170:13-170:16", ((new CspInteger((byte)2).multiply(_idx)).add(new CspInteger((byte)1))).toBigInteger(), (_W).intValue(), false);setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 171:6-171:20");
final CspInteger _len = new LoggedFiniteCspInteger("" + "len", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 171:13-171:16", new CspInteger((byte)0).toBigInteger(), (_W).intValue(), false);setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 172:6-172:23");
final CspInteger _snum = new LoggedCspInteger("" + "snum", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 172:13-172:17", _num.toBigInteger());setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 173:6-173:19");
final CspInteger _newval = new LoggedCspInteger("" + "newval", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 173:13-173:19", BigInteger.ZERO);setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 176:6-185:8");
label2:
for (;;) {
int trueIdx3 = -1;
if (((_num.compareTo(new CspInteger((byte)1)))!= 0 ? CspInteger.TRUE : CspInteger.FALSE).booleanValue()) {
trueIdx3 = 0;
}
switch (trueIdx3) {
case -2:
break label2;
case -1:
break label2;
case 0:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 177:9-179:10");
{
Wait w4 = null;
label5:
for (;;) {
int trueIdx6 = -1;
if (_odd_1(RuntimeUtils.copyInt(_num)).booleanValue()) {
trueIdx6 = 0;
}
if (_even_0(RuntimeUtils.copyInt(_num)).booleanValue()) {
if (trueIdx6 == -1)
trueIdx6 = 1;
else {
throw new NonDeterminismException("Error -- Multiple guards " + trueIdx6 + " and " + 1 + " true for deterministic repetition or selection at " + "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast:177:9-179:10");
}
}
switch (trueIdx6) {
case -1:
if (w4 == null)
w4 = WaitFactory.newWait(in, out, getNodesWithDifferentValue(true), getNodesWithDifferentValue(false));
final Pair/*<Waitable,Long>*/ p4 = select2(w4);
updateTime(((Long)p4.getSecond()).longValue());
final Waitable wa4 = (Waitable) p4.getFirst();
Arbiter.removeFromWait(w4, wa4);
break;
case 0:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 177:25-177:45");
_newval.setValue(((new CspInteger((byte)3).multiply(_num)).add(new CspInteger((byte)1))));
}
break label5;
}
case 1:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 178:25-178:41");
_newval.setValue((_num.divide(new CspInteger((byte)2))));
}
break label5;
}
}
}
w4 = null;
}
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 180:9-180:21");
_num.setValue(_newval);
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 182:9-182:66");
{
Wait w7 = null;
label8:
for (;;) {
int trueIdx9 = -1;
if (_assert_inrange.booleanValue()) {
trueIdx9 = 0;
}
switch (trueIdx9) {
case -1:
/* skip */
break label8;
case 0:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 182:30-182:64");
handleAssert(whereAmI, ((_num.compareTo(_newval))== 0 ? CspInteger.TRUE : CspInteger.FALSE), new CspString("overflow!"));
;
}
break label8;
}
}
}
w7 = null;
}
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 184:9-184:14");
_len.setValue(new CspInteger((byte)1), Fold.ADD_FUNCTION);
}
break;
}
}
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 176:6-185:8");
yield();
}
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 187:6-187:22");
handleAssert(whereAmI, ((_num.compareTo(new CspInteger((byte)1)))== 0 ? CspInteger.TRUE : CspInteger.FALSE), null);
;
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 188:6-188:18");
_steps.setValue(_len, Fold.ADD_FUNCTION);
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 190:6-195:7");
{
Wait w10 = null;
label11:
for (;;) {
int trueIdx12 = -1;
if (((_len.compareTo(_maxlen))> 0 ? CspInteger.TRUE : CspInteger.FALSE).booleanValue()) {
trueIdx12 = 0;
}
switch (trueIdx12) {
case -1:
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 194:17-194:21");
/* skip */
}
break label11;
case 0:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 191:9-191:21");
_maxlen.setValue(_len);
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 192:9-192:48");
_sendStatus_2(((CspInteger) (new CspInteger((byte)0)).duplicate()), RuntimeUtils.copyInt(_id), RuntimeUtils.copyInt(_len), RuntimeUtils.copyInt(_snum), RuntimeUtils.copyInt(_steps));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 193:9-193:18");
_steps.setValue(new CspInteger((byte)0));
}
break label11;
}
}
}
w10 = null;
}
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 197:6-206:7");
{
Wait w13 = null;
label14:
for (;;) {
int trueIdx15 = -1;
if ((((_i.remainder(_UPDATEINTERVAL)).compareTo(new CspInteger((byte)0)))== 0 ? CspInteger.TRUE : CspInteger.FALSE).booleanValue()) {
trueIdx15 = 0;
}
switch (trueIdx15) {
case -1:
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 205:17-205:21");
/* skip */
}
break label14;
case 0:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 198:8-198:41");
_sendStatus_2(((CspInteger) (new CspInteger((byte)-1)).duplicate()), RuntimeUtils.copyInt(_id), RuntimeUtils.copyInt(new CspInteger((byte)0)), RuntimeUtils.copyInt(new CspInteger((byte)0)), RuntimeUtils.copyInt(_steps));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 199:8-199:17");
_steps.setValue(new CspInteger((byte)0));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 201:8-201:17");
final CspInteger _dummy = new LoggedCspInteger("" + "dummy", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 201:12-201:17", BigInteger.ZERO);setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 202:8-202:18");
_dummy.setValue((new CspInteger(receive(_STRT).getValue())));
}
break label14;
}
}
}
w13 = null;
}
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 207:6-207:9");
_i.setValue(new CspInteger((byte)1), Fold.ADD_FUNCTION);
}
break;
}
}
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 168:4-208:5");
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
