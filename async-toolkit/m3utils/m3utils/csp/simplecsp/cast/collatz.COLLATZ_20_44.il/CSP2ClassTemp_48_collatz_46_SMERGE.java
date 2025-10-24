// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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

public final class CSP2ClassTemp_48_collatz_46_SMERGE extends CspRuntimeAbstractDevice {

// instance variables
private int outputOffset = 0;
private int inputOffset = 0;
private int nodeOffset = 0;
// declare channel vars
// outputPortDecl: (port-definition L in (array (range 0 1) (channel standard.channel.bd(312) 1 8343699359066055009355553539724812947666814540455674882605631280555545803830627148527195652096 f ((port-definition C out (structure standard.channel.bdc ((port-definition q out (node #f 1)) (port-definition a in (node #f 1))))) (port-definition D out (node #t 312))))))
public final CspChannelInArray1 _L;
// outputPortDecl: (port-definition R out (channel standard.channel.bd(312) 1 8343699359066055009355553539724812947666814540455674882605631280555545803830627148527195652096 f ((port-definition C out (structure standard.channel.bdc ((port-definition q out (node #f 1)) (port-definition a in (node #f 1))))) (port-definition D out (node #t 312)))))
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
public CSP2ClassTemp_48_collatz_46_SMERGE(final DeviceParameters deviceParams, final ChannelInput[] in, final ChannelOutput[] out, final WideNode[] nodes
) throws InterruptedException {
super(deviceParams.getName(), in, out, nodes, deviceParams.isOutputSuppressed(), deviceParams.getArbitrationMode());
_srandom(new CspInteger(new BigInteger(Long.toString(deviceParams.getSeed()))));
setDigitalTau(deviceParams.getDigitalTau());
this._L = new CspChannelInArray1(0, 1, "standard.channel.bd(312)", in, inputOffset);
inputOffset += 2;
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
public CspInteger _max_0_real(final CspInteger _a,
final CspInteger _b) throws InterruptedException {
final CspInteger _max = new LoggedCspInteger("" + "max", BigInteger.ZERO);{
setWhereAmI("/nfs/site/disks/or_lhdk75_disk0037/w137/gorda/mnystroe/p4/hw-dev/cast/standard/attributes.cast 319:7-319:38");
{
Wait w0 = null;
label1:
for (;;) {
int trueIdx2 = -1;
if (((_a.compareTo(_b))> 0 ? CspInteger.TRUE : CspInteger.FALSE).booleanValue()) {
trueIdx2 = 0;
}
switch (trueIdx2) {
case -1:
{
setWhereAmI("/nfs/site/disks/or_lhdk75_disk0037/w137/gorda/mnystroe/p4/hw-dev/cast/standard/attributes.cast 319:32-319:37");
_max.setValue(_b);
}
break label1;
case 0:
{
{
setWhereAmI("/nfs/site/disks/or_lhdk75_disk0037/w137/gorda/mnystroe/p4/hw-dev/cast/standard/attributes.cast 319:15-319:20");
_max.setValue(_a);
}
break label1;
}
}
}
w0 = null;
}
}
return _max;
}
public CspInteger _max_0(final CspInteger _a,
final CspInteger _b) throws InterruptedException {
try {
enterFrame("max");
return _max_0_real( _a,
 _b);
} finally { leaveFrame(); } 
}
public void _recv_1_real(final CspInteger _which,
final _status_2 _s) throws InterruptedException {
final CspInteger _xs = new LoggedCspInteger("" + "xs", "empty_parse_position 0:0-0:0", BigInteger.ZERO);{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 317:7-317:26");
final CspInteger _oldops = new LoggedCspInteger("" + "oldops", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 317:11-317:17", _s._ops.toBigInteger());setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 318:7-318:32");
final CspInteger _oldlen = new LoggedCspInteger("" + "oldlen", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 318:11-318:17", _s._l._length.toBigInteger());setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 320:7-320:18");
_xs.setValue((new CspInteger(receive(_L.get(_which, "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast", 320, 9)).getValue())));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 322:7-322:20");
unpack(_s, _xs)
;
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 325:7-325:48");
_s._l._length.setValue(_max_0(RuntimeUtils.copyInt(_oldlen), RuntimeUtils.copyInt(_s._l._length)));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 326:7-326:29");
_s._ops.setValue(_oldops, Fold.ADD_FUNCTION);
}
}
public void _recv_1(final CspInteger _which,
final _status_2 _s) throws InterruptedException {
try {
enterFrame("recv");
_recv_1_real( _which,
 _s);
} finally { leaveFrame(); } 
}
public void _checksend_3_real(final _status_2 _os,
final _status_2 _s) throws InterruptedException {
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 336:7-341:11");
{
Wait w3 = null;
label4:
for (;;) {
int trueIdx5 = -1;
if (CspInteger.valueOf(((_os._l._length.compareTo(_s._l._length))!= 0 ? CspInteger.TRUE : CspInteger.FALSE).booleanValue() || _s._tok.booleanValue()).booleanValue()) {
trueIdx5 = 0;
}
switch (trueIdx5) {
case -1:
/* skip */
break label4;
case 0:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 338:7-338:10");
send(_R, pack(_s).toBigInteger());
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 338:12-338:22");
_s._ops.setValue(new CspInteger((byte)0));
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 338:24-338:30");
_os.setValue(_s);
}
break label4;
}
}
}
w3 = null;
}
}
}
public void _checksend_3(final _status_2 _os,
final _status_2 _s) throws InterruptedException {
try {
enterFrame("checksend");
_checksend_3_real( _os,
 _s);
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
public class _status_2 implements CspCloneableValue, CspStructure {
public CspInteger _tok;
public CspInteger _id;
public CspInteger _ops;
public _longest_so_far_4 _l;

public _status_2() {
this(true);
}
public _status_2(final boolean init) {
if (init) {
 _tok = new CspBoolean(BigInteger.ZERO); _id = new FiniteCspInteger(BigInteger.ZERO, (_W).intValue(), true); _ops = new FiniteCspInteger(BigInteger.ZERO, (new CspInteger(new BigInteger("128"))).intValue(), false); _l = new _longest_so_far_4();}
}
public _status_2(final CspInteger _tok,
final CspInteger _id,
final CspInteger _ops,
final _longest_so_far_4 _l) {
this();
this._tok.setValue(_tok);
this._id.setValue(_id);
this._ops.setValue(_ops);
this._l.setValue(_l);
}
public _status_2(_status_2 other) {
setValue(other);
}
public void setValue(CspValue other) {
setValue((_status_2) other);
}
public void setValue(CspValue value, Fold.BinaryFunction modifier) {
throw new UnsupportedOperationException();
}
public void setValue(_status_2 other) {
this._tok.setValue(other._tok);
this._id.setValue(other._id);
this._ops.setValue(other._ops);
this._l.setValue(other._l);
}
public CspCloneableValue duplicate() {
_status_2 result = new 
_status_2(false);
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
public class _Loggedstatus_2 extends _status_2 {
public _Loggedstatus_2(final String varName) {
super(false);
 _tok = new LoggedCspBoolean((varName + ".") + "tok", BigInteger.ZERO); _id = new LoggedFiniteCspInteger((varName + ".") + "id", BigInteger.ZERO, (_W).intValue(), true); _ops = new LoggedFiniteCspInteger((varName + ".") + "ops", BigInteger.ZERO, (new CspInteger(new BigInteger("128"))).intValue(), false); _l = new _Loggedlongest_so_far_4((varName + ".") + "l");}
public _Loggedstatus_2(final String varName, final String parsePos) {
this(varName);
topFrame().addVariable(varName, parsePos, this);
}
public _Loggedstatus_2(final String varName, _status_2 val) {
this(varName);
setValue(val);
}
public _Loggedstatus_2(final String varName, final String parsePos, _status_2 val) {
this(varName, val);
topFrame().addVariable(varName, parsePos, this);
}
}
// initStmt = (sequence (var ((decl (id SYM_NONE) (integer #t #t () ()) none 10_0))) (var ((decl (id ULVT_A_TRANSISTOR) (integer #t #t () ()) none 10_9))) (var ((decl (id ALWAYS_HIGH) (integer #t #t () ()) none 10_1))) (var ((decl (id real_time) (boolean #t) none 10_0))) (var ((decl (id SYM_SKIP_FIRST) (integer #t #t () ()) none 10_2))) (var ((decl (id INIT_HIGH) (integer #t #t () ()) none 10_1))) (var ((decl (id ALWAYS_LOW) (integer #t #t () ()) none 10_0))) (var ((decl (id HDC_TRANSISTOR) (integer #t #t () ()) none 10_4))) (var ((decl (id SYM_PARTIAL) (integer #t #t () ()) none 10_1))) (var ((decl (id PROCESS) (integer #t #t () ()) none 10_1278))) (var ((decl (id SLACKER_SIGNOFF) (boolean #t) none 10_-1))) (var ((decl (id LVT_A_TRANSISTOR) (integer #t #t () ()) none 10_7))) (var ((decl (id NONCONST) (integer #t #t () ()) none 10_3))) (var ((decl (id HVT_TRANSISTOR) (integer #t #t () ()) none 10_3))) (var ((decl (id ULVT_TRANSISTOR) (integer #t #t () ()) none 10_5))) (var ((decl (id LVT_TRANSISTOR) (integer #t #t () ()) none 10_2))) (var ((decl (id SVT_A_TRANSISTOR) (integer #t #t () ()) none 10_6))) (var ((decl (id SlackerSkipProteusSubcells) (boolean #t) none 10_0))) (var ((decl (id NO_STATICIZER) (integer #t #t () ()) none 10_-1))) (var ((decl (id IDLE_UNKNOWN) (integer #t #t () ()) none 10_2))) (var ((decl (id FULL_STATICIZER) (integer #t #t () ()) none 10_1))) (var ((decl (id INIT_LOW) (integer #t #t () ()) none 10_0))) (var ((decl (id DEFAULT_UP_DELAY) (integer #t #t () ()) none 10_100))) (var ((decl (id INIT_RANDOM) (integer #t #t () ()) none 10_-1))) (var ((decl (id FAST_REPEATER_BP) (integer #t #t () ()) none 10_45))) (var ((decl (id SYM_FULL_X) (integer #t #t () ()) none 10_5))) (var ((decl (id SVT_TRANSISTOR) (integer #t #t () ()) none 10_1))) (var ((decl (id DEFAULT_DN_DELAY) (integer #t #t () ()) none 10_100))) (var ((decl (id WEAK_STATICIZER) (integer #t #t () ()) none 10_0))) (var ((decl (id COMB_STATICIZER) (integer #t #t () ()) none 10_2))) (var ((decl (id FULL_COMB_STATICIZER) (integer #t #t () ()) none 10_3))) (var ((decl (id HVT_A_TRANSISTOR) (integer #t #t () ()) none 10_8))) (var ((decl (id GATED) (integer #t #t () ()) none 10_1))) (var ((decl (id ALWAYS_ON) (integer #t #t () ()) none 10_0))) (var ((decl (id SYM_TRUNK) (integer #t #t () ()) none 10_4))) (var ((decl (id scan_coverage_model) (boolean #t) none 10_0))) (var ((decl (id SYM_FULL) (integer #t #t () ()) none 10_3))) (var ((decl (id AUTO_STATICIZER) (integer #t #t () ()) none 10_4))) (var ((decl (id IDLE_1) (integer #t #t () ()) none 10_1))) (var ((decl (id FULL_AUTO_STATICIZER) (integer #t #t () ()) none 10_5))) (var ((decl (id IDLE_0) (integer #t #t () ()) none 10_0))) (var ((decl (id METASTABLE_LEAF_ROUTED) (boolean #t) none 10_-1))) (var ((decl (id BEST_REPEATER_BP) (integer #t #t () ()) none 10_60))) (var ((decl (id printstep) (integer #t #t () ()) none 10_4))) (var ((decl (id STATUSW) (integer #t #t () ()) none 10_312))) (var ((decl (id W) (integer #t #t () ()) none 10_61))) (var ((decl (id INTERVAL) (integer #t #t () ()) none 10_10000))) (var ((decl (id Billion) (integer #t #t () ()) none 10_1000000000))))

// initStmt = (sequence (var ((decl (id real_time) (boolean #t) none 10_0))) (var ((decl (id SLACKER_SIGNOFF) (boolean #t) none 10_-1))) (var ((decl (id SlackerSkipProteusSubcells) (boolean #t) none 10_0))) (var ((decl (id scan_coverage_model) (boolean #t) none 10_0))) (var ((decl (id METASTABLE_LEAF_ROUTED) (boolean #t) none 10_-1))) (var ((decl (id W) (integer #t #t () ()) none 10_61))))

// static vars
static final CspInteger _real_time = new CspBoolean(new CspInteger((byte)0).toBigInteger());static final CspInteger _SLACKER_SIGNOFF = new CspBoolean(new CspInteger((byte)-1).toBigInteger());static final CspInteger _SlackerSkipProteusSubcells = new CspBoolean(new CspInteger((byte)0).toBigInteger());static final CspInteger _scan_coverage_model = new CspBoolean(new CspInteger((byte)0).toBigInteger());static final CspInteger _METASTABLE_LEAF_ROUTED = new CspBoolean(new CspInteger((byte)-1).toBigInteger());static final CspInteger _W = new CspInteger(new CspInteger((byte)61).toBigInteger());
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
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 349:4-349:12");
final _status_2 _s = new _Loggedstatus_2("" + "s", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 349:11-349:12");setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 351:4-366:7");
label6:
for (;;) {
int trueIdx7 = -1;
if (new CspInteger((byte)-1).booleanValue()) {
trueIdx7 = 0;
}
switch (trueIdx7) {
case -2:
break label6;
case -1:
break label6;
case 0:
{
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 353:6-353:19");
final _status_2 _os = new _Loggedstatus_2("" + "os", "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 353:13-353:15", _s);setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 355:6-361:8");
LinkedList alternatives8 = new LinkedList();
alternatives8.add(new Alternative(
new NullaryPredicate() {
public boolean evaluate() throws InterruptedException {
return (probe(_L.get(new CspInteger((byte)0), "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast", 358, 10)) ? CspInteger.TRUE : CspInteger.FALSE).booleanValue();
}
}, 
new Action() {
public void execute() throws InterruptedException {
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 358:16-358:26");
_recv_1(RuntimeUtils.copyInt(new CspInteger((byte)0)), _s);
}
}
}
));
alternatives8.add(new Alternative(
new NullaryPredicate() {
public boolean evaluate() throws InterruptedException {
return (probe(_L.get(new CspInteger((byte)1), "/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast", 360, 10)) ? CspInteger.TRUE : CspInteger.FALSE).booleanValue();
}
}, 
new Action() {
public void execute() throws InterruptedException {
{
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 360:16-360:26");
_recv_1(RuntimeUtils.copyInt(new CspInteger((byte)1)), _s);
}
}
}
));
arbitrate(new Arbiter((Alternative[])alternatives8.toArray(new Alternative[0]),
(Linkage) null,
Arbiter.NON_LINKED, in, out, getNodesWithDifferentValue(true), getNodesWithDifferentValue(false), arbiterRandom, CSP2ClassTemp_48_collatz_46_SMERGE.this),"/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 355:6-361:8");
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 364:6-364:22");
_checksend_3(_os, _s);
}
break;
}
}
setWhereAmI("/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils/csp/simplecsp/cast/collatz.cast 351:4-366:7");
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
