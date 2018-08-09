%left '+' '-' '*' '/'
%nonassoc oNEGATIVEOP
%nonassoc oPOSITIVEOP
%start mystart

mystart:
	only compilation

compilation:
	wusafe_intfc oUNSAFE Interface
	wusafe_module oUNSAFE Module
	wusafe_geninf oUNSAFE GenInf
	wusafe_genmod oUNSAFE GenMod
	wousafe_intfc Interface
	wousafe_module Module
	wousafe_geninf GenInf
	wousafe_genmod GenMod

Interface:
	wimpanddecl oINTERFACE oID ';' importlist decllist oEND oID '.'
	wimpanddecleq oINTERFACE oID '=' oID GenActls oEND oID '.'

GenInf:
	only oGENERIC oINTERFACE oID GenFmls ';' importlist decllist oEND oID '.'

GenMod:
	only oGENERIC oMODULE oID GenFmls ';' importlist Block oID '.'

GenActls:
	nonempty '(' oID idlist ')'
	empty '(' ')'

decllist:
	empty
	nonempty decl decllist 

decl:
	const_nonempty oCONST constdecllist
	typedecl_nonempty oTYPE typedecllist
	exception oEXCEPTION exceptionlist
	var oVAR variabledecllist
	proc1 ProcedureHead ';'
	proc2 ProcedureHead '=' Block oID ';'
	rev oREVEAL qualidlist_reveal

qualidlist_reveal:
	nothing
	something_eq qualid '=' type ';' qualidlist_reveal
	something_sub qualid oSUBTYPEOP type ';' qualidlist_reveal

ProcedureHead:
	only oPROCEDURE oID signature

ProcedureNonRaisesType:
	only oPROCEDURE signature_nonraises

ProcedureRaisesType:
	only oPROCEDURE signature_raises

signature_raises:
	typed_alpha '(' Formals ')' ':' nonProcType oRAISES Raises
	typed_beta '(' Formals ')' ':' ProcedureRaisesType oRAISES Raises
	nontyped '(' Formals ')' oRAISES Raises

signature_nonraises:
	typed '(' Formals ')' ':' type
	nontyped '(' Formals ')'


signature:
        raises signature_raises
        nonraises signature_nonraises

Raises:
	any oANY
	empty '{' '}'
	nonempty '{' qualid qualidlist '}'

qualid:
	simple oID
	not_simple oID '.' oID

qualidlist:
	nothing
	something ',' qualid qualidlist

exceptionlist:
	empty
	only exceptiondecl ';' exceptionlist

exceptiondecl:
	empty oID
	nonempty oID '(' type ')'

constdecllist:
	empty
	only constdecl ';' constdecllist

constdecl:
	empty oID '=' constexpression
	nonempty oID ':' type '=' constexpression

constexpression:
	myexpr expression

typedecllist:
	empty
	only typedecl ';' typedecllist

importlist:
	nothing
	something importlist import

import:
	asimport oIMPORT importitem importitemlist ';'
	fromimport oFROM oID oIMPORT oID idlist ';'

importitemlist:
	nothing
	something ',' importitem importitemlist

importitem:
	no_as oID
	as oID oAS oID

idlist:
	nothing
	something ',' oID idlist

Formals:
	empty
	nonempty Formal PostFormal

PostFormal:
	empty
	semi ';'
	nonempty ';' Formal PostFormal

Formal:
	modetype Mode oID idlist ':' type
	modeconst Mode oID idlist oASSOP constexpression
	modeboth Mode oID idlist ':' type oASSOP constexpression
	nomodetype oID idlist ':' type 
	nomodeconst oID idlist oASSOP constexpression
	nomodeboth oID idlist ':' type oASSOP constexpression

Mode:
	val oVALUE
	var oVAR
	rdonly oREADONLY

variabledecllist:
	empty
	nonempty variabledecl ';' variabledecllist

variabledecl:
	basictype oID idlist ':' type
	basicexpr oID idlist oASSOP expression
	both oID idlist ':' type oASSOP expression

GenFmls:
	empty '(' ')'
	nonempty '(' oID idlist ')'

Module:
	blocknexports oMODULE oID oEXPORTS oID idlist ';' importlist Block oID '.'
	blocknnoexports oMODULE oID idlist ';' importlist Block oID '.'
	genactlnexports oMODULE oID oEXPORTS oID idlist '=' oID GenActls oEND oID '.'
	genactlnnoexports oMODULE oID '=' oID GenActls oEND oID '.'

typedecl:
	eq oID '=' type
	sub oSUBTYPEOP type

Block:
	wdecl decllist oBEGIN kindastart oEND

expression:
	single E1
	double expression oOR E1
	call CallSt

E1:
	single E2
	double E1 oAND E2

E2:
	single E3
	double oNOT E2

E3:
	single E4
	double E3 Relop E4

E4:
	single E5
	double E4 Addop E5

E5:
	single E6
	double E5 Mulop E6

E6:
	plus oPOSITIVEOP E6
	minus oNEGATIVEOP E6
	next E7

E7:
	wsel E7 Selector
	wosel E8

E8:
	mynum oNUMBER
	mycharlit oCHARLITERAL
	mytextlit oTEXTLITERAL
	constr Constructor
	myid oID
	par '(' expression ')'

Selector:
	carat '^'
	dotid '.' oID
	myexpr '[' exprlist ']'

exprlist:
	two exprlist ',' expression
	one expression

Constructor:
	nothing '}' ';' ':' '{'

ActualList:
	one Actual
	two ActualList ',' Actual

Actual:
	mytype type
	expr expression
	assop_expr oID oASSOP expression

Addop:
	add '+'
	minus '-'
	ampersand '&'

Mulop:
	mul '*'
	div '/'
	divword oDIV
	mod oMOD

Relop:
	eq '='
	neq '#'
	lt oLTOP
	lte oLTEOP
	gt oGTOP
	gte oGTEOP
	in oIN
	
kindastart:
	empty
	nonempty statement statement_recursive

statement_recursive:
	empty
	semi ';'
	nonempty ';' statement statement_recursive

CaseSt:
	both oCASE expression oOF CaseList oELSE kindastart oEND
	nocase oCASE expression oOF oELSE kindastart oEND
	noelse oCASE expression oOF CaseList oEND
	neither oCASE expression oOF oEND

CaseList:
	single Case
	more CaseList '|' Case

Case:
	only LabelsList oASSOP kindastart

LabelsList:
	single Labels
	more LabelsList '|' Labels

Labels:
	one constexpression
	two constexpression oTWOPERIODS constexpression

AssignSt:
	only expression oASSOP expression

CallSt:
	empty expression '(' ')'
	nonempty expression '(' ActualList ')'

ExitSt:
	only oEXIT

EvalSt:
	only oEVAL expression

ForSt:
	noby oFOR oID oASSOP expression oTO expression oDO kindastart oEND
	by oFOR oID oASSOP expression oTO expression oBY expression oDO kindastart oEND

IfSt:
	noelifwelse oIF expression oTHEN kindastart oELSE kindastart oEND
	noelifnoelse oIF expression oTHEN kindastart oEND
	welse oIF expression oTHEN kindastart elsiflist oELSE kindastart oEND
	noelse oIF expression oTHEN kindastart elsiflist oEND

LockSt:
	only oLOCK expression oDO kindastart oEND

LoopSt:
	only oLOOP kindastart oEND

RaiseSt:
	wexpr oRAISE qualid '(' expression ')'
	noexpr oRAISE qualid

RepeatSt:
	only oREPEAT kindastart oUNTIL expression

ReturnSt:
	noexpr oRETURN expression
	expr oRETURN

TCaseSt:
	both oTYPECASE expression oOF TCaseList oELSE kindastart oEND
	wtcase oTYPECASE expression oOF TCaseList oEND
	welse oTYPECASE expression oOF oELSE kindastart oEND
	neither oTYPECASE expression oOF oEND

TCaseList:
	simple TCase
	complex TCaseList ',' TCase

TCase:
	empty reftypelist oASSOP kindastart
	nonempty reftypelist '(' oID ')' oASSOP kindastart

elsiflist:
	single oELSIF expression oTHEN kindastart
	double elsiflist oELSIF expression oTHEN kindastart

TryXptSt:
	both oTRY kindastart oEXCEPT HandlerList oELSE kindastart oEND
	hand oTRY kindastart oEXCEPT HandlerList oEND
	else oTRY kindastart oEXCEPT oELSE kindastart oEND
	neither oTRY kindastart oEXCEPT oEND

TryFinSt:
	only oTRY kindastart oFINALLY kindastart oEND

HandlerList:
	one Handler
	more HandlerList '|' Handler

Handler:
	noid qualid qualidlist oASSOP kindastart
	wid qualid qualidlist '(' oID ')' oASSOP kindastart

WhileSt:
	only oWHILE expression oDO kindastart oEND

WithSt:
	only oWITH BindingList oDO kindastart oEND

BindingList:
	single Binding
	double BindingList ',' Binding

Binding:
	only oID '=' expression

statement:
	a AssignSt
	b Block
	c CallSt
	d CaseSt
	e ExitSt
	f EvalSt
	g ForSt
	h IfSt
	i LockSt
	j LoopSt
	k RaiseSt
	l RepeatSt
	m ReturnSt
	n TCaseSt
	o TryXptSt
	p TryFinSt
	q WhileSt
	r WithSt

TypeName:
	qid qualid
	rt oROOT
	urt oUNTRACED oROOT

ordtypelist:
	lone OrdType
	more ordtypelist ',' OrdType

reftypelist:
	lone ReferenceType
	more reftypelist ',' ReferenceType

PackedType:
	only oBITS constexpression oFOR notProcedureNonRaisesType

EnumType:
	empty '{' '}'
	nonempty '{' oID idlist '}'

RecordType:
	only oRECORD Fields oEND

Fields:
	empty
	semicolon FieldList ';'
	nosemi FieldList

FieldList:
	single Field
	double FieldList ';' Field

Field:
	bothtypeconstexpr oID idlist ':' type oASSOP constexpression
	onlytype oID idlist ':' type
	onlyconstexpr oID idlist oASSOP constexpression

RefType:
	both oUNTRACED MyBrand oREF nonProcType
	utrace oUNTRACED oREF nonProcType
	brnd MyBrand oREF nonProcType
	neither oREF nonProcType

MyBrand:
	wtl oBRANDED oTEXTLITERAL
	wotl oBRANDED

SubrangeType:
	only oLEFTBRACKET constexpression oTWOPERIODS constexpression oRIGHTBRACKET

ObjectType:
	notowbrand MyBrand oOBJECT Fields opt_methods opt_overrides oEND
	notowobrand oOBJECT Fields opt_methods opt_overrides oEND
	wbrand type_or_obj MyBrand oOBJECT Fields opt_methods opt_overrides oEND
	wobrand type_or_obj oOBJECT Fields opt_methods opt_overrides oEND

type_or_obj:
	t TypeName
	o ObjectType

opt_methods:
	nothing
	something oMETHODS Methods

opt_overrides:
	nothing
	something oOVERRIDES MyOverrides

MyOverrides:
	nothing
	something OverrideList
	wsemi OverrideList ';'

OverrideList:
	single Override
	double OverrideList ';' Override

Override:
	only oID oASSOP constexpression

Methods:
	nothing
	something MethodList
	semi MethodList ';'

MethodList:
	single Method
	double MethodList ';' Method

Method:
	alone oID signature
	wcexpr oID signature oASSOP constexpression

OrdType:
       j SubrangeType               
       d EnumType

ArrayTypeAlpha:
	wtype oARRAY ordtypelist oOF notProcedureNonRaisesType
	open oARRAY oOF notProcedureNonRaisesType

ArrayTypeBeta:
	wtype oARRAY ordtypelist oOF ProcedureNonRaisesType
	open oARRAY oOF ProcedureNonRaisesType

SetTypeAlpha:
	only oSET oOF notProcedureNonRaisesType

SetTypeBeta:
	only oSET oOF ProcedureNonRaisesType


nonProcType:
	a TypeName
	b ArrayTypeAlpha
	c PackedType
	g RecordType
	i SetTypeAlpha
	k '(' type ')'
        o OrdType
        r ReferenceType
               
ReferenceType:
        e ObjectType
        h RefType
              

notProcedureNonRaisesType:
        alpha nonProcType
        beta ProcedureRaisesType

containingProcedureNonRaisesType:
        plain ProcedureNonRaisesType
        array ArrayTypeBeta
        set  SetTypeBeta
               
type:
        alpha notProcedureNonRaisesType
        beta  containingProcedureNonRaisesType
