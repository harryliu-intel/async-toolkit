
#define _LN(link, text) \
<A HREF = "link">text</A>

#define _A(name) \
<A NAME = "name"></A>

#define _HTML_BEGIN(title) \
<HTML> \
<HEAD> \
<TITLE>title</TITLE> \
</HEAD> \
<BODY BGCOLOR="#ffffff" VLINK="#006633"> \
<H2>title</H2> \
<P> \

#define _H(title) <H3>title</H3>
#define _TT(text) <TT>text</TT>
#define _IT(text) <I>text</I>
#define _BF(text) <B>text</B>
#define _C(text) <P><CENTER>text</CENTER><P>

#define PL_BEGIN(title) _HTML_BEGIN(parserlib: title)

#define PL_END \
<HR> \
<A HREF = "index.html">[parserlib page]</A> \
</BODY></HTML> \


// tables

#define _TR2(forma,formb) <TR><TD><TT>forma,formb</TT></TD><TD>
#define _TR(form) <TR><TD>_TT(form)</TD><TD>
#define _TRP(form) <TR><TD><PRE>form</PRE></TD><TD>
#define TR_ </TD></TR>
#define _TRE </TD></TR>



// tok

#define _TS _LN(ktok.html#spec, token specification)
#define _TI _LN(ktok.html#intf, token interface)


// ext

#define _EXT _LN(kext.html,ext)


// lex and yacc

#define LY_BEGIN(lex, klex, lexer, MylangLex, Lexer, l) \
PL_BEGIN(klex: defining the lexer) \
A _IT(lexer interface) and its implementation are generated \
_LN(m3build.html,automatically) by m3build by running the command \
_C(_TT(lex mylang.l  [ -t mylang.t [-ti3 mylangTok.i3] ]  [ -o mylangLex.i3 ])) \
where _TT(Mylang.t) is a _TS, _TT(MylangTok.i3) is a _TI, _TT(Mylang.l) is a \
_IT(lexer specification), and _TT(MylangLex.i3) is the generated lexer \
interface. \
_A(spec)_H(lexer specification) \


