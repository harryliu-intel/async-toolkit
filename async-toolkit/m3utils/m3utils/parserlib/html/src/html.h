
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
#define _C(text) <P><CENTER>text</CENTER><P>

#define PL_BEGIN(title) _HTML_BEGIN(parserlib: title)

#define PL_END \
<HR> \
<A HREF = "index.html">[parserlib page]</A> \
</BODY></HTML> \
