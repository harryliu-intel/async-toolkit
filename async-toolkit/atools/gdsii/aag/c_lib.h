// "$Id: //depot/user/aubrey/src/lib/libaag/c_lib.h#1 $ AAG";
#ifndef _c_lib_h
#define _c_lib_h

/*******************************************************************************
*
*                            C _ L I B
*                            =========
* Purpose:
*    A library of miscellaneous functions.
*
* Functions:
*    fillstr(s, n, c) - Fill string [s] with [n] [c]'s.
*    strsave(s) - Return a pointer to a copy of [s].
*    vsc_strccpy(s, sc, n) - Copy from [sc] into [s] until a blank or [n] chars.
*    strcpyc(sc, s, n) - Copy from [s] into [sc], fill [sc] with trailing blanks.
*    strncpyc(s, sc, n) - Copy characters from [sc] starting at [n] into [s].
*    strloc(t, ti, s) - Search [t] starting at [ti] for [s].
*    ntbscpy(r, s) - Copy [s] into [r] without trailing blanks (whitespace).
*    trimtb(s) - Trim trailing blanks from [s].
*    trimlb(s) - Trim leading blanks from [s].
*    gettoken(tok, s, delim) - Copy next token from [s] into [tok].
*    strupper(s) - Convert all alpha characters in [s] to uppercase.
*    strlower(s) - Convert all alpha characters in [s] to lowercase.
*    strsub(s, c, sc) - Substitute all occurences of [c] in [s] with [sc].
*    strrev(s) - Reverse order of characters in [s].
*    itoa(s, num) - Convert [num] to ascii digits in [s].
*    ltoa(s, num) - Convert long [num] to ascii digits in [s].
*
* Notes:
*    -  No range checking, so beware!
*
*******************************************************************************/
#ifdef __cplusplus
extern "C" {
#endif
void fillstr (char *s, int n, int c);
char *strsave (char *s);
void vsc_strccpy (char *s, char *sc, int n);
void strcpyc (char *sc, char *s, int n);
void strncpyc (char s[], char sc[], int n);
int strloc (char t[], int ti, char s[]);
void ntbscpy (char *r, char *s);
void trimtb (char *s);
void trimlb (char s[]);
int gettoken (char *tok, char *s, char *delim);
void strupper (char *s);
void strlower (char *s);
void strsub (char s[], int c, int sc);
void strrev (char s[]);
void itoa (char s[], int num);
int is_number (char *word);
void upper_case (char *word);
void ltoa_aux (char s[], long num);
void vsc_add_extension(char file_name[], char extension[]);
#ifdef __cplusplus
}
#endif


#define STR_SHORT 16
#define STR_MED   32
#define STR_LONG  128
#define STR_FILE  1024

#endif

