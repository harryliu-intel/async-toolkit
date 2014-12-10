/* File: c_lib.c */
// "$Id$ AAG";

/*******************************************************************************
*
*                            C _ L I B
*                            =========
* Purpose:
*    A library of miscellaneous functions.
*
* Functions:
*    memcpy(r, s, n) - Copy [n] bytes from [s] into r[].
*    memset(s, c, n) - Set [n] bytes in [s] to [c].
*    fillstr(s, n, c) - Fill string [s] with [n-1] [c]'s.
*    strsave(s) - Return a pointer to a copy of [s].
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
*    is_number(word) - checks string for being numeric (from Aubrey strings.c)
*    upper_case(word) - same as strupper (from Aubrey strings.c)
*
* Notes:
*    -  No range checking, so beware!
*
*******************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#ifdef hpux
extern char *malloc ();
#else
#include <stdlib.h>
#endif
#include "c_std.h"
#include "c_lib.h"
#include <errno.h>
extern int errno;


/***
*    FILLSTR
*   ---------
* Fill string [s] with [n] [c]'s.
***/
void fillstr(char *s, int n, int c)

   {  /* fillstr */

   /* variables */
     int i;

   /* mainline */
   for(i = 0; i < (n-1); i++)
      {
      *s++ = c;
      }
   *s = '\0';

   }    /* fillstr */


/***
*    STRSAVE
*   ---------
* Return a pointer to a copy of [s].  Caller is resposible for
* freeing memory allocated to the copy.
***/
/* strsave is multiply defined in Verilog BackPlane version for LWB */
#ifndef VSCPLI
char *strsave(char *s)

   {  /* strsave */

   /* variables */
   char *p;

   if ((p = malloc((unsigned)strlen(s)+1)) != NULL)
      {
      (void) strcpy(p, s);
      }

   return(p);

   }  /* strsave */
#endif

/***
*    STRCCPY
*   ---------
* Copy characters from [sc] into [s] until a whitespace or [n] characters
* are copied.  [s] will be NULL terminated.
***/
void vsc_strccpy(char *s, char *sc, int n)

   {  /* vsc_strccpy */

   for ( ; n > 0 && *sc; n--)
      {
      if (isspace((int)*sc))
         {
         break;
         }
      else
         {
         *s++ = *sc++;
         }
      }

   *s = '\0';

   }  /* vsc_strccpy */


/***
*    STRCPYC
*   ---------
* Copy characters from [s] into [sc] until a NULL terminator or [n] characters
* are copied.  [sc] will be blank filled, NOT NULL terminated.
***/
void strcpyc(char *sc, char *s, int n)

   {  /* strcpyc */

   for ( ; n>0 && *s; n--)
      {
      *sc++ = *s++;
      }
   for ( ; n>0; n--)
      {
      *sc++ = ' ';
      }

   }  /* strcpyc */


/***
*    STRNCPYC
*   ----------
* Copy characters from [sc] starting at [n] into [s].
* [s] will be NULL terminated.
***/
void strncpyc(char *s, char *sc, int n)

   {  /* strncpyc */

   /* variables */
   int  i;
   BOOL finished;

   i = 0;
   finished = FALSE;
   while (!finished)
      {
      if (!sc[n] || (n > MAXSTR-1) || (i > MAXSTR-1))
         {
         finished = TRUE;
         }
      else
         {
         s[i] = sc[n];
         i++;
         n++;
         }
      }

   s[i] = '\0';

   }  /* strncpyc */


/***
*    STRLOC
*   --------
* Search string [t] starting at index [ti] for search string [s].
* The function will return the starting index [s] in [t] if found;
* otherwise a (-1) will be returned.
***/
int strloc(char *t, int ti, char *s)

   {   /* strloc */

   /* variables */
   BOOL found;
   int  i;
   int  limit;
   int  sii, tii;
   int  slen, tlen;

   i = 0;
   tlen = strlen(t);
   slen = strlen(s);

   /* assume we can't find S in T */
   found = FALSE;

   /* is search string's length in range */
   if (slen > MAXSTR)
      {
      (void) fprintf(stderr, "strloc: Subscript range error:\n");
      (void) fprintf(stderr,
         "   The search string's len %d is > MAXSTR %d\n", slen, MAXSTR);
      return(-1);
      }

   /* is t's length in range? */
   else if (tlen > MAXSTR)
      {
      (void) fprintf(stderr, "strloc: Subscript range error:\n");
      (void) fprintf(stderr,
         "   The string to be searched (%d) is > MAXSTR %d\n", tlen, MAXSTR);
      return(-1);
      }

   /* is T's index within the length of T ??? */
   else if (ti > tlen)
      {
      (void) fprintf(stderr, "strloc: Subscript range error:\n");
      (void) fprintf(stderr,
         "   The index %d, into the search string is > than it's length %d.\n",
               ti, tlen);
      return(-1);
      }

   /*
     are there enough characters in T starting at TI to match the chars
     in S ???
   */
   else if ((tlen - (ti - 1)) >= (slen))
      {
      i = ti;
      limit = tlen - slen + 1;
      while ((i <= limit) && (! found))
         {
         if (s[0] == t[i])
            {
            sii = 1;
            tii = i + 1;
            while ((sii > 0) && (sii < slen))
               {
               if (s[sii] != t[tii])
                  {
                  sii = 0;
                  }
               else
                  {
                  sii = sii + 1;
                  tii = tii + 1;
                  }
               }

            if (sii >= slen)
               {
               found = TRUE;
               }

            }   /* if (s[1] == m[i]) */

         i = i + 1;
         }

      if (! found)
         {
         i = 0;  /* will be -1 at return */
         }
      }

   return(i-1);  /* reference from 0 */

   }   /* strloc */


/***
*    NTBSCPY
*   ---------
* Copy [s] into [r] without trailing blanks.
***/
void ntbscpy(char *r, char *s)

   {  /* ntbscpy */

   /* variables */
   char *p;

   p = s;
   while (*s != '\0')
      {
      if (! isspace((int)*s))
         {
         while (p <= s)
            {
            *r++ = *p++;
            }
         }
      s++;
      }

   *r = '\0';

   }  /* ntbscpy */


/***
*    TRIMTB
*   --------
* Trim trailing blanks from s.
***/
void trimtb(char *s)

   {  /* trimtb */

   /* variables */
   char *p;

   p = s;
   while (*s != '\0')
      {
      if (! isspace((int)*s))
         {
         p = s;
         }
      s++;
      }

   *(++p) = '\0';

   }  /* trimtb */


/***
*    TRIMLB
*   --------
* Trim leading blanks from s.
***/
void trimlb(char *s)

   {  /* trimlb */

   /* variables */
   char *p;

   p = s;
   while (isspace ((int)*p))
      p++;
   (void) strcpy (s, p);

   }  /* trimlb */


/***
*    GETTOKEN
*   ----------
* Copy the next token in [s] into [tok], return the number of characters
* accessed to obtain [tok] (0 indicates no token).  Use [delim] to supply
* a list of delimiters.  This function will skip leading delimiters before
* it starts coping [tok].
***/
int gettoken(char *tok, char *s, char *delim)

   {  /* gettoken */

   /* variables */
   int accessed;

   (void) strcpy(tok, "");
   accessed = 0;
   while ((*s != '\0') && (strchr(delim, *s)))
      {
      accessed++;
      s++;
      }
   while ((*s != '\0') && (strchr(delim, *s) == NULL))
      {
      accessed++;
      *tok++ = *s++;
      }

   *tok = '\0';
   return(accessed);

   }  /* gettoken */


/***
*    STRUPPER
*   ----------
* Convert all alpha characters in [s] to uppercase.
* [S] is assumed to be a NULL terminated string.
***/
void strupper(char *s)

   {  /* strupper */

     /* variables */
     while (*s) {
       if (islower((int)*s))
         *s = toupper(*s);
       s++;
     }
   }  /* strupper */

/***
*    STRLOWER
*   ----------
* Convert all alpha characters in [s] to lowercase.
* [S] is assumed to be a NULL terminated string.
***/
void strlower(char *s)

   {  /* strlower */

     /* variables */
     while (*s) {
       if (isupper((int)*s))
         *s = tolower(*s);
       s++;
     }
   }  /* strlower */


/***
*    STRSUB
*   ---------
* Substitute all occurences of [c] in [s] with [sc].
***/
void strsub(char *s, int c, int sc)

   {   /* strsub */

   /* variables */
   int i;
   int len;

   len = strlen(s);
   i = 0;

   while (i <= len)
      {
      if (s[i] == c)
         {
         s[i] = sc;
         }
      i = i + 1;
      }

   }   /* strsub */


/***
*    STRREV
*   --------
* Reverse the order of characters in [s].
***/
void strrev(char *s)

   {   /* strrev */

   /* variables */
   int c;
   int i;
   int j;

   for (i = 0, j = strlen(s)-1; i < j; i++, j--)
      {
      c = s[i];
      s[i] = s[j];
      s[j] = c;
      }

   }   /* strrev */


/***
*    ITOA
*   ------
* Convert [num] to ascii digits in [s].
***/
void itoa(char *s, int num)

   {   /* itoa */

   /* variables */
   int   i;
   int   sign;

   /* record the sign and make num positive */
   if ((sign = num) < 0)
      {
      num = -num;
      }

   i = 0;
   /* generate digits in reverse order */
   do
      {
      s[i++] = num % 10 + '0';
      }

   /* delete it */
   while (( num /= 10) > 0);

   if (sign < 0)
      {
      s[i++] = '-';
      }

   s[i] = '\0';
   strrev(s);

   }   /* itoa */


/***
*    LTOA
*   ------
* Convert long [num] to ascii digits in [s].
***/
void ltoa_aux(char *s, long num)

   {   /* itoa */

   /* variables */
   long  i;
   long  sign;

   /* record the sign and make num positive */
   if ((sign = num) < 0)
      {
      num = -num;
      }

   i = 0;
   /* generate digits in reverse order */
   do
      {
      s[i++] = num % 10 + '0';
      }

   /* delete it */
   while (( num /= 10) > 0);

   if (sign < 0)
      {
      s[i++] = '-';
      }

   s[i] = '\0';
   strrev(s);

   }   /* ltoa */


/***************************************************************************
*
* NAME: is_number
*
* Purpose: Returns a 1 if the passed char* is a valid number, otherwise
*          returns 0. Doesn't handle exponents.
*
* from Aubreys strings.c  Mar-01-95
***************************************************************************/
int is_number(char *word)

   {
   if (!word ||
         (*word != '-' && *word != '.' && !isdigit ((int)*word)))
      return (0);
   word++;
   while (*word)
      {
      if ((*word != '.') && !(isdigit ((int)*word)))
         return (0);
      word++;
      }
   return (1);
   }


/***************************************************************************
*
* NAME: upper_case
*
* Purpose: upper cases the passed char*.
*          This duplicates the operation of strupper and should not be
*          used; eventually it should be nuked.
*
* from Aubreys strings.c  Mar-01-95
***************************************************************************/
void upper_case(char *word)

   {
   while ((*word = (islower ((int)*word)) ? toupper (*word) : *word))
      word++;
   }


/*********************************************************************
 *
 * NAME: vsc_add_extension
 *
 * Purpose:
 *    This routine will create a file name containing the extension
 *    passed.
 *
 * Inputs:
 *    FILE_NAME   -  File name with/without extension.
 *    EXTENSION   -  The extension to use.
 *
 * Outputs:
 *    FILE_NAME   -  File name with extension concatinated.
 *
 ************************************************************************/
void vsc_add_extension(char *file_name, char *extension)
{
  char temp_name[MAXSTR];
  BOOL finished;
  int  i;

  strcpy(temp_name, file_name);
  strcpy(file_name, "");

  finished = FALSE;
  i = strlen(temp_name);
  while (! finished)
    {
      if (i == 0)
        {
          finished = TRUE;
        }
      else if (temp_name[i] == '/')    /* ../ */
        {
          finished = TRUE;
          i = 0;
        }
      else if (temp_name[i] == '.')
        {
          finished = TRUE;
        }
      else
        {
          i = i - 1;
        }
    }

  if (i > 0)
    {
      vsc_strccpy(file_name, temp_name, i);
    }
  else
    {
      strcpy(file_name, temp_name);
    }

  strcat(file_name, extension);

}   /* vsc_add_extension */
