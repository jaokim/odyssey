#ifndef AOS4_MISSING_FUNCS_H
#define AOS4_MISSING_FUNCS_H

#ifdef __cplusplus
extern "C" {
#endif


/*****************************************************************************
    NAME */

	int strncasecmp (

/*  SYNOPSIS */
	const char * str1,
	const char * str2,
	size_t	     n)

/*  FUNCTION
	Calculate str1 - str2 ignoring case. Upto n characters are taken
	into account.
    INPUTS
	str1, str2 - Strings to compare
    RESULT
	The difference of the strings. The difference is 0, if both are
	equal, < 0 if str1 < str2 and > 0 if str1 > str2. Note that
	it may be greater then 1 or less than -1.
    NOTES
	This function is not part of a library and may thus be called
	any time.
    EXAMPLE
    BUGS
    SEE ALSO
    INTERNALS
******************************************************************************/
{
    /* 
	If n == 0, then this will never be initialized, but is used 
	later on, so we have to initialize it.
    */
    int diff = 0;

    /* No need to check *str2 since: a) str1 is equal str2 (both are 0),
	then *str1 will terminate the loop b) str1 and str2 are not equal
	(eg. *str2 is 0), then the diff part will be FALSE. I calculate
	the diff first since a) it's more probable that the first chars
	will be different and b) I don't need to initialize diff then. */
    while (n && !(diff = tolower (*str1) - tolower (*str2)) && *str1)
    {
	/* advance both strings. I do that here, since doing it in the
	    check above would mean to advance the strings once too often */
	str1 ++;
	str2 ++;

	n--;
    }

    /* Now return the difference. */
    return diff;
} /* strncasecmp */


#if OS(AMIGAOS4)

#include <config.h>
#include <ctype.h>
#include <string.h>

int strcasecmp (const char * str1, const char * str2)
{
     int diff;

     /* No need to check *str2 since: a) str1 is equal str2 (both are 0),
        then *str1 will terminate the loop b) str1 and str2 are not equal
        (eg. *str2 is 0), then the diff part will be FALSE. I calculate
        the diff first since a) it's more probable that the first chars
        will be different and b) I don't need to initialize diff then. */
     while (!(diff = tolower (*str1) - tolower (*str2)) && *str1)
     {
        /* advance both strings. I do that here, since doing it in the
            check above would mean to advance the strings once too often */
        str1 ++;
        str2 ++;
     }

     /* Now return the difference. */
     return diff;
}



#define ForeachNode(l,n) \
    for (n=(void *)(((struct List *)(l))->lh_Head); \
	((struct Node *)(n))->ln_Succ; \
	n=(void *)(((struct Node *)(n))->ln_Succ))

#define ForeachNodeSafe(l,n,n2) \
    for (n=(void *)(((struct List *)(l))->lh_Head); \
	    (n2=(void *)((struct Node *)(n))->ln_Succ); \
	    n=(void *)n2)

#endif



#ifdef __cplusplus
}
#endif

#endif

