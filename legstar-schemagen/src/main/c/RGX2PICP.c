/*====================================================================*/
/*                                                                    */
/*           Copyright (C) 2006 by Fady Moussallam.                   */
/*                                                                    */
/* This program is free software; you can redistribute it and/or      */
/* modify it under the terms of the GNU Lesser General Public License */
/* as published by the Free Software Foundation; either version 2.1   */
/* of the License, or (at your option) any later version.             */
/*                                                                    */
/* This library is distributed in the hope that it will be useful,    */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of     */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU   */
/* Lesser General Public License for more details.                    */
/*                                                                    */
/* You should have received a copy of the GNU Lesser General Public   */
/* License along with this library; if not, write to the Free         */
/* Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  */
/* 02111-1307 USA                                                     */
/*                                                                    */
/*   Author      - Fady Moussallam  (fady@legsem.com)                 */
/*   Module      - REG2PICL                                           */
/*   Purpose     - Creates a regular expression from a cobol PICTURE  */
/*   Language    - C                                                  */
/*   System      - Tested on Windows XP Pro and z/OS 1.5              */
/*   History     - 08 June 2006  - Original Implementation            */
/*   Notes       - Both XML schema and ASN1 accept regular expressions*/
/*                 to further refine a type description. This module  */
/*                 takes a PICTURE clause and generates regex.        */
/*                                                                    */
/*   Limitations -                                                    */
/*                                                                    */
/*   To do       -                                                    */
/*====================================================================*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "RGX2PICS.h"
#define COMPILE_REGEX2PICTURE
#include "RGX2PICM.h"

/*-------------------------------------------------------------------*/
/* Constants                                                         */
/*-------------------------------------------------------------------*/
#define A_REGEX "[a-zA-Z ]"
#define B_REGEX "\\s"
#define E_REGEX "E"
#define G_REGEX "\\N{DBCSString}" /* Not a regular string type */
#define N_REGEX "\\N{BMPString}"
#define P_REGEX "[\\d\\.]"
#define S_REGEX ""
#define V_REGEX ""
#define X_REGEX "\\N{IA5String}"  /* Questionable for binary data */
#define Z_REGEX "[1-9\\s]"
#define NINE_REGEX "\\d"
#define ZERO_REGEX "0"
#define SLASH_REGEX "/"
#define COMMA_REGEX "[,\\s]"
#define POINT_REGEX "\\."
#define STAR_REGEX "[1-9\\*]"
#define PLUS_REGEX "[\\+\\-\\d]"
#define MINUS_REGEX "[\\+\\-\\d]"
#define DB_REGEX "DB"
#define CR_REGEX "CR"
#define CURRENCY_REGEX "[$\\d]"  /* Should not be hardcoded */

/*-------------------------------------------------------------------*/
/* Global variables                                                  */
/*-------------------------------------------------------------------*/
int debug_trace = 0;            /* 0=Trace mode                      */
char* error_msg = 0;            /* Error message                     */
char* in_picture = 0;           /* Input picture clause              */
char* out_regex;                /* Output regular expression         */
int regex_max_size = 0;         /* Maximum size of regular expression*/
enum REGEXSTYLE regex_style;    /* There are variants for regex      */

/*-------------------------------------------------------------------*/
/* Prototypes for picture to regex methods                           */
/*-------------------------------------------------------------------*/
int countChar(char* charseq, int* nCC, int* iC);
int placePattern(char* pattern, int nCC, int* oC);


/*====================================================================*/
/* This is the main conversion routine from picture to regex          */
/*====================================================================*/
int Picture2Regex(int debug_mode,
                char* picture,
                char* regex,
                int regexMaxSize,
                enum REGEXSTYLE regexStyle,
                char* msg)
{
    int iC = 0;
    int oC = 0;
    char CC = ' ';
    int nCC = 0;

    debug_trace = debug_mode;/* Get trace mode from caller            */
    error_msg = msg;         /* Get feedback message area from caller */
    in_picture = picture;    /* Input Cobol picture clause            */
    out_regex = regex;       /* Maximum size of regular expression    */
    regex_style = regexStyle; /* Get us which style of regex is needed*/

    memset(out_regex,'\0',1);
    regex_max_size = regexMaxSize; /* */

    if (debug_trace) printf("Picture2Regex\n");

    for(iC = 0;iC < (int)strlen(picture);iC++)
    {
        CC = toupper(picture[iC]);
        nCC = 0;
        if (0 != countChar(picture + iC, &nCC, &iC))
            return 8;
        switch(CC)
        {
        case 'A':if (0 != placePattern(A_REGEX, nCC, &oC))
                     return 8;
            break;
        case 'B':if (0 != placePattern(B_REGEX, nCC, &oC))
                     return 8;
            break;
        case 'E':if (0 != placePattern(E_REGEX, nCC, &oC))
                     return 8;
            break;
        case 'G':if (0 != placePattern(
                     (regex_style == asn1_regex)?G_REGEX:".",
                     nCC, &oC))
                     return 8;
            break;
        case 'N':if (0 != placePattern(
                     (regex_style == asn1_regex)?N_REGEX:".",
                     nCC, &oC))
                     return 8;
            break;
        case 'P':if (0 != placePattern(P_REGEX, nCC, &oC))
                     return 8;
            break;
        case 'S':
            break;
        case 'V': 
            break;
        case 'X':if (0 != placePattern(
                     (regex_style == asn1_regex)?X_REGEX:".",
                     nCC, &oC))
                     return 8;
            break;
        case 'Z':if (0 != placePattern(Z_REGEX, nCC, &oC))
                     return 8;
            break;
        case '9':if (0 != placePattern(NINE_REGEX, nCC, &oC))
                     return 8;
            break;
        case '0':if (0 != placePattern(ZERO_REGEX, nCC, &oC))
                     return 8;
            break;
        case ',':if (0 != placePattern(COMMA_REGEX, nCC, &oC))
                     return 8;
            break;
        case '.':if (0 != placePattern(POINT_REGEX, nCC, &oC))
                     return 8;
            break;
        case '*':if (0 != placePattern(STAR_REGEX, nCC, &oC))
                     return 8;
            break;
        case '+':if (0 != placePattern(PLUS_REGEX, nCC, &oC))
                     return 8;
            break;
        case '-':if (0 != placePattern(MINUS_REGEX, nCC, &oC))
                     return 8;
            break;
        case 'D':
            {
                if (0 != placePattern(DB_REGEX, nCC, &oC))
                     return 8;
                iC++;
                break;
            }
        case 'C':
            {
                if (0 != placePattern(CR_REGEX, nCC, &oC))
                     return 8;
                iC++;
                break;
            }
        case '$':if (0 != placePattern(CURRENCY_REGEX, nCC, &oC))
                     return 8;
            break;
        }
    }
    return 0;
}

/*====================================================================*/
/* This will count the total number of consecutive characters         */
/*====================================================================*/
int countChar(char* charseq, int* nCC, int* iC)
{
    int nC = 0;
    char CC = ' ';
    char * pdest = NULL;
    int result = 0;
    char sCount[18];
    int nCount = 0;

    *nCC = 0;

    /* Don't process empty strings */
    if (strlen(charseq) < 1)
        return 0;

    /* See how many times the first char occurs */
    CC = charseq[0];
    for(nC = 0;(nC < (int)strlen(charseq));nC++)
    {
        /* This is a case of a sequence like 9999 or XXX */
        if (CC == toupper(charseq[nC]))
            *nCC = *nCC + 1;
        else
            /* This is the case like 9(5) or X(4) */
            if ('(' == charseq[nC])
            {
                nCount = 0;
                /* Search for the closing parenthesis */
                pdest = strchr(charseq + nC, ')');
                if (NULL == pdest)
                {
                    sprintf(error_msg,
                    "Unbalanced parenthesis in picture clause%s\n");
                    return 8;
                }
                /* Calculate the size of this numeric */
                result = (int)(pdest - (charseq + nC) - 1);
                if (result < 1)
                {
                    sprintf(error_msg,
                    "Empty parenthesis in picture clause%s\n");
                    return 8;
                }
                /* Convert the string into an integer */
                strncpy(sCount, charseq + nC + 1, result);
                memset(sCount+result, '\0',1);
                nCount = atoi(sCount);
                if (nCount < 1)
                {
                    sprintf(error_msg,
                    "Invalid char within parenthesis in picture %s\n");
                    return 8;
                }
                /* Increment the char count */
                *nCC += nCount - 1;
                /* Set current char on the closing parenthesis */
                nC += result + 1;

            }
            else
            {
                /* Stay on last character analyzed */
                *iC = *iC + nC - 1;
                return 0;
            }
    }
    *iC = *iC + nC;
    return 0;

}

/*====================================================================*/
/* Given a pattern and a count creates part of the regex              */
/*====================================================================*/
int placePattern(char* pattern, int nCC, int* oC)
{
    char sOccurs[33];
    /* See if the pattern fits */
    if ((*oC + (int)strlen(pattern)) > regex_max_size)
    {
        sprintf(error_msg,
            "Not enough space in regular expression string %s\n");
            return 8;
    }
    /* Add the pattern */
    strcat(out_regex, pattern);
    *oC += (int)strlen(pattern);

    if (nCC > 1)
    {
        /* Add the number of occurence */
        if (regex_style == asn1_regex)
            sprintf(sOccurs,"#(%d)",nCC);
        else
            sprintf(sOccurs,"{%d}",nCC);

        /* See if the occurences fits */
        if ((*oC + (int)strlen(sOccurs)) > regex_max_size)
        {
            sprintf(error_msg,
                "Not enough space in regular expression string %s\n");
                return 8;
        }
        strcat(out_regex, sOccurs);
        *oC += (int)strlen(sOccurs);
    }
    return 0;
}
