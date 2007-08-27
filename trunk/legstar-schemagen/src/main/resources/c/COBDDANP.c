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
/*   Module      - COBDDANP                                           */
/*   Purpose     - PICTURE ans USAGE analysis DLL                     */
/*   Language    - C                                                  */
/*   System      - Tested on Windows XP Pro and z/OS 1.5              */
/*   History     - 08 June 2006  - Original Implementation            */
/*   Notes       - Module to analyze a COBOL PICTURE/USAGE clause     */
/*                 deriving data item type and various other          */
/*                 characteristics such as sign, digits, etc.         */
/*   Limitations -                                                    */
/*   To do       -                                                    */
/*====================================================================*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "COBDDPAS.h"
#include "COBDDANS.h"
#define COMPILE_COBDDANALYZE
#include "COBDDANM.h"

/*-------------------------------------------------------------------*/
/* Global variables                                                  */
/*-------------------------------------------------------------------*/
int debug_trace = 0;            /* 0=Trace mode                      */
char*  error_msg;                /* Error message                     */
COBOL_COMPILER_OPTIONS* in_cobol_options; /* Input options in effect */
char* in_usage;                 /* Input usage clause                */
char* in_picture;               /* Input picture clause              */
int*  in_signSeparate;          /* Input sign is separate            */
enum DATATYPE* out_DataType;    /* Output data item type             */
int* out_TotalDigits;           /* Output total number of digits     */
int* out_FractionDigits;        /* Output fractional digits          */
int* out_Sign;                  /* Output sign                       */
int* out_ByteLength;            /* Output total number of bytes      */

/*-------------------------------------------------------------------*/
/* Prototypes for picture analyzer methods                           */
/*-------------------------------------------------------------------*/
int analyzePictureClause(SYMBOL_COUNTS* pC, char* picture);
int countSymbols(char* picture, int* offset, int* nSymbol);
int assignNSymbols(char symbol, int nSymbol,SYMBOL_COUNTS* pC,
                    int bDecimalPoint);
int deriveDataType(char* usage, SYMBOL_COUNTS* pC,
                   enum DATATYPE* pDataType);
int calculateByteLength(enum DATATYPE* pDataType, SYMBOL_COUNTS* pC,
                        int* byteLength, int signSeparate);
void evaluateNumericProperties(SYMBOL_COUNTS* pC);
void traceInput();
void traceOutput();

/*====================================================================*/
/* This is the main picture analyzer routine                          */
/*====================================================================*/
int    COBDDAnalyze(
           int debug_mode,    /* 1= Debug traces in stdout         */
           COBOL_COMPILER_OPTIONS* cobol_options, /* Input options
                                                     in effect     */
           char* usage,       /* Input Cobol usage clause          */
           char* picture,     /* Input Cobol picture clause        */
           int*  signSeparate,/* Input Is sign separated. This has
                                 an influence on zoned decimal byte
                                 length.                           */
           enum DATATYPE* pDataType, /* Output Data item type 
                                 derived from picture clause       */
           int* pTotalDigits, /* Output total number of digits     */
           int* pFractionDigits, /* Output fractional digits       */
           int* pSign,        /* Output sign                       */
           int* pByteLength,  /* Total length of item in bytes     */
           char* msg)         /* Error message if any              */
{
    /* Counters per symbol type        */
    SYMBOL_COUNTS pC =
     {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; 
    int rc = 0;                    /* General purpose return code     */

    /* Save input parameters and make them global                     */
    debug_trace = debug_mode;
    error_msg = msg;
    in_cobol_options = cobol_options;
    in_usage = usage; 
    in_picture = picture; 
    in_signSeparate = signSeparate; 
    out_DataType = pDataType;
    out_TotalDigits = pTotalDigits;
    out_FractionDigits = pFractionDigits;
    out_Sign = pSign;
    out_ByteLength = pByteLength;

    if (debug_trace)  traceInput();

    /* first gather symbols from the picture clause                 */
    if (NULL != in_picture && strlen(in_picture) > 0) {
        rc = analyzePictureClause(&pC, in_picture);
        if (rc != 0) {
            if (debug_trace)
                printf("analyzePictureClause failed: %s\n", error_msg);
            return rc;
        }
    }
    
    /* Infer a data type from USAGE clause and symbols in the PICTURE
       clause                                                       */
    rc = deriveDataType(in_usage, &pC, out_DataType);
    if (rc != 0) {
        if (debug_trace)
            printf("deriveDataType failed: %s\n", error_msg);
        return rc;
    }

    /* Determine the total byte length of this data item            */
    rc = calculateByteLength(out_DataType,
                            &pC,
                            out_ByteLength,
                            *in_signSeparate);
    if (rc != 0) {
        if (debug_trace)
            printf("calculateByteLength failed: %s\n", error_msg);
        return rc;
    }

    /* Determine the numeric properties for numeric items           */
    if (*out_DataType == packed_decimal_item ||
        *out_DataType == zoned_decimal_item ||
        *out_DataType == numeric_edited_item ||
        *out_DataType == external_floating_item ||
        *out_DataType == binary_item ||
        *out_DataType == native_binary_item)
    {
        evaluateNumericProperties(&pC);
    } else {
        *out_Sign = FALSE;
        *out_TotalDigits = 0;
        *out_FractionDigits = 0;
    }

                             
    if (debug_trace)  traceOutput();

    return 0;
}

/*====================================================================*/
/* Analyze the picture clause character by character populating       */
/* a symbol counter structure. Empty picture results in a counter     */
/* structure fill of zeroes.                                          */
/*====================================================================*/
int analyzePictureClause(SYMBOL_COUNTS* pC, char* picture)
{
    int i;
    int rc = 0;                    /* General purpose return code     */
    char symbol = ' ';             /* A symbol from PICTURE clause    */
    int nSymbol = 0;               /* Number of similar symbols       */
    int bDecimalPoint = FALSE;     /* Fractional digits indicator     */

    if (debug_trace)  printf("  analyzePictureClause...\n");

    /* The symbol P (assumed decimal scaling position) in the leftmost
       position means this is a relative number smaller than 0        */
    if (strlen(picture) > 0 && picture[0] == 'P')
        bDecimalPoint = TRUE;

    for(i=0; i < (int)strlen(picture); i++)
    {
        nSymbol = 1;
        symbol = picture[i];
        switch(symbol)
        {
            /* CR and DB need to move one additional character */
            case 'C':i++;break;
            case 'D':i++;break;
            /* Need to expand count symbol(nSymbol) style */
            case '(':
                {
                    symbol = picture[i - 1];
                    rc = countSymbols(picture, &i, &nSymbol);
                    /* Since we already counted the first occurence
                       of the symbol (the one preceding the open
                       parenthesis), we decrement the total count
                       by one */
                    nSymbol--;
                    if (rc != 0) return rc;
                    break;
                }
            /* Must take note if switching to fractional part */
            case ',':if (in_cobol_options->decimal_point_is_comma)
                        bDecimalPoint = TRUE;break;
            case '.':if (!in_cobol_options->decimal_point_is_comma)
                        bDecimalPoint = TRUE;break;
            case 'V':bDecimalPoint = TRUE; break;
        }

        /* Increment symbol counters */
        rc = assignNSymbols(symbol, nSymbol, pC, bDecimalPoint);
        if (rc != 0) return rc;
    }
    return 0;
}

/*====================================================================*/
/* Returns symbol occurences when S(nn) notation is used.             */
/* Upon entry, offset should point to the opening parenthesis         */
/* following the symbol. This routine also increments the offset to   */
/* point to the closing parenthesis upon return.                      */
/*====================================================================*/
int countSymbols(char* picture, int* offset, int* nSymbol)
{
    char symbol;   /* symbol which occurences we want to count        */
    char * pdest;  /* a pointer to the closing parenthesis            */
    int nNumLen;   /* number of chars in the (nn) sub-clause          */
    char sCount[MAX_SYMBOL_OCCURENCE]; /* inner content of the (nn)
                                          sub-clause                  */

    if (debug_trace)  printf("  countSymbols...\n");

    /* Check that we are on an opening parenthesis    */
    if (picture[*offset] != '(')
    {
        strcpy(error_msg,"Invalid position for symbol count");
        return 8;
    }
    /* There must be a preceding character    */
    if (*offset == 0)
    {
        strcpy(error_msg,"PIC starting with parenthesis");
       return 8;
    }
    /* Preceding character gives the symbol    */
    symbol = picture[*offset - 1];
    
    /* Detect numeric between parenthesis */
    pdest = strchr(picture+ *offset, ')');
    if (pdest == NULL)
    {
        strcpy(error_msg,"PIC unbalanced parenthesis");
        return 8;
    }

    /* Calculate the size of this numeric */
    nNumLen = pdest - (picture+ *offset) - 1;
    if (nNumLen < 1)
    {
        strcpy(error_msg,"PIC empty parenthesis");
        return 8;
    }

    /* Convert the string into an integer */
    strncpy(sCount, picture + *offset +1, nNumLen);
    memset(sCount+nNumLen, '\0',1);
    *nSymbol = atoi(sCount);
    if (*nSymbol < 1)
    {
        strcpy(error_msg,"PIC invalid char between parenthesis");
        return 8;
    }

    /* Adjust i to point to the last parenthesis */
    *offset += nNumLen + 1;

    return 0;
}

/*====================================================================*/
/* Assign number of symbols to the corresponding counter in the       */
/* symbol counting structure. The bDecimalPoint parameter tells if we */
/* are counting the fractional part of a numeric or not.              */
/*====================================================================*/
int assignNSymbols(char symbol,
                  int nSymbol,
                  SYMBOL_COUNTS* pC,
                  int bDecimalPoint)
{
    if (debug_trace)  printf("  assignNSymbols...\n");

   switch(symbol)
    {
    case 'A':pC->nA += nSymbol; break;
    case 'B':if (bDecimalPoint) pC->nFraB += nSymbol;
             else pC->nEntB += nSymbol; break;
    case 'E':pC->nE += nSymbol; break;
    case 'G':pC->nG += nSymbol; break;
    case 'N':pC->nN += nSymbol; break;
    case 'P':if (bDecimalPoint) pC->nFraP += nSymbol;
             else pC->nEntP += nSymbol; break;
    case 'S':pC->nS += nSymbol; break;
    case 'V':pC->nV += nSymbol; break;
    case 'X':pC->nX += nSymbol; break;
    case '/':pC->nSlash += nSymbol; break;
    case 'C':pC->nCR += nSymbol; break;
    case 'D':pC->nDB += nSymbol; break;
    case ',':pC->nComma += nSymbol; break;
    case '.':pC->nPoint += nSymbol; break;
    case 'Z':if (bDecimalPoint) pC->nFraZ += nSymbol;
             else pC->nEntZ += nSymbol; break;
    case '9':if (bDecimalPoint) pC->nFra9 += nSymbol;
             else pC->nEnt9 += nSymbol; break;
    case '0':if (bDecimalPoint) pC->nFra0 += nSymbol;
             else pC->nEnt0 += nSymbol; break;
    case '+':if (bDecimalPoint) pC->nFraPlus += nSymbol;
             else pC->nEntPlus += nSymbol; break;
    case '-':if (bDecimalPoint) pC->nFraMinus += nSymbol;
             else pC->nEntMinus += nSymbol; break;
    case '*':if (bDecimalPoint) pC->nFraAstx += nSymbol;
             else pC->nEntAstx += nSymbol;break;
    default: if(symbol == in_cobol_options->currency_sign)
                if (bDecimalPoint)
                    pC->nFraCs += nSymbol;
                else pC->nEntCs += nSymbol;
             else {
                sprintf(error_msg,
                    "Unrecognized symbol %c in picture clause",symbol);
                return 8;
             }
    }
    return 0;
}

/*====================================================================*/
/* Derive a data item type from PICTURE symbols                       */
/* This method also adjust the USAGE in case cobol is assuming some   */
/* default.                                                           */
/*====================================================================*/
int deriveDataType(char* usage,
                   SYMBOL_COUNTS* pC,
                   enum DATATYPE* pDataType)
{
    enum DATATYPE dataType = unknown;

    if (debug_trace)  printf("  deriveDataType...\n");

    /* Some item types are only determined by the usage */
    if (strlen(usage) > 0 && strcmp(usage,"DISPLAY") != 0)
    {
        if (strcmp(usage,"BINARY") == 0)
            if (in_cobol_options->trunc_bin == TRUE)
                dataType = native_binary_item;
            else
                dataType = binary_item;
        else if (strcmp(usage,"COMP-1") == 0)
                dataType = single_float_item;
        else if (strcmp(usage,"COMP-2") == 0)
                dataType = double_float_item;
        else if (strcmp(usage,"COMP-5") == 0)
                dataType = native_binary_item;
        else if (strcmp(usage,"NATIONAL") == 0)
                dataType = national_item;
        else if (strcmp(usage,"DISPLAY-1") == 0)
                dataType = dbcs_item;
        else if (strcmp(usage,"PACKED-DECIMAL") == 0)
                dataType = packed_decimal_item;
        else if (strcmp(usage,"INDEX") == 0)
                dataType = index_item;
        else if (strcmp(usage,"POINTER") == 0)
                dataType = pointer_item;
        else if (strcmp(usage,"PROCEDURE-POINTER") == 0)
                dataType = proc_pointer_item;
        else if (strcmp(usage,"FUNCTION-POINTER") == 0)
                dataType = func_pointer_item;
        else if (strcmp(usage,"OBJECT") == 0)
                dataType = object_item;
    }
    if (dataType != unknown)
    {
        *pDataType = dataType;
        return 0;
    }

    /* Usage was not enough, continue analysis using the picture clause */
    /* Symbol A and X are alphanumeric */
    if ((pC->nA + pC->nX) > 0)
    {
        /* If there are any editing chars 9  B  0  /  */
        if ((pC->nEnt9 + pC->nEntB + pC->nEnt0 + pC->nSlash) > 0)
            *pDataType = alphanumeric_edited_item;
        else
            /* Symbol A only are alphabetic */
            if (pC->nX == 0)
                *pDataType = alphabetic_item;
            else
                *pDataType = alphanumeric_item;
        /* Make sure we have a usage */
        strcpy(usage,"DISPLAY");
        return 0;
    }

    /* Symbol G is for DBCS */
    if (pC->nG > 0) 
    {
        *pDataType = dbcs_item;
        /* Make sure the usage is DISPLAY-1 for DBCS */
        strcpy(usage,"DISPLAY-1");
        return 0;
    }

    /* Symbol N is for DBCS or UTF16 */
    if (pC->nN > 0)
    {
        if (in_cobol_options->nsymbol_dbcs) {
            /* Make sure the usage is DISPLAY-1 for DBCS */
            strcpy(usage,"DISPLAY-1");
            *pDataType = dbcs_item;
        }
        else {
            /* Make sure the usage is NATIONAL */
            strcpy(usage,"NATIONAL");
            *pDataType = national_item;
        }
        return 0;
    }

    /* Symbol E is for external floating */
    if (pC->nE > 0)
    {
        *pDataType = external_floating_item;
        /* Make sure we have a usage */
        strcpy(usage,"DISPLAY");
        return 0;
    }

    /* Must be a numeric at this stage */
    /* If there are any editing chars
    B  /  Z  0  ,  .  *  +  -  CR  DB  cs */
    if ((pC->nSlash +
            pC->nEntB + pC->nFraB +
            pC->nEntZ + pC->nFraZ +
            pC->nEnt0 + pC->nFra0 +
            pC->nComma + pC->nPoint +
            pC->nEntAstx + pC->nEntAstx +
            pC->nEntPlus + pC->nFraPlus +
            pC->nEntMinus + pC->nFraMinus +
            pC->nCR + pC->nDB +
            pC->nEntCs + pC->nFraCs) > 0)
    {
        *pDataType = numeric_edited_item;
        /* Make sure we have a usage */
        strcpy(usage,"DISPLAY");
        return 0;
    }

    /* Zoned numerics have usage display  */
    if ((pC->nEnt9 + pC->nFra9 + pC->nEntP + pC->nFraP) > 0)
    {
        *pDataType = zoned_decimal_item;
        /* Make sure we have a usage */
        strcpy(usage,"DISPLAY");
        return 0;
    }

    /* This is probably a group item with no usage and no picture
       clause. Cobol assumes group items are of class alphanumeric
       so we do the same.                                           */
    *pDataType = alphanumeric_item;
    return 0;
}

/*====================================================================*/
/* Calculate the byte length of a data item                           */
/*====================================================================*/
int calculateByteLength(enum DATATYPE* pDataType,
                        SYMBOL_COUNTS* pC,
                        int* byteLength,
                        int signSeparate)
{
    if (debug_trace)  printf("  calculateByteLength...\n");

    switch(*pDataType)
    {
        case alphabetic_item:*byteLength = pC->nA;break;
        case national_item:*byteLength = 2 * pC->nN;break;
        case dbcs_item:
            *byteLength = (2 * pC->nG) + (2 * pC->nEntB) + (2 * pC->nN);
            break;
        case alphanumeric_edited_item:
            *byteLength =
            (pC->nA + pC->nX + pC->nEnt9 + pC->nEntB + pC->nEnt0
             + pC->nSlash);
            break;
        case alphanumeric_item:
            *byteLength = (pC->nA + pC->nX + pC->nEnt9);break;
        case octet_stream_item:
            *byteLength = pC->nX;break;
        case single_float_item:*byteLength = 4;break;
        case double_float_item:*byteLength = 8;break;
        case packed_decimal_item:
            *byteLength =
                floor(((pC->nEnt9 + pC->nFra9) / 2) + 1);
            break;
        case zoned_decimal_item:
            {
                *byteLength = pC->nEnt9 + pC->nFra9;
                if (signSeparate == TRUE)
                    *byteLength += 1;
                break;
            }
        case numeric_edited_item:
            *byteLength =
             ( pC->nEntB + pC->nFraB
             + pC->nEntZ + pC->nFraZ
             + pC->nEnt9 + pC->nFra9
             + pC->nEnt0 + pC->nFra0
             + pC->nComma + pC->nPoint 
             + pC->nEntMinus + pC->nFraMinus
             + pC->nEntPlus + pC->nFraPlus
             + pC->nSlash
             + pC->nEntAstx + pC->nFraAstx
             + pC->nEntCs + pC->nFraCs
             + (2 * pC->nCR) + (2* pC->nDB));
            break;
        case index_item:*byteLength = 4;break;
        case pointer_item:*byteLength = 4;break;
        case proc_pointer_item:*byteLength = 8;break;
        case func_pointer_item:*byteLength = 4;break;
        case binary_item:
        case native_binary_item:
            {
                if ((pC->nEnt9 + pC->nFra9) < 5)
                    *byteLength = 2;
                else if ((pC->nEnt9 + pC->nFra9) < 10)
                    *byteLength = 4;
                else
                    *byteLength = 8;
                break;
            }
        case external_floating_item:
            /* The mantissa always starts with a sign and the
               exponent part is always [+-]99. The only characters
               allowed are 9 . and V in the mantissa. The E character
               occupies an additional byte.*/
                *byteLength = pC->nEntPlus + pC->nFraPlus +
                         pC->nEntMinus + pC->nFraMinus +
                         pC->nEnt9 + pC->nFra9 + pC->nPoint + 1;
                break;
        case object_item:
        case unknown:
        default:
            /* This is not supposed to happen */
            strcpy(error_msg,
                    "Unknown or unsupported data item type");
            return 8;
    }

    return 0;
}

/*====================================================================*/
/* This routine determines the sign, digits and so forth              */
/*====================================================================*/
void evaluateNumericProperties(SYMBOL_COUNTS* pC)
{
    if (debug_trace)  printf("  evaluateNumericProperties...\n");

    /* Set the number of fractional digits */
    *out_FractionDigits = pC->nFra9 +
                          pC->nFraZ +
                          pC->nFra0 +
                          pC->nFraB +
                          pC->nFraAstx;
    
    /* Set the total number of digits */
    *out_TotalDigits = *out_FractionDigits +
                          pC->nEnt9 +
                          pC->nEntZ +
                          pC->nEnt0 +
                          pC->nEntB +
                          pC->nEntAstx;

    /* cs, + and -, if they occur more than once, are floating
       insertion symbols starting at the second occurence. For
       instance with a PICTURE of +,+++,999.99, the value
       -123456.789 becomes -123,456.78. This means that all
       such characters except, the first one, hold digits
       positions. */
    if (*out_DataType == numeric_edited_item) {
        if ((pC->nEntPlus + pC->nFraPlus) > 1) {
            if (pC->nEntPlus > 1) {
                *out_TotalDigits += ((pC->nEntPlus - 1)
                    + pC->nFraPlus);
                *out_FractionDigits += pC->nFraPlus;
            } else if (pC->nEntPlus == 1) {
                *out_TotalDigits += pC->nFraPlus;
                *out_FractionDigits += pC->nFraPlus;
            } else {
                *out_TotalDigits += (pC->nFraPlus - 1);
                *out_FractionDigits += (pC->nFraPlus - 1);
            }
        }
        if ((pC->nEntMinus + pC->nFraMinus) > 1) {
            if (pC->nEntMinus > 1) {
                *out_TotalDigits += ((pC->nEntMinus - 1)
                    + pC->nFraMinus);
                *out_FractionDigits += pC->nFraMinus;
            } else if (pC->nEntMinus == 1) {
                *out_TotalDigits += pC->nFraMinus;
                *out_FractionDigits += pC->nFraMinus;
            } else {
                *out_TotalDigits += (pC->nFraMinus - 1);
                *out_FractionDigits += (pC->nFraMinus - 1);
            }
        }
        if ((pC->nEntCs + pC->nFraCs) > 1) {
            if (pC->nEntCs > 1) {
                *out_TotalDigits += ((pC->nEntCs - 1) + pC->nFraCs);
                *out_FractionDigits += pC->nFraCs;
            } else if (pC->nEntCs == 1) {
                *out_TotalDigits += pC->nFraCs;
                *out_FractionDigits += pC->nFraCs;
            } else {
                *out_TotalDigits += (pC->nFraCs - 1);
                *out_FractionDigits += (pC->nFraCs - 1);
            }
        }
    }

    /* For External floating points, the total digits and fractional
       digits will be for the mantissa only. So far the exponent
       digits were also counted so we need to remove them from the
       count. */
    if (*out_DataType == external_floating_item) {
        if (pC->nFra9 > 2) {
            *out_TotalDigits-= 2;
            *out_FractionDigits -= 2;
        }
        else
        {
            *out_TotalDigits-= 2;
            *out_FractionDigits = 0;
        }

    }
    /* Set the signed attribute */
    *out_Sign = (pC->nS +
                 pC->nEntPlus +
                 pC->nEntMinus +
                 pC->nFraPlus +
                 pC->nFraMinus +
                 pC->nDB +
                 pC->nCR > 0)? TRUE : FALSE;

 
}

/*====================================================================*/
/* Produce a trace of all input parameters                            */
/*====================================================================*/
void traceInput()
{
    printf("COBDDAnalyze started with parameters:\n");
    printf(
    "  Usage=%s, Picture=%s, SignSeparate=%d\n",in_usage,
                                   in_picture,
                                   *in_signSeparate);
    printf("  Cobol options:\n");
    printf("    Include debug lines   :%s\n",
        (in_cobol_options->include_debug_lines == TRUE)?"True":"False");
    printf("    Trunc bin             :%s\n",
        (in_cobol_options->trunc_bin == TRUE)?"True":"False");
    printf("    Currency symbol       :%c\n",
        in_cobol_options->currency_sign);
    printf("    Decimal point is comma:%s\n",
        (in_cobol_options->decimal_point_is_comma == TRUE)
                                                ?"True":"False");
    printf("    Nsymbol DBCS          :%s\n",
        (in_cobol_options->nsymbol_dbcs == TRUE)?"True":"False");
    printf("    Quote                 :%s\n",
        (in_cobol_options->quote == TRUE)?"True":"False");
}

/*====================================================================*/
/* Produce a trace of all output parameters                           */
/*====================================================================*/
void traceOutput()
{
    printf("COBDDAnalyze ended returning:\n");
    printf("    DataType              :%d\n",*out_DataType);
    printf("    TotalDigits           :%d\n",*out_TotalDigits);
    printf("    FractionDigits        :%d\n",*out_FractionDigits);
    printf("    Sign                  :%d\n",*out_Sign);
    printf("    ByteLength            :%d\n",*out_ByteLength);
}

