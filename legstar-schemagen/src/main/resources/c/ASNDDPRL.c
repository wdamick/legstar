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
/*   Module      - ASNDDPRL                                           */
/*   Purpose     - Generates ASN1 from in-memory data structure       */
/*   Language    - C                                                  */
/*   System      - Tested on Windows XP Pro and z/OS 1.5              */
/*   History     - 08 June 2006  - Original Implementation            */
/*   Notes       - Takes as input a structure created by COBDDParse   */
/*                 and generates ASN1 descriptions. Some generation   */
/*                 options are customizable.                          */
/*                                                                    */
/*   Limitations -                                                    */
/*                                                                    */
/*   To do       - Add CHOICES for redefines                          */
/*                 See how IA5String matches cobol strings            */
/*                 Numeric displays should not be translated into     */
/*                  integers or reals                                 */
/*                 Bit ordering in Integers is important              */
/*                 DEFAULT could be used to store the VALUE           */
/*                 Packed-Decimal are not described correctly         */
/*====================================================================*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "COBDDPAS.h"
#include "ASNDDPRS.h"
#define COMPILE_DDASNPRODUCE
#include "ASNDDPRM.h"
#include "RGX2PICS.h"
#include "RGX2PICM.h"

/*-------------------------------------------------------------------*/
/* Constants                                                         */
/*-------------------------------------------------------------------*/
#define MAX_OUTLINE_LEN 128     /* Maximum size of output line       */

/*-------------------------------------------------------------------*/
/* Global variables                                                  */
/*-------------------------------------------------------------------*/
int debug_trace = 0;            /* 0=Trace mode                      */
char* produce_msg = 0;          /* Error message                     */
FILE* output_file;              /* Pointer to output stream          */
STREE_NODE* STreeRoot;          /* Root of the structure tree        */
ASN_PRODUCTION_OPTIONS* asn_options;   /* Options for ASN1 production*/

/*-------------------------------------------------------------------*/
/* Prototypes for ASN production methods                             */
/*-------------------------------------------------------------------*/

int             ASN_ProduceElement (STREE_NODE* node);

int             ASN_ProduceComplexType(STREE_NODE* node);

int             ASN_produceSimpleElement(COBOL_DATA_DESCRIPTION* dds,
                                    char* indent,
                                    char* xsName);
int             ASN_produceComplexElement(COBOL_DATA_DESCRIPTION* dds,
                                    char* indent,
                                    char* xsName);
int             ASN_startRecord    (char* indent,
                                    char* xsName);
int             ASN_closeComplexType(char* indent,
                                    char* xsName);
int             ASN_formatName     (COBOL_DATA_DESCRIPTION* dds,
                                    char* xsdName);
int             ASN_processFloat   (COBOL_DATA_DESCRIPTION* dds,
                                    char* indent);
int             ASN_processDecimal (COBOL_DATA_DESCRIPTION* dds,
                                    char* indent);
int             ASN_processNumericEdited (COBOL_DATA_DESCRIPTION* dds,
                                    char* indent);
int             ASN_processString  (COBOL_DATA_DESCRIPTION* dds,
                                    char* indent);
int             ASN_processValue   (COBOL_DATA_DESCRIPTION* dds,
                                    char* indent);

/*====================================================================*/
/* This is the main ASN producing routing fo a given structure tree   */
/*====================================================================*/
int AsnDDProduce(int debug_mode,
                STREE_NODE* root,
                ASN_PRODUCTION_OPTIONS* asnOptions,
                char* outFileName,
                char* produceMsg)
{
    STREE_NODE* currentNode;
    debug_trace = debug_mode;/* Get trace mode from caller            */
    produce_msg = produceMsg;/* Get feedback message area from caller */
    asn_options = asnOptions; /* Get ASN production options           */

    if (debug_trace) printf("ASN_produce\n");

    /* Check root node */
    if (!root)
    {
        sprintf(produce_msg, "invalid data structure root node\n");
        return(8);
    }
    STreeRoot = root;

    /* Prepare output file */
    if (0 == strcmp("stdout", outFileName))
        output_file = stdout;
    else
    {
        output_file = fopen(outFileName, "w");
        if( output_file == NULL )
        {
            sprintf(produce_msg, "invalid output file %s\n",
                outFileName);
            return(8);
        }
    }

    /* If required, create the schema definition entries */
    if (asn_options->asn_header_footer)
    {
        fprintf(output_file,"%s ",asn_options->module_name);
        if (strlen(asn_options->module_name_OID) > 0)
            fprintf(output_file,"{%s}\n",asn_options->module_name_OID);
        fprintf(output_file,"DEFINITIONS %s %s ::=\n",
            asn_options->tag_default,asn_options->extension_default);
        fprintf(output_file,"BEGIN\n");
    }

    /* Define some useful basic types */
    fprintf(output_file,
        "-- Definitions for common bounded integers--\n");
    fprintf(output_file,
        "   UnsignedShort ::= INTEGER (0..65535)\n");
    fprintf(output_file,
        "   Short ::= INTEGER (-32768..32767)\n");
    fprintf(output_file,
        "   UnsignedInt ::= INTEGER (0..4294967295)\n");
    fprintf(output_file,
        "   Int ::= INTEGER (-2147483648..2147483647)\n");
    fprintf(output_file,
        "   UnsignedLong ::= INTEGER (0..18446744073709551)\n");
    fprintf(output_file,
        "   Long ::= INTEGER (-9223372036854775..9223372036854775)\n");
    fprintf(output_file,
        "--  --\n");

    /* First pass: produce direct children elements */
    currentNode = STreeRoot->firstChild;
    
    /* Create an artifial root type to group all levels 01 */
    /* If we were passed a root name, use it otherwise create an
       artificial root.     */
    if (strlen(STreeRoot->dds.cobolName) > 0)
    {
        fprintf(output_file," %s ::= SEQUENCE {\n",
                STreeRoot->dds.cobolName);
    }
    else
        fprintf(output_file," RootType ::= SEQUENCE {\n");

    while(currentNode)
    {
        ASN_ProduceElement(currentNode);
        currentNode = currentNode->firstSibling;
         /* components must be comma seperated */
        if (currentNode)
            fprintf(output_file,"   ,\n");
    }

    /* Close the root type */
    fprintf(output_file," }\n");

    /* Second pass: produce direct children complex types */
    currentNode = STreeRoot->firstChild;
    while(currentNode)
    {
        ASN_ProduceComplexType(currentNode);
        currentNode = currentNode->firstSibling;
    }
    if (asn_options->asn_header_footer)
    {
        fprintf(output_file,"END\n");
    }

    fclose(output_file);
    return 0;
}

/*====================================================================*/
/* This is called on each children to produce elements                */
/*====================================================================*/
int ASN_ProduceElement(STREE_NODE* node)
{
    char indent[MAX_OUTLINE_LEN];
    char xsName[MAX_DATANAME_LEN];
    int indFactor = 3;
    
    if (debug_trace) printf("ASN_ProduceElement\n");

    /* Indent to enhance readability of the ASN1 */
    memset(indent, ' ',indFactor); 
    indent[indFactor] = '\0';

    /* Pretty the Cobol name */
    ASN_formatName(&node->dds, xsName);

    /* For record we produce a recordType element */
    if (node->firstChild)
        return ASN_produceComplexElement(&node->dds ,indent, xsName);

    /* For simple items we produce a simple element */
    return ASN_produceSimpleElement(&node->dds ,indent, xsName);

}

/*====================================================================*/
/* This is called on each children complex type                       */
/*====================================================================*/
int ASN_ProduceComplexType(STREE_NODE* node)
{
    STREE_NODE* currentNode;
    char indent[MAX_OUTLINE_LEN];
    char xsName[MAX_DATANAME_LEN];
    int indFactor = 1;
    
    if (debug_trace) printf("ASN_ProduceComplexType\n");

    /* Simple types have been already defined  */
    if (!node->firstChild)
        return 0;

    /* Indent to enhance readability of the ASN */
    memset(indent, ' ',indFactor); 
    indent[indFactor] = '\0';

    /* Pretty the Cobol name */
    ASN_formatName(&node->dds, xsName);

    /* Records are complex types  */
    ASN_startRecord(indent, xsName);

    /* First pass: produce direct children elements */
    currentNode = node->firstChild;
    while(currentNode)
    {
        ASN_ProduceElement(currentNode);
        currentNode = currentNode->firstSibling;
        /* components must be comma seperated */
        if (currentNode)
            fprintf(output_file,"%s  ,\n",indent);
    }
    
    /* Close the complex types  */
    ASN_closeComplexType(indent, xsName);

    /* Second pass: produce direct children complex types */
    currentNode = node->firstChild;
    while(currentNode)
    {
        ASN_ProduceComplexType(currentNode);
        currentNode = currentNode->firstSibling;
    }
    return 0;
}

/*====================================================================*/
/* Close an complexType definition                                    */
/*====================================================================*/
int ASN_closeComplexType(char* indent,
                   char* xsName)
{
    if (debug_trace) printf("ASN_closeComplexType\n");

    fprintf(output_file,"%s}\n",indent);
    return 0;
}

/*====================================================================*/
/* Start a complex type of type Record                                */
/*====================================================================*/
int ASN_startRecord(char* indent,
                    char* xsName)
{
    if (debug_trace) printf("ASN_startRecord\n");

    /* Types must start with uppercase */
    xsName[0] = toupper(xsName[0]);
    fprintf(output_file,
        "%s%s%s ::= SEQUENCE {\n",indent
        ,xsName
        ,asn_options->type_suffix
        );
    return 0;
}

/*====================================================================*/
/* Post an elementary item producing XML schema output                */
/*====================================================================*/
int ASN_produceSimpleElement(COBOL_DATA_DESCRIPTION* dds,
                             char* indent,
                             char* xsName)
{
    if (debug_trace) printf("ASN_produceSimpleElement\n");

    fprintf(output_file,
        "%s%s ",indent,xsName);

    /* Add occurences only if different from default */
    if ((dds->minOccurs != 1) || (dds->maxOccurs != 1))
    {
        if (strlen(dds->dependingOn) > 0)
            fprintf(output_file,
                "SEQUENCE (SIZE(%d..%d)) OF ",
                dds->minOccurs, dds->maxOccurs);
        else
            fprintf(output_file,
                "SEQUENCE (SIZE(%d)) OF ",
                dds->maxOccurs);
    }

    switch(dds->dataType)
    {
    case alphabetic_item:
    case alphanumeric_item:
    case alphanumeric_edited_item:
    case dbcs_item:
    case national_item:
        ASN_processString(dds, indent);
        break;
    case binary_item:
    case native_binary_item:
    case packed_decimal_item:
    case zoned_decimal_item:
        ASN_processDecimal(dds, indent);
        break;
    case numeric_edited_item:
        ASN_processNumericEdited(dds, indent);
        break;
    case single_float_item:
    case double_float_item:
    case external_floating_item:
        ASN_processFloat(dds, indent);
        break;
    default:
        ASN_processString(dds, indent);
    }

    /* Add annotations to preserve Cobol unique attributes */
    if (asn_options->cobol_annotation)
    {
        fprintf(output_file, "%s    -- PICTURE=%s --\n",
                indent,dds->picture);
        fprintf(output_file, "%s    -- USAGE=%s --\n",
                indent, dds->usage);
       
        if (0 < strlen(dds->dependingOn))
            fprintf(output_file,
                    "%s    -- DEPENDING ON=%s --\n",
                    indent, dds->dependingOn);
        if (0 < strlen(dds->redefines))
            fprintf(output_file,
                    "%s    -- REDEFINES=%s --\n",
                    indent, dds->redefines);

        /* Values need some manipulations   */
        if (strlen(dds->value) > 0) ASN_processValue(dds, indent);
    }

    return 0;
}

/*====================================================================*/
/* Produce the element tag in the case of a record                    */
/*====================================================================*/
int ASN_produceComplexElement(COBOL_DATA_DESCRIPTION* dds,
                             char* indent,
                             char* xsName)
{
    if (debug_trace) printf("ASN_produceComplexElement\n");

    /* element ::= ElementType */
    fprintf(output_file,
        "%s%s ",indent,xsName);

    /* Add occurences only if different from default */
    if ((dds->minOccurs != 1) || (dds->maxOccurs != 1))
    {
        if (strlen(dds->dependingOn) > 0)
            fprintf(output_file,
                "SEQUENCE (SIZE(%d..%d)) OF ",
                dds->minOccurs, dds->maxOccurs);
        else
            fprintf(output_file,
                "SEQUENCE (SIZE(%d)) OF ",
                dds->maxOccurs);
    }

    
    /* Uppercase the type name first letter */
    xsName[0] = toupper(xsName[0]);

    /* element ::= elementType */
    fprintf(output_file,"%s%s\n",
        xsName,asn_options->type_suffix);

    /* Add annotations to preserve Cobol unique attributes */
    if (asn_options->cobol_annotation)
    {
       
        if (0 < strlen(dds->dependingOn))
            fprintf(output_file,
                    "%s    -- DEPENDING ON=%s --\n",
                    indent, dds->dependingOn);
        if (0 < strlen(dds->redefines))
            fprintf(output_file,
                    "%s    -- REDEFINES=%s --\n",
                    indent, dds->redefines);

    }

    return 0;
}

/*====================================================================*/
/* Value might need some cleaning to appear in an XSD                 */
/*====================================================================*/
int ASN_processValue(COBOL_DATA_DESCRIPTION* dds, char* indent)
{
    /* We have to assume a maximum expansion due to entity resolution */
    char dest[2*MAX_VALUE_LEN];
    memset(dest, '\0',1);

    if (debug_trace) printf("ASN_processValue\n");

    strcpy(dest, dds->value );

    fprintf(output_file, "%s    -- VALUE=%s --\n",
                indent, dest);

return 0;
}

/*====================================================================*/
/* String sympleType                                                  */
/*====================================================================*/
int ASN_processString(COBOL_DATA_DESCRIPTION* dds, char* indent)
{
    if (debug_trace) printf("ASN_processString\n");

    fprintf(output_file, "IA5String (SIZE(%d))\n",dds->byteLength);
    return 0;
}

/*====================================================================*/
/* Decimal sympleType                                                 */
/*====================================================================*/
int ASN_processDecimal(COBOL_DATA_DESCRIPTION* dds, char* indent)
{
    char xsBase[30];
    char xsIncl[30];
    int i;

    if (debug_trace) printf("ASN_processDecimal\n");

    if (dds->fractionDigits == 0)
    {
        /* Integers */
        if (dds->totalDigits < 5)
            strcpy(xsBase,(dds->sign)?"Short":"UnsignedShort");
        else
        if (dds->totalDigits < 10)
            strcpy(xsBase,(dds->sign)?"Int":"UnsignedInt");
        else
            strcpy(xsBase,(dds->sign)?"Long":"UnsignedLong");

        fprintf(output_file, "%s ",xsBase);

        /* Prepare minInclusive and maxInclusive */
        i= dds->totalDigits;
        memset(xsIncl,'9',i);
        memset(xsIncl+i,'\0',1);

        if (dds->sign)
            fprintf(output_file,"(-%s..%s)\n",xsIncl,xsIncl);
        else
            fprintf(output_file,"(0..%s)\n",xsIncl);
   }
    else
    {
        /* Decimals */
        fprintf(output_file, "REAL ");

        /* Prepare minInclusive and maxInclusive */
        i= dds->totalDigits;
        memset(xsIncl,'9',i);
        memset(xsIncl+i,'\0',1);

        if (dds->sign)
            fprintf(output_file,
            "({mantissa -%s,base 10,exponent -%d}..",
            xsIncl,dds->fractionDigits);
        else
            fprintf(output_file,
            "(0..");

        fprintf(output_file,
        "{mantissa %s,base 10,exponent -%d})\n",
        xsIncl,dds->fractionDigits);
    }


    return 0;
}

/*====================================================================*/
/* Edited numeric sympleType                                          */
/*====================================================================*/
int ASN_processNumericEdited(COBOL_DATA_DESCRIPTION* dds, char* indent)
{
    char regex[MAX_REGEX_LEN];
    if (debug_trace) printf("ASN_processNumericEdited\n");

    /* Transform picture into a regular expression */
    if (0 != Picture2Regex(debug_trace,
                            dds->picture,
                            regex,
                            MAX_REGEX_LEN,
                            asn1_regex,
                            produce_msg))
        return 8;

    fprintf(output_file, "IA5String (PATTERN \"%s\")\n",regex);
    return 0;
}

/*====================================================================*/
/* Float sympleType                                                   */
/*====================================================================*/
int ASN_processFloat(COBOL_DATA_DESCRIPTION* dds, char* indent)
{
    if (debug_trace) printf("ASN_processFloat\n");

    fprintf(output_file, "REAL ");
    if ((dds->dataType == single_float_item) ||
        (dds->dataType == external_floating_item))
    {
        fprintf(output_file,
        "({mantissa -16777216,base 10,exponent 128}..");
        fprintf(output_file,
        "{mantissa 16777216,base 10,exponent 128})");
    }
    else
    {
        fprintf(output_file,
        "({mantissa -72057594037927936,base 10,exponent 128}..");
        fprintf(output_file,
        "{mantissa 72057594037927936,base 10,exponent 128})");
    }
    return 0;
}

/*====================================================================*/
/* Cobol names may look ugly in an XSD, this will pretty them up      */
/*====================================================================*/
int ASN_formatName(COBOL_DATA_DESCRIPTION* dds, char* xsName)
{
    int i = 0;
    int l = 0;
    int bf = 1;

    if (debug_trace) printf("ASN_formatName\n");

    /* Apply user rules */
    for(i=0;i < (int)strlen(dds->cobolName); i++)
    {
        if (dds->cobolName[i] == '-')
        {
            bf = 1;
            if (asn_options->replace_minus)
            {
                xsName[l] = asn_options->replace_minus_char;
                l++;
            }
            else
                if (!asn_options->remove_minus)
                {
                    xsName[l] = dds->cobolName[i];
                    l++;
                }
        }
        else
        {
            if (asn_options->uppercase_to_lower)
            {
                if (asn_options->firstchar_upper)
                {
                    if (bf)
                        xsName[l] = toupper(dds->cobolName[i]);
                    else
                        xsName[l] = tolower(dds->cobolName[i]);
                }
                else
                    xsName[l] = tolower(dds->cobolName[i]);
            }
            else
            {
                xsName[l] = dds->cobolName[i];
            }
            l++;
            bf = 0;
        }
    }

    /* Apply ASN1 rules, component names must start by lowercase */
    xsName[0] = tolower(xsName[0]);

    xsName[l] = '\0';

    return 0;
}

/*====================================================================*/
/* This module has responsibility over the asn1 production option     */
/* structure. This method allows the caller to get default values.    */
/*====================================================================*/
void  getDefaultAsnOptions(ASN_PRODUCTION_OPTIONS* asnOptions)
{
    /* Set the asn productions options  */
    asnOptions->asn_header_footer = ASN_HEADER_FOOTER;
    strcpy(asnOptions->module_name, MODULE_NAME);
    strcpy(asnOptions->module_name_OID, MODULE_NAME_OID);
    strcpy(asnOptions->tag_default, TAG_DEFAULT);
    strcpy(asnOptions->extension_default, EXTENSION_DEFAULT);
    asnOptions->replace_minus = REPLACE_MINUS;
    asnOptions->replace_minus_char = REPLACE_MINUS_CHAR;
    asnOptions->remove_minus = REMOVE_MINUS;
    asnOptions->uppercase_to_lower = UPPERCASE_TO_LOWER;
    asnOptions->firstchar_upper = FIRSTCHAR_UPPER;
    asnOptions->cobol_annotation = COBOL_ANNOTATION;
    strcpy(asnOptions->type_suffix, TYPE_SUFFIX);
    strcpy(asnOptions->array_suffix, ARRAY_SUFFIX);
    return;
}


