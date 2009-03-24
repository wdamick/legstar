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
/*   Module      - COBDDPAL                                           */
/*   Purpose     - Parses a COBOL data section to in-memory structure */
/*   Language    - C                                                  */
/*   System      - Tested on Windows XP Pro and z/OS 1.5              */
/*   History     - 08 June 2006  - Original Implementation            */
/*   Notes       - Module to analyze a COBOL PICTURE/USAGE clause     */
/*                 deriving data item type and various other          */
/*                 characteristics such as sign, digits, etc.         */
/*                                                                    */
/*   Limitations - Object oriented Cobol not supported                */
/*                 Group level USAGE and VALUE are not supported      */
/*                 Pseudo-text is not supported                       */
/*                 COPY and -INCLUDE are not resolved                 */
/*                 Only delimited literals can continue on next line  */
/*                                                                    */
/*   To do       - Name conflict resolution(twice thesame COBOL name) */
/*                 Only delimited literals can continue on next line  */
/*                 Synchronized support                               */
/*                 Dates support                                      */
/*                 Private items and default values, hexBinary types  */
/*                 Level 88 and 66                                    */
/*                 Edited numerics and alpha (Test patterns and size) */
/*                 Hexadecimal literal values and values with Z'      */
/*                 DBCS and National processing                       */
/*                 Figurative constant ALL x                          */
/*                 Figurative constant using symbolic-character       */
/*                 DBCS and National processing                       */
/*====================================================================*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

#include "COBDDPAS.h"
#include "COBDDANM.h"
#define COMPILE_COBDDPARSE
#include "COBDDPAM.h"

/*-------------------------------------------------------------------*/
/* Constants                                                         */
/*-------------------------------------------------------------------*/
#define MAX_LINE_LEN 256        /* Cobol statements should normally 
                                   be < 73, but more characters might
                                   sometime follow.                  */
#define MAX_NODES 1024          /* Maximum number of imbedded levels
                                   in a cobol structure              */
#define STATEMENT_DELIMITER '.'                                   
#define MAX_CLAUSE_LEN 18       /* Largest cobol clause              */

/*-------------------------------------------------------------------*/
/* Type of cobol line                                                */
/*-------------------------------------------------------------------*/
enum current_IASTATES
{
    comment       /* comment line               */
    ,debugging    /* a debugging line           */
    ,continuation /* continues a previous value */
    ,code         /* normal code                */
};

/*-------------------------------------------------------------------*/
/* Type of cobol clause                                              */
/*-------------------------------------------------------------------*/
enum INDCLAUSE
{
    blank_when_zero_clause  /* item contains nothing but spaces when 
                               its value is zero */
    ,external_clause        /* specifies that the storage associated 
                               with a data item is associated with the 
                               run unit rather than with any particular
                               program */
    ,global_clause          /* specifies that a data-name is available
                               to every program contained within the
                               program that declares it      */
    ,justified_clause       /* overrides standard positioning rules for
                               receiving items */
    ,occurs_clause          /* specifies tables whose elements can be
                               referred to by indexing or
                               subscripting */
    ,picture_clause         /* specifies the general characteristics and
                               editing requirements of an elementary
                               item  */
    ,signTrailing_clause    /* specifies if sign is stored in the last
                               byte (the default)*/
    ,signLeading_clause     /* specifies if sign is stored in the first
                               byte */
    ,signSeparate_clause    /* specifies if sign is stored in its own
                               separate byte */
    ,redefines_clause       /* allows you to use different data
                               description entries to describe the same
                               computer storage area */
    ,depending_on_clause    /* variable size array */
    ,synchronized_clause    /* specifies the alignment of an elementary
                               item on a natural boundary in storage */
    ,usage_binary           /* specifies the format of a data item in
                               computer storage */
    ,usage_comp_1
    ,usage_comp_2
    ,usage_comp_5
    ,usage_display
    ,usage_display_1
    ,usage_index
    ,usage_national
    ,usage_packed_decimal
    ,usage_pointer
    ,usage_proc_pointer
    ,usage_func_pointer
    ,usage_object

    ,value_clause           /* specifies the initial contents of a data
                               item   */
    ,date_format_clause     /* data item is a windowed or expanded date
                               field    */
    ,not_a_clause
};

/*-------------------------------------------------------------------*/
/* Parsing line context                                              */
/*-------------------------------------------------------------------*/
enum INDPARSECONTEXT
{
    data_name        /* looking for a data name     */
    ,cobol_clause    /* looking for a cobol clause  */
};

/*-------------------------------------------------------------------*/
/* Heap used to preprocess/postprocess record items                  */
/*-------------------------------------------------------------------*/
typedef struct {
   STREE_NODE*   node;       /* Pointer to data structure node       */
} NODE_HEAP;

/*-------------------------------------------------------------------*/
/* Global variables                                                  */
/*-------------------------------------------------------------------*/
int debug_trace = 0;            /* 0=Trace mode                      */
char* parse_msg = 0;            /* Parsing message                   */
int line_count = 0;             /* Tracks lines parsed               */
FILE* input_file = NULL;        /* Pointer to input stream           */
COBOL_COMPILER_OPTIONS* cobol_options; /* Cobol compiler options in
                                      effect           */
char current_line[MAX_LINE_LEN];/* Current line of code being parsed */
char current_SNA[7];            /* Current sequence number area      */
char current_IA;                /* Current indicator area            */
char current_AAB[66];           /* Current area A and B              */
int current_pos;                /* Current position in area A and B  */
char current_tok[161];          /* Current parsed token              */
int idxHeap = -1;               /* Pointer to heap top               */
NODE_HEAP* nodeHeap;            /* Temporary LIFO node heap          */

/*-------------------------------------------------------------------*/
/* Prototypes  internal methods                                      */
/*-------------------------------------------------------------------*/

int             parseProlog        (char* inFileName,
                                    STREE_NODE* STreeRoot);

int             parseEpilog        (int rc);

int             parseStatement     ();

int             detectLevelNumber  ();

int             analyzeDataDesc    (int levelNumber,
                                    COBOL_DATA_DESCRIPTION* dds);

enum INDCLAUSE  isClause           ();

int             completeDataDesc   (COBOL_DATA_DESCRIPTION* dds);

int             setPicture         (COBOL_DATA_DESCRIPTION* dds);

int             setValue           (COBOL_DATA_DESCRIPTION* dds);

int             setOccurs          (COBOL_DATA_DESCRIPTION* dds);

int             setDepending       (COBOL_DATA_DESCRIPTION* dds);

int             setRedefines       (COBOL_DATA_DESCRIPTION* dds);

COBOL_DATA_DESCRIPTION* lookupDds  (STREE_NODE* STreeRoot,
                                    char* cobolName);

int             getNextLine        ();

int             splitLine          ();

int             lineTypeIs         ();

int             getNextToken       ();

char*           getTokenFromCode   (char *areaX,
                                    int* nTokenPos,
                                    int* nTokenLen,
                                    int* bTokenComplete);

int             extractLiteral     (char* inStr, char delim ,
                                    int* bTokenComplete);

void            traceInput         (char* inFileName);

void            traceLineType      (int lineType);

void            traceClauseType    (enum INDCLAUSE indClause);

void            traceItem          (COBOL_DATA_DESCRIPTION* dds);

/*====================================================================*/
/* Main entry point                                                   */
/*====================================================================*/
int CobDDParse(int debug_mode,
               char* inFileName,
               COBOL_COMPILER_OPTIONS* cobolOptions,
               STREE_NODE* STreeRoot,
               char* parseMsg)
{
    int rc = 0;                /* General purpose return code        */
    debug_trace = debug_mode;  /* Get trace mode from caller         */
    parse_msg = parseMsg;   /* Get feedback message area from caller */
    cobol_options = cobolOptions; /* Get cobol options               */

   /* Initialize */
    rc = parseProlog(inFileName, STreeRoot);

    while((rc == 0) && (getNextLine() == 0))
    {
        if (strlen(current_line) > 0)
        {
            splitLine();
            switch (lineTypeIs())
            {
            case comment: break;       /*ignore comments */
            case continuation:  break; /*continuation are treated
                                       in setValue */
            case debugging:
                /* Process debug lines only if requested */
                if (cobol_options->include_debug_lines == FALSE)
                    break;
            case code:
                rc = parseStatement();
                break;
            default:break;
            };
        }
    }
    /* Cleanup */
    return parseEpilog(rc);
}

/*====================================================================*/
/* Prepare environment                                                */
/*====================================================================*/
int parseProlog(char* inFileName, STREE_NODE* STreeRoot)
{
    int rc = 0;                /* General purpose return code        */
    if (debug_trace) traceInput(inFileName);

    current_pos = 0;            
    memset(current_tok,'\0',1);

    /* Prepare input file */
    if( inFileName == NULL )
    {
        sprintf(parse_msg, "Missing input file");
        return(8);
    }
    if (0 == strcmp("stdin", inFileName))
        input_file = stdin;
    else
    {
        input_file = fopen(inFileName, "r");
        if( input_file == NULL )
        {
            sprintf(parse_msg, "Invalid input file %s", inFileName);
            perror(parse_msg);
            return(8);
        }
    }

    /* Allocate the node processing heap */
    nodeHeap = malloc( MAX_NODES * sizeof(NODE_HEAP) );
    if( nodeHeap == NULL ) {
      sprintf(parse_msg, "Insufficient memory available" );
      return(8);
    }

    /* First item in heap is the root */
    idxHeap = 0;
    nodeHeap[idxHeap].node = STreeRoot;

    line_count = 0; /* No lines read yet */

    return rc;
}

/*====================================================================*/
/* Perform all processing left and cleanup                            */
/*====================================================================*/
int parseEpilog(int rc)
{
    if (nodeHeap != NULL)
        free( nodeHeap );

    if (input_file != NULL)
        fclose(input_file);

    if (debug_trace)
    {
        if (rc ==0)
            printf("Parse ended\n");
        else
            printf("Parse ended in error\n");
    }

    return rc;
}

/*====================================================================*/
/* Line of code analysis                                              */
/*====================================================================*/
int parseStatement()
{
    int levelNumber;
    STREE_NODE* node;
    int rc = 0;                    /* General purpose return code     */
    char analysis_msg[256];        /* returned from item analysis     */

    current_pos = 0;            
    memset(current_tok,'\0',1);

    if (debug_trace) printf("parseStatement\n");

    /* See if ther is a level number (data description)  */
    levelNumber = detectLevelNumber();
    if ( levelNumber > 0)
    {
        /* Allocate a node for this data description */
        node = STree_Allocate();
        if (!node) return 8;

        /* Populate the data description statement */
        rc = analyzeDataDesc(levelNumber,&node->dds);
        if (rc != 0) {
            /* We need to append the line number to current error
               message */
            sprintf(analysis_msg,"Line=%d, %s",line_count,parse_msg);
            strcpy(parse_msg,analysis_msg );
            return 8;
        }

        /* Pop up all lower level nodes which cannot be fathers or
           siblings of the current node because they belong to
           a father that is deeper than current node.          */
        while ((idxHeap > 0) &&
               (levelNumber <= nodeHeap[idxHeap].node->fatherLevel))
            idxHeap--;

        /* The current item in the heap is now necessarily a father
           or a sibling.                                           */
        if (nodeHeap[idxHeap].node->dds.levelNumber < levelNumber)
        {
            /* Propagate father level on current child */
            node->fatherLevel = nodeHeap[idxHeap].node->dds.levelNumber;
            /* Parent can now be identified as a group item */
            nodeHeap[idxHeap].node->dds.itemType = group_item;
            /* Have parent point to us then add to heap */
            nodeHeap[idxHeap].node->firstChild = node;
            idxHeap++;
            nodeHeap[idxHeap].node = node;
        }
        else
        {
            /* Propagate father level on current sibling           */
            node->fatherLevel = nodeHeap[idxHeap].node->fatherLevel;
            /* Have older brother point to us then replace in heap */
            nodeHeap[idxHeap].node->firstSibling = node;
            nodeHeap[idxHeap].node = node;
        }
   }
    return 0;
}

/*====================================================================*/
/* See if there is a valid level number                               */
/*====================================================================*/
int detectLevelNumber()
{
    int levelNumber;
    int rc = 0;                    /* General purpose return code     */
    if (debug_trace) printf("detectLevelNumber\n");

    rc = getNextToken();
    if (rc != 0) return rc;

    if (strlen(current_tok) == 0) return 0;

    /* Try brute conversion */
    levelNumber = atoi(current_tok);
    if ((levelNumber > 0 && levelNumber < 50))
        return levelNumber;

    /* Level 77 should behave like 01 */
    if (levelNumber == 77)
        return 1;

    /* Ignore levels 88 and 66 */
    if ((levelNumber == 88) || (levelNumber == 66))
        return 0;

    return 0;
}

/*====================================================================*/
/* Analyze the data description Statement                             */
/* Populates a structure holding characteristics of the data item     */
/*====================================================================*/
int analyzeDataDesc(int levelNumber,
                    COBOL_DATA_DESCRIPTION* dds)
{
    enum INDCLAUSE indc;
    enum INDPARSECONTEXT parseContext;
    int rc = 0;                    /* General purpose return code     */

    if (debug_trace) printf("analyzeDataDesc\n");

    /* Set the data description structure */
    dds->levelNumber = levelNumber;
    /* By default, this is a filler until we find a data name
       Append the line count to make a unique name   */
    sprintf(dds->cobolName,"FILLER-%d",line_count);
    if (levelNumber == 88)
        dds->itemType = condition_name;
    else
    /* By default, this is an elementary item. This might change if
       we later find children.                                     */
        dds->itemType = elementary_item;

    /* First, we are looking for a data item name */
    parseContext = data_name;

    /* Since level started the tokenizer, get token following level */
    rc = getNextToken();
    while ((strlen(current_tok) > 0) && (rc == 0))
    {
        indc = isClause();
        if (debug_trace) traceClauseType(indc);
        switch(parseContext)
        {
        case data_name:
            if (indc == not_a_clause)
            {
                /* We were looking for a dataname and this is not a
                   cobol clause. Assume it is the dataname .
                   FILLERS must be made unique */
                if ((0 == strcmp(current_tok,"FILLER")) ||
                    (0 == strcmp(current_tok,"filler")))
                    sprintf(dds->cobolName,"FILLER-%d",line_count);
                else
                    strcpy(dds->cobolName, current_tok);
                rc = getNextToken();
                parseContext = cobol_clause;
                break;
            }
            else
            {
                /* We were looking for a dataname and this is a
                   cobol clause. Assume this is a filler        */
                sprintf(dds->cobolName,"FILLER-%d",line_count);
                parseContext = cobol_clause;
            }
        case cobol_clause:
            switch(indc)
            {
            case picture_clause:
                rc = setPicture(dds);
                if (rc != 0) return rc;
                rc = getNextToken();
                break;
            case value_clause:
                rc = setValue(dds);                
                if (rc != 0) return rc;
                rc = getNextToken();
                break;
            case occurs_clause:
                rc = setOccurs(dds);                
                if (rc != 0) return rc;
                break;
            case depending_on_clause:
                rc = setDepending(dds);                
                if (rc != 0) return rc;
                rc = getNextToken();
                break;
            case redefines_clause:
                rc = setRedefines(dds);                
                if (rc != 0) return rc;
                rc = getNextToken();
                break;
            case usage_binary:
                strcpy(dds->usage, "BINARY");
                rc = getNextToken();
                break;
            case usage_comp_1:
                strcpy(dds->usage, "COMP-1");
                rc = getNextToken();
                break;
            case usage_comp_2:
                strcpy(dds->usage, "COMP-2");
                rc = getNextToken();
                break;
            case usage_comp_5:
                strcpy(dds->usage, "COMP-5");
                rc = getNextToken();
                break;
            case usage_display_1:
                strcpy(dds->usage, "DISPLAY-1");
                rc = getNextToken();
                break;
            case usage_index:
                strcpy(dds->usage, "INDEX");
                rc = getNextToken();
                break;
            case usage_national:
                strcpy(dds->usage, "NATIONAL");
                rc = getNextToken();
                break;
            case usage_packed_decimal:
                strcpy(dds->usage, "PACKED-DECIMAL");
                rc = getNextToken();
                break;
            case usage_pointer:
                strcpy(dds->usage, "POINTER");
                rc = getNextToken();
                break;
            case justified_clause:
                dds->justifiedRight = TRUE;                
                rc = getNextToken();
                break;
            case signTrailing_clause:
                dds->signLeading = FALSE;                
                rc = getNextToken();
                break;
            case signLeading_clause:
                dds->signLeading = TRUE;                
                rc = getNextToken();
                break;
            case signSeparate_clause:
                dds->signSeparate = TRUE;                
                rc = getNextToken();
                break;
            case usage_proc_pointer:
                strcpy(dds->usage, "PROCEDURE-POINTER");
                rc = getNextToken();
                break;
            case usage_func_pointer:
                strcpy(dds->usage, "FUNCTION-POINTER");
                rc = getNextToken();
                break;
            case usage_object:
                sprintf(parse_msg,
                "Line %d, Object oriented Cobol not supported %s",
                            line_count,    
                            dds->cobolName);
                return 8;
            default:
                rc = getNextToken();
                break;
            }
            break;
        default: rc = getNextToken();
        }
        if (rc != 0) return rc;
    }
    if (rc != 0) return rc;

    /* Store the relative line number. This is useful if we later 
       need to reconstruct the original statement order               */
    dds->srceLine = line_count;

    /* At this stage, the statement has been completely parsed
       some information have to be derived from usage and picture     */
    rc = completeDataDesc(dds);

    return rc;
}

/*====================================================================*/
/* Determine if current_tok is the start of a cobol clause            */
/*====================================================================*/
enum INDCLAUSE isClause()
{
    int i = 0;
    char clause[MAX_CLAUSE_LEN];

    if (strlen(current_tok) >= MAX_CLAUSE_LEN)
        return not_a_clause;

    /* Uppercase the current_tok to reduce tests */
    strcpy(clause, current_tok);
    for(i=0;i < (int)strlen(clause); i++)
        clause[i] = toupper(clause[i]);

    /* Determine if this is a Cobol clause */
    if (strcmp("BLANK",clause) == 0)
        return blank_when_zero_clause;
    if (strcmp("DATE",clause) == 0)
        return date_format_clause;
    if (strcmp("EXTERNAL",clause) == 0)
        return external_clause;
    if (strcmp("GLOBAL",clause) == 0)
        return global_clause;
    if ((strcmp("JUST",clause) == 0) ||
        (strcmp("JUSTIFIED",clause) == 0))
        return justified_clause;
    if (strcmp("OCCURS",clause) == 0)
        return occurs_clause;
    if ((strcmp("PIC",clause) == 0) ||
        (strcmp("PICTURE",clause)) == 0)
        return picture_clause;
    if (strcmp("REDEFINES",clause) == 0)
        return redefines_clause;
    if (strcmp("DEPENDING",clause) == 0)
        return depending_on_clause;
    if (strcmp("LEADING",clause) == 0)
        return signLeading_clause;
    if (strcmp("LEADING",clause) == 0)
        return signLeading_clause;
    if (strcmp("TRAILING",clause) == 0)
        return signTrailing_clause;
    if (strcmp("SEPARATE",clause) == 0)
        return signSeparate_clause;
    if ((strcmp("SYNC",clause) == 0) ||
        (strcmp("SYNCHRONIZED",clause) == 0))
        return synchronized_clause;
    if (strcmp("VALUE",clause) == 0)
        return value_clause;
    /* USAGE is not a safe indicator, so ignore it */
    if (strcmp("USAGE",clause) == 0)
    {
        if (0 != getNextToken()) return not_a_clause;
        if (strlen(current_tok) == 0) return not_a_clause;

 
        /* Ignore the IS keyword  */
        if ((strcmp("IS", current_tok) == 0) ||
            (strcmp("is", current_tok) == 0))
        {
            if (0 != getNextToken()) return not_a_clause;
            if (strlen(current_tok) == 0) return not_a_clause;
        }

        if (strlen(current_tok) > MAX_CLAUSE_LEN)
            return not_a_clause;

        /* Uppercase the current_tok to reduce tests */
        strcpy(clause, current_tok);
        for(i=0;i < (int)strlen(clause); i++)
            clause[i] = toupper(clause[i]);
    }

    /* Because USAGE is not mandatory (and is often skipped) we must
       determine if this is a particular USAGE type */

     if (strcmp("BINARY",clause) == 0)
        return usage_binary;
     if ((strcmp("COMP",clause) == 0) ||
         (strcmp("COMPUTATIONAL",clause) == 0))
        return usage_binary;
     if ((strcmp("COMP-1",clause) == 0) ||
         (strcmp("COMPUTATIONAL-1",clause) == 0))
        return usage_comp_1;
     if ((strcmp("COMP-2",clause) == 0) ||
         (strcmp("COMPUTATIONAL-2",clause) == 0))
        return usage_comp_2;
     if ((strcmp("COMP-3",clause) == 0) ||
         (strcmp("COMPUTATIONAL-3",clause) == 0))
        return usage_packed_decimal;
     if ((strcmp("COMP-4",clause) == 0) ||
         (strcmp("COMPUTATIONAL-4",clause) == 0))
        return usage_binary;
     if ((strcmp("COMP-5",clause) == 0) ||
         (strcmp("COMPUTATIONAL-5",clause) == 0))
        return usage_comp_5;
     if (strcmp("DISPLAY",clause) == 0)
        return usage_display;
     if (strcmp("DISPLAY-1",clause) == 0)
        return usage_display_1;
     if (strcmp("INDEX",clause) == 0)
        return usage_index;
     if (strcmp("NATIONAL",clause) == 0)
        return usage_national;
     if (strcmp("PACKED-DECIMAL",clause) == 0)
        return usage_packed_decimal;
     if (strcmp("POINTER",clause) == 0)
        return usage_pointer;
     if (strcmp("PROCEDURE-POINTER",clause) == 0)
        return usage_proc_pointer;
     if (strcmp("FUNCTION-POINTER",clause) == 0)
        return usage_func_pointer;
     if (strcmp("OBJECT",clause) == 0)
        return usage_object;
     return not_a_clause;
}

/*====================================================================*/
/* Derive more information from clauses parsed                        */
/*====================================================================*/
int completeDataDesc(COBOL_DATA_DESCRIPTION* dds)
{
    int rc = 0;                    /* General purpose return code     */
    if (debug_trace) printf("completeDataDesc\n");

	/* Call the specialized DLL that derive data type and other
       characteristics                                                */
    rc = COBDDAnalyze(
           debug_trace,          /* 1= Debug traces in stdout         */
           cobol_options,        /* Input options in effect           */
           dds->usage,           /* Input Cobol usage clause          */
           dds->picture,         /* Input Cobol picture clause        */
           &dds->signSeparate,   /* Input Is sign separated. This has
                                  an influence on zoned decimal byte
                                   length.                            */
           &dds->dataType,       /* Output Data item type 
                                  derived from picture clause         */
           &dds->totalDigits,    /* Output total number of digits     */
           &dds->fractionDigits, /* Output fractional digits          */
           &dds->sign,           /* Output sign                       */
           &dds->byteLength,     /* Total length of item in bytes     */
           parse_msg);           /* Error message if any              */

    /* Set minOccurs for arrays if it wasn't parsed from the  code    */
    if ((dds->minOccurs == 0) && (dds->maxOccurs > 0)) {
        if (strlen(dds->dependingOn) == 0)
            dds->minOccurs = dds->maxOccurs;
    }
    return rc;
}

/*====================================================================*/
/* Processing a cobol PICTURE clause                                  */
/*====================================================================*/
int setPicture(COBOL_DATA_DESCRIPTION* dds)
{
    int rc = 0;                    /* General purpose return code     */
    int i = 0;
    if (debug_trace) printf("setPicture\n");

    /* See what kind of PICTURE is that  */
    if (0 != getNextToken()) return rc;
    if (0 == strlen(current_tok))
    {
        sprintf(parse_msg, "Line %d, Nothing follows PIC for %s",
                    line_count,    
                    dds->cobolName);
        return 8;
    }

    /* Ignore the IS keyword  */
    if ((strcmp("IS", current_tok) == 0) ||
        (strcmp("is", current_tok) == 0))
    {
        if (0 != getNextToken()) return rc;
        if (0 == strlen(current_tok))
        {
            sprintf(parse_msg,
                "Line %d, Nothing follows PIC IS for %s",
                    line_count,    
                    dds->cobolName);
            return 8;
        }
    }
    /* Store uppercase version of PIC to simplify analysis  */
    for(i=0;i < (int)strlen(current_tok); i++)
        current_tok[i] = toupper(current_tok[i]);
    strcpy(dds->picture, current_tok);
    return 0;
}

/*====================================================================*/
/* Processing a cobol VALUE clause                                    */
/*====================================================================*/
int setValue(COBOL_DATA_DESCRIPTION* dds)
{
    int rc = 0;                    /* General purpose return code     */
    if (debug_trace) printf("setValue\n");

    /* Values have different formats  */

    if (dds->itemType == elementary_item)
    {
        if (0 != getNextToken()) return rc;
        if (0 == strlen(current_tok))
        {
            sprintf(parse_msg,
                "Line %d, Nothing follows VALUE for %s",
                    line_count,
                    dds->cobolName);
            return 8;
        }

        /* Ignore the IS keyword  */
        if ((strcmp("IS", current_tok) == 0) ||
            (strcmp("is", current_tok) == 0))
        {
            if (0 != getNextToken()) return rc;
            if (0 == strlen(current_tok))
            {
                sprintf(parse_msg,
                    "Line %d, Nothing follows VALUE IS for %s",
                        line_count,
                        dds->cobolName);
                return 8;
            }
        }
		/* Quote has a different meaning depending on the compiler 
		   options. Here we explicit the content. */
		if ((strcmp(current_tok, "QUOTE") == 0) ||
			(strcmp(current_tok, "QUOTES") == 0) ||
			(strcmp(current_tok, "quote") == 0) ||
			(strcmp(current_tok, "quotes") == 0))
		{
			if (cobol_options->quote == TRUE)
				strcpy(dds->value, "QUOTE");
			else
				strcpy(dds->value,"APOST");
		} else {
			strcpy(dds->value, current_tok);
		}

    }

    return 0;
}

/*====================================================================*/
/* Processing a cobol OCCURS clause                                  */
/*====================================================================*/
int setOccurs(COBOL_DATA_DESCRIPTION* dds)
{
    char occurs_dd[MAX_DATANAME_LEN];
    int minOccurs = 0;
    int maxOccurs = 0;

    int rc = 0;                    /* General purpose return code     */
    if (debug_trace) printf("setOccurs\n");

    /* Following the OCCURS there must be a numeric value  */
    if (0 != getNextToken()) return rc;
    if (0 == strlen(current_tok))
    {
        sprintf(parse_msg, "Line %d, Nothing follows OCCURS for %s",
                    line_count,    
                    dds->cobolName);
        return 8;
    }
    strcpy(occurs_dd, current_tok);

    if (0 != getNextToken()) return rc;
    if (0 != strlen(current_tok))
    {
        if ((strcmp("TO", current_tok) == 0) ||
            (strcmp("to", current_tok) == 0))
        {
            /* At this stage, the first ddname is the lower bound */
            minOccurs = atoi(occurs_dd);
            /* Get us the upperbound      */
            if (0 != getNextToken()) return rc;
            if (0 != strlen(current_tok))
                maxOccurs = atoi(current_tok);
            else
            {
                sprintf(parse_msg,
                    "Line %d, Nothing follows OCCURS TO for %s",
                            line_count,    
                            dds->cobolName);
                return 8;
            }
            /* Move one token ahead to be positioned like other cases */
            rc = getNextToken();
        }
        else
        {
            /* No TO means lower bound was not specified */
            maxOccurs = atoi(occurs_dd);
            /* No need to move ahead we are already on the next token */
        }
    }
    else
    {
        /* No TO means lower bound was not specified */
        maxOccurs = atoi(occurs_dd);
        /* Move one token ahead to be positioned like other cases */
        rc = getNextToken();
    }

    dds->minOccurs = minOccurs;
    dds->maxOccurs = maxOccurs;

    return rc;
}

/*====================================================================*/
/* Processing a cobol DEPENDING ON clause                             */
/*====================================================================*/
int setDepending(COBOL_DATA_DESCRIPTION* dds)
{
    COBOL_DATA_DESCRIPTION* odoObjectNode = NULL; /* Object we depend
                                                     on               */
    int rc = 0;                    /* General purpose return code     */
    if (debug_trace) printf("setDepending\n");

    /* Get the variable name we depend on  */
    if (0 != getNextToken()) return rc;
    if (0 == strlen(current_tok))
    {
        sprintf(parse_msg,
            "Line %d, Nothing follows DEPENDING for %s",
                line_count,
                dds->cobolName);
        return 8;
    }

    /* Ignore the ON keyword  */
    if ((strcmp("ON", current_tok) == 0) ||
        (strcmp("on", current_tok) == 0))
    {
        if (0 != getNextToken()) return rc;
        if (0 == strlen(current_tok))
        {
            sprintf(parse_msg,
                "Line %d, Nothing follows DEPENDING IS for %s",
                    line_count,
                    dds->cobolName);
            return 8;
        }
    }
    strcpy(dds->dependingOn, current_tok);

    /* Lookup this ODO object (cobol data description we depend on)
       starting the search at the top of the heap which holds the 
       root item.                                                     */
    if (debug_trace) printf("lookupDds for %s\n", dds->dependingOn);
    odoObjectNode = lookupDds(nodeHeap[0].node, dds->dependingOn);
    /* Since we might be parsing a cobol fragment, we are not
       guaranteed to find the object.                                 */
    if (odoObjectNode == NULL) {
        if (debug_trace) printf("Cannot find ODO object %s\n",
                                 dds->dependingOn);
    }
    else {
        /* Mark the ODO object for later referral                     */
        odoObjectNode->odoObject = TRUE;
    }

    return 0;
}

/*====================================================================*/
/* Processing a cobol REDEFINES    clause                             */
/*====================================================================*/
int setRedefines(COBOL_DATA_DESCRIPTION* dds)
{
    COBOL_DATA_DESCRIPTION* redObjectNode = NULL; /* Object redefined */
    int rc = 0;                    /* General purpose return code     */
    if (debug_trace) printf("setRedefines\n");

    /* Get the variable name we depend on  */
    if (0 != getNextToken()) return rc;
    if (0 == strlen(current_tok))
    {
        sprintf(parse_msg,
            "Line %d, Nothing follows REDEFINES for %s",
                line_count,
                dds->cobolName);
        return 8;
    }

    strcpy(dds->redefines, current_tok);

    /* Lookup the redefined object (cobol data description redefined)
       starting the search at the top of the heap which holds the 
       root item.                                                     */
    if (debug_trace) printf("lookupDds for %s\n", dds->redefines);
    redObjectNode = lookupDds(nodeHeap[0].node, dds->redefines);
    /* Since we might be parsing a cobol fragment, we are not
       guaranteed to find the object.                                 */
    if (redObjectNode == NULL) {
        if (debug_trace) printf("Cannot find redefined object %s\n",
                                 dds->redefines);
    }
    else {
        /* Mark the redefined object for later referral               */
        redObjectNode->redefined = TRUE;
    }

    return 0;
}

/*====================================================================*/
/* Utility method to search for a data description corresponding to a */
/* cobol name.                                                        */
/*====================================================================*/
COBOL_DATA_DESCRIPTION* lookupDds  (STREE_NODE* node,
                                    char* cobolName)
{
    STREE_NODE* currentNode;
    COBOL_DATA_DESCRIPTION* result = NULL;
    /* Does this node corresponds to the cobol name we are searching
       for ?                                                          */
    if (strcmp(node->dds.cobolName, cobolName) == 0)
        return &node->dds;
    /* No. Search each of our children */
    currentNode = node->firstChild;
    while(currentNode && !result)
    {
        result = lookupDds(currentNode, cobolName);
        currentNode = currentNode->firstSibling;
    }
    return result;
}


/*====================================================================*/
/* Get a line of code delimited by a carriage return/line feed        */
/* returns a pointer to line or 8 if no crlf is found                 */
/*====================================================================*/
int getNextLine()
{
    if (debug_trace) printf("getNextLine\n");

    if (NULL == fgets(current_line ,MAX_LINE_LEN ,input_file))
        return 8;
    else
    {
        /* Replace crlf */
        if ((strlen(current_line) > 0) 
            && (current_line[strlen(current_line) - 1] == '\n')) {
            current_line[strlen(current_line) - 1] = '\0';
        }
        /* When the cobol file is coming from windows but is
         * processed under *nix, the 'n' applies for the line
         * feed only and there is an annoying carriage return left */
        if ((strlen(current_line) > 0)  
            && (current_line[strlen(current_line) - 1] == '\r')) {
            current_line[strlen(current_line) - 1] = '\0';
        }
        line_count += 1;
        return 0;
    }
}
/*====================================================================*/
/* Split a cobol line into 4 areas                                    */
/*====================================================================*/
int splitLine()
{
    int la;

    if (debug_trace) printf("splitLine:");

    /* Initialize */
    strcpy(current_SNA, "");
    current_IA = ' ';
    strcpy(current_AAB, "");

    /* Since splitline recreates areaAB, current_pos must be 0 */
    current_pos = 0;

    /* Get the sequence number area */
    if (strlen(current_line) < 6)
        return 0;
    memcpy(current_SNA,current_line,6);
    memset(current_SNA+6,'\0',1);

    /* Get the indicator area */
    if (strlen(current_line) < 7)
        return 0;
    current_IA = current_line[6];

    /* Get the area A and B */
    if (strlen(current_line) < 8)
        return 0;
    la = (strlen(current_line) < 72)? (strlen(current_line) - 7) : 65;
    memcpy(current_AAB,current_line+7,la);
    memset(current_AAB+la,'\0',1);
    if (debug_trace)
    {
        printf("seqNum=%s,ind=%c,AB=%s,lAB=%d\n", 
            current_SNA,current_IA,current_AAB,strlen(current_AAB));
        traceLineType(lineTypeIs());
    }

    return 0;
}
/*====================================================================*/
/* Analyze the indicator area to detect line type                     */
/*====================================================================*/
int lineTypeIs()
{
    switch(current_IA)
    {
        case '*':return comment; break;
        case 'D':return debugging; break;
        case 'd':return debugging; break;
        case '-':return continuation; break;
        default: return code;
    }
}

/*====================================================================*/
/* Get the next token from a file until either a token is found       */
/* or the statement is complete or we are at end of file              */
/* For literals spanning multiple lines, this concatenates the values */
/* from each line                                                     */
/* a return code of zero is ok, otherwise there is a problem          */
/*====================================================================*/
int getNextToken()
{
    char seps[]   = " .,;\t\n";
    char* pdest = NULL;
    char* areaX = NULL;
    int nTokenPos = 0;
    int nTokenLen = 0;
    int bTokenComplete = 0;
    int bExpectContinuation = 0;
    int stmtEnd = 0;
    memset(current_tok,'\0',1);

    while(!bTokenComplete)
    {
        areaX = current_AAB + current_pos;
        switch (lineTypeIs())
        {
        case comment: break; /*ignore comments */
        case continuation:
            if (bExpectContinuation)
            {
                /* This should be the continuation of a previous token
                search for starting delimiter (must be there) */
                nTokenPos = strcspn(areaX, "\'\"");
                /* On a continuation line there must be a delimiter */
                if (nTokenPos >= (int)strlen(areaX))
                {
                    sprintf(parse_msg,
                    "Line %d, Continuation line without delimiters",
                            line_count);
                    return 8;
                }
                
                /* Skip initial delimiter */
                nTokenPos += 1;

                /* Delimited literals might end on same line */
                pdest = strchr(areaX+nTokenPos,areaX[nTokenPos - 1]);
                if (pdest)
                {
                    nTokenLen = pdest - (areaX+nTokenPos) +1;
                    /* Skip final delimiter */
                    strncat(current_tok,areaX+nTokenPos,nTokenLen - 1);
                    bTokenComplete = 1;
                    bExpectContinuation = 0;
                    current_pos += nTokenPos + nTokenLen;
                    return 0;
                }
                else
                {
                    /* This literal is to be continued,
                    store the current part */
                    nTokenLen = strlen(areaX) - nTokenPos;
                    strncat(current_tok,areaX+nTokenPos,nTokenLen);
                    current_pos += nTokenPos + nTokenLen;
                    areaX += current_pos;
                }
            }
        case debugging:
        case code:
            nTokenPos = strspn(areaX, seps);
            if (nTokenPos < (int)strlen(areaX))
                pdest = getTokenFromCode(areaX,
                                        &nTokenPos,
                                        &nTokenLen,
                                        &bTokenComplete);
            else
                pdest = NULL;
            if (pdest != NULL)
            {
                if (bTokenComplete)
                    return 0;
                else
                    bExpectContinuation = 1;
            }
            break;
        default:break;
        };

        /* No more current_toks on current line.
           See if statement is finished */
        stmtEnd = (current_pos > 0)?current_pos - 1:0;
        if (NULL != (void*)(strchr(current_AAB+stmtEnd,
            STATEMENT_DELIMITER)))
            return 0;

        /* Not complete yet, get the next line of code) */
        if (0 != getNextLine())
            /* We have reached the end of the input file */
            return 0;
        else
            splitLine();

    }
    return 0;
}

/*====================================================================*/
/* Get the a token from a line of code (not continuation)             */
/* Receives an area to search from a starting position                */
/* returns NULL if no token found. Otherwise returns a pointer to 1st */
/* char of token within area and token length                         */
/* If token continues on next line, bTokenComplete is false           */
/*====================================================================*/
char* getTokenFromCode (char *areaX,
                        int* nTokenPos,
                        int* nTokenLen,
                        int* bTokenComplete)
{
    char seps[]   = " \t\n";
    int nStartLitDelim = 0;
    int nTl = 0;
    char cDelim = '\0';

    cDelim = areaX[*nTokenPos];
    /* See if this is the start of a delimited literal
        ' " X' X" Z' Z" G' G" N' N" NX' NX"        */
    switch( cDelim )
    {
    case '\"':nStartLitDelim = 1;break;
    case '\'':nStartLitDelim = 1;break;
    case 'X':
    case 'Z':
    case 'G':
        if (*nTokenPos < ((int)strlen(areaX) - 1)) {
            if ((areaX[*nTokenPos + 1] == '\'') ||
                (areaX[*nTokenPos + 1] == '\"')) {
                nStartLitDelim = 2;
            }
        }
        break;
    case 'N':
        if (*nTokenPos < ((int)strlen(areaX) - 1)) {
            if ((areaX[*nTokenPos + 1] == '\'') ||
                (areaX[*nTokenPos + 1] == '\"')) {
                nStartLitDelim = 2;
            } else {
                if (*nTokenPos < ((int)strlen(areaX) - 2))
                    if ((areaX[*nTokenPos + 2] == '\'') ||
                        (areaX[*nTokenPos + 2] == '\"')) {
                        nStartLitDelim = 3;
                    }
            }
        }
        break;
    default:break;
    }
    /* Not a delimited literal, see where it ends */
    if (!nStartLitDelim)
    {
        /* This token might extend to col 72, in theory it could 
            continue on next line. Ignore this case for now */
        nTl = strcspn(areaX+*nTokenPos, seps);
        /* Prepare next position to tokenize */
        current_pos += *nTokenPos + nTl;
        if (nTl > 0) {
            strncat(current_tok,areaX+*nTokenPos,nTl);
            /* Strip any trailing delimiters */
            if ((current_tok[nTl - 1] == ',') ||
                (current_tok[nTl - 1] == ';') ||
                (current_tok[nTl - 1] == '.')) {
                nTl--;
                current_tok[nTl] = '\0';
            }
        }
        *nTokenLen = nTl;
        *bTokenComplete = 1;
        return areaX+*nTokenPos;
    }
    
    /* From now on this is a delimited literal. Skip initial delimiter*/
    *nTokenPos += nStartLitDelim;

    /* Delimited literals might end on same line but not necessarily  */
    current_pos += extractLiteral(areaX+*nTokenPos,
                                  areaX[*nTokenPos - 1] ,
								  bTokenComplete);
    return areaX+*nTokenPos;
}

/*====================================================================*/
/* A literal is delimited by quotes or apostrophes.  An embedded      */
/* quotation mark or apostrophe is represented by a pair of           */
/* quotation marks ("") or a pair of apostrophes ('').                */
/*====================================================================*/
int extractLiteral(char* inStr, char delim , int* bTokenComplete)
{
    char* psrce = NULL;
    char* pdest = NULL;
    int newpos = 0;
    int seglen = 0;

    /* Search for the end delimiter */
    psrce = inStr;
    pdest = strchr(inStr,delim);
    while (pdest)
    {
        seglen = pdest - psrce;
        /* accumulate values in current token */
        strncat(current_tok,psrce,seglen);
        /* check if this is a pair of delimiters*/
        if ((strlen(pdest) > 1) && (pdest[0] == pdest[1]))
        {
            /* one of the delimiters is actually part of the data */
            strncat(current_tok,pdest,1);
            psrce = pdest + 2;
            pdest = strchr(psrce,delim);
            newpos += 1;
        }
        else
        {
            pdest = NULL;
            *bTokenComplete = 1;
        }
        newpos += seglen + 1;
    }

    return newpos;
}

/*====================================================================*/
/* Produce a trace of all input parameters                            */
/*====================================================================*/
void traceInput(char* inFileName)
{
    printf("COBDDParse started with parameters:\n");
    printf(
    "  inFileName=%s\n",inFileName);
    printf("  Cobol options:\n");
    printf("    Include debug lines   :%s\n",
       (cobol_options->include_debug_lines == TRUE)?"True":"False");
    printf("    Trunc bin             :%s\n",
        (cobol_options->trunc_bin == TRUE)?"True":"False");
    printf("    Currency symbol       :%c\n",
       cobol_options->currency_sign);
    printf("    Decimal point is comma:%s\n",
       (cobol_options->decimal_point_is_comma == TRUE)?"True":"False");
    printf("    Nsymbol DBCS          :%s\n",
       (cobol_options->nsymbol_dbcs == TRUE)?"True":"False");
    printf("    Quote                 :%s\n",
       (cobol_options->quote == TRUE)?"True":"False");
}

/*====================================================================*/
/* Trace line type                                                    */
/*====================================================================*/
void traceLineType(int lineType)
{
    printf("Line Type Is:");
    switch(lineType)
    {
        case comment:printf("comment\n");break;
        case debugging:printf("debugging\n");break;
        case continuation:printf("continuation\n");break;
        case code:printf("code\n");break;
    }
}

/*====================================================================*/
/* Trace clause type                                                  */
/*====================================================================*/
void traceClauseType(enum INDCLAUSE indClause)
{
    printf("Clause type:");
    switch(indClause)
    {
        case blank_when_zero_clause:printf("BLANK WHEN ZERO\n");break;
        case external_clause:printf("EXTERNAL\n");break;        
        case global_clause:printf("GLOBAL\n");break;          
        case justified_clause :printf("JUSTIFIED\n");break;      
        case occurs_clause:printf("OCCURS\n");break;          
        case picture_clause:printf("PICTURE\n");break;         
        case signTrailing_clause:printf("SIGN TRAILING\n");break;    
        case signLeading_clause:printf("SIGN LEADING\n");break;     
        case signSeparate_clause:printf("SIGN SEPARATE\n");break;    
        case redefines_clause:printf("REDEFINES\n");break;       
        case depending_on_clause:printf("DEPENDING ON\n");break;    
        case synchronized_clause:printf("SYNCHRONIZED\n");break;    
        case usage_binary:printf("BINARY\n");break;           
        case usage_comp_1:printf("COMP-1\n");break;
        case usage_comp_2:printf("COMP-2\n");break;
        case usage_comp_5:printf("COMP-5\n");break;
        case usage_display:printf("DISPLAY\n");break;
        case usage_display_1:printf("DISPLAY-1\n");break;
        case usage_index:printf("INDEX\n");break;
        case usage_national:printf("NATIONAL\n");break;
        case usage_packed_decimal:printf("PACKED DECIMAL\n");break;
        case usage_pointer:printf("POINTER\n");break;
        case usage_proc_pointer:printf("PROCEDURE POINTER\n");break;
        case usage_func_pointer:printf("FUNCTION POINTER\n");break;
        case usage_object:printf("OBJECT\n");break;
        case value_clause:printf("VALUE\n");break;           
        case date_format_clause:printf("DATE\n");break;    
        case not_a_clause:printf("NOT A CLAUSE\n");break;
        default:printf("NOT A CLAUSE\n");break;
    }
}

/*====================================================================*/
/* Produce a trace of all data item characteristics                   */
/*====================================================================*/
void traceItem(COBOL_DATA_DESCRIPTION* dds)
{
    
    char traceItemType[11];
    char traceDataType[25];
    
    switch(dds->itemType)
    {
    case group_item: strcpy(traceItemType,"group"); break;
    case elementary_item: strcpy(traceItemType,"elementary"); break;
    case condition_name: strcpy(traceItemType,"condition"); break;
    default:strcpy(traceItemType,"error");
    }
    
    switch(dds->dataType)
    {
    case alphabetic_item:
        strcpy(traceDataType,"alphabetic_item"); break;
    case national_item:
        strcpy(traceDataType,"national_item"); break;
    case dbcs_item:
        strcpy(traceDataType,"dbcs_item"); break;
    case alphanumeric_edited_item:
        strcpy(traceDataType,"alphanumeric_edited_item"); break;
    case alphanumeric_item:
        strcpy(traceDataType,"alphanumeric_item"); break;
    case octet_stream_item:
        strcpy(traceDataType,"octet_stream_item"); break;
    case single_float_item:
        strcpy(traceDataType,"single_float_item"); break;
    case double_float_item:
        strcpy(traceDataType,"double_float_item"); break;
    case packed_decimal_item:
        strcpy(traceDataType,"packed_decimal_item"); break;
    case zoned_decimal_item:
        strcpy(traceDataType,"zoned_decimal_item"); break;
    case numeric_edited_item:
        strcpy(traceDataType,"numeric_edited_item"); break;
    case index_item:
        strcpy(traceDataType,"index_item"); break;
    case pointer_item:
        strcpy(traceDataType,"pointer_item"); break;
    case proc_pointer_item:
        strcpy(traceDataType,"proc_pointer_item"); break;
    case func_pointer_item:
        strcpy(traceDataType,"func_pointer_item"); break;
    case object_item:
        strcpy(traceDataType,"object_item"); break;
    case external_floating_item:
        strcpy(traceDataType,"external_floating_item"); break;
    case binary_item:
        strcpy(traceDataType,"binary_item"); break;
    case native_binary_item:
        strcpy(traceDataType,"native_binary_item"); break;
    case unknown:
        strcpy(traceDataType,"not_elementary"); break;
    default:strcpy(traceDataType,"error");
    }

    printf(
    "level=%d name=%s itemType=%s",dds->levelNumber,
                                   dds->cobolName,
                                   traceItemType);
    printf(
    " pic=%s dataType=%s length=%d",dds->picture,
                                     traceDataType,
                                     dds->byteLength);
    printf(
    " totalDigits=%d fractionDigits=%d value=%s",dds->totalDigits,
                                     dds->fractionDigits,
                                     dds->value);
    printf(
    " sign=%d minOccurs=%d maxOccurs=%d",dds->sign,
                                     dds->minOccurs,
                                     dds->maxOccurs);

    printf(
    "  dependingOn=%s redefines=%s\n",dds->dependingOn,
                                      dds->redefines);
    if (dds->odoObject)
        printf(" is ODO Object\n");
}



/*====================================================================*/
/* Binary tree to describe the data structures                        */
/*====================================================================*/
/*====================================================================*/
/* Allocate a new node and link to its description                    */
/*====================================================================*/
STREE_NODE* STree_Allocate()
{
    STREE_NODE* node;
    if (debug_trace) printf("STree_Allocate\n");

    /* Allocate the new node */
    node = malloc( sizeof(STREE_NODE) );
    if( node == NULL ) {
      sprintf(parse_msg, "Insufficient memory available" );
      return(NULL);
    }
    /* Initialize pointers */
    node->firstChild = NULL;
    node->firstSibling = NULL;
    node->fatherLevel = 0;

    /* Initialize data structure */
    node->dds.levelNumber = 0;
    node->dds.cobolName[0] = '\0';
    node->dds.itemType = elementary_item;
    node->dds.picture[0] = '\0';
    strcpy(node->dds.usage, "");
    node->dds.dataType = unknown;
    node->dds.value[0] = '\0';
    node->dds.byteLength = 0;
    node->dds.justifiedRight = FALSE;
    node->dds.totalDigits = 0;
    node->dds.fractionDigits = 0;
    node->dds.sign = FALSE;
    node->dds.signSeparate = FALSE;
    node->dds.signLeading = FALSE;
    node->dds.minOccurs = 0;
    node->dds.maxOccurs = 0;
    node->dds.dependingOn[0] = '\0';
    node->dds.odoObject = FALSE;
    node->dds.redefines[0] = '\0';
    node->dds.redefined = FALSE;
    node->dds.srceLine = 0;

    return node;
}

/*====================================================================*/
/* Free all children of a node                                        */
/*====================================================================*/
int STree_Free(STREE_NODE* parent)
{
    if (debug_trace) printf("STree_Free\n");

    if (!parent) return 0;

    if (parent->firstChild != NULL)
        STree_Free((STREE_NODE*)parent->firstChild);

    if (parent->firstSibling != NULL)
        STree_Free((STREE_NODE*)parent->firstSibling);

    free(parent);

    return 0;
}

/*====================================================================*/
/* Used to display the content of the tree                            */
/*====================================================================*/
int STree_Print(STREE_NODE* parent)
{
    if (!parent) return 0;

    traceItem(&parent->dds);

    if (parent->firstChild != NULL)
        STree_Print((STREE_NODE*)parent->firstChild);

    if (parent->firstSibling != NULL)
        STree_Print((STREE_NODE*)parent->firstSibling);

    return 0;
}

/*====================================================================*/
/* This module has responsibility over the cobol option structure.    */
/* This method allows the caller to get default values                */
/*====================================================================*/
void  getDefaultCobolOptions(COBOL_COMPILER_OPTIONS* cobolOptions)
{
    /* Set the cobol compiler default options in effect */
    cobolOptions->currency_sign = CURRENCY_SIGN;
    cobolOptions->include_debug_lines = INCLUDE_DEBUG_LINES;
    cobolOptions->decimal_point_is_comma = DECIMAL_POINT_IS_COMMA;
    cobolOptions->nsymbol_dbcs = NSYMBOL_DBCS;
    cobolOptions->quote = COB_QUOTE;
    return;
}


