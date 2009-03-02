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
/*   Module      - XSDDDPRL                                           */
/*   Purpose     - Generates XSD from in-memory data structure        */
/*   Language    - C                                                  */
/*   System      - Tested on Windows XP Pro and z/OS 1.5              */
/*   History     - 08 June 2006  - Original Implementation            */
/*   Notes       - Takes as input a structure created by COBDDParse   */
/*                 and generates an XML Schema with Cobol and JAXB    */
/*                 annotations.                                       */
/*                                                                    */
/*   Limitations -                                                    */
/*                                                                    */
/*   To do       -                                                    */
/*====================================================================*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "COBDDPAS.h"
#include "XSDDDPRS.h"
#define COMPILE_DDXSDPRODUCE
#include "XSDDDPRM.h"
#include "RGX2PICS.h"
#include "RGX2PICM.h"

/*-------------------------------------------------------------------*/
/* Constants                                                         */
/*-------------------------------------------------------------------*/
#define MAX_OUTLINE_LEN 128     /* Maximum size of output line       */
/*-------------------------------------------------------------------*/
/* Macros                                                            */
/*-------------------------------------------------------------------*/
#define ISDIGIT(c)      ( ((c) >= '0') && ((c) <= '9') )
/*-------------------------------------------------------------------*/
/* Global variables                                                  */
/*-------------------------------------------------------------------*/
int debug_trace = 0;            /* 0=Trace mode                      */
char* produce_msg = 0;          /* Error message                     */
FILE* output_file;              /* Pointer to output stream          */
STREE_NODE* STreeRoot;          /* Root of the structure tree        */
XSD_PRODUCTION_OPTIONS* xsd_options;   /* Options for XSD production */
int choice_started = FALSE;     /* Redefines sequence in progress    */
int cobolAnnotate = FALSE;      /* Add cobol to XML annotations      */
int jaxbAnnotate = FALSE;       /* Add jaxb annotations              */

/*-------------------------------------------------------------------*/
/* Prototypes for XSD production methods                             */
/*-------------------------------------------------------------------*/

int       XSD_ProduceElement (STREE_NODE* node);

int       XSD_ProduceHeader();

int       XSD_CheckOptions();

int       XSD_ProduceComplexType(STREE_NODE* node);

int       XSD_produceSimpleElement(COBOL_DATA_DESCRIPTION* dds,
                              char* indent,
                              char* xsName);
int       XSD_produceComplexElement(COBOL_DATA_DESCRIPTION* dds,
                              char* indent,
                              char* xsName);
int       XSD_produceAnnotations(COBOL_DATA_DESCRIPTION* dds,
                             char* indent);
int       XSD_startComplexType (COBOL_DATA_DESCRIPTION* dds,
								char* indent,
                                char* xsName);
int       XSD_closeComplexType(char* indent,
                              char* xsName);
int       XSD_formatName     (char* cobolName,
                              char* xsdName);
int       XSD_entityEncode   (char* source,
                              char* dest);
int       XSD_processFloat   (COBOL_DATA_DESCRIPTION* dds,
                              char* indent);
int       XSD_processDecimal (COBOL_DATA_DESCRIPTION* dds,
                              char* indent);
int       XSD_processNumericEdited (COBOL_DATA_DESCRIPTION* dds,
                              char* indent);
int       XSD_processString  (COBOL_DATA_DESCRIPTION* dds,
                              char* indent);
int       XSD_processNational  (COBOL_DATA_DESCRIPTION* dds,
                              char* indent);
int       XSD_processOctetStream  (COBOL_DATA_DESCRIPTION* dds,
                              char* indent);
int       XSD_processValue   (COBOL_DATA_DESCRIPTION* dds,
                              char* indent);
int       XSD_nameConflict(STREE_NODE* srcNode,
						       COBOL_DATA_DESCRIPTION* dds);
int       XSD_resolveFigurative(char* inStr,
								COBOL_DATA_DESCRIPTION* dds);

/*====================================================================*/
/* This is the main XSD producing routing fo a given structure tree   */
/*====================================================================*/
int XsdDDProduce(int debug_mode,
                STREE_NODE* root,
                XSD_PRODUCTION_OPTIONS* xsdOptions,
                char* outFileName,
                char* produceMsg)
{
    int rc = 0;
    debug_trace = debug_mode;/* Get trace mode from caller            */
    produce_msg = produceMsg;/* Get feedback message area from caller */
    xsd_options = xsdOptions; /* Get XSD production options           */

    if (debug_trace) printf("XSD_produce\n");

    /* Check root node */
    if (!root)
    {
        sprintf(produce_msg, "invalid data structure root node");
        return(8);
    }
    STreeRoot = root;

    /* Validate input options */
    rc = XSD_CheckOptions();
    if (rc != 0) return rc;

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
            perror(produce_msg);
            return(8);
        }
    }

    /* If required, create the schema definition entries */
    if (xsd_options->xsd_header_footer)
    {
        rc = XSD_ProduceHeader();
        if (rc != 0) {
            if (output_file != NULL)
                fclose(output_file);
            return rc;
        }
    }

    rc = XSD_ProduceComplexType(STreeRoot);
    if (rc != 0) {
        if (output_file != NULL)
            fclose(output_file);
        return rc;
    }

    if (xsd_options->xsd_header_footer)
    {
        fprintf(output_file,"</%s:schema>\n",xsd_options->xs_prefix);
    }

    if (output_file != NULL)
        fclose(output_file);

    return 0;
}

/*====================================================================*/
/* Perform validation over the XSD generation options                 */
/*====================================================================*/
int XSD_CheckOptions()
{
    /* First check that minimum schema header data is available */
    if ((xsd_options->xs_prefix == NULL) ||
        (strlen(xsd_options->xs_prefix) == 0)) {
        sprintf(produce_msg, "Missing XML schema prefix");
        return(8);
    }
    if ((xsd_options->xs_ns == NULL) ||
        (strlen(xsd_options->xs_ns) == 0)) {
        sprintf(produce_msg, "Missing XML schema namespace");
        return(8);
    }
    if ((xsd_options->xsns_prefix == NULL) ||
        (strlen(xsd_options->xsns_prefix) == 0)) {
        sprintf(produce_msg, "Missing target XML schema prefix");
        return(8);
    }
    if ((xsd_options->xsns_ns == NULL) ||
        (strlen(xsd_options->xsns_ns) == 0)) {
        sprintf(produce_msg, "Missing target XML schema namespace");
        return(8);
    }
    /* If no cobol to XML prefix was provided, exclude cobol annotations*/
    if ((xsd_options->xscb_prefix == NULL) ||
        (strlen(xsd_options->xscb_prefix) == 0)) {
        cobolAnnotate = FALSE;
    }
    else
        cobolAnnotate = TRUE;

    /* If cobol annotations are requested, then a namespace must
       be provided */
    if (cobolAnnotate == TRUE) {
        if ((xsd_options->xscb_ns == NULL) ||
            (strlen(xsd_options->xscb_ns) == 0)) {
            sprintf(produce_msg,
                   "Missing cobol to XML schema namespace");
            return(8);
        }
    }

    /* If no JAXB prefix was provided, exclude jaxb annotations*/
    if ((xsd_options->xsjaxb_prefix == NULL) ||
        (strlen(xsd_options->xsjaxb_prefix) == 0)) {
        jaxbAnnotate = FALSE;
    }
    else
        jaxbAnnotate = TRUE;

    /* If jaxb annotations are requested, then namespace, version
       and package must be provided */
    if (jaxbAnnotate == TRUE) {
        if ((xsd_options->xsjaxb_ns == NULL) ||
            (strlen(xsd_options->xsjaxb_ns) == 0)) {
            sprintf(produce_msg,
                   "Missing JAXB schema namespace");
            return(8);
        }
        if ((xsd_options->xsjaxb_version == NULL) ||
            (strlen(xsd_options->xsjaxb_version) == 0)) {
            sprintf(produce_msg,
                   "Missing JAXB version");
            return(8);
        }
        if ((xsd_options->xsjaxb_package == NULL) ||
            (strlen(xsd_options->xsjaxb_package) == 0)) {
            sprintf(produce_msg,
                   "Missing JAXB package");
            return(8);
        }
    }

    return 0;
}

/*====================================================================*/
/* This generates the header schema tag                               */
/*====================================================================*/
int XSD_ProduceHeader()
{
    fprintf(output_file,"<?xml version=\"1.0\"?>\n");
    fprintf(output_file,"<%s:schema ",xsd_options->xs_prefix);
    fprintf(output_file,"xmlns:%s=\"%s\"\n",xsd_options->xs_prefix,
        xsd_options->xs_ns);
    fprintf(output_file,"targetNamespace=\"%s\"\n",
        xsd_options->xsns_ns);
    fprintf(output_file,"xmlns=\"%s\"\n",xsd_options->xsns_ns);
    fprintf(output_file,"xmlns:%s=\"%s\"\n",
        xsd_options->xsns_prefix,xsd_options->xsns_ns);
    if (strlen(xsd_options->xscb_prefix) > 0)
        fprintf(output_file,"xmlns:%s=\"%s\"\n",
            xsd_options->xscb_prefix,xsd_options->xscb_ns);
    if (jaxbAnnotate == TRUE) {
        fprintf(output_file,"xmlns:%s=\"%s\"\n",
            xsd_options->xsjaxb_prefix,xsd_options->xsjaxb_ns);
        fprintf(output_file,"%s:version=\"%s\"\n",
            xsd_options->xsjaxb_prefix,xsd_options->xsjaxb_version);
        if (strlen(xsd_options->xscb_prefix) > 0)
            fprintf(output_file,
                "%s:extensionBindingPrefixes=\"%s\"\n",
                xsd_options->xsjaxb_prefix,xsd_options->xscb_prefix);
    }
    fprintf(output_file,"elementFormDefault=\"qualified\">\n");

    if (jaxbAnnotate == TRUE) {
    /* Add JAXB annotation to specify the output package   */
        fprintf(output_file," <%s:annotation><%s:appinfo>\n",
            xsd_options->xs_prefix,xsd_options->xs_prefix);
        fprintf(output_file," <%s:schemaBindings>\n",
            xsd_options->xsjaxb_prefix);
        fprintf(output_file," <%s:package  name=\"%s\"/>\n",
            xsd_options->xsjaxb_prefix,xsd_options->xsjaxb_package);
        fprintf(output_file," </%s:schemaBindings>\n",
            xsd_options->xsjaxb_prefix);
        fprintf(output_file," </%s:appinfo></%s:annotation>\n",
            xsd_options->xs_prefix,xsd_options->xs_prefix);
    }

    return 0;

}

/*====================================================================*/
/* This is called on each child to produce elements                   */
/*====================================================================*/
int XSD_ProduceElement(STREE_NODE* node)
{
    char indent[MAX_OUTLINE_LEN];
    char xsName[MAX_DATANAME_LEN];
    int indFactor = 0;
    STREE_NODE* siblingNode;
    int rc = 0;
    int openingChoice = FALSE;
    
    if (debug_trace) printf("XSD_ProduceElement %s\n",
		node->dds.cobolName);

    /* Indent to enhance readability of the XSD */
    indFactor = (node->dds.levelNumber == 01 ||
                 node->dds.levelNumber == 77)?1:5;
    memset(indent, ' ',indFactor); 
    indent[indFactor] = '\0';

    /* Pretty the Cobol name */
    XSD_formatName(node->dds.cobolName, xsName);

    /* If this node is redefined by its next sibling, start a choice
        group (unless there is already one started)                   */
    if (node->firstSibling)
    {
        siblingNode = (STREE_NODE*)node->firstSibling;
        if (strlen(siblingNode->dds.redefines) > 0)
        {
            openingChoice = TRUE;
            if (choice_started == FALSE)
            {
                fprintf(output_file,"%s<%s:choice>\n",
                    indent,xsd_options->xs_prefix);
                choice_started = TRUE;
            }
        }
    }

    /* For record we produce a recordType element */
    if (node->firstChild)
        rc = XSD_produceComplexElement(&node->dds ,indent, xsName);
    else
        /* For simple items we produce a simple element */
        rc = XSD_produceSimpleElement(&node->dds ,indent, xsName);
    if (rc != 0) return rc;

    /* If a choice group has been started, check if it needs to close.
       Make sure we don't immediatly close the one we just opened.    */
    if ((openingChoice == FALSE) && (choice_started == TRUE) )
    {
        fprintf(output_file,"%s</%s:choice>\n",
            indent,xsd_options->xs_prefix);
        choice_started = FALSE;
    }

    return rc;
}

/*====================================================================*/
/* This is called on each child of complex type                       */
/*====================================================================*/
int XSD_ProduceComplexType(STREE_NODE* node)
{
    STREE_NODE* currentNode;
    char indent[MAX_OUTLINE_LEN];
    char xsName[MAX_DATANAME_LEN];
    int indFactor = 0;
    
    if (debug_trace) printf("XSD_ProduceComplexType %s\n",
		node->dds.cobolName);

    /* Make sure this node has children otherwise it should be
       treated as a simple type.                                     */
    if (!node->firstChild)
        return 0;

    /* Complex Types are flattened (no indentation)  */
    indFactor = 1;
    memset(indent, ' ',indFactor); 
    indent[indFactor] = '\0';

    /* If this complex type does not have a name, it must be the 
       root element where the client decided he did not want 
       hight level elements to appear in the final XSD.       */
    if (strlen(node->dds.cobolName) > 0)
    {
        /* Pretty the Cobol name */
        XSD_formatName(node->dds.cobolName, xsName);

        /* Create complex type opening tag  */
        XSD_startComplexType(&node->dds, indent, xsName);

        /* First pass: produce direct children elements */
        currentNode = node->firstChild;
        while(currentNode)
        {
            XSD_ProduceElement(currentNode);
            currentNode = currentNode->firstSibling;
        }
        
        /* Close the complex type tag  */
        XSD_closeComplexType(indent, xsName);
    }

    /* Second pass: produce direct children complex types */
    currentNode = node->firstChild;
    while(currentNode)
    {
		if (currentNode->firstChild)
		{
			XSD_ProduceComplexType(currentNode);
		}
        currentNode = currentNode->firstSibling;
    }
    return 0;
}

/*====================================================================*/
/* Close an complexType definition                                    */
/*====================================================================*/
int XSD_closeComplexType(char* indent,
                   char* xsName)
{
    if (debug_trace) printf("XSD_closeComplexType\n");

    fprintf(output_file,"%s   </%s:sequence>\n",
        indent,xsd_options->xs_prefix);
    fprintf(output_file,"%s</%s:complexType>\n",
        indent,xsd_options->xs_prefix);

    return 0;
}

/*====================================================================*/
/* Start a complex type of type Record                                */
/*====================================================================*/
int XSD_startComplexType(COBOL_DATA_DESCRIPTION* dds,
						 char* indent, char* xsName)
{
    if (debug_trace) printf("XSD_startComplexType\n");

    /* If there is another complex type with the same name, we need
	   to disambiguate the type name. We append the source line number
	   to the type name for that effect.*/
	if (TRUE == XSD_nameConflict(STreeRoot, dds))
	{
		fprintf(output_file,
			"%s<%s:complexType name=\"%s%d%s\">\n",
			indent,xsd_options->xs_prefix,xsName,dds->srceLine,
			xsd_options->type_suffix);
	} else {
		fprintf(output_file,
			"%s<%s:complexType name=\"%s%s\">\n",
			indent,xsd_options->xs_prefix,xsName,
			xsd_options->type_suffix);
	}
    fprintf(output_file,"%s   <%s:sequence>\n",
        indent,xsd_options->xs_prefix);

    return 0;
}

/*====================================================================*/
/* Post an elementary item producing XML schema output                */
/*====================================================================*/
int XSD_produceSimpleElement(COBOL_DATA_DESCRIPTION* dds,
                             char* indent,
                             char* xsName)
{
    if (debug_trace) printf("XSD_produceSimpleElement %s\n",
		dds->cobolName);

    /* <xsd_options->xs_prefix:element name= */
    fprintf(output_file,
        "%s<%s:element name=\"%s\"",indent,
        xsd_options->xs_prefix,xsName);

    /* Add occurences only if different from default */
    if (dds->maxOccurs > 0)
        fprintf(output_file,
        " minOccurs=\"%d\" maxOccurs=\"%d\"",
          dds->minOccurs, dds->maxOccurs);
    
    fprintf(output_file,">\n");

    /* Add annotations to preserve Cobol unique attributes */
    if (cobolAnnotate == TRUE )
        XSD_produceAnnotations(dds, indent);

    /* Simple type definition */
    fprintf(output_file, "%s  <%s:simpleType>\n",
        indent,xsd_options->xs_prefix);
    fprintf(output_file, "%s    <%s:restriction base="
          ,indent,xsd_options->xs_prefix);
    switch(dds->dataType)
    {
        case alphabetic_item:
        case alphanumeric_item:
        case alphanumeric_edited_item:
        case external_floating_item:
            XSD_processString(dds, indent);
            break;
        case binary_item:
        case native_binary_item:
        case packed_decimal_item:
        case zoned_decimal_item:
            XSD_processDecimal(dds, indent);
            break;
        case numeric_edited_item:
            XSD_processNumericEdited(dds, indent);
            break;
        case single_float_item:
        case double_float_item:
            XSD_processFloat(dds, indent);
            break;
        case national_item:
            XSD_processNational(dds, indent);
            break;
        case dbcs_item:
            XSD_processDbcs(dds, indent);
            break;
        case octet_stream_item:
        case index_item:
        case pointer_item:
        case proc_pointer_item:
        case func_pointer_item:
            XSD_processOctetStream(dds, indent);
            break;
        default:
            XSD_processString(dds, indent);
    }
    /* Cobol data items are fixed length */
    fprintf(output_file, "%s    </%s:restriction>\n",indent,
        xsd_options->xs_prefix);
    fprintf(output_file, "%s  </%s:simpleType>\n",indent,
        xsd_options->xs_prefix);
    fprintf(output_file, "%s</%s:element>\n",indent,
        xsd_options->xs_prefix);

    return 0;
}

/*====================================================================*/
/* Produce the element tag in the case of a record                    */
/*====================================================================*/
int XSD_produceComplexElement(COBOL_DATA_DESCRIPTION* dds,
                             char* indent,
                             char* xsName)
{
    if (debug_trace) printf("XSD_produceComplexElement %s\n",
		dds->cobolName);

    /* <xsd_options->xs_prefix:element name= */
    fprintf(output_file,
        "%s<%s:element name=\"%s\"",indent,
        xsd_options->xs_prefix,xsName);

    /* Add occurences only if different from default */
    if (dds->maxOccurs > 0)
        fprintf(output_file,
        " minOccurs=\"%d\" maxOccurs=\"%d\"",
          dds->minOccurs, dds->maxOccurs);
    
    /* If there is another complex type with the same name, we need
	   to disambiguate the type name. We append the source line number
	   to the type name for that effect.*/
	if (TRUE == XSD_nameConflict(STreeRoot, dds))
	{
		fprintf(output_file," type=\"%s:%s%d%s\">\n",
			xsd_options->xsns_prefix,
			xsName,dds->srceLine,xsd_options->type_suffix);
	} else {
		fprintf(output_file," type=\"%s:%s%s\">\n",
			xsd_options->xsns_prefix,xsName,xsd_options->type_suffix);
	}

    /* Add annotations to preserve Cobol unique attributes */
    if (cobolAnnotate == TRUE )
        XSD_produceAnnotations(dds, indent);

    fprintf(output_file, "%s</%s:element>\n",indent,
        xsd_options->xs_prefix);

    return 0;
}

/*====================================================================*/
/* Create XSD annotations describing the original cobol element       */
/*====================================================================*/
int XSD_produceAnnotations(COBOL_DATA_DESCRIPTION* dds,
                             char* indent)
{
 
    if (debug_trace) printf("XSD_produceSimpleAnnotations\n");

    fprintf(output_file, "%s  <%s:annotation>\n",
        indent,xsd_options->xs_prefix);
    fprintf(output_file, "%s    <%s:appinfo>\n",
        indent,xsd_options->xs_prefix);

    fprintf(output_file, "%s    <%s:cobolElement",
            indent, xsd_options->xscb_prefix);

    fprintf(output_file, " levelNumber='%02d'",dds->levelNumber);
    fprintf(output_file, " cobolName='%s'",dds->cobolName);

    /* Simple elements annotations */
    if (dds->itemType == elementary_item) {
        switch(dds->dataType)
        {
            case alphabetic_item:
                fprintf(output_file, " type='%s'","ALPHABETIC_ITEM");
                break;
            case national_item:
                fprintf(output_file, " type='%s'","NATIONAL_ITEM");
                break;
            case dbcs_item:
                fprintf(output_file, " type='%s'","DBCS_ITEM");
                break;
            case alphanumeric_edited_item:
                fprintf(output_file, " type='%s'",
                                      "ALPHANUMERIC_EDITED_ITEM");
                break;
            case alphanumeric_item:
                fprintf(output_file, " type='%s'","ALPHANUMERIC_ITEM");
                break;
            case octet_stream_item:
                fprintf(output_file, " type='%s'","OCTET_STREAM_ITEM");
                break;
            case single_float_item:
                fprintf(output_file, " type='%s'","SINGLE_FLOAT_ITEM");
                break;
            case double_float_item:
                fprintf(output_file, " type='%s'","DOUBLE_FLOAT_ITEM");
                break;
            case packed_decimal_item:
                fprintf(output_file, " type='%s'",
                                     "PACKED_DECIMAL_ITEM");
                break;
            case zoned_decimal_item:
                fprintf(output_file, " type='%s'","ZONED_DECIMAL_ITEM");
                break;
            case numeric_edited_item:
                fprintf(output_file, " type='%s'",
                                       "NUMERIC_EDITED_ITEM");
                break;
            case index_item:
                fprintf(output_file, " type='%s'","INDEX_ITEM");
                break;
            case pointer_item:
                fprintf(output_file, " type='%s'","POINTER_ITEM");
                break;
            case proc_pointer_item:
                fprintf(output_file, " type='%s'","PROC_POINTER_ITEM");
                break;
            case func_pointer_item:
                fprintf(output_file, " type='%s'","FUNC_POINTER_ITEM");
                break;
            case object_item:
                fprintf(output_file, " type='%s'","OBJECT_ITEM");
                break;
            case external_floating_item:
                fprintf(output_file, " type='%s'",
                                       "EXTERNAL_FLOATING_ITEM");
                break;
            case binary_item:
                fprintf(output_file, " type='%s'","BINARY_ITEM");
                break;
            case native_binary_item:
                fprintf(output_file, " type='%s'","NATIVE_BINARY_ITEM");
                break;
            default:
                fprintf(output_file, " type='%s'","ALPHANUMERIC_ITEM");
        }
        if (strlen(dds->picture) > 0)
            fprintf(output_file, " picture='%s'",dds->picture);
        if (strlen(dds->usage) > 0)
            fprintf(output_file, " usage='%s'",dds->usage);
        if (0 < dds->byteLength)
            fprintf(output_file, " byteLength='%d'",dds->byteLength);

        if (TRUE == dds->justifiedRight)
            fprintf(output_file, " justifiedRight='%s'","true");

        /* Numeric elements annotations */ 
        if (0 < dds->totalDigits) {
            fprintf(output_file, " signed='%s'",
                                (dds->sign == 0)?"false":"true");
            fprintf(output_file, " totalDigits='%d'",dds->totalDigits);
            if (0 < dds->fractionDigits)
                fprintf(output_file, " fractionDigits='%d'",
                                dds->fractionDigits);
            if (TRUE == dds->signLeading)
                fprintf(output_file, " signLeading='%s'","true");
            if (TRUE == dds->signSeparate)
                fprintf(output_file, " signSeparate='%s'","true");
        }
    }
    else {
        fprintf(output_file, " type='%s'","GROUP_ITEM");
    }
    if (0 < dds->maxOccurs) {
        fprintf(output_file, " minOccurs='%d'",dds->minOccurs);
        fprintf(output_file, " maxOccurs='%d'",dds->maxOccurs);
    }
    if (0 < strlen(dds->dependingOn))
        fprintf(output_file, " dependingOn='%s'",dds->dependingOn);
    if (dds->odoObject)
        fprintf(output_file, " isODOObject='true'");
    if (0 < strlen(dds->redefines))
        fprintf(output_file, " redefines='%s'",dds->redefines);
	if (TRUE == dds->redefined) {
        fprintf(output_file, " isRedefined='%s'","true");
        fprintf(output_file, " unmarshalChoiceStrategyClassName='%s'","");
	}
    if (0 < dds->srceLine)
        fprintf(output_file, " srceLine='%d'",dds->srceLine);


    /* Values are inserted as element values rather than attributes  */
    if (strlen(dds->value) > 0) {
        fprintf(output_file, ">\n");
        XSD_processValue(dds, indent);
        fprintf(output_file, "%s    </%s:cobolElement>\n",
            indent,xsd_options->xscb_prefix);
    }
    else {
        fprintf(output_file, "/>\n");
    }


    fprintf(output_file, "%s    </%s:appinfo>\n",
        indent,xsd_options->xs_prefix);
    fprintf(output_file, "%s  </%s:annotation>\n",
        indent,xsd_options->xs_prefix);

    return 0;
}

/*====================================================================*/
/* Value might need some cleaning to appear in an XSD                 */
/*====================================================================*/
int XSD_processValue(COBOL_DATA_DESCRIPTION* dds, char* indent)
{
    /* We have to assume a maximum expansion due to entity resolution */
    char dest[2*MAX_VALUE_LEN];
    memset(dest, '\0',1);

    if (debug_trace) printf("XSD_processValue\n");

	XSD_resolveFigurative(dds->value, dds);

    XSD_entityEncode(dds->value, dest);

    fprintf(output_file, "%s    <%s:value>%s</%s:value>\n",
            indent, xsd_options->xscb_prefix, dest,
            xsd_options->xscb_prefix);

return 0;
}

/*====================================================================*/
/* String sympleType                                                  */
/*====================================================================*/
int XSD_processString(COBOL_DATA_DESCRIPTION* dds, char* indent)
{
    if (debug_trace) printf("XSD_processString\n");

    fprintf(output_file, "\"%s:string\">\n",xsd_options->xs_prefix);
    fprintf(output_file,
        "%s    <%s:length value='%d'/>\n",
            indent, xsd_options->xs_prefix,dds->byteLength);
    return 0;
}

/*====================================================================*/
/* National sympleType                                                */
/*====================================================================*/
int XSD_processNational(COBOL_DATA_DESCRIPTION* dds, char* indent)
{
    if (debug_trace) printf("XSD_processNational\n");

    fprintf(output_file, "\"%s:string\">\n",xsd_options->xs_prefix);
    fprintf(output_file,
        "%s    <%s:length value='%d'/>\n",
            indent, xsd_options->xs_prefix,dds->byteLength / 2);
    return 0;
}

/*====================================================================*/
/* Dbcs sympleType                                                    */
/*====================================================================*/
int XSD_processDbcs(COBOL_DATA_DESCRIPTION* dds, char* indent)
{
    if (debug_trace) printf("XSD_processDbcs\n");

    fprintf(output_file, "\"%s:string\">\n",xsd_options->xs_prefix);
    fprintf(output_file,
        "%s    <%s:length value='%d'/>\n",
            indent, xsd_options->xs_prefix,dds->byteLength / 2);
    return 0;
}

/*====================================================================*/
/* Octet stream sympleType                                            */
/*====================================================================*/
int XSD_processOctetStream(COBOL_DATA_DESCRIPTION* dds, char* indent)
{
    if (debug_trace) printf("XSD_processOctetStream\n");

    fprintf(output_file, "\"%s:hexBinary\">\n",xsd_options->xs_prefix);
    fprintf(output_file,
        "%s    <%s:length value='%d'/>\n",
            indent, xsd_options->xs_prefix,dds->byteLength);
    return 0;
}

/*====================================================================*/
/* Decimal sympleType                                                 */
/*====================================================================*/
int XSD_processDecimal(COBOL_DATA_DESCRIPTION* dds, char* indent)
{
    char xsBase[30];
    char xsIncl[30];
    int i;

    if (debug_trace) printf("XSD_processDecimal\n");

    if (dds->fractionDigits == 0)
    {
        /* Integers */
        if (dds->totalDigits < 5)
            strcpy(xsBase,(dds->sign)?"short":"unsignedShort");
        else
        if (dds->totalDigits < 10)
            strcpy(xsBase,(dds->sign)?"int":"unsignedInt");
        else
        if (dds->totalDigits < 20)
            strcpy(xsBase,(dds->sign)?"long":"unsignedLong");
        else
            strcpy(xsBase,(dds->sign)?"integer":"integer");
         fprintf(output_file, "\"%s:%s\">\n",
             xsd_options->xs_prefix,xsBase);
        /* Prepare minInclusive and maxInclusive for numeric types
           that are bounded by the digits.                       */
         if (dds->dataType != native_binary_item) {
            i= dds->totalDigits;
            memset(xsIncl,'9',i);
            memset(xsIncl+i,'\0',1);

            if (dds->sign)
                fprintf(output_file,
            "%s    <%s:minInclusive value='-%s'/>\n",
                    indent,xsd_options->xs_prefix, xsIncl);
            else
                fprintf(output_file,
                "%s    <%s:minInclusive value='0'/>\n",
                    indent,xsd_options->xs_prefix);
            fprintf(output_file,
            "%s    <%s:maxInclusive value='%s'/>\n",
                indent, xsd_options->xs_prefix,(xsIncl));
         }
   }
    else
    {
        /* Decimals */
        fprintf(output_file, "\"%s:decimal\">\n",
            xsd_options->xs_prefix);
        fprintf(output_file,
            "%s    <%s:totalDigits value='%d'/>\n",
                indent,xsd_options->xs_prefix, dds->totalDigits);
        fprintf(output_file,
         "%s    <%s:fractionDigits value='%d'/>\n",
                indent,xsd_options->xs_prefix, dds->fractionDigits);
        
        /* Prepare minInclusive and maxInclusive */
        i= dds->totalDigits - dds->fractionDigits;
        memset(xsIncl,'9',i);
        memset(xsIncl+i,'.',1);
        i++;
        memset(xsIncl+i,'9',dds->fractionDigits);
        i += dds->fractionDigits;
        memset(xsIncl+i,'\0',1);

        if (dds->sign)
            fprintf(output_file,
         "%s    <%s:minInclusive value='-%s'/>\n",
                indent,xsd_options->xs_prefix, xsIncl);
        else
            fprintf(output_file,
         "%s    <%s:minInclusive value='0.0'/>\n",
                indent,xsd_options->xs_prefix);
        fprintf(output_file,
         "%s    <%s:maxInclusive value='%s'/>\n",
            indent,xsd_options->xs_prefix, (xsIncl));
    }


    return 0;
}

/*====================================================================*/
/* Edited numeric sympleType                                          */
/*====================================================================*/
int XSD_processNumericEdited(COBOL_DATA_DESCRIPTION* dds, char* indent)
{
    char regex[MAX_REGEX_LEN];
    if (debug_trace) printf("ASN_processNumericEdited\n");

    /* Transform picture into a regular expression */
    if (0 != Picture2Regex(debug_trace,
                            dds->picture,
                            regex,
                            MAX_REGEX_LEN,
                            xsd_regex,
                            produce_msg))
        return 8;

    fprintf(output_file, "\"%s:string\">\n",xsd_options->xs_prefix);
    fprintf(output_file,
        "%s    <%s:pattern value='%s'/>\n",
            indent, xsd_options->xs_prefix,regex);
    return 0;
}

/*====================================================================*/
/* Float sympleType                                                   */
/*====================================================================*/
int XSD_processFloat(COBOL_DATA_DESCRIPTION* dds, char* indent)
{
    if (debug_trace) printf("XSD_processFloat\n");

    if ((dds->dataType == single_float_item) ||
        (dds->dataType == external_floating_item))
        fprintf(output_file, "\"%s:float\">\n",xsd_options->xs_prefix);
    else
        fprintf(output_file, "\"%s:double\">\n",xsd_options->xs_prefix);
    return 0;
}

/*====================================================================*/
/* Cobol names may look ugly in an XSD, this will pretty them up      */
/*====================================================================*/
int XSD_formatName(char* cobolName, char* xsName)
{
    int i = 0;                     /* current char in cobol name      */
    int l = 0;                     /* current char in XML schema name */
    int wordBreak = TRUE;          /* word break indicator. 
                                      always true for first char.     */

    if (debug_trace) printf("XSD_formatName\n");

    /* Apply rules */
    for(i=0;i < (int)strlen(cobolName); i++)
    {
        /* Minus sign is a Cobol word break indicator. Replace it or
           ignore it depending on the current options.                 */
        if (cobolName[i] == '-')
        {
            wordBreak = TRUE;
            if (xsd_options->remove_minus)
                continue;

            if (xsd_options->replace_minus)
                xsName[l] = xsd_options->replace_minus_char;
            else
                xsName[l] = cobolName[i];
            l++;
            continue;
        }

        /* Digits are also word breaks indicators but they are 
           never ignored.                                             */
        if (ISDIGIT(cobolName[i])) {
            wordBreak = TRUE;
            xsName[l] = cobolName[i];
            l++;
            continue;
        }

        /* if lowercasing option is on, it will affect all non-digit 
           characters unless they happen to immediatly follow a 
           word break in which case they might be uppercased.         */
        if (xsd_options->uppercase_to_lower)
        {
            if (xsd_options->firstchar_upper)
            {
                if (wordBreak)
                    xsName[l] = toupper(cobolName[i]);
                else
                    xsName[l] = tolower(cobolName[i]);
            }
            else
                xsName[l] = tolower(cobolName[i]);
        }
        else
        {
            xsName[l] = cobolName[i];
        }
        l++;
        wordBreak = FALSE;
    }
    xsName[l] = '\0';

    return 0;
}

/*====================================================================*/
/* Encode entities for XML compatibility                              */
/*====================================================================*/
int XSD_entityEncode(char* source, char* dest)
{
    int i = 0;
    int l = 0;

    if (debug_trace) printf("XSD_entityEncode\n");

    for(i=0;i< (int)strlen(source);i++)
    {
        switch(source[i])
        {
        case '&':memcpy(dest+l,"&amp;",5);l+=5;break;
        case '<':memcpy(dest+l,"&lt;",4);l+=4;break;
        case '>':memcpy(dest+l,"&gt;",4);l+=4;break;
        case '\'':memcpy(dest+l,"&apos;",6);l+=6;break;
        case '\"':memcpy(dest+l,"&quot;",6);l+=6;break;
        default:memcpy(dest+l,source+i,1);l+=1;break;
        }
    }
    memset(dest+l,'\0',1);
    return 0;
}

/*====================================================================*/
/* This module has responsibility over the xsd production option      */
/* structure. This method allows the caller to get default values.    */
/*====================================================================*/
void  getDefaultXsdOptions(XSD_PRODUCTION_OPTIONS* xsdOptions)
{
    /* Set the xsd productions default options  */
    xsdOptions->replace_minus = REPLACE_MINUS;
    xsdOptions->replace_minus_char = REPLACE_MINUS_CHAR;
    xsdOptions->remove_minus = REMOVE_MINUS;
    xsdOptions->uppercase_to_lower = UPPERCASE_TO_LOWER;
    xsdOptions->firstchar_upper = FIRSTCHAR_UPPER;
    strcpy(xsdOptions->type_suffix, TYPE_SUFFIX);
    strcpy(xsdOptions->xs_prefix, XS_PREFIX);
    strcpy(xsdOptions->xs_ns, XS_NS);
    strcpy(xsdOptions->xsns_prefix, XSNS_PREFIX);
    strcpy(xsdOptions->xsns_ns, XSNS_NS);
    strcpy(xsdOptions->xscb_prefix, XSCB_PREFIX);
    strcpy(xsdOptions->xscb_ns, XSCB_NS);
    strcpy(xsdOptions->xsjaxb_prefix, XSJAXB_PREFIX);
    strcpy(xsdOptions->xsjaxb_ns, XSJAXB_NS);
    strcpy(xsdOptions->xsjaxb_version, XSJAXB_VERSION);
    strcpy(xsdOptions->xsjaxb_package, XSJAXB_PACKAGE);
    xsdOptions->xsd_header_footer = XSD_HEADER_FOOTER;
    return;
}

/*====================================================================*/
/* Search for name conflicts. If a COBOL identifier occurs more than  */
/* once in the source this method returns TRUE.                       */
/*====================================================================*/
int XSD_nameConflict(STREE_NODE* srcNode, COBOL_DATA_DESCRIPTION* dds)
{
    STREE_NODE* currentNode;

	/* Different source lines but same COBOL name is a conflict */
	if ((srcNode->dds.srceLine != dds->srceLine)
		&& (0 == strcmp(srcNode->dds.cobolName, dds->cobolName)))
	{
		return TRUE;
	}
	
	currentNode = srcNode->firstChild;
    while(currentNode)
    {
		/* Only deal with complex types */
		if (currentNode->firstChild
			&& TRUE == XSD_nameConflict(currentNode, dds))
		{
			return TRUE;
		}
        currentNode = currentNode->firstSibling;
    }
	return FALSE;
}

/*====================================================================*/
/* Figurative constants are reserved words that name and refer to     */
/* specific constant values. Since consumers are unlikely to          */
/* understand them, we replace them with their actual value.          */
/*====================================================================*/
int XSD_resolveFigurative(char* inStr, COBOL_DATA_DESCRIPTION* dds)
{
    int i = 0;
    if ((strcmp(inStr, "ZERO") == 0) ||
        (strcmp(inStr, "ZEROS") == 0) ||
        (strcmp(inStr, "ZEROES") == 0) ||
        (strcmp(inStr, "zero") == 0) ||
        (strcmp(inStr, "zeros") == 0) ||
        (strcmp(inStr, "zeroes") == 0))
        strcpy(dds->value,"0");
    else
    /* Space is used to fill the data element */
    if ((strcmp(inStr, "SPACE") == 0) ||
        (strcmp(inStr, "SPACES") == 0) ||
        (strcmp(inStr, "space") == 0) ||
        (strcmp(inStr, "spaces") == 0))
    {
        memset(dds->value,' ',dds->byteLength);
        memset(dds->value+dds->byteLength, '\0',1);
    }
    else
    /* This is a proposed representation for HIGH-VALUE */
    if ((strcmp(inStr, "HIGH-VALUE") == 0) ||
        (strcmp(inStr, "HIGH-VALUES") == 0) ||
        (strcmp(inStr, "high-value") == 0) ||
        (strcmp(inStr, "high-values") == 0))
    {
        strcpy(dds->value,"0x");
        for (i = 0; i < (int)dds->byteLength; i++)
            strcat(dds->value,"FF");
    }
    else
    if ((strcmp(inStr, "LOW-VALUE") == 0) ||
        (strcmp(inStr, "LOW-VALUES") == 0) ||
        (strcmp(inStr, "low-value") == 0) ||
        (strcmp(inStr, "low-values") == 0))
    {
        strcpy(dds->value,"0x");
        for (i = 0; i < (int)dds->byteLength; i++)
            strcat(dds->value,"00");
    }
    else
    /* Quote translates to an XML entity.*/
    if ((strcmp(inStr, "QUOTE") == 0) ||
        (strcmp(inStr, "QUOTES") == 0) ||
        (strcmp(inStr, "quote") == 0) ||
        (strcmp(inStr, "quotes") == 0))
    {
        strcpy(dds->value,"\"");
    }
    if (strcmp(inStr, "APOST") == 0)
    {
        strcpy(dds->value,"\'");
    }
    else
    /* Treat nulls like low-values  */
    if ((strcmp(inStr, "NULL") == 0) ||
        (strcmp(inStr, "NULLS") == 0) ||
        (strcmp(inStr, "null") == 0) ||
        (strcmp(inStr, "nulls") == 0))
    {
        strcpy(dds->value,"0x");
        for (i = 0; i < (int)dds->byteLength; i++)
            strcat(dds->value,"00");
    }

    return 0;
}
