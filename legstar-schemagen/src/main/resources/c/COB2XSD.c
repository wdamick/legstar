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
/*   Module      - COB2XSD                                            */
/*   Purpose     - DLL implementing Cobol2XMLSchema and Cobol2Ans1    */
/*   Language    - C                                                  */
/*   System      - Tested on Windows XP Pro and z/OS 1.5              */
/*   History     - 08 June 2006  - Original Implementation            */
/*   Notes       - High level DLL that wraps COBDDParse followed      */
/*                 by DDXSDProduce. Should be easier to call from     */
/*                 .Net interop or Java JNI.                          */
/*   Limitations -                                                    */
/*   To do       -                                                    */
/*====================================================================*/
#include <stdio.h>
#include <string.h>

#include "COBDDPAS.h" /* Cobol dd parsing structures */
#include "XSDDDPRS.h" /* Xsd production structures */
#define COMPILE_COB2XSD
#include "COB2XSDM.h" /* Cobol to XSD external methods */
#include "COBDDPAM.h" /* Cobol dd parsing external methods */
#include "XSDDDPRM.h" /* Xsd production  external methods */

/*-------------------------------------------------------------------*/
/* Global variables                                                  */
/*-------------------------------------------------------------------*/
COBOL_COMPILER_OPTIONS cobol_options;
COBOL_COMPILER_OPTIONS* pcobolOptions;
XSD_PRODUCTION_OPTIONS xsd_options;
XSD_PRODUCTION_OPTIONS* pxsdOptions;
STREE_NODE* STreeRoot;          /* Root of the structure tree        */

/*-------------------------------------------------------------------*/
/* Prototypes  internal methods                                      */
/*-------------------------------------------------------------------*/
int Cobol2Structure(int debug_mode,
                    char* inFileName,
                    COBOL_COMPILER_OPTIONS* cobolOptions,
                    char* parseMsg);

/*====================================================================*/
/* Cobol to XML schema generation                                     */
/*====================================================================*/
int Cobol2XMLSchema(int debug_mode,
                    char* inFileName,
                    char* inRootName,
                    char* outFileName,
                    COBOL_COMPILER_OPTIONS* cobolOptions,
                    XSD_PRODUCTION_OPTIONS* xsdOptions,
                    char* parseMsg)
{
    int rc = 0;
    
    /* Phase 1: Create a data structure tree from the Cobol file */
    rc = Cobol2Structure(debug_mode,
                         inFileName,
                         cobolOptions,
                         parseMsg);
    if (rc != 0) return rc;

    /* If no XSD options are passed, get a default one  */
    if (NULL == xsdOptions) {
        getDefaultXsdOptions(&xsd_options);
        pxsdOptions = &xsd_options;
    }
    else {
        pxsdOptions = xsdOptions;
    }

    /* If client is requesting a root name, set it  */
    if ((NULL != inRootName) && (strlen(inRootName) >0))
        strcpy(STreeRoot->dds.cobolName,inRootName); 

    /* Phase2: Produce an XML Schema from the data structure tree */
    rc = XsdDDProduce(debug_mode,
                      STreeRoot,
                      pxsdOptions,
                      outFileName,
                      parseMsg);
    if (rc != 0)
        printf("%s\n",parseMsg);

    /* Free the structure tree */
     STree_Free(STreeRoot);

    return rc;
}

/*====================================================================*/
/* Parse the input Cobol source into an in-memory structure           */
/*====================================================================*/
int Cobol2Structure(int debug_mode,
                    char* inFileName,
                    COBOL_COMPILER_OPTIONS* cobolOptions,
                    char* parseMsg)
{
    int rc = 0;

    /* If no cobol options are passed, get a default one  */
    if (NULL == cobolOptions) {
        getDefaultCobolOptions(&cobol_options);
        pcobolOptions = &cobol_options;
    }
    else {
        pcobolOptions = cobolOptions;
    }

    /* allocate the root node of the structure tree */
    STreeRoot = STree_Allocate();
    if (!STreeRoot) {
        printf("Out of memory\n");
        return 8;
    }

    /* Phase 1: Create a data structure tree from the Cobol file */
    rc =  CobDDParse(debug_mode,
                     inFileName,
                     pcobolOptions,
                     STreeRoot,
                     parseMsg);
    if (rc != 0)
    {
        printf("%s\n",parseMsg);
        STree_Free(STreeRoot);
        return rc;
    }

    if (debug_mode) STree_Print(STreeRoot);

    return rc;
}

