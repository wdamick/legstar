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
/*   Module      - PARSTSTS                                           */
/*   Purpose     - Sample program for COB2XSD testing                 */
/*   Language    - C                                                  */
/*   System      - Tested on Windows XP Pro and z/OS 1.5              */
/*   History     - 08 June 2006  - Original Implementation            */
/*   Notes       - Takes file names for Cobol source and target       */
/*                 Generates an XML schema.                           */
/*                                                                    */
/*====================================================================*/

#include <stdio.h>
#include <string.h>
#include "COBDDPAS.h" /* Cobol dd parsing structures */
#include "XSDDDPRS.h" /* Xsd production structures */
#include "COB2XSDM.h" /* Cobol to XSD external methods */

/*-------------------------------------------------------------------*/
/* Constants                                                         */
/*-------------------------------------------------------------------*/
#define DEBUG_TRACE 1           /* 0=No traces                       */
#define COBOL_FILE "stdin"      /* File to parse                     */
#define OUTPUT_FILE "stdout"    /* where output is written           */

int main(int argc, char *argv[])
{
    char msg[256];
    char inFile[256];
    char outFile[256];
    int rc = 0;
    COBOL_COMPILER_OPTIONS cobolOptions;
    XSD_PRODUCTION_OPTIONS xsdOptions;

    /* get the input file name if any, otherwise default to stdin    */
    if (argc > 1)
        strcpy(inFile, argv[1]);
    else
        strcpy(inFile, COBOL_FILE);

    /* get the output file name if any, otherwise default to stdout  */
    if (argc > 2)
        strcpy(outFile, argv[2]);
    else
        strcpy(outFile, OUTPUT_FILE);
    
    /* Initialize Cobol options with default values */
    getDefaultCobolOptions(&cobolOptions);
    cobolOptions.include_debug_lines = FALSE;
    cobolOptions.currency_sign = '€';

    /* Initialize Xsd generation options with default values */
    getDefaultXsdOptions(&xsdOptions);

    /* Call the Cobol to XML Schema generator  */
    rc =  Cobol2XMLSchema(DEBUG_TRACE,
                  inFile,
                  "MyRoot",
                  outFile,
                  &cobolOptions,
                  &xsdOptions,
                  msg);

    if (rc != 0)
    {
        printf("msg=%s",msg);
        return rc;
    }
    return 0;
}


