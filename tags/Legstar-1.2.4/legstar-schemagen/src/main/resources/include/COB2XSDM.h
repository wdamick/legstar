#ifndef _DEFINED_MCOB2XSD 
#define _DEFINED_MCOB2XSD
/*====================================================================*/
/*                                                                    */
/* MODULE NAME = MDDXSDPRODUCE                                        */
/*                                                                    */
/* Description of the methods exported by DDSXSDProduce.dll.          */
/*                                                                    */
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/* Prototypes exports                                                */
/*-------------------------------------------------------------------*/
#ifdef COMPILE_COB2XSD
#ifdef MVS
#pragma export(Cobol2XMLSchema)
#pragma export(getDefaultCobolOptions)
#pragma export(getDefaultAsnOptions)
#pragma export(getDefaultXsdOptions)
#endif
#define LIBSPEC
#else
#define LIBSPEC extern
#endif
LIBSPEC    int    Cobol2XMLSchema(
           int debug_mode,    /* 1= Debug traces in stdout         */
           char* inFileName,  /* Input Cobol file name             */
           char* inRootName,  /* Higher level complexType for XSD  */
           char* outFileName, /* Output XSD file name              */
           COBOL_COMPILER_OPTIONS* cobolOptions,  /* Cobol compiler
                                                options in effect  */
           XSD_PRODUCTION_OPTIONS* xsdOptions, /* XSD options      */
           char* parseMsg);   /* Error message if any              */

LIBSPEC    void  getDefaultCobolOptions
                            (COBOL_COMPILER_OPTIONS* cobolOptions);
LIBSPEC    void  getDefaultXsdOptions
                            (XSD_PRODUCTION_OPTIONS* xsdOptions);
#undef LIBSPEC


#endif /* _DEFINED_MCOB2XSD */

