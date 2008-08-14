#ifndef _DEFINED_MCOBDDPARSE 
#define _DEFINED_MCOBDDPARSE
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
#ifdef COMPILE_COBDDPARSE
#ifdef MVS
#pragma export(CobDDParse)
#pragma export(STree_Allocate)
#pragma export(STree_Print)
#pragma export(STree_Free)
#pragma export(getDefaultCobolOptions)
#endif
#define LIBSPEC
#else
#define LIBSPEC extern
#endif
LIBSPEC    int    CobDDParse(
           int debug_mode,    /* 1= Debug traces in stdout         */
           char* inFileName,  /* Input Cobol file name             */
           COBOL_COMPILER_OPTIONS* cobol_options,  /* Cobol compiler
                                                options in effect  */
           STREE_NODE* root,  /* Root node of data structure tree  */
           char* parseMsg);   /* Error message if any              */

LIBSPEC    STREE_NODE*     STree_Allocate     ();
LIBSPEC    int             STree_Print        (STREE_NODE* parent);
LIBSPEC    int             STree_Free         (STREE_NODE* parent);
LIBSPEC    void  getDefaultCobolOptions
                            (COBOL_COMPILER_OPTIONS* cobolOptions);

#undef LIBSPEC


#endif /* _DEFINED_MCOBDDPARSE */

