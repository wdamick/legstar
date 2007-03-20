#ifndef _DEFINED_MDDXSDPRODUCE 
#define _DEFINED_MDDXSDPRODUCE
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
#ifdef COMPILE_DDXSDPRODUCE
#pragma export(XsdDDProduce)
#pragma export(getDefaultXsdOptions)
#define LIBSPEC
#else
#define LIBSPEC extern
#endif
LIBSPEC    int    XsdDDProduce(
           int debug_mode,    /* 1= Debug traces in stdout         */
           STREE_NODE* root,  /* Root node of data structure tree  */
           XSD_PRODUCTION_OPTIONS* xsdOptions, /* XSD options      */
           char* outFileName, /* Output XML schema file name       */
           char* produceMsg); /* Error message if any              */

LIBSPEC    void  getDefaultXsdOptions
                            (XSD_PRODUCTION_OPTIONS* xsdOptions);
#undef LIBSPEC

#endif /* _DEFINED_MDDXSDPRODUCE */

