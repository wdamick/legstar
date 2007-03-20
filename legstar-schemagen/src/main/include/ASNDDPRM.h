#ifndef _DEFINED_MDDASNPRODUCE 
#define _DEFINED_MDDASNPRODUCE
/*====================================================================*/
/*                                                                    */
/* MODULE NAME = MDDASNPRODUCE                                        */
/*                                                                    */
/* Description of the methods exported by DDSASNProduce.dll.          */
/*                                                                    */
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/* Prototypes exports                                                */
/*-------------------------------------------------------------------*/
#ifdef COMPILE_DDASNPRODUCE
#pragma export(AsnDDProduce)
#pragma export(getDefaultAsnOptions)
#define LIBSPEC
#else
#define LIBSPEC extern
#endif
LIBSPEC    int    AsnDDProduce(
           int debug_mode,    /* 1= Debug traces in stdout         */
           STREE_NODE* root,  /* Root node of data structure tree  */
           ASN_PRODUCTION_OPTIONS* asnOptions, /* ASN1 options     */
           char* outFileName, /* Output ASN1 file name             */
           char* produceMsg); /* Error message if any              */

LIBSPEC    void  getDefaultAsnOptions
                            (ASN_PRODUCTION_OPTIONS* asnOptions);
#undef LIBSPEC

#endif /* _DEFINED_MDDASNPRODUCE */

