#ifndef _DEFINED_DDSTRUCT 
#define _DEFINED_DDSTRUCT
/*====================================================================*/
/*                                                                    */
/* MODULE NAME = DDSTRUCT                                             */
/*                                                                    */
/* Data description storage structure                                 */
/* Structure is organized as a binary tree.                           */
/*                                                                    */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/* Constants                                                         */
/*-------------------------------------------------------------------*/
#define MAX_DATANAME_LEN 31     /* Cobol data names cannot exceed 30 */
#define MAX_PICTURE_LEN 51      /* Set a max for picture clause      */
#define MAX_USAGE_LEN 33        /* Set a max for usage clause        */
#define MAX_VALUE_LEN 161       /* Literals cannot exceed 160        */
#define TRUE  1                 /* Simple boolean simulation         */
#define FALSE 0                 /* Simple boolean simulation         */

/*-------------------------------------------------------------------*/
/* Cobol compiler default options                                    */
/*-------------------------------------------------------------------*/
#define CURRENCY_SIGN '$'       /* Currency symbol used by compiler  */
#define INCLUDE_DEBUG_LINES FALSE /* Ignore Cobol debug lines or not */
#define TRUNC_BIN FALSE         /* Compiler option for full binary   
                                   support for Binary, comp & comp-4 */
#define DECIMAL_POINT_IS_COMMA 0 /* The comma is decimal separator   */
#define NSYMBOL_DBCS FALSE      /* UTF16 support as opposed to DBCS  */
#define COB_QUOTE FALSE         /* FALSE=APOST TRUE=QUOTE            */

/*-------------------------------------------------------------------*/
/* Structure describing the cobol compiler options in effect         */
/*-------------------------------------------------------------------*/
typedef struct {
  int include_debug_lines;    /* 1 = DEBUG lines are to be parsed    */
  int trunc_bin;      /* 1 = TRUNC(BIN) option is in effect          */
  char currency_sign;   /* The currency symbol used in numerics      */
  int decimal_point_is_comma; /* 1= Comma is the decimal separator   */
  int nsymbol_dbcs;   /* The NSYMBOL(DBCS) option is in effect       */
  int quote;          /* 1 = QUOTE option in effect otherwise APOST  */
} COBOL_COMPILER_OPTIONS;

/*-------------------------------------------------------------------*/
/* Type of item                                                      */
/*-------------------------------------------------------------------*/
enum ITEMTYPE
{
    group_item          /* Group item                  */
    ,elementary_item    /* Item with a PICTURE clause  */
    ,condition_name     /* A level 88 condition name   */
};

/*-------------------------------------------------------------------*/
/* Type of data for elementary items                                 */
/*-------------------------------------------------------------------*/
enum DATATYPE
{
    alphabetic_item      /* Characters only                          */
    ,national_item       /* PIC N when USAGE NATIONAL is specified   */
    ,dbcs_item           /* PIC Contains G, G and B, or N            */
    ,alphanumeric_edited_item /* PIC X or A with some B,0 or /       */
    ,alphanumeric_item   /* The usual PIC X or 9 A combination       */
    ,octet_stream_item   /* PIC X containing binary data             */
    ,single_float_item   /* Single precision floating point          */
    ,double_float_item   /* Double precision floating point          */
    ,packed_decimal_item /* Compressed decimal (Comp-3, packed)      */
    ,zoned_decimal_item  /* Simple display numeric                 */
    ,numeric_edited_item /* Display numeric with editing pattern     */
    ,index_item          /* A 4 bytes index                          */
    ,pointer_item        /* A 4 bytes pointer to a memory block      */
    ,proc_pointer_item   /* A 8 bytes pointer to ENTRY or PROGRAM ID */
    ,func_pointer_item   /* A 4 bytes pointer to ENTRY or PROGRAM ID */
    ,object_item         /* Points to class name (OO extensions)     */
    ,external_floating_item   /* +/- mantissa E +/- exponent         */
    ,binary_item         /* Binary, Comp, Comp-4, Comp_5 (Big Endian)*/
    ,native_binary_item  /* COMP-5 or binary_item when trunc=bin     */
    ,unknown             /* Other item types                        */
};

/*-------------------------------------------------------------------*/
/* Data description structure                                        */
/*-------------------------------------------------------------------*/
typedef struct {
   int    levelNumber;   /* Level of the record                      */
   char   cobolName[MAX_DATANAME_LEN];  /* Cobol item name           */
   enum ITEMTYPE itemType;  /* Type of data item (record, ...)       */
   char   picture[MAX_PICTURE_LEN];   /* Picture clause              */
   char   usage[MAX_USAGE_LEN];     /* Usage clause                  */
   enum DATATYPE dataType;  /* Type of elementary item (numeric,...) */
   char   value[MAX_VALUE_LEN]; /* default value of elementary item  */
   int    byteLength;    /* Size in bytes of data item               */
   int    justifiedRight;/* Whether text is right justified          */
   int    totalDigits;   /* Total number of digits for numerics      */
   int    fractionDigits;/* Zero for integers                        */
   int    sign;          /* Signed data description                  */
   int    signSeparate;  /* Sign occupies 1 byte for zoned decimal   */
   int    signLeading;   /* Sign occupies 1 byte for zoned decimal   */
   int    minOccurs;     /* Lower bound for an array                 */
   int    maxOccurs;     /* Upper bound for an array                 */
   char   dependingOn[MAX_DATANAME_LEN]; /* Variable size arrays     */
   int    odoObject;     /* 1 means this is the object of at least
                            one Occurs Depending On clause.          */
   char   redefines[MAX_DATANAME_LEN]; /* Alternative choice         */
   int    redefined;     /* 1 means this at least one following 
                            shares the same storage.                 */
   int    srceLine;      /* Line number relative to source file      */

} COBOL_DATA_DESCRIPTION;

/*-------------------------------------------------------------------*/
/* Tree structure used to store data descriptions in their hierarchy */
/*-------------------------------------------------------------------*/
typedef struct {
   void*  firstChild;    /* Points to first child or NULL            */
   void*  firstSibling;  /* Points to first sibling or NULL          */
   int    fatherLevel;   /* Level of this node father (0 if root)    */
   COBOL_DATA_DESCRIPTION  dds;    /* Data description               */
} STREE_NODE;


#endif /* _DEFINED_DDSTRUCT */

