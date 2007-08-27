#ifndef _DEFINED_SDDASNPRODUCE 
#define _DEFINED_SDDASNPRODUCE
/*====================================================================*/
/*                                                                    */
/* MODULE NAME = SDDASNPRODUCE                                        */
/*                                                                    */
/*--------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/* Constant values                                                   */
/*-------------------------------------------------------------------*/
#define MAX_MODULE_NAME_LEN 257 /* Maximum size of module name       */
#define MAX_TAG_DEFAULT_LEN 127  /* Maximum size of tag attribute    */
#define MAX_EXT_DEFAULT_LEN 127  /* Maximum size of extensibility
                                    attribute                        */
#define MAX_TYPE_SUFFIX_LEN 17  /* Maximum size of type suffix       */
#define MAX_ARRAY_SUFFIX_LEN 17  /* Maximum size of array suffix     */
/*-------------------------------------------------------------------*/
/* ASN1 default production rules                                     */
/*-------------------------------------------------------------------*/
#define REPLACE_MINUS 0         /* Replace - sign from cobol names   */
#define REPLACE_MINUS_CHAR '_'  /* Char to use to replace - sign     */
#define REMOVE_MINUS 1          /* Remove - sign from cobol names    */
#define UPPERCASE_TO_LOWER 1    /* Lower case chars from cobol name  */
#define FIRSTCHAR_UPPER 1       /* Uppercase first char and chars    
                                   following minus sign              */
#define TYPE_SUFFIX "Type"      /* Used to build complex type names  */
#define ARRAY_SUFFIX "Array"    /* Used to build array names         */
#define COBOL_ANNOTATION 1      /* Keep cobol unique attributes as 
                                   schema annotations                */
#define ASN_HEADER_FOOTER 1     /* Add module definitions            */
#define MODULE_NAME "TozModule" /* The asn1 module name              */
#define MODULE_NAME_OID ""      /* Registered identifier if any      */
#define TAG_DEFAULT "AUTOMATIC TAGS"  /* Type of tagging required    */
#define EXTENSION_DEFAULT ""    /* EXTENSIBILITY IMPLIED             */

/*-------------------------------------------------------------------*/
/* Structure describing the ASN1 production options in effect        */
/*-------------------------------------------------------------------*/
typedef struct {
  int asn_header_footer;   /* Add module definitions                 */
  char module_name[MAX_MODULE_NAME_LEN]; /* ASN1 module name         */
  char module_name_OID[MAX_MODULE_NAME_LEN];  /* ASN1 module name
                                               identifier if any     */
  char tag_default[MAX_TAG_DEFAULT_LEN];  /* EXPLICIT, IMPLICIT or
                                             AUTOMATIC TAGS          */
  char extension_default[MAX_EXT_DEFAULT_LEN]; /* EXTENSIBILITY
                                                  IMPLIED            */

  int replace_minus;       /* 1 = Replace - sign from cobol names    */
  char replace_minus_char; /* Char to use to replace - sign          */
  int remove_minus;        /* 1 = Remove - sign from cobol names     */
  int uppercase_to_lower;  /* 1= Comma is the decimal separator      */
  int firstchar_upper;     /* 1 = Uppercase first char and chars    
                                   following minus sign              */
  int cobol_annotation;    /* 1 = Keep cobol unique attributes as 
                                   schema annotations                */
  char type_suffix[MAX_TYPE_SUFFIX_LEN]; /* Used to build complex 
                                            type names               */
  char array_suffix[MAX_ARRAY_SUFFIX_LEN];  /* Used to build array
                                               names                 */
} ASN_PRODUCTION_OPTIONS;

#endif /* _DEFINED_SDDASNPRODUCE */

