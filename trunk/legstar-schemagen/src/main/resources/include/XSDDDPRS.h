#ifndef _DEFINED_SDDXSDPRODUCE 
#define _DEFINED_SDDXSDPRODUCE
/*====================================================================*/
/*                                                                    */
/* MODULE NAME = SDDXSDPRODUCE                                        */
/*                                                                    */
/*--------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/* Constant values                                                   */
/*-------------------------------------------------------------------*/
#define MAX_PFX_LEN 16          /* Maximum size of namespace prefix  */
#define MAX_NS_LEN 256          /* Maximum size of namespace name    */
#define MAX_TYPE_SUFFIX_LEN 16  /* Maximum size of type suffix       */
#define MAX_VERSION_LEN 6       /* Maximum size of JAXB version      */
#define MAX_PACKAGE_LEN 256     /* Maximum size of target package    */
/*-------------------------------------------------------------------*/
/* XSD production rules default values                               */
/*-------------------------------------------------------------------*/
#define REPLACE_MINUS 0         /* Replace - sign from cobol names   */
#define REPLACE_MINUS_CHAR '_'  /* Char to use to replace - sign     */
#define REMOVE_MINUS 1          /* Remove - sign from cobol names    */
#define UPPERCASE_TO_LOWER 1    /* Lower case chars from cobol name  */
#define FIRSTCHAR_UPPER 1       /* Uppercase first char and chars    
                                   following minus sign              */
#define TYPE_SUFFIX "Type"      /* Used to build complex type names  */
#define XS_PREFIX "xs"          /* Xml schema namespace prefix       */
#define XS_NS "http://www.w3.org/2001/XMLSchema" /* XML Schema
                                   namespace  */
#define XSNS_PREFIX "xsns"      /* Target namespace prefix           */
#define XSNS_NS "http://tempuri/schemagen" /* The output schema 
                                   namespace  */ 
#define XSCB_PREFIX "cb"        /* Xml schema Cobol appinfo          */
#define XSCB_NS "http://www.legsem.com/xml/ns/coxb" /* The Cobol 
                                    appinfo schema namespace  */ 
#define XSJAXB_PREFIX "jaxb"    /* Jaxb namespace prefix             */
#define XSJAXB_NS "http://java.sun.com/xml/ns/jaxb" /* Jaxb namespace*/ 
#define XSJAXB_VERSION "2.0"    /* Jaxb version                      */
#define XSJAXB_PACKAGE "com.legstar.test.coxb" /* Jaxb package name 
                                   used for generated classes        */
#define XSD_HEADER_FOOTER 1     /* Add schema definitions            */

/*-------------------------------------------------------------------*/
/* Structure describing the XSD production options in effect         */
/*-------------------------------------------------------------------*/
typedef struct {
  int replace_minus;       /* 1 = Replace - sign from cobol names    */
  char replace_minus_char; /* Char to use to replace - sign          */
  int remove_minus;        /* 1 = Remove - sign from cobol names     */
  int uppercase_to_lower;  /* 1= Comma is the decimal separator      */
  int firstchar_upper;     /* 1 = Uppercase first char and chars    
                                   following minus sign              */
  char type_suffix[MAX_TYPE_SUFFIX_LEN + 1]; /* Used to build complex 
                                            type names               */
  char xs_prefix[MAX_PFX_LEN + 1];/* Xml schema namespace prefix     */
  char xs_ns[MAX_NS_LEN + 1];     /* XML Schema namespace            */
  char xsns_prefix[MAX_PFX_LEN + 1]; /* Target namespace prefix      */
  char xsns_ns[MAX_NS_LEN + 1];   /* Target namespace                */
  char xscb_prefix[MAX_PFX_LEN + 1]; /* Schema Cobol appinfo prefi x */
  char xscb_ns[MAX_NS_LEN + 1];   /* Cobol appinfo schema namespace  */
  char xsjaxb_prefix[MAX_PFX_LEN + 1];/* Jaxb namespace prefix       */
  char xsjaxb_ns[MAX_NS_LEN + 1]; /* Jaxb namespace                  */
  char xsjaxb_version[MAX_VERSION_LEN + 1]; /* Jaxb version          */
  char xsjaxb_package[MAX_PACKAGE_LEN + 1]; /* Jaxb package used for
                                           generated classes         */
  int xsd_header_footer;   /* Add schema definitions                 */
} XSD_PRODUCTION_OPTIONS;

#endif /* _DEFINED_SDDXSDPRODUCE */

