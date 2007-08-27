#ifndef _DEFINED_SCOBDDANALYZE 
#define _DEFINED_SCOBDDANALYZE
/*====================================================================*/
/*                                                                    */
/* MODULE NAME = COBDDANALYZE                                        */
/*                                                                    */
/*--------------------------------------------------------------------*/

#define MAX_SYMBOL_OCCURENCE 18 /* Maximum number of consecutive
                                  occurences of a symbol in a 
                                  PICTURE clause                     */
/*-------------------------------------------------------------------*/
/* Counters used is parsing the PICTURE clause                       */
/*-------------------------------------------------------------------*/
typedef struct {
    int nA; /* Number of A in the picture clause */
    int nEntB; /* Number of B in the entire part */
    int nFraB; /* Number of B in the fractional part */
    int nE; /* Number of E in the picture clause */
    int nG; /* Number of G in the picture clause */
    int nN; /* Number of N in the picture clause */
    int nEntP; /* Number of P in the entire part */
    int nFraP; /* Number of P in the fractional part */
    int nS; /* Number of S in the picture clause */
    int nV; /* Number of V in the picture clause */
    int nX; /* Number of X in the picture clause */
    int nEntZ; /* Number of Z in the entire part */
    int nFraZ; /* Number of Z in the fractional part */
    int nEnt9; /* Number of 9 in the entire part */
    int nFra9; /* Number of 9 in the fractional part */
    int nEnt0; /* Number of 0 in the entire part  */
    int nFra0; /* Number of 0 in the fractional part  */
    int nEntPlus; /* Number of + in the entire part */
    int nFraPlus; /* Number of + in the fractional part */
    int nEntMinus; /* Number of - in the entire part */
    int nFraMinus; /* Number of - in the fractional part */
    int nEntAstx; /* Number of * in the entire part */
    int nFraAstx; /* Number of * in the fractional part */
    int nEntCs; /* Number of currency symbols in the entire part */
    int nFraCs; /* Number of currency symbols in the fractional part */
    int nSlash; /* Number of / in the picture clause */
    int nComma; /* Number of , in the picture clause */
    int nPoint; /* Number of . in the picture clause */
    int nCR; /* Number of CR in the picture clause */
    int nDB; /* Number of DB in the picture clause */
} SYMBOL_COUNTS;


#endif /* _DEFINED_SCOBDDANALYZE */

