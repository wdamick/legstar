//P390FXMC   JOB  (ACCT#),'FADY',
//*          RESTART=REFRESH,
//           MSGCLASS=X,NOTIFY=&SYSUID,PRTY=14,REGION=0M
//SBPROCS JCLLIB ORDER=CICSTS23.CICS.SDFHPROC
//CMPCON  EXEC DFHYITDL,PROGLIB='P390.DEV.LOAD',
//             LE370HLQ=CEE,
//             INDEX='CICSTS23.CICS',
//             CPARM='RENT,SOURCE,LIST,OFFSET,LONGNAME,DLL',
//             PPARM=DLLNAME(LSHTTAPI),
//             COMPHLQ=CBC,
//             OUTC='*'
//*
//*-------------------------------------------------------
//* CICS TRANSLATION SUBSTITUTIONS
//*-------------------------------------------------------
//TRN.SYSIN DD DISP=SHR,DSN=P390.DEV.C(LSHTTAPI)
//*-------------------------------------------------------
//* COMPILE SUBSTITUTIONS
//*-------------------------------------------------------
//C.SYSLIB  DD
// DD
// DD
// DD
// DD
// DD DISP=SHR,DSN=P390.DEV.H
//*-------------------------------------------------------
//* PRE-LINK SUBSTITUTIONS
//*-------------------------------------------------------
//PLKED.SYSIN  DD
//             DD DISP=SHR,DSN=P390.DEV.DECK(LSCOMLOG)
//             DD DISP=SHR,DSN=P390.DEV.DECK(LSHTTLIB)
//PLKED.SYSLIB DD DISP=SHR,DSN=P390.DEV.OBJ
//PLKED.SYSDEFSD DD DSN=P390.DEV.DECK(LSHTTAPI),DISP=SHR
//*-------------------------------------------------------
//* LINK SUBSTITUTIONS
//*-------------------------------------------------------
//LKED.SYSLIN DD
//     DD DSN=*.PLKED.SYSMOD,DISP=(OLD,PASS)
//     DD *
/*
//LKED.SYSLIB DD
//     DD
//     DD DISP=SHR,DSN=P390.DEV.LOAD
//LKED.SYSLMOD DD DSN=P390.DEV.LOAD(LSHTTAPI),DISP=SHR
//COPYOBJT EXEC PGM=IEBGENER
//SYSUT1   DD DSN=*.CMPCON.PLKED.SYSMOD,DISP=(OLD,DELETE)
//SYSUT2   DD DSN=P390.DEV.OBJ(LSHTTAPI),DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//
