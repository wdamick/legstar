//P390FXMC   JOB  (ACCT#),'FADY',
//*          RESTART=REFRESH,
//           MSGCLASS=X,NOTIFY=&SYSUID,PRTY=14,REGION=0M
//*---------------------------------------------------------------------*
//* C COMPILATION                                                       *
//*---------------------------------------------------------------------*
//C        EXEC PGM=CBCDRVR,REGION=4M,
//         PARM='RENT,SOURCE,NOSEARCH,OFFSET,LIST,DEFINE(MVS),DLL'
//STEPLIB  DD DSN=CBC.SCBCCMP,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//SYSIN DD DISP=SHR,DSN=P390.DEV.C(LSHTTLIB)
//SYSLIB   DD DISP=SHR,DSN=TCPIP.SEZACMAC
//         DD DISP=SHR,DSN=P390.DEV.H
//         DD DISP=SHR,DSN=CEE.SCEEH.H
//SYSLIN   DD DSN=&&LOAD,DISP=(,PASS),
//            UNIT=SYSDA,SPACE=(3200,(30,30)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)
//SYSPRINT DD SYSOUT=*
//SYSCPRT  DD SYSOUT=*
//SYSTERM  DD DUMMY
//SYSUT1   DD DSN=&&SYSUT1,DISP=(,PASS),
//            UNIT=SYSDA,SPACE=(3200,(30,30)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)
//SYSUT2   DD DSN=&&SYSUT2,DISP=(,PASS),
//            UNIT=SYSDA,SPACE=(3200,(30,30)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)
//SYSUT3   DD DSN=&&SYSUT3,DISP=(,PASS),
//            UNIT=SYSDA,SPACE=(3200,(30,30)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)
//SYSUT4   DD DSN=&&SYSUT4,DISP=(,PASS),
//            UNIT=SYSDA,SPACE=(3200,(30,30)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)
//SYSUT5   DD DSN=&&SYSUT5,DISP=(,PASS),
//            UNIT=SYSDA,SPACE=(3200,(30,30)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)
//SYSUT6   DD DSN=&&SYSUT6,DISP=(,PASS),
//            UNIT=SYSDA,SPACE=(3200,(30,30)),
//            DCB=(RECFM=FB,LRECL=3200,BLKSIZE=12800)
//SYSUT7   DD DSN=&&SYSUT7,DISP=(,PASS),
//            UNIT=SYSDA,SPACE=(3200,(30,30)),
//            DCB=(RECFM=FB,LRECL=3200,BLKSIZE=12800)
//SYSUT8   DD DSN=&&SYSUT8,DISP=(,PASS),
//            UNIT=SYSDA,SPACE=(3200,(30,30)),
//            DCB=(RECFM=FB,LRECL=3200,BLKSIZE=12800)
//SYSUT9   DD DSN=&&SYSUT9,DISP=(,PASS),UNIT=SYSDA,
//         SPACE=(3200,(30,30)),DCB=(RECFM=VB,LRECL=137,BLKSIZE=882)
//SYSUT10  DD DUMMY
//SYSUT14  DD DSN=&&SYSUT14,DISP=(,PASS),
//            UNIT=SYSDA,SPACE=(3200,(30,30)),
//            DCB=(RECFM=FB,LRECL=3200,BLKSIZE=12800)
//*
//*---------------------------------------------------------------------*
//* PRE-LINK                                                            *
//*---------------------------------------------------------------------*
//PLKED    EXEC PGM=EDCPRLK,COND=(7,LT,C),
//         REGION=4M,PARM='DLLNAME(LSHTTLIB)'
//STEPLIB  DD DSN=CEE.SCEERUN,DISP=SHR
//         DD DSN=CBC.SCBCCMP,DISP=SHR
//SYSMSGS  DD DSN=CEE.SCEEMSGP(EDCPMSGE),DISP=SHR
//SYSLIB   DD DISP=SHR,DSN=P390.DEV.LOAD
//         DD DISP=SHR,DSN=TCPIP.SEZARNT1
//SYSIN    DD DSN=*.C.SYSLIN,DISP=(OLD,DELETE)
//SYSMOD   DD DSN=&&PLNK,DISP=(,PASS),
//            UNIT=SYSDA,SPACE=(3200,(30,30)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSDEFSD DD DISP=SHR,DSN=P390.DEV.DECK(LSHTTLIB)
//*
//*---------------------------------------------------------------------*
//* LINK-EDIT                                                           *
//*---------------------------------------------------------------------*
//LKED     EXEC PGM=IEWL,REGION=4M,
//         PARM='RENT,LIST,MAP,LET,XREF',
//         COND=((7,LT,C),(7,LT,PLKED))
//SYSLIB   DD DSN=CEE.SCEELKED,DISP=SHR
//         DD DISP=SHR,DSN=P390.DEV.LOAD
//         DD DISP=SHR,DSN=TCPIP.SEZATCP
//         DD DISP=SHR,DSN=TCPIP.SEZACMTX
//SYSLIN   DD DSN=*.PLKED.SYSMOD,DISP=(OLD,PASS)
//         DD DDNAME=SYSIN
//SYSLMOD  DD DSN=P390.DEV.LOAD(LSHTTLIB),DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 INCLUDE SYSLIB(EZACIC17)
 NAME LSHTTLIB(R)
/*
//*---------------------------------------------------------------------*
//* SAVE OBJECT MODULE                                                  *
//*---------------------------------------------------------------------*
//COPYOBJT EXEC PGM=IEBGENER,
//         COND=((7,LT,C),(7,LT,PLKED),(7,LT,LKED))
//SYSUT1   DD DSN=*.PLKED.SYSMOD,DISP=(OLD,DELETE)
//SYSUT2   DD DSN=P390.DEV.OBJ(LSHTTLIB),DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*
