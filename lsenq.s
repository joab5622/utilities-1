*PROCESS ALIGN,NOCOMPAT,                                             
*PROCESS DXREF,FLAG(ALIGN,CONT,RECORD)
*PROCESS NOFOLD,NOINFO,PC(ON,DATA,GEN,MCALL),RENT,
*PROCESS RA2,NORLD,MXREF,RXREF,USING(MAP,WARN(13))
*PROCESS TYPECHECK(NOMAGNITUDE,REGISTER)
*WARNING - THIS PROGRAM REQUIRES THE HIGH-LEVEL ASSEMBLER
*          AS WELL AS LE/370
*          THIS PROGRAM IS RE-ENTRANT.
         PUSH  PRINT
         PRINT NOGEN
         IEABRCX DEFINE
         IEABRCX DISABLE
         IEABRCX ENABLE
_BALR    OPSYN BALR
BALR     OPSYN BASR
         POP   PRINT
LSENQ    CEEENTRY PPA=LSENQ_PPA,                                       X
               MAIN=YES,                                               X
               AUTO=DSASIZE,                                           X
               BASE=R11_32
         USING CEECAA,R12_32
         USING CEEDSA,R13_32
         J     GO
GOBACK   DS    0H
         CEETERM RC=RETURN_CODE,                                       X
               MF=(E,CEETERM_BLOCK)
GO       DS    0H
         LR    R9_32,R1_32        SAVE R1 UPON ENTRY
         USING PARMS,R9_32
         LARL  R15_32,DO_OCTAL
         LARL  R15_32,DO_HEX  
         ST    R15_32,ENCODE@
         LA    R1_32,CALLX
         L     R15_32,CEE3INF
         CALL  (15),(SYS,ENV,MEMBER,GPID,FC),VL,                       X
               MF=(E,(1))
         LHI   R15_32,1
         ST    R15_32,MODIFIER
         LHI   R7_32,16 
         TM    SYS,X'02'          UNIX?
         JNO   INITERR            NO
         LOAD  EP=BPX1WRT
         ST    R0_32,BPX1WRT
         LOAD  EP=BPX1WRV
         ST    R0_32,BPX1WRV
         MVC   GQLST(GQPARML),GQPARM
         MVI   TAB,X'05'
         LA    R0_32,TAB
         ST    R0_32,TAB@
         LHI   R0_32,1
         ST    R0_32,TABL
         BCTR  R7_32,0            15
         L     R2_32,@ARGC
         L     R3_32,@ARGVL
         L     R4_32,@ARGV
         ST    R2_32,MODIFIER
         L     R2_32,0(,R2_32)
         CHI   R2_32,3
         JH    INITERR
         BCTR  R7_32,0            14
         CHI   R2_32,2
         JL    INITERR
         BCTR  R7_32,0            13
         L     R6_32,4(,R4_32)    &ARGV[1]
         LT    R5_32,4(,R3_32)    &LENGTH OF ARG[1]
         JZ    INITERR            ZERO IS NOT GOOD
         BCTR  R7_32,0            12
         LT    R5_32,0(,R5_32)    LENGTH OF ARG[1]
         JZ    INITERR            ZERO IS NOT GOOD
         BCTR  R7_32,0            11
         BCTR  R5_32,0            DON'T COUNT TRAILING NULL
         LTR   R5_32,R5_32        BETTER HAVE SOME ACTUAL DATA!
         JZ    INITERR            ZERO IS NOT GOOD
         MVC   QNAME,=CL8'SYSDSN'
         CHI   R2_32,2            ONLY RNAME SPECIFIED?
         JE    DORNAME            YES
         BCTR  R7_32,0            10
         CHI   R5_32,8            MAX OF 8
         JH    INITERR            BUT IT ISN'T
         BCTR  R5_32,0
         MVC   QNAME,=CL8' '
         EX    R5_32,CP_QNAME
         L     R6_32,8(,R4_32)    &ARGV[2]          
         LT    R5_32,8(,R3_32)    &LENGTH OF &ARGV[2]
         JZ    INITERR            CAN'T BE ZERO!
         BCTR  R7_32,0            9 
         LT    R5_32,0(,R5_32)    LENGTH OF ARG[2]
         JZ    INITERR            ZERO IS NOT GOOD
         BCTR  R7_32,0            8
         BCTR  R5_32,0            DON'T COUNT TRAILING NULL
         LTR   R5_32,R5_32        BETTER HAVE SOME ACTUAL DATA!
         JZ    INITERR            ZERO IS NOT GOOD
DORNAME  DS    0H
*
* AT THIS POINT R6 POINTS TO EITHER ARGV[1] OR
* ARGV[2], DEPENDING ON THE NUMBER OF ARGUMENTS
* USED.
* IF ARGC==2, THEN R6=&ARGV[1] R5=ARGVL[1]
* IF ARGC==3, THEN R6=&ARGV[2] R5=ARGVL[2]
         CHI   R5_32,255          CHECK AGAINST MAX LENGTH
         JH    INITERR            TOO LONG
         BCTR  R7_32,0            7
         ST    R5_32,RNAME_LEN
         BCTR  R5_32,0            DECREMENT BY 1
         EX    R5_32,CP_RNAME
*        CHI   R2_32,2            QNAME SPECIFIED?
*        JNE   OK1                YES, ALREADY DONE
*        MVI   CONTENTION,1
*        CLC   =X'839695A38595A389969500',0(R6_32)
*        JE    OK1
*        MVI   CONTENTION,0
*
* FOR CONVIENCE, I WILL FORCE RNAMES TO UPPER CASE IF    
* THE QNAME IS DEFAULTED TO SYSDSN
*        CLC   QNAME,=CL8'SYSDSN'
*        JNE   OK1
         CHI   R2_32,2
         JNE   OK1
         L     R5_32,RNAME_LEN
         BCTR  R5_32,0
         EX    R5_32,TR_RNAME
OK1      DS    0H
         LA    R8_32,QNAMEX       RECEIVING FIELD
         ST    R8_32,FIELD1@
         LHI   R9_32,8
         LA    R10_32,QNAME
*        AGO   .DOWRT1
         BRASL R14_32,DO_ENCODE
         SL    R8_32,FIELD1@
         ST    R8_32,FIELD1L
         chi   r8_32,8*4
         jh    *+2
*        STM   R8_32,R9_32,FIELD1@
         L     R0_32,TAB@
         L     R1_32,TABL
         STM   R0_32,R1_32,TAB1
*        LA    R0_32,RNAME
*        L     R1_32,RNAME_LEN
*        STM   R0_32,R1_32,FIELD2@
         LA    R8_32,RNAMEX
         ST    R8_32,FIELD2@
         L     R9_32,RNAME_LEN
         LA    R10_32,RNAME
         BRASL R14_32,DO_ENCODE
         SL    R8_32,FIELD2@
         ST    R8_32,FIELD2L
         chi   r8_32,255*4
         jh    *+2
         LM    R0_32,R1_32,=A(NL,1)
         STM   R0_32,R1_32,TAB2
         LHI   R0_32,4
         ST    R0_32,IOV_COUNT
         LA    R1_32,CALLX
         L     R15_32,BPX1WRV
         CALL  (15),                                                   X
               (FD2,IOV_COUNT,IOV_STRUC,                               X
               IOV_ALET,IOV_BUFFER_ALET,                               X
               RETURN_VALUE,RETURN_CODE,REASON_CODE),VL,               X
               MF=(E,(1))
         ago   .nowrt1
.DOWRT1  ANOP
         ST    R0_32,BUFFERA
         MVI   QNAME+L'QNAME,X'05' Place TAB in output
         MVC   QNAME+L'QNAME(1),TAB Place separator in output
         L     R5_32,RNAME_LEN
         LA    R5_32,RNAME(R5_32)
         MVI   0(R5_32),X'15'     Place NEL in output
         SLR   R5_32,R4_32
         AHI   R5_32,1
         ST    R5_32,COUNT
         L     R15_32,BPX1WRT
         LA    R1_32,CALLX
         CALL  (15),(FD2,BUFFERA,ALET,COUNT,                           X
               RETVAL,                                                 X
               RETCODE,                                                X
               RSNCODE),VL,                                            X
               MF=(E,(1))
         AGO   .NOWRT2
.nowrt1  anop
.NOWRT2  ANOP
         L     R2_32,STORAGE_LENGTH
         STORAGE OBTAIN,                                               X
               LENGTH=(2),                                             X
               SP=10,                                                  X
               BNDRY=PAGE
         ST    R1_32,RETURN@
         MVC   0(4,R1_32),STORAGE_LENGTH
         LA    R1_32,CALLX
         L     R15_32,ISGQUERY
         CALL  (15),                                                   X
               (QNAME,RNAME,RNAME_LEN+3,GQLST,RETURN@),                X
               VL,                                                     X
               MF=(E,(1))
         ST    R15_32,RETURN_CODE
         LTR   R15_32,R15_32
         JZ    GOOD1
         CHI   R15_32,4           RC==4?
         JNE   ERROR1             NO
         STM   R15_32,R0_32,F_RC
*
* IF RC==4 AND MODIFIER=X'....0404' THIS MEANS
* THAT NOTHING WAS FOUND. THIS IS FINE.
         CLC   =X'0404',F_MODIFIER+2
         JNE   ERROR1
         XC    RETURN_CODE,RETURN_CODE
         J     GOOD2
ERROR1   DS    0H
         LA    R1_32,CALLX
         LM    R3_32,R4_32,F_RC
         CVD   R15_32,D_RC
         CVD   R0_32,D_MODIFIER
         L     R2_32,RETURN@
         LA    R2_32,8(,R2_32)
         ST    R2_32,BUFFERA
         L     R15_32,SPRINTF
         CALL  (15),                                                   X
               ((2),FORMAT,(3),(4)),                                   X
               MF=(E,(1))
         ST    R15_32,COUNT
         L     R15_32,BPX1WRT
         LA    R1_32,CALLX
         CALL  (15),(FD2,BUFFERA,ALET,COUNT,                           X
               RETVAL,                                                 X
               RETCODE,                                                X
               RSNCODE),VL,                                            X
               MF=(E,(1))
         J     GOOD2
GOOD1    DS    0H
         L     R2_32,RETURN@
         LA    R3_32,8(,R2_32)
         USING RETURN_STRUC,R3_32
*        LM    R0_32,R1_32,=A(TAB,1)
         L     R0_32,TAB@
         L     R1_32,TABL
         STM   R0_32,R1_32,TAB1
         STM   R0_32,R1_32,TAB2
         STM   R0_32,R1_32,TAB3
         STM   R0_32,R1_32,TAB4
         STM   R0_32,R1_32,TAB5
         STM   R0_32,R1_32,TAB6
         STM   R0_32,R1_32,TAB7
         STM   R0_32,R1_32,TAB8
         LM    R0_32,R1_32,=A(NL,1)
         STM   R0_32,R1_32,NL1
PRINT    DS    0H
         CL    R3_32,4(,R2_32)
         JNL   END_PRINT
*        LHI   R0_32,8
*        LA    R1_32,RTN_QNAME
*        ST    R1_32,FIELD1@
*        ST    R0_32,FIELD1L
         LA    R8_32,QNAMEX
         ST    R8_32,FIELD1@
         LHI   R9_32,8
         LA    R10_32,RTN_QNAME
         BRASL R14_32,DO_ENCODE
         LHI   R0_32,8
         SL    R8_32,FIELD1@
         ST    R8_32,FIELD1L
         chi   r8_32,8*4  
         jh    *+2
         LA    R1_32,RTN_JOBNAME
         ST    R1_32,FIELD3@ 
         ST    R0_32,FIELD3L 
         LA    R1_32,RTN_EXCSHR
         ST    R1_32,FIELD5@
         ST    R0_32,FIELD5L 
         LA    R1_32,RTN_SYSNAME
         ST    R1_32,FIELD4@ 
         ST    R0_32,FIELD4L 
         LA    R1_32,RTN_STATUS
         ST    R1_32,FIELD6@
         ST    R0_32,FIELD6L
         LA    R1_32,RTN_SCOPE
         ST    R1_32,FIELD7@
         ST    R0_32,FIELD7L
         UNPK  D_RC(9),RTN_TCB(5)
         TR    D_RC(8),TOHEX-X'F0'
         LA    R1_32,D_RC
         ST    R1_32,FIELD8@
         ST    R0_32,FIELD8L
         UNPK  F_RC(5),RTN_ASID(3)
         TR    F_RC(4),TOHEX-X'F0'
         LA    R1_32,F_RC
         ST    R1_32,FIELD9@
         MVC   FIELD9L,=F'4'
*        MVC   FIELD2L,RTN_RLEN
*        LA    R1_32,RTN_RNAME
*        ST    R1_32,FIELD2@
         LA    R8_32,RNAMEX
         ST    R8_32,FIELD2@
         L     R9_32,RTN_RLEN
         LA    R10_32,RTN_RNAME
         BRASL R14_32,DO_ENCODE
         LHI   R0_32,8
         SL    R8_32,FIELD2@
         ST    R8_32,FIELD2L
         MVC   IOV_COUNT,=A(IOV_STRUC_L/8)
         L     R10_32,IOV_COUNT
         chi   r8_32,255*4
         jh    *+2
         chi   r10_32,iov_struc_l/8
         jh    *+2
*        la r1_32,iov_struc
*        la r2_32,96(,r1_32)
*        la r3_32,96(,r2_32)
*        la r4_32,96(,r3_32)
*        la r5_32,96(,r4_32)
*        ex 0,*
         LA    R1_32,CALLX
         L     R15_32,BPX1WRV
         CALL  (15),                                                   X
               (FD1,IOV_COUNT,IOV_STRUC,                               X
               IOV_ALET,IOV_BUFFER_ALET,                               X
               RETURN_VALUE,RETURN_CODE,REASON_CODE),VL,               X
               MF=(E,(1))
         LA    R3_32,RTN_NEXT
         J     PRINT
         DROP  R3
END_PRINT DS   0H
         AGO   .NODUMP2
DUMP     DS    0H
         LA    R1_32,CALLX
         L     R15_32,CEE3DMP
         CALL  (15),                                                   X
               (TITLE,OPTIONS,FC),                                     X
               VL,                                                     X
               MF=(E,(1))
.NODUMP2 ANOP
GOOD2    DS    0H
         LT    R1_32,RETURN@
         JZ    NOFREE
         L     R2_32,0(,R1)
         STORAGE RELEASE,                                              X
               LENGTH=(2),                                             X
               SP=10,                                                  X
               ADDR=(1)  
NOFREE   DS    0H
         SLR   R15_32,R15_32
         J     GOBACK
INITERR  DS    0H
         ST    R7_32,RETURN_CODE
         CHI   R7_32,16           NOT UNIX?
         JNE   BADPARMS           BAD PARMS
* I CAN'T ISSUE A MESSAGE, SO I GUESS A WTO IS NEEDED
         WTO   'LSENQ NEEDS A UNIX ENVIRONMENT',                       X
               ROUTCDE=11,                                             X
               DESC=7
BADPARMS DS    0H
         LA    R2_32,ERRMSG1                          
         ST    R2_32,BUFFERA      MESSAGE HERE
         LA    R1_32,CALLX
         L     R15_32,SPRINTF
*
* Use sprintf to make a nice looking message.
         CALL  (15),                                                   X
               ((2),ERRFMT1,(7)),                                      X
               MF=(E,(1))
         ST    R15_32,COUNT
         L     R15_32,BPX1WRT
         LA    R1_32,CALLX
         CALL  (15),(FD2,BUFFERA,ALET,COUNT,                           X
               RETVAL,                                                 X
               RETCODE,                                                X
               RSNCODE),VL,                                            X
               MF=(E,(1))
         J     GOBACK
DO_ENCODE DS   0H
         L     R15_32,ENCODE@
         BR    R15_32
DO_HEX   DS    0H
         STM   R14_32,R12_32,12(R13_32)
         CHI   R9_32,255
         JH    *+2
         LA    R1_32,PRNTABLE     POINT TO PRINT TABLE
         SLR   R0_32,R0_32        TEST CHAR IS X'00'
H_TROO   TROO  R8_32,R10_32,0     MOVE AND TEST
         JO    H_TROO             LOOP ON CC=3
         JE    H_TROOE            FINISHED!
*
* TRANSLATE TO HEXADECIMAL
*        LHI   R3_32,C'\x'
*        STH   R3_32,0(R8_32)
         MVC   0(3,R8_32),=C'\0x'
         SLR   R3_32,R3_32
         UNPK  3(3,R8_32),0(2,R10_32)
         TR    3(2,R8_32),TOHEX-240
         LA    R10_32,1(,R10_32)
         LA    R8_32,5(,R8_32)
         LA    R1_32,PRNTABLE
         BCTR  R9_32,0
         LTR   R9_32,R9_32
         JP    H_TROO
H_TROOE  DS    0H
         ST    R8_32,8*4+20(,R13_32)
         LM    R14_32,R12_32,12(R13_32) 
         BR    R14_32
DO_OCTAL DS    0H
         STM   R14_32,R12_32,12(R13_32)
         CHI   R9_32,255
         JH    *+2
         LA    R1_32,PRNTABLE     POINT TO PRINT TABLE
         SLR   R0_32,R0_32        TEST CHAR IS X'00'
O_TROO   TROO  R8_32,R10_32,0     MOVE AND TEST
         JO    O_TROO             LOOP ON CC=3
         JE    O_TROOE            FINISHED!
*
* TRANSLATE TO OCTAL
         SLR   R3_32,R3_32
         IC    R3_32,0(,R10_32)   LOAD BYTE
         MVI   0(R8_32),C'\'      PLACE ESCAPE CHAR IN OUTPUT
         STC   R3_32,3(,R8_32)
         NI    3(R8_32),X'07'
         OI    3(R8_32),X'F0'
         SRL   R3_32,3
         STC   R3_32,2(,R8_32)
         NI    2(R8_32),X'07'
         OI    2(R8_32),X'F0'
         SRL   R3_32,3
         STC   R3_32,1(,R8_32)
         NI    1(R8_32),X'07'
         OI    1(R8_32),X'F0'
         LA    R10_32,1(,R10_32)
         LA    R8_32,4(,R8_32)
         LA    R1_32,PRNTABLE
         BCTR  R9_32,0
         LTR   R9_32,R9_32
         JP    O_TROO
O_TROOE  DS    0H
         ST    R8_32,8*4+20(,R13_32)
         LM    R14_32,R12_32,12(R13_32) 
         BR    R14_32
STORAGE_LENGTH DC A(4*1024*1024)
FD1      DC    F'1'
FD2      DC    F'2'
ALET     DC    A(0)
IOV_ALET EQU   ALET
IOV_BUFFER_ALET EQU ALET
CEE3INF  DC    V(CEE3INF)
SPRINTF  DC    V(SPRINTF)
CEE3DMP  DC    V(CEE3DMP)
ISGQUERY DC    V(ISGQUERY)
CP_QNAME MVC   QNAME(0),0(R6_32)
CP_RNAME MVC   RNAME(0),0(R6_32)
TR_RNAME TR    RNAME(0),TOUPPER
TOHEX    DC    C'0123456789ABCDEF'
FORMAT   DC    C'ISGQUERY Return code=%i Modifier=%#8x',X'1500'
ERRFMT1  DC    C'LSENQ Parameter error. code=%i',X'1500'
NL       DC    X'15'
TITLE    DC    CL80'LSENQ DUMP'   
OPTIONS  DC    CL255'BLOCKS,STORAGE,REGST(256),GENOPTS' 
TOUPPER  DC    256AL1(*-TOUPPER)
         ORG   TOUPPER+X'81'
         DC    C'ABCDEFGHI'
         ORG   TOUPPER+X'91'
         DC    C'JKLMNOPQR'
         ORG   TOUPPER+X'A2'
         DC    C'STUVWXYZ'
         ORG
         DS    0D
PRNTABLE PRNTABLE abcdefghijklmnopqrstuvwxyz  
PRNTABLE PRNTABLE ABCDEFGHIJKLMNOPQRSTUVWXYZ  
PRNTABLE PRNTABLE 0123456789                  
PRNTABLE PRNTABLE -_.+=()@#$
*        ORG   PRNTABLE+C' '
*        DC    C' '
         ORG
         ISGQUERY MF=(L,GQPARM)                                         
         LTORG *
LSENQ_PPA CEEPPA LIBRARY=NO,                                           X
               PPA2=YES,                                               X
               EXTPROC=YES,                                            X
               TSTAMP=YES,                                             X
               PEP=YES,                                                X
               INSTOP=NO,                                              X
               EXITDSA=NO,                                             X
               OWNEXM=YES,                                             X
               EPNAME=LSENQ,                                           X
               VER=1,                                                  X
               REL=1,                                                  X
               MOD=0,                                                  X
               DSA=YES
         CEEDSA
* DYNAMIC AREA IS DEFINED HERE.
* THIS IS WITHIN A DSECT, SO NO DATA IS REALLY INITIALIZED
         DS    0D                 FORCE DOUBLEWORD
NL_CATD  DS    A
ENCODE@  DS    A
D_RC     DS    D
D_MODIFIER DS  D
F_RC     DS    F
F_MODIFIER DS  F
RETURN@  DS    A
GQLST    DS    (GQPARML)X
BPX1WRT  DC    A(0)               DYNAMICALLY LOADED
BPX1WRV  DC    A(0)               DYNAMICALLY LOADED
COUNT    DS    F
BUFFERA  DS    A
RETVAL   DS    A(0)
RETCODE  DS    A(0)
RSNCODE  DS    A(0)
QNAME@   DS    A
RNAME@   DS    A
SYS      DS    F
* BIT MEANING
*    0 X'80'
*    CURRENTLY EXECUTING IN THE CICS ENVIRONMENT
*    1 X'40'
*    CURRENTLY EXECUTING IN A CICS_PIPI ENVIRONMENT
*    2-3 X'30'
*    RESERVED FOR OTHER SPECIFIC CICS ENVIRONMENTS
*    4 X'10'
*    CURRENTLY EXECUTING IN A TSO ENVIRONMENT
*    5 X'08'
*    CURRENTLY EXECUTING IN A BATCH ENVIRONMENT
*    6 X'04'
*    CURRENTLY EXECUTING IN A Z/OS UNIX ENVIRONMENT
*    7-28
*    RESERVED FOR FUTURE USE
*    29
*    CURRENTLY EXECUTING ON Z/VSE(TM)
*    30
*    CURRENTLY EXECUTING ON Z/OS
*    31
*    PREVIOUSLY INDICATED AS EXECUTING ON Z/OS.E
*
ENV      DS    F
*
MEMBER   DS    F
*
GPID     DS    F
FC       DS    F
BUFL     DS    F
RETURN_CODE DS F
MODIFIER DS    F
RETURN_VALUE DC A(0)               PREVIOUS DEFAULT DUB
REASON_CODE DC A(0)
CEETERM_BLOCK CEETERM MF=L
CALLX    CALL  ,(0,                                                    X
               0,                                                      X
               0,                                                      X
               0,                                                      X
               0,                                                      X
               0,                                                      X
               0,                                                      X
               0,                                                      X
               0,                                                      X
               0,                                                      X
               0,                                                      X
               0),VL,MF=L
IOV_COUNT DS   F
RNAME_LEN DS   F
TAB@     DS    A
TABL     DS    F
QNAME    DS    CL8
RNAME    DS    CL255
QNAMEX   DS    6CL(L'QNAME)
RNAMEX   DS    6CL(L'RNAME)
         ORG   QNAME
ERRMSG1  DS    CL(20+L'ERRFMT1)
         ORG
IOV_STRUC DS   0D
FIELD1@  DS    A
FIELD1L  DS    F
TAB1     DS    2F
FIELD2@  DS    A
FIELD2L  DS    F
TAB2     DS    2F
FIELD3@  DS    A
FIELD3L  DS    F
TAB3     DS    2F
FIELD4@  DS    A
FIELD4L  DS    F
TAB4     DS    2F
FIELD5@  DS    A
FIELD5L  DS    F
TAB5     DS    2F
FIELD6@  DS    A
FIELD6L  DS    F
TAB6     DS    2F
FIELD7@  DS    A
FIELD7L  DS    F
TAB7     DS    2F
FIELD8@  DS    A
FIELD8L  DS    F
TAB8     DS    2F
FIELD9@  DS    A
FIELD9L  DS    F
NL1      DS    2F
IOV_STRUC_L EQU *-IOV_STRUC
CONTENTION DS  X
TAB      DC    X'05'
DSASIZE  EQU   *-CEEDSA
         BPXYCONS DSECT=YES,LIST=YES
         BPXYIOV
         CEECAA
         ISGYQUAA
         ISGYCON
         ISGRNLE
PARMS    DSECT
@ARGC    DS    A                  ADDRESS OF NUMBER OF ARGUMENTS
@ARGVL   DS    A                  ADDRESS OF VECTOR OF LENGTH OF ARGS
@ARGV    DS    A                  ADDRESS OF VECTOR OF ARGS
@ENVL    DS    A                  ADDRESS OF NUMBER OF ENV VARS
@ENVVL   DS    A                  ADDRESS OF VECTOR OF LENGTH OF ENV VA
@ENV     DS    A                  ADDRESS OF VECTOR OF ENV VARS
RETURN_STRUC DSECT
RTN_QNAME DS   CL8                ISGYQUAARSQNAME
RTN_JOBNAME DS CL8                ISGYQUAARQXJOBNAME
RTN_RLEN DS    F                  ISGYQUAARSRNAMELEN
RTN_SYSNAME DS CL8                ISGYQUAARQXSYSNAME
RTN_EXCSHR DS  CL8                ISGYQUAARQFLAGS1
*ISGYQUAARQCONTROL EQ X'80'       ON-SHARED
*ISGYQUAARQOWNER   EQ X'08'
*ISGYQUAARQMATU    EQ X'04'
RTN_STATUS DS  CL8
RTN_SCOPE  DS  CL8                ISGYQUAARSSCOPE
* ISGYQUAA_KSTEP
* ISGYQUAA_KSYSTEM
* ISGYQUAA_KSYSPLEX
RTN_TCB  DS    A
RTN_ASID DS    H
RTN_RNAME DS   CL255
RTN_NEXT EQU   *
RTN_LEN  EQU   *-RTN_QNAME
         regs
         AGO   .SKIP1
R0       EQU   0,,,,GR                                                          
R1       EQU   1,,,,GR                                                          
R2       EQU   2,,,,GR                                                          
R3       EQU   3,,,,GR                                                          
R4       EQU   4,,,,GR                                                          
R5       EQU   5,,,,GR                                                          
R6       EQU   6,,,,GR                                                          
R7       EQU   7,,,,GR                                                          
R8       EQU   8,,,,GR                                                          
R9       EQU   9,,,,GR                                                          
R10      EQU   10,,,,GR                                                         
R11      EQU   11,,,,GR                                                         
R12      EQU   12,,,,GR                                                         
R13      EQU   13,,,,GR                                                         
R14      EQU   14,,,,GR                                                         
R15      EQU   15,,,,GR                                                         
R0_64    EQU   0,,,,GR64                                                        
R1_64    EQU   1,,,,GR64                                                        
R2_64    EQU   2,,,,GR64                                                        
R3_64    EQU   3,,,,GR64                                                        
R4_64    EQU   4,,,,GR64                                                        
R5_64    EQU   5,,,,GR64                                                        
R6_64    EQU   6,,,,GR64                                                        
R7_64    EQU   7,,,,GR64                                                        
R8_64    EQU   8,,,,GR64                                                        
R9_64    EQU   9,,,,GR64                                                        
R10_64   EQU   10,,,,GR64                                                       
R11_64   EQU   11,,,,GR64                                                       
R12_64   EQU   12,,,,GR64                                                       
R13_64   EQU   13,,,,GR64                                                       
R14_64   EQU   14,,,,GR64                                                       
R15_64   EQU   15,,,,GR64                                                       
R0_32    EQU   0,,,,GR32                                                        
R1_32    EQU   1,,,,GR32                                                        
R2_32    EQU   2,,,,GR32                                                        
R3_32    EQU   3,,,,GR32                                                        
R4_32    EQU   4,,,,GR32                                                        
R5_32    EQU   5,,,,GR32                                                        
R6_32    EQU   6,,,,GR32                                                        
R7_32    EQU   7,,,,GR32                                                        
R8_32    EQU   8,,,,GR32                                                        
R9_32    EQU   9,,,,GR32                                                        
R10_32   EQU   10,,,,GR32                                                       
R11_32   EQU   11,,,,GR32                                                       
R12_32   EQU   12,,,,GR32                                                       
R13_32   EQU   13,,,,GR32                                                       
R14_32   EQU   14,,,,GR32                                                       
R15_32   EQU   15,,,,GR32                                                       
AR0      EQU   0,,,,AR                                                          
AR1      EQU   1,,,,AR                                                          
AR2      EQU   2,,,,AR                                                          
AR3      EQU   3,,,,AR                                                          
AR4      EQU   4,,,,AR                                                          
AR5      EQU   5,,,,AR                                                          
AR6      EQU   6,,,,AR                                                          
AR7      EQU   7,,,,AR                                                          
AR8      EQU   8,,,,AR                                                          
AR9      EQU   9,,,,AR                                                          
AR10     EQU   10,,,,AR                                                         
AR11     EQU   11,,,,AR                                                         
AR12     EQU   12,,,,AR                                                         
AR13     EQU   13,,,,AR                                                         
AR14     EQU   14,,,,AR                                                         
AR15     EQU   15,,,,AR                                                         
.SKIP1   ANOP
         END   LSENQ
