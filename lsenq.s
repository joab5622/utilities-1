*PROCESS ALIGN,NOCOMPAT,                                             
*PROCESS DXREF,FLAG(ALIGN,CONT,RECORD)
*PROCESS NOFOLD,NOINFO,PC(ON,DATA,GEN,MCALL),RENT,
*PROCESS RA2,NORLD,MXREF,RXREF,USING(MAP,WARN(13))
*PROCESS TYPECHECK(MAGNITUDE,REGISTER)
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
         LA    R1_32,CALLX
         L     R15_32,CEE3INF
         CALL  (15),(SYS,ENV,MEMBER,GPID,FC),VL,                       X
               MF=(E,(1))
         LHI   R15_32,1
         ST    R15_32,MODIFIER
         LHI   R7_32,16 
         ST    R7_32,RETURN_CODE 
         TM    SYS,X'02'          UNIX?
         JNO   GOBACK             NO
         LOAD  EP=BPX1WRT
         ST    R0_32,BPX1WRT
         LOAD  EP=BPX1WRV
         ST    R0_32,BPX1WRV
         MVC   GQLST(GQPARML),GQPARM
         BCTR  R7_32,0            15
         ST    R7_32,RETURN_CODE
         L     R2_32,@ARGC
         L     R3_32,@ARGVL
         L     R4_32,@ARGV
         ST    R2_32,MODIFIER
         L     R2_32,0(,R2_32)
         CHI   R2_32,3
         JH    GOBACK
         BCTR  R7_32,0            14
         ST    R7_32,RETURN_CODE
         CHI   R2_32,2
         JL    GOBACK
         BCTR  R7_32,0            13
         ST    R7_32,RETURN_CODE
         L     R6_32,4(,R4_32)    &ARGV[1]
         LT    R5_32,4(,R3_32)    &LENGTH OF ARG[1]
         JZ    GOBACK             ZERO IS NOT GOOD
         BCTR  R7_32,0            12
         ST    R7_32,RETURN_CODE
         LT    R5_32,0(,R5_32)    LENGTH OF ARG[1]
         JZ    GOBACK             ZERO IS NOT GOOD
         BCTR  R7_32,0            11
         ST    R7_32,RETURN_CODE
         BCTR  R5_32,0            DON'T COUNT TRAILING NULL
         LTR   R5_32,R5_32        BETTER HAVE SOME ACTUAL DATA!
         JZ    GOBACK             ZERO IS NOT GOOD
         MVC   QNAME,=CL8'SYSDSN'
         CHI   R2_32,2            ONLY RNAME SPECIFIED?
         JE    DORNAME            YES
         BCTR  R7_32,0            10
         ST    R7_32,RETURN_CODE
         CHI   R5_32,8            MAX OF 8
         JH    GOBACK             BUT IT ISN'T
         BCTR  R5_32,0
         MVC   QNAME,=CL8' '
         EX    R5_32,CP_QNAME
         L     R6_32,8(,R4_32)    &ARGV[2]          
         LT    R5_32,8(,R3_32)    &LENGTH OF &ARGV[2]
         JZ    GOBACK             CAN'T BE ZERO!
         BCTR  R7_32,0            9 
         ST    R7_32,RETURN_CODE
         LT    R5_32,0(,R5_32)    LENGTH OF ARG[2]
         JZ    GOBACK             ZERO IS NOT GOOD
         BCTR  R7_32,0            8
         ST    R7_32,RETURN_CODE
         BCTR  R5_32,0            DON'T COUNT TRAILING NULL
         LTR   R5_32,R5_32        BETTER HAVE SOME ACTUAL DATA!
         JZ    GOBACK             ZERO IS NOT GOOD
DORNAME  DS    0H
*
* AT THIS POINT R6 POINTS TO EITHER ARGV[1] OR
* ARGV[2], DEPENDING ON THE NUMBER OF ARGUMENTS
* USED.
* IF ARGC==2, THEN R6=&ARGV[1] R5=ARGVL[1]
* IF ARGC==3, THEN R6=&ARGV[2] R5=ARGVL[2]
         CHI   R5_32,255          CHECK AGAINST MAX LENGTH
         JH    GOBACK             TOO LONG
         BCTR  R7_32,0            7
         ST    R7_32,RETURN_CODE
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
         MVI   QNAME+L'QNAME,X'05' Place TAB in output
         L     R5_32,RNAME_LEN
         LA    R5_32,RNAME(R5_32)
         MVI   0(R5_32),X'15'     Place NEL in output
         LA    R4_32,QNAME
         ST    R4_32,BUFFERA
         SLR   R5_32,R4_32
         AHI   R5_32,1
         ST    R5_32,COUNT
*        ago   .nowrt1
         L     R15_32,BPX1WRT
         LA    R1_32,CALLX
         CALL  (15),(FD2,BUFFERA,ALET,COUNT,                           X
               RETVAL,                                                 X
               RETCODE,                                                X
               RSNCODE),VL,                                            X
               MF=(E,(1))
.nowrt1  anop
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
               VL,                                                     X
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
         LM    R0_32,R1_32,=A(TAB,1)
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
         LHI   R0_32,8
         LA    R1_32,RTN_QNAME
         ST    R1_32,QNAME@
         ST    R0_32,QNAMEL
         LA    R1_32,RTN_JOBNAME
         ST    R1_32,JOBNAME@
         ST    R0_32,JOBNAMEL
         LA    R1_32,RTN_EXCSHR
         ST    R1_32,EXCSHR@
         LA    R1_32,RTN_SYSNAME
         ST    R1_32,SYSNAME@
         ST    R0_32,SYSNAMEL
         LA    R1_32,RTN_EXCSHR
         ST    R1_32,EXCSHR@
         ST    R0_32,EXCSHRL
         LA    R1_32,RTN_STATUS
         ST    R1_32,STATUS@
         ST    R0_32,STATUSL
         LA    R1_32,RTN_SCOPE
         ST    R1_32,SCOPE@
         ST    R0_32,SCOPEL
         UNPK  D_RC(9),RTN_TCB(5)
         TR    D_RC(8),TOHEX-X'F0'
         LA    R1_32,D_RC
         ST    R1_32,TCB@
         ST    R0_32,TCBL
         UNPK  F_RC(5),RTN_ASID(3)
         TR    F_RC(4),TOHEX-X'F0'
         LA    R1_32,F_RC
         ST    R1_32,ASID@
         MVC   ASIDL,=F'4'
         MVC   RNAMEL,RTN_RLEN
         LA    R1_32,RTN_RNAME
         ST    R1_32,RNAME@
         MVC   IOV_COUNT,=A(IOV_STRUC_L/8)
         L     R10_32,IOV_COUNT
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
TAB      DC    X'05'
NL       DC    X'15'
*TITLE    DC    CL80'LSENQ DUMP'  
*OPTIONS  DC    CL255'BLOCKS,STORAGE,REGST(256),GENOPTS'
TOUPPER  DC    256AL1(*-TOUPPER)
         ORG   TOUPPER+X'81'
         DC    C'ABCDEFGHI'
         ORG   TOUPPER+X'91'
         DC    C'JKLMNOPQR'
         ORG   TOUPPER+X'A2'
         DC    C'STUVWXYZ'
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
IOV_STRUC DS   0D
QNAME@   DS    A
QNAMEL   DS    F
TAB1     DS    2F
RNAME@   DS    A
RNAMEL   DS    F
TAB2     DS    2F
JOBNAME@ DS    A
JOBNAMEL DS    F
TAB3     DS    2F
SYSNAME@ DS    A
SYSNAMEL DS    F
TAB4     DS    2F
EXCSHR@  DS    A
EXCSHRL  DS    F
TAB5     DS    2F
STATUS@  DS    A
STATUSL  DS    F
TAB6     DS    2F
SCOPE@   DS    A
SCOPEL   DS    F
TAB7     DS    2F
TCB@     DS    A
TCBL     DS    F
TAB8     DS    2F
ASID@    DS    A
ASIDL    DS    F
NL1      DS    2F
IOV_STRUC_L EQU *-IOV_STRUC
RNAME_LEN DS   F
QNAME    DS    CL8
         DS    X                  Reserve byte for NEL
RNAME    DS    CL255
         DS    X                  Reserver byte for NEL
CONTENTION DS  X
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
