*PROCESS ALIGN,NOCOMPAT,DXREF,FLAG(ALIGN,CONT,RECORD)
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
         SYSSTATE ASCENV=P,
               AMODE64=NO,
               ARCHLVL=2
TEMPLATE CEEENTRY PPA=TEMPLATE_PPA,
               MAIN=YES,
               AUTO=DSASIZE,
               BASE=R11_32
         USING CEECAA,R12_32
         USING CEEDSA,R13_32
         J     GO
GOBACK   DS    0H
         CEETERM RC=RETURN_CODE,
               MF=(E,CEETERM_BLOCK)
GO       DS    0H
         LR    R9_32,R1_32        SAVE R1 UPON ENTRY
         USING PARMS,R9_32
         LA    R1_32,CALLX
         L     R15_32,CEE3INF
         CALL  (15),(SYS,ENV,MEMBER,GPID,FC),VL,
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
         ST    R7_32,RETURN_CODE
         L     R2_32,@ARGC
         L     R3_32,@ARGVL
         L     R4_32,@ARGV
         ST    R2_32,MODIFIER
         L     R2_32,0(,R2_32)
         CHI   R2_32,?
         JH    GOBACK
         AGO   .NODUMP2
DUMP     DS    0H
         LA    R1_32,CALLX
         L     R15_32,CEE3DMP
         CALL  (15),
               (TITLE,OPTIONS,FC),
               VL,
               MF=(E,(1))
.NODUMP2 ANOP
         SLR   R15_32,R15_32
         J     GOBACK
FD1      DC    F'1'
ALET     DC    A(0)
IOV_ALET EQU   ALET
IOV_BUFFER_ALET EQU ALET
CEE3INF  DC    V(CEE3INF)
SPRINTF  DC    V(SPRINTF)
CEE3DMP  DC    V(CEE3DMP)
ISGQUERY DC    V(ISGQUERY)
TOHEX    DC    C'0123456789ABCDEF'
FORMAT   DC    C'ISGQUERY Return code=%i Modifier=%#8x',X'1500'
TAB      DC    X'05'
NL       DC    X'15'
TITLE    DC    CL80'TEMPLATE DUMP'
OPTIONS  DC    CL255'BLOCKS,STORAGE,REGST(256),GENOPTS'
TOUPPER  DC    256AL1(*-TOUPPER)
         ORG   TOUPPER+X'81'
         DC    C'ABCDEFGHI'
         ORG   TOUPPER+X'91'
         DC    C'JKLMNOPQR'
         ORG   TOUPPER+X'A2'
         DC    C'STUVWXYZ'
         ORG
         LTORG *
TEMPLATE_PPA CEEPPA LIBRARY=NO,
               PPA2=YES,
               EXTPROC=YES,
               TSTAMP=YES,
               PEP=YES,
               INSTOP=YES,
               EXITDSA=NO,
               OWNEXM=YES,
               EPNAME=TEMPLATE,
               VER=1,
               REL=1,
               MOD=0,
               DSA=YES
         CEEDSA
* DYNAMIC AREA IS DEFINED HERE.
* THIS IS WITHIN A DSECT, SO NO DATA IS REALLY INITIALIZED
         DS    0D                 FORCE DOUBLEWORD
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
FC       DS    3F
BUFL     DS    F
RETURN_CODE DS F
MODIFIER DS    F
RETURN_VALUE DC A(0)               PREVIOUS DEFAULT DUB
REASON_CODE DC A(0)
CEETERM_BLOCK CEETERM MF=L
CALLX    DS    30A
IOV_COUNT DS   F
IOV_STRUC DS   0D
IOV_STRUC_L EQU *-IOV_STRUC
DSASIZE  EQU   *-CEEDSA
         BPXYCONS DSECT=YES,LIST=YES
         BPXYIOV
         BPXYERNO LIST=YES
         CEECAA
PARMS    DSECT
@ARGC    DS    A                  ADDRESS OF NUMBER OF ARGUMENTS
@ARGVL   DS    A                  ADDRESS OF VECTOR OF LENGTH OF ARGS
@ARGV    DS    A                  ADDRESS OF VECTOR OF ARGS
@ENVL    DS    A                  ADDRESS OF NUMBER OF ENV VARS
@ENVVL   DS    A                  ADDRESS OF VECTOR OF LENGTH OF ENV VA
@ENV     DS    A                  ADDRESS OF VECTOR OF ENV VARS
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
         END   TEMPLATE
