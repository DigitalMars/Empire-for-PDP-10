
 
* TEST ROUTINES FOR PATH
 
        SUBROUTINE TEST3(Z5)
        INCLUDE 'COMMON.EMP/NOLIST'
        DIMENSION OK(5),COMM(30)
        DATA OK/'+',' ','O','5','*'/
        DATA COMM/'D','E','W','Q','A','Z','X','C','S',
     & 'Q','B','F','T','G','W','J',-1,-1,-1,'O','P','R','I','M','K','H',
     & 'S','?','Y','L'/
 
!Q: REFRESH SCREEN
!B: TYPE BEG
!F: TYPE END
!T: TRACE
!G: GO
!O: RETURN
!W: DIR=-DIR
 
        Z6=Z5
        WHTFLG=0
        MOVFLG=0
        OLDJ=JECTOR
        CALL SECTOR(2)
        call cursor(240)
        call strout('Editor  ',21)
 1000   LINE=KLINE(KI,JECTOR)
        IADJST=LINE+KI-300
        IF(Z6==0) Z6=IADJST+1240
        DIR=1
  100   CALL CURSOR(Z6-IADJST)
        CALL ECHOFF
        CALL OUTCHR("7)
        E=GETCHX(E)
        Z7=Z6
        DO 101 I=1,8
  101   IF(E==COMM(I)) Z6=Z6+IARROW(I)
        IF(ORDER(Z6)==0) GOTO 1001
        Z6=Z7
        GOTO 2000
 1001   IF(Z6==Z7) GOTO 102
        GOTO 100
  102   DO 103 I=10,30
        J=I
  103   IF(E==COMM(I)) GOTO 104
        GOTO 2000
  104   IF(PASS) GOTO (10,11,12,13,14,16,17,18,19) J-9
        GOTO (15,21,22,23,24,25,26,27,28,29,30) J-19
        GOTO 2000
   10   ISEC=-1
        CALL SECTOR(0)
        GOTO 100
   11   BEG=Z6
        CALL OUTCHR("102)
        GOTO 100
   12   END=Z6
        CALL OUTCHR("105)
        GOTO 100
   13   FLAG=1000
        CALL ECHOON
        CALL PATH(BEG,END,DIR,OK,FLAG)
        CALL ECHOFF
        GOTO 100
   14   FLAG=1001
        CALL ECHOON
        CALL PATH(BEG,END,DIR,OK,FLAG)
        CALL ECHOFF
        GOTO 100
   15   CALL ECHOON
        JECTOR=OLDJ                     !RESTORE SECTOR NUMBER
        call cursor(240)
        call outstr('         ')
        CALL SECTOR(2)
        RETURN
   16   DIR=-DIR
        GOTO 100
   17   H2=30
        OWN2=A(1,Z6)
        CALL FIND(OWN2,Z6,Z8,H2)
        CODNUM=CODE(Z8-1500)
        CALL CURSOR(50)
        CALL STROUT(' code:',0)
        TYPE 147,CODNUM
  147   FORMAT('+ ',I7,1X$)
        GOTO 100
   18   CONTINUE
   19   CONTINUE
   20   CONTINUE
        STOP
 
*P: PRINT OUT NEW SECTOR
   21   ISEC=-1
        CALL CURSOR(240)
        CALL STROUT(' New sector:',0)
        CALL ECHOON
        CALL OUTCHR("7)
        E=GETCHX(E)
        JECTOR=IPHASE(E)
        CALL SECTOR(2)
        ISEC=-1
        Z6=0
        call cursor(240)
        call strout('Editor  ',21)
        GOTO 1000
 
*R: PRINT OUT THE ROUND NUMBER
   22   CALL CURSOR(140)
        CALL STROUT(' Round #',0)
        CALL DECPRT(MDATE)
        CALL SPACE
        GOTO 100
 
*I: DIRECTIONAL STASIS
   23   AB=A(1,Z6)
        IF((AB<'A').OR.(AB>'T')) GOTO 2000
        CALL OUTCHR("7)
        E=GETCHX(E)
        DO 200 I=1,8
        J=I
  200   IF(COMM(I)==E) GOTO 201
        GOTO 2000
  201   IF(AB#'O') GOTO 202
        FIPATH(CITFND(Z6))=J+6100
        GOTO 100
  202   H2=30
        CALL FIND(AB,Z6,MOVFLG,H2)
        CALL CMYCOD(MOVFLG,J+6100)
        GOTO 100
 
*M: SAY WE WANT TO MOVE TO A LOCATION
   24   AB=A(1,Z6)
        IF((AB<'A').OR.(AB>'T')) GOTO 2000
        IF(AB#'O') GOTO 301
        WHTFLG='CITY'
        MOVFLG=CITFND(Z6)
        GOTO 100
  301   H2=30
        CALL FIND(AB,Z6,MOVFLG,H2)
        WHTFLG='UNIT'
        GOTO 100
 
*K: WAKE UP
   25   AB=A(1,Z6)
        IF((AB<'A').OR.(AB>'T')) GOTO 2000
        IF(AB#'O') GOTO 401
        FIPATH(CITFND(Z6))=0
        GOTO 100
  401   H2=30
        CALL FIND(AB,Z6,MOVFLG,H2)
        CALL CMYCOD(MOVFLG,0)
        GOTO 100
 
*H: GO HERE
   26   IF(WHTFLG#'CITY') GOTO 501
        FIPATH(MOVFLG)=Z6
        GOTO 100
  501   IF(WHTFLG#'UNIT') GOTO 2000
        CALL CMYCOD(MOVFLG,Z6)
        GOTO 100
 
*S: GOTO SLEEP
   27   AB=A(1,Z6)
        IF((AB<'A').OR.(AB>'T')) GOTO 2000
        IF(AB=='O') GOTO 2000
        H2=30
        CALL FIND(AB,Z6,MOVFLG,H2)
        CALL CMYCOD(MOVFLG,50)
        GOTO 100
 
*?: REQUEST INFO
   28   AB=A(1,Z6)
        IF((AB=='X').AND.(PASS)) GOTO 601
        IF((AB<'A').OR.(AB>'T')) GOTO 2000
        IF(AB=='O') GOTO 601
        IF((AB=='A').OR.(AB=='F')) GOTO 604
        H2=30
        CALL FIND(AB,Z6,MOVFLG,H2)
        CALL CURSOR(40)
        CALL STROUT(' Hits left:',0)
        CALL DECPRT(H(MOVFLG-700))
        CALL SPACE
  604   CALL STSOUT(MYCODE(MOVFLG))
        GOTO 100
  601   J=CITFND(Z6)
        CALL CURSOR(200)
        CALL STROUT(' Location:',0) ; CALL DECPRT(Z6)
        CALL STROUT('  Producing:',0)
        DO 602 I=1,8
  602   IF(PHASE(J)==PHAZE(I+8)) TYPE 603,PHAZE(I)
  603   FORMAT('+',A1,$)
        CALL STROUT('  Completion:',0) ; CALL DECPRT(FOUND(J))
        CALL STROUT('  FPath:',0)
        IF(FIPATH(J)<100) CALL STROUT('Sit',0)
        IF((FIPATH(J)>100).AND.(FIPATH(J)<6000)) CALL DECPRT(FIPATH(J))
        IF(FIPATH(J)>6100) TYPE 603, COMM(FIPATH(J)-6100)
        CALL SPACE
        GOTO 100
 
*Y: ENTER NEW PHASE
   29   AB=A(1,Z6)
        IF(AB#'O') GOTO 2000
        J=CITFND(Z6)
        CALL CURSOR(40)
        CALL STROUT('New production:',0)
        CALL PHASIN(J)
        GOTO 100
 
*L: SET ARMY TO MOVE AT RANDOM
   30   AB=A(1,Z6)
        IF(AB#'A') GOTO 2000
        H2=30
        CALL FIND(AB,Z6,MOVFLG,H2)
        CALL CMYCOD(MOVFLG,100)
        GOTO 100
 
 2000   CALL HUH
        GOTO 100
        END
 
 
        SUBROUTINE HUH
        CALL CURSOR(40)
        CALL STROUT(' Huh? ',0)
        END
 
        SUBROUTINE SPACE
        CALL STROUT('',50)
        END
 
        INTEGER FUNCTION CITFND(Z6)
        INCLUDE 'COMMON.EMP/NOLIST'
        DO 100 I=1,70
  100   IF(X(I)==Z6) GOTO 101
        IF(.NOT.PATH) RETURN
        CALL STROUT('CITFND ERROR',0)
        RETURN
  101   CITFND=I
        RETURN
        END
 
        SUBROUTINE DIREC
        CALL CURSOR(140)
        CALL STROUT(' Read the directions! ',0)
        END
 
        SUBROUTINE PHASIN(NUM)
        INCLUDE 'COMMON.EMP/NOLIST'
        CALL ECHOON
        CALL OUTCHR("7)
        E=GETCHX(E)
        DO 100 I=1,8
  100   IF(E==PHAZE(I)) GOTO 101
        CALL HUH
        RETURN
  101   PHASE(NUM)=PHAZE(I+8)
        FOUND(NUM)=MDATE+6*PHASE(NUM)
        RETURN
        END
 
        SUBROUTINE TEST4(Z6,FLAG,DIR,MOVE1,MOVNUM,BEG,
     & END,G2,FLAG2)
        IMPLICIT INTEGER(A-Z)
        COMMON/KXK/IADJST
        DIMENSION G2(100)
 
        CALL CURSOR(Z6-IADJST,KURSOR)
        IF(FLAG2=='MOV') CALL OUTCHR("107)
        IF(FLAG2=='SHORE') CALL OUTCHR("110)
        IF(FLAG==1001) RETURN
        CALL OUTCHR("7)
        E=GETCHX(E)
        IF(E==' ') RETURN
        IF(E=='G') GOTO 100
        CALL CURSOR(0,KURSOR)
        TYPE 101,Z6,MOVE1,MOVNUM
  101   FORMAT(' Z6:',I4,' MOVE1:',I1,' MOVNUM:',I3)
        CALL CURSOR(100,KURSOR)
        TYPE 103,BEG,END,IADJST,DIR,FLAG
  103   FORMAT(' BEG:'I4' END:'I4' IADJST:'I4' DIR:'I2' FLAG:'I4)
        TYPE 104,FLAG2
  104   FORMAT(' FLAG2:',A5)
        RETURN
  100   CALL CURSOR(0,KURSOR)
        TYPE 102,G2
  102   FORMAT(1X,16I5)
        RETURN
        END
 
 


.