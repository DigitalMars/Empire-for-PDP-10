
C SUBROUTINE 11
        FUNCTION IPORT(Z6)
        INTEGER Z6,X(70)
        COMMON/IARROW/IARROW(0:9)
        COMMON /X/X
        IPORT=0
        ID=500
        DO 100 I=1,70
        IF(X(I)==0) GOTO 100
        IF((A(1,X(I))#'X').OR.(EDGER(Z6)==0.0)) GOTO 100
        IF(IDIST(X(I),Z6)>=ID) GOTO 100
        IPORT=X(I)
        ID=IDIST(X(I),Z6)
  100   CONTINUE
        IF(IPORT#0) RETURN
        IPORT=INT(RAN(C1)*5798.+102.)
        RETURN
        END
 
        FUNCTION MOVCOR
     &(IFO,ITURN,Z6,MOVE,IH1,IS1,AGGR,OWN1,EXPLOR,DIR)
        INCLUDE 'COMMON.EMP/NOLIST'
 
* CHECK FOR IMPOSSIBLE CONDITION FOR MOVE
        IF((.NOT.PASS).OR.(IABS(MOVE)<=8)) GOTO 502
        CALL OUTCHR("32)
        TYPE 4034,OWN1,Z6,MOVE,IFO
 4034   FORMAT(1X,A1,' @ ',I4,' ATTEMPTED ',G,' WITH IFO ',I4)
 
  502   MOVE=IABS(MOVE)
 
        IF(ITURN==1) BLAH=0.
        IF(BLAH<0.) MOVE=ICORR(I2+INT(RAN(C1)*3.)-1)
 
* CHECK FOR SOMETHING TO ATTACK, OR, SOMETHING TO RUN FROM
* BLAH<0: RUN
* BLAH>=0: ATTACK
        DO 100 IX=1,8
        I1=IX
        LOC=Z6+IARROW(I1)
        AB=A(1,LOC)
        IF(D1(LOC)#'.') GOTO 100
        IF((AB<'B').OR.(AB>'T')) GOTO 100       !IF SHIP OR PLANE, INVESTIGATE
        BLAH=ATTACK(OWN1,AB,IH1,AGGR)
        IF(BLAH>=0.) GOTO 402                   !ATTACK IT
        GOTO 300                                !RUN FROM IT
  100   CONTINUE
        I1=0                            !NOTHING OF INTEREST HERE
        GOTO 400
 
* SELECT AN APPROPRIATE ESCAPE MOVE
  300   IS=INT(RAN(C1)*3.)
        DO 301 IN=1,8
        I2=IN
        IF((IS==0).OR.(IN>3)) GOTO 501
        IF(IS#1) GOTO 500
        IF(IN==1) I2=2
        IF(IN==2) I2=3
        IF(IN==3) I2=1
        GOTO 501
  500   IF(IN==1) I2=3
        IF(IN==2) I2=1
        IF(IN==3) I2=2
  501   I=IARROW(ISCAPE(I2,I1))+Z6
        IF((A(1,I)=='.').AND.(ORDER(I)==0)) GOTO 350
  301 CONTINUE
        I1=0
        GOTO 400
  350   I1=ISCAPE(I2,I1)
        IF(D1(I)#'.') CALL STROUT('ISCAPE ERROR',11)
        GOTO 402
 
  400   IF(EXPLOR==0.) GOTO 405
        EXPMAX=0
        DO 404 IX=MOVE,MOVE+7
        I1=ICORR(IX)
        LOC1=Z6+IARROW(I1)
        IF(ORDER(LOC1)#0) GOTO 404
        IF(A(1,LOC1)#'.') GOTO 404
        NEXP=0
        IF(A(0,LOC1+IARROW(I1))==' ') NEXP=1
        IF(A(0,LOC1+IARROW(ICORR(I1-1)))==' ') NEXP=NEXP+1
        IF(A(0,LOC1+IARROW(ICORR(I1+1)))==' ') NEXP=NEXP+1
        IF(A(0,LOC1+IARROW(ICORR(I1+2)))==' ') NEXP=NEXP+1
        IF(A(0,LOC1+IARROW(ICORR(I1-2)))==' ') NEXP=NEXP+1
        IF(NEXP==5) GOTO 402
        IF(NEXP<=EXPMAX) GOTO 404
        EXPMAX=NEXP
        I11=I1
  404   CONTINUE
        I1=0
        IF(EXPMAX==0) GOTO 405
        I1=I11
        IF(D1(Z6+IARROW(I1))#'.') CALL STROUT('EXPLOR ERROR',11)
        GOTO 402
  405   I2=MOVE
        LOC1=Z6+IARROW(MOVE)
        AB=A(1,LOC1)
        IF((AB=='.').AND.(ORDER(LOC1)==0)) GOTO 402
        M=MOVE
        IA=ICORR(M-DIR*3)
        IF(A(1,Z6+IARROW(IA))#'.') M=IA
        DO 401 I=0,7*DIR,DIR
        I2=ICORR(M+I)
        I3=Z6+IARROW(I2)
  401 IF((A(1,I3)=='.').AND.(ORDER(I3)==0)) GOTO 402
        I2=0
  402 IF(I1#0) I2=I1
        IF(((OWN1=='5').AND.(IFO#8)).OR.
     &(A(1,Z6+IARROW(MOVE))#'X')) MOVE=I2
        MOVCOR=MOVE
        IF(D1(Z6+IARROW(MOVE))=='+')CALL STROUT('AHEM,AHEM',1)
        RETURN
        END
 
 


.