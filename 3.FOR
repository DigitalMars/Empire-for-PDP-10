
 
 
 
C SUBROUTINE 3
        FUNCTION VICTRY(H1,H2)
        SUM=0.0
        END=H1-1.
        R=H1+H2-1.
        DO 100 Y=0.,END
  100 SUM=SUM+1./(FAC(Y)*FAC(R-Y))
        VICTRY=SUM*FAC(R)*(.5^R)
        RETURN
        END
 
        FUNCTION FAC(X)
        FAC=1.0
        IF(X.LT.2)RETURN
        DO 1 I=2,INT(X)
1       FAC=FAC*FLOAT(I)
        RETURN
        END
 
 
        FUNCTION COST(OWN,H)
        REAL COSTAB(14),COSVAL(14)
        DATA COSVAL/1.,1.,2.,3.,2.,4.,3.,1.,4.,5.,6.,10.,14.,15./
        DATA COSTAB/'F','D','S','T','R','C','B',
     &              '2','3','4','5','6','7','8'/
        DO 1 I=1,14
    1   IF(OWN==COSTAB(I))GO TO 2
        COST=0.
        RETURN
    2   COST=COSVAL(I)
        IF(I>=9)COST=COST-H
        RETURN
        END
 
        SUBROUTINE LTR(Z6,ITURN)
        REAL D2(0:2)
        INTEGER L6,Z6
        REAL G2(0:6)
        COMMON/MODE/MODE,KURSOR,JECTOR ,ISEC
        COMMON/IARROW/IARROW(0:9)
        IF(MODE#1)GOTO202;CALL SENSOR(Z6); RETURN
  202   IF(ITURN#1) GOTO 301
        DO 5000 I7=1,8
        I8=Z6+IARROW(I7)
 5000 IF(A(1,I8)#A(2,I8)) GOTO 5001
        GOTO 301
 5001 TYPE 100
  100 FORMAT(' BEFORE SENSOR PROBE')
        L6=Z6
        IF(L6<101) L6=L6+100
        IF(L6>5900) L6=L6-100
        IF(L6/100*100==L6) L6=L6-1
        IF(L6/100*100+1==L6) L6=L6+1
        DO 600 I=-101,99,100
        DO 659 I9=0,2
  659 D2(I9)=D1(L6+I+I9)
        DO 660 I9=0,2
  660 G2(I9)=A(2,L6+I+I9)
  600 TYPE 200,(G2(J),J=0,2),(D2(J),J=0,2)
  200 FORMAT(1X,3A1,3X,3A1)
        CALL SENSOR(Z6)
        TYPE 300
  300 FORMAT(' AFTER SENSOR PROBE')
  301 L6=Z6
        IF(L6<301) L6=L6+300-(L6-1)/100*100
        IF(L6>5700) L6=L6-(L6-1)/100*100+5600
        IF((L6-1)/100*100+97<L6) L6=97+(L6-1)/100*100
        IF((L6-1)/100*100+4>L6) L6=L6/100*100+4
        DO 500 I=-303,297,100
        DO 661 I9=0,6
  661 G2(I9)=A(2,L6+I+I9)
  500 TYPE 400, (G2(J),J=0,6)
  400 FORMAT(1X,7A1)
  201   CALL STROUT('',1)
        RETURN
        END
 
 
 
 
 
        SUBROUTINE TEST(J)
        TYPE 100,J
  100   FORMAT(1X/' TEST POINT-',G$/)
        RETURN
        END
 


.