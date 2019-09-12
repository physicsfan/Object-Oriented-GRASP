
SUBROUTINE ONEPARTICLEJJ(KA,IOPAR,JA,JB,IA1,IA2,VSHELL)
!*******************************************************************
!   The main program for evaluating the reduced matrix elements of *
!   a one particle operator for configurations in jj-coupling.     *
!                                                                  *
!   This restructured package uses the CONTAINS keyword to group   *
!   related subroutines together into self-contained blocks,       *
!   rather than relying on calls to subroutines in separate files. *
!   The ultimate aim is to reduce the size and complexity of the   *
!   "librang90" library.                                           *  
!                                                                  *
!   Call(s) to: [LIB92]: CFP, FIXJ, GENSUM, ICHOP, IROW1, ISPAR,   *
!                        ITJPO, ITRIG, SETQNA, VIJOUT.             *
!               [NJGRAF]: NJGRAF.                                  *
!                                                                  *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!   Restructured by A. Senchuk                     September 2019  * 
!                                                                  *
!*******************************************************************
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
  USE vast_kind_param, ONLY:  DOUBLE
  USE parameter_def,   ONLY:  NNNW
  USE CONS_C
  USE m_C
  USE orb_C,   ONLY: NW, NAK
  USE dumx_C,  ONLY: JLIS, JC1S, JC2S
  USE trk_C

!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
  USE itrig_I
  USE ichkq1_I
  USE ichop_I
  USE ispar_I
  USE itjpo_I
  USE setqna_I
  USE recop00_I
  USE recop1_I
  USE recop2_I
  USE wj1_I
  USE c0t5s_I
  USE rmeajj_I
  
  IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
  INTEGER, INTENT(IN)  :: KA,IOPAR,JA,JB
  INTEGER, INTENT(OUT) :: IA1,IA2
  REAL(DOUBLE), INTENT(OUT) :: VSHELL(NNNW)
!      DIMENSION VSHELL(NNNW)
!      DIMENSION IS(2),KS(2)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
  INTEGER :: KK,IJ,IDQ,JA1,JA2,JW,NDQ,NS,ISH,I,II,I1,IM,  &
       KS1,KS2,NX,NPEELM
  INTEGER, DIMENSION(2) :: IS,KS
  REAL(DOUBLE) :: TCOEFF
!-----------------------------------------------
  IA1 = 0
  KK = KA+KA+1
  IF(ITRIG(ITJPO(JA),ITJPO(JB),KK).EQ.0)RETURN
  IF((IOPAR.NE.0).AND.(ISPAR(JA)*ISPAR(JB)*IOPAR.NE.1))RETURN
  IF(ICHKQ1(JA,JB).EQ.0)RETURN
!
  CALL SETQNA (JA,JB)
!
  DO IJ = 1,NW
     VSHELL(IJ) = ZERO
  END DO
!
!   Analyse peel shell interactions
  IDQ = 0
  JA1 = 0
  JA2 = 0
  IF (NPEEL .NE. 0) THEN
     DO JW = 1,NPEEL
        IJ = JLIST(JW)
        NDQ = NQ1(IJ)-NQ2(IJ)
        IF (ABS (NDQ) .GT. 1) RETURN
        IF (NDQ .GT. 0) THEN
           JA1 = JW
           IDQ = IDQ+1
        ELSEIF (NDQ .LT. 0) THEN
           JA2 = JW
           IDQ = IDQ+1
        ENDIF
     END DO
     IF (IDQ .GT. 2) RETURN
!
!   Evaluate the array VSHELL
!
!   Then there are two possibilities IDQ = 0 or IDQ = 2
!   if IDQ = 0, then loop over all shells by index ISH
!   if IDQ = 2, then one orbital fixed on each side
     NS = NPEEL
  ENDIF
  IF (IDQ .EQ. 2) GOTO 19
!
!   Loop over shells when IDQ = 0
  ISH = 0
  IF (NPEEL .EQ. 0) GOTO 9
  DO I = 1,NPEEL
     JLIS(I) = JLIST(I)
  END DO
  IF (NPEEL .EQ. 1) GOTO 9
  NPEELM = NPEEL-1
  DO I = 1,NPEELM
     JC1S(I) = JJC1(I)
     JC2S(I) = JJC2(I)
  END DO
!
!   If ISH .GT. NW, then loop is over and return
9 ISH = ISH+1
  IF (ISH .GT. NW) RETURN
  IF (ICHOP (ISH,JA) .EQ. -1) GOTO 9
  IF (ICHOP (ISH,JA) .EQ. 0) GOTO 16
!
!   Case one --- the ISH-th shell is in the core or in the peel and
!   closed for both sides
  I = 1
  IF (NPEEL.EQ.0) GOTO 15
  DO I = 1,NPEEL
     IJ = JLIST(I)
     IF (ISH .LT. IJ) GOTO 11
  END DO
  I = NPEEL+1
  GOTO 13
11 IM = NPEEL-I+1
  DO II = 1,IM
     JLIST(NPEEL+2-II) = JLIST(NPEEL+1-II)
     IF (NPEEL.EQ.II) GOTO 13
     JJC1(NPEEL+1-II) = JJC1(NPEEL-II)
     JJC2(NPEEL+1-II) = JJC2(NPEEL-II)
  END DO
13 CONTINUE
  IF (I .LT. 3) GOTO 14
  JJC1(I-1) = JJC1(I-2)
  JJC2(I-1) = JJC2(I-2)
  GOTO 15
14 I1 = JLIST(1)
  JJC1(1) = JJQ1(3,I1)
  JJC2(1) = JJQ2(3,I1)
15 JLIST(I) = ISH
  JA1 = I
  JA2 = I
  NS = NPEEL+1
  GOTO 19
!
!   Case two --- the ISH-th shell is in the peel and open for either
!   side
16 NS = NPEEL
  DO JW = 1,NPEEL
     NX = ISH-JLIST(JW)
     IF (NX.EQ.0) GOTO 18
  END DO
18 JA1 = JW
  JA2 = JW
!
!   Main computation
!
!     JA1, JA2 are the indices of interacting shells in JLIST
!     IA1, IA2 are the indices of interacting shells in NW
19 IA1 = JLIST(JA1)
  IA2 = JLIST(JA2)
  KS1 = 2*ABS (NAK(IA1))
  KS2 = 2*ABS (NAK(IA2))
!
!   Check triangular condition for the active shells
  IF (ITRIG (KS1,KS2,KK).EQ.1) GOTO 99
  IF (IDQ .EQ. 2) RETURN
  GOTO 100
!
!   Set tables of quantum numbers of non-interacting spectator shells
99 CONTINUE
  IF (IDQ .EQ. 0) THEN
!GG
!GG   Cia orginalioje programoje yra klaida
!GG
!GG        IF(JA .EQ. JB) THEN
     CALL ONEPARTICLEJJ1(NS,KA,JA,JB,JA1,JA2,TCOEFF)
!GG          CALL ONESCALAR1(NS,JA,JB,JA1,JA2,TCOEFF)
!GG        ELSE
!GG          TCOEFF = 0.0D 00 
!GG        END IF
  ELSE IF (IDQ .EQ. 2) THEN 
!
!   IDQ = 2 Case
!
!       Permutation factor for IDQ = 2
     CALL ONEPARTICLEJJ2(NS,KA,JA1,JA2,TCOEFF)
!GG        CALL ONESCALAR2(JA,JB,JA1,JA2,TCOEFF)
     VSHELL(1) = TCOEFF 
     RETURN
  END IF
!
!   End of loop over parent states
!
!
!   IDQ = 0 CASE
  VSHELL(ISH) = TCOEFF
!
!   Loop over all shells when IDQ = 0
100 CONTINUE
  IF (NPEEL .EQ. 0) GOTO 9
  DO I = 1,NPEEL
     JLIST(I) = JLIS(I)
  END DO
  IF (NPEEL .EQ. 1) GOTO 9
  NPEELM = NPEEL-1
  DO  I = 1,NPEELM
     JJC1(I)  = JC1S(I)
     JJC2(I)  = JC2S(I)
  END DO
  GOTO 9
  RETURN

CONTAINS

  SUBROUTINE ONEPARTICLEJJ1(NS,KA,JJA,JJB,JA,JB,COEFF)
!*******************************************************************
!   --------------  SECTION METWO    SUBPROGRAM 03  -------------  *
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF MATRIX ELEMENTS        *
!     OF ONE PARTICLE OPERATOR IN CASE :           N'1 = N1        *
!                                                  N'2 = N2        *
!                                                                  *
!      SUBROUTINE CALLED:                                          *
!                                                                  *
!   Written by  G. Gaigalas                                        *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!   Restructured by A. Senchuk                     September 2019  *    
!                                                                  *
!*******************************************************************
!
  IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    INTEGER, INTENT(IN) :: NS,KA,JJA,JJB,JA,JB
    REAL(DOUBLE), INTENT(OUT) :: COEFF
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    INTEGER      :: IAT
    REAL(DOUBLE) :: REC,WJ,QM1,QM2
!-----------------------------------------------
!
!     THE CASE 11   + -
!
    COEFF=ZERO
    IF(JA == JB) THEN
       IF(JJA /= JJB) THEN
          CALL RECOP00(NS,JA,JA,KA,IAT)
          IF(IAT == 0)RETURN
       END IF
       CALL RECOP1(NS,JA,KA,0,IAT,REC)
       IF(IAT == 0)RETURN
       CALL PERKO2(JA,JA,JA,JA,1)
       QM1=HALF
       QM2=-HALF
       CALL WJ1(IK1,BK1,ID1,BD1,KA,QM1,QM2,WJ)
       IF(DABS(WJ) > EPS) THEN
          CALL RECOP1(NS,JA,KA,1,IAT,REC)
          COEFF=WJ*REC*DSQRT(DBLE(ID1(3)+1))/DSQRT(DBLE(2*KA+1))
          COEFF=-COEFF
       END IF
    END IF
    RETURN
  END SUBROUTINE ONEPARTICLEJJ1
!
!#################################################################
!    
  SUBROUTINE ONEPARTICLEJJ2(NS,KA,JA,JB,COEFF)
!*******************************************************************    
!   --------------  SECTION METWO    SUBPROGRAM 03  -------------  *
!                                                                  *
!     THIS PACKAGE DETERMINES THE VALUES OF MATRIX ELEMENTS        *
!     OF ONE PARTICLE OPERATOR IN CASE :           N'1 = N1 +- 1   *
!                                                  N'2 = N2 -+ 1   *
!                                                                  *
!      SUBROUTINE CALLED:                                          *
!                                                                  *
!   Written by  G. Gaigalas                                        *
!   Transform to fortran 90/95 by G. Gaigalas       December 2012  *
!   The last modification made by G. Gaigalas       October  2017  *
!   Restructured by A. Senchuk                     September 2019  *    
!                                                                  *
!*******************************************************************
!
    IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    INTEGER, INTENT(IN)       :: NS,KA,JA,JB
    REAL(DOUBLE), INTENT(OUT) :: COEFF
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    INTEGER      :: I,IA,IB,IAT,IJ,IFAZ,IQMM1,IQMM2,JIBKS1,JIB, &
         KS1,KS2
    REAL(DOUBLE) :: REC,A1,A2,A3,S1,S2,QM1,QM2
!-----------------------------------------------
!
!     THE CASE 12   + -
!
    COEFF=ZERO
    IA=MIN0(JA,JB)
    IB=MAX0(JA,JB)
    CALL RECOP00(NS,IA,IB,KA,IAT)
    IF(IAT == 0)RETURN
    IJ=JLIST(IA)
    KS1=(IABS(NAK(IJ))*2)-1
    IJ=JLIST(IB)
    KS2=(IABS(NAK(IJ))*2)-1
    CALL RECOP2(NS,IA,IB,KS1,KS2,KA,0,IAT,REC)
    IF(IAT == 0)RETURN
    CALL PERKO2(JA,JB,JA,JA,2)
    QM1=HALF
    QM2=-HALF
    IQMM1=QM1+QM1+TENTH*QM1
    IF(IK1(4) /= (ID1(4)+IQMM1)) RETURN
    IQMM2=QM2+QM2+TENTH*QM2
    IF(IK2(4) /= (ID2(4)+IQMM2)) RETURN
    CALL C0T5S(BD1(1),BD1(3),QM1,BK1(1),BK1(3),A2)
    IF(DABS(A2) < EPS) RETURN
    CALL C0T5S(BD2(1),BD2(3),QM2,BK2(1),BK2(3),A3)
    IF(DABS(A3) < EPS) RETURN
    CALL RMEAJJ(IK1(3),IK1(1),IK1(7),IK1(6),ID1(1),ID1(7),ID1(6),S1)
    IF(DABS(S1) < EPS) RETURN
    CALL RMEAJJ(IK2(3),IK2(1),IK2(7),IK2(6),ID2(1),ID2(7),ID2(6),S2)
    IF(DABS(S2) < EPS) RETURN
    A1=S1*S2*A2*A3
    CALL RECOP2(NS,IA,IB,KS1,KS2,KA,1,IAT,REC)
    COEFF=A1*REC/DSQRT(DBLE((2*KA+1)*(IK1(7)+1)*(IK2(7)+1)))
    JIB=IB-1
    IFAZ=0
    DO  I=IA,JIB
       IJ=JLIST(I)
       IFAZ=IFAZ+NQ1(IJ)
    END DO
    IFAZ=IFAZ+1
    IF(MOD(IFAZ,2) /= 0)COEFF=-COEFF
    IF(IA.NE.JA) THEN
       IFAZ = IK1(3)+IK2(3)-2*KA+2
       IF(MOD(IFAZ,4) /= 0)COEFF=-COEFF
    ENDIF
    COEFF=-COEFF*SQRT(DBLE(ID1(3)+1))
    RETURN
  END SUBROUTINE ONEPARTICLEJJ2
  
END SUBROUTINE ONEPARTICLEJJ