C
      SUBROUTINE ENGBAL
      USE MODULE1,ONLY :
     &    AREAXZ,HX,HZ,HXL,HZL
     &  , HXB,HZB
     &  , QL,IBOUND,IBL,IBR,IBT,IZONE
     &  , PR,POLD,PORC,CORFP,CORFF
     &  , ZERO,ONE,ONEM,ONEM4,ONEM5,ONEM6,ONEM7,ONEM8,ONEM9
     &  , ONEM10,ONEM12,ONEP12,ONEM50,ONEP50,ONEM5M,PONEM,ONE199,PRCSN
     &  , PIE,F1P8
     &  , LX,LY,LZ    
     &  ,DHT=>F6S,THCNEW=>F7S,HTERM=>F8S,TFLUXO=>CA71
     &  ,TFLUXU=>CA72,DDX=>CA73,DDY=>CA81,DDZ=>CA82,DTEMP=>CA83
     &  , ALX1,BLX1,ALY1,BLY1
     &  , ALZ1,BLZ1,RPERMX,RPERMY
     &  , RPERMZ
     &  , P4RW,E4W,S4RC,P4RC,E4C,T44,S4RW
     &  , SGI,IGAS,NPHAS
     &  , TQLOS,RINO,RINU,TEM
     &  , TVHC,TCONO,TCONU,IHLOS,IANAL
     &  , CRTC,CVSPR,CVSPL,DENS,CUMHI,CUMHP,TEMPI,IENG
     &  , BVI,TEMINJ,DENO,DENU,CVSPO,CVSPU
     &  , ENTHE,TEMREF
     &  , TTCHG
     &  , CTOT,C,CSE,S
     &  , CE  
     &  , VIS,RPERM,PERMX,PERMY
     &  , PERMZ,QI,QB,Q,QT
     &  , CUMQI,CUMQP,PWF
     &  , EL,DX,DY,DZ,R,RP
     &  , RPSQ,IJKPOS,IPRESS
     &  , DT,CURANT,NXM1,NX,NY,NZ,NXNY,NBL,NBLW,N
     &  , DEN1,DEN2,DEN23,DEN3,DEN7,DEN8,IDEN
     &  , DEN,PRC
     &  , PORMY,P,CPC,EPC
     &  , SSTAR,IOW 
     &  , TRSX,TRSY,TRSZ,TX
     &  , TY,TZ,CONVX,CONVY
     &  , CONVZ,VELX,VELY,VELZ
     &  , POR,RKF
     &  , PSTAND,COMPR,COMPC,CTERM         
     &  , LL
     &  , DDYY
     &  , TEMPOB,TEMPUB
     &  , LWKSP1
     &  , FOREC,FOREC1,RESPV,RESATK,BTO,VB
     &  , CUMI,CUMP,OIP,OP,T,TINJ,WHPV
     &  , PRF,ICNT,IINJ,INEC,IRST
     &  , DCMAX,IDISPC,ICF,ICOORD,ITC,IUNIT
     &  , DNX,DNZ
     &  ,TFLM=>WKSP1,COMP1=>WKSP2,COE4=>WKSP3
     &  ,COE5=>WKSP4,WKSP1=>WKSP5 
     &  ,RR=>AW,RRSQ=>AE,RRP=>AN,RRPSQ=>AS
     &  ,REDT=>AT,GELT=>AB,TEMPK=>AC,TAKNEW=>BV
     &  ,CG1=>DUM1,CG2=>DUM2,CG3=>DUM3,HION=>DUM4
     &  ,YZAREA=>DUM5,XZAREA=>DUM6,XYAREA=>DUM7
     &  , NWELL,IRO,IDW,IFLAG,IDIR
     &  , IWC,JWC,KWC,NWBC
C
C     -----------------------------------------------------------
C     PURPOSE : THIS SUBROUTINE SOLVES THE ENERGY BALANCE EQ. AND
C               CALCULATES THE HEATLOSS TO OVER- AND UNDER- BURDEN
C
C     CALL   : NONE
C     -----------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      NBLM1 = NBL-1
      NBLM2 = NBL-NX
      NBLM3 = NBL-NXNY
      FACT1 = 144.
      TWO = 2.
C
C     INITIALIZE THE HEATLOSS 
C
      IF (ICNT.EQ.1) THEN
         DO 1 I = 1,NXNY
            TTCHG(I) = -999
            RINO(I) = 0.0
 1       CONTINUE
         DO 5 I = NBLM3+1,NBL
            TTCHG(I) = -999
            RINU(I) = 0.0
 5       CONTINUE
C
         TEMPOB = TEMPI
         TEMPUB = TEMPI
      ENDIF 
C
      IF (ICOORD.NE.3) THEN
C
C ----FLEXI GRID
C
         IF (ICOORD.EQ.4) THEN
C
            DO 6 J = 1,NY
               IK = 0
            DO 6 K = 1,NZ
            DO 6 I = 1,NX
               IJK = I+(K-1)*NXNY+(J-1)*NX
               IK = IK+1
               DDX(IJK) = DNX(IK)
               DDY(IJK) = DY(J)
               DDZ(IJK) = DNZ(IK)
  6         CONTINUE
C
C     RE-CALCULATE CELL FACE AREAS
C
            DO 10 J = 1,NY
               IK = 0
            DO 10 K = 1,NZ
            DO 10 I = 1,NX
               IJK = I+(K-1)*NXNY+(J-1)*NX
               IK = IK+1
               YZAREA(IJK) = HZ(IK)*DY(J)
               XZAREA(IJK) = AREAXZ(IK)
               XYAREA(IJK) = HX(IK)*DY(J)
  10        CONTINUE
         ELSE
            IJK = 0
            DO 7 K = 1,NZ
            DO 7 J = 1,NY
            DO 7 I = 1,NX
               IJK = IJK+1
               DDX(IJK) = DX(I)
               DDY(IJK) = DY(J)
               DDZ(IJK) = DZ(K)
  7         CONTINUE
         ENDIF
      ELSE
         IJK = 0
         DO 8 K = 1,NZ
         DO 8 I = 1,NX
            IJK = IJK+1
            DDX(IJK) = DX(I)
            DDY(IJK) = DDYY(I)
            DDZ(IJK) = DZ(K)
  8      CONTINUE
      ENDIF
C
C     -------------------------------------------------------------------
C     CALCULATE THE NODE RADII AND BOUNDARY DISTANCES OF EACH GRID BLOCK
C     FOR RADIAL COORDINATE
C     REMINDER:
C     1) R, R(I), IS NODE RADII, 2) RPSQ, R(I+1/2)^2, = R(I+1)^2-R(I)^2/
C     LN((R(I+1)/R(I))^2), 3) RP, R(I+1/2), IS GRID BOUNDARY = SQRT(RPSQ)
C     , 4) DX IS DISTANCE BETWEEN THE GRID BLOCKS, R(I+1/2)^2-R(I-1/2)^2,=
C      RPSQ(I)-RPSQ(I-1)
C     ---------------------------------------------------------------------
C
      IF (ICOORD.EQ.2) THEN
         IK = 0
         DO 12 K = 1,NZ
         DO 12 I = 1,NX
            IK = IK+1
            RR(IK) = R(I)
            RRSQ(IK) = R(I)**2
            RRP(IK) = RP(I)
            DDY(IK) = PIE
 12      CONTINUE
      ENDIF
C
C     ZERO THE DHT ARRAY 
C
      DO 15 I = 1,NBL
         DHT(I) = 0.0
 15   CONTINUE
C
C     *********************************
C     COMPUTE THERMAL CONDUCTIVITIES
C     *********************************
C
C     X-DIRECTION
C
      IF (NX.GT.1) THEN
C
C     EVALUATE THE FLUX
C
         IF (ICOORD.NE.2) THEN
C
C ----FLEXI GRID
C
            IF (ICOORD.EQ.4) THEN
               DO 40 I = 2,NBL
                  HTERM(I) = -2.*CRTC*(TEM(I)-TEM(I-1))/
     *                       (DDX(I-1)*(ONE+ALX1(I-1)))
40             CONTINUE
            ELSE
               DO 43 I = 2,NBL
                  HTERM(I) = -2.*CRTC*(TEM(I)-TEM(I-1))/(DDX(I)+
     *                       DDX(I-1)) 
43             CONTINUE
            ENDIF
         ELSE
            DO 45 I = 2,NBL
               HTERM(I) = -2.*CRTC*RRP(I-1)*(TEM(I)-TEM(I-1))/(RRSQ(I)
     *                  -RRSQ(I-1))
 45         CONTINUE
         ENDIF
C
         DO 50 I = 1,NBL,NX
            HTERM(I) = 0.0
 50      CONTINUE
C
C      DIFFERENCE THE FLUX
C
         IF (ICOORD.NE.2) THEN
            IF (ICOORD.EQ.4) THEN
C
C-----FLEXI
C
               DO 51 I = 1,NBLM1
                  HTERM(I+1) = HTERM(I+1)*YZAREA(I)
 51            CONTINUE
               DO 61 I = 1,NBLM1
                  DHT(I) = DHT(I)-(HTERM(I+1)-HTERM(I))
 61            CONTINUE
               DHT(NBL) = DHT(NBL)+HTERM(NBL)
            ELSE
               DO 60 I = 1,NBLM1
                  DHT(I) = DHT(I)-(DDY(I)*HTERM(I+1)-DDY(I-1)*HTERM(I))
     *                    /(DDY(I)*DDX(I))
 60            CONTINUE
               DHT(NBL) = DHT(NBL)+DDY(NBL-1)*HTERM(NBL)/(DDY(NBL)*
     *                    DDX(NBL))
            ENDIF
         ELSE
            DO 65 I = 1,NBLM1
               DHT(I) = DHT(I)-2.*(RRP(I)*HTERM(I+1)-RRP(I-1)*HTERM(I))
     *                  /DDX(I)
 65         CONTINUE
            DHT(NBL) = DHT(NBL)+2.*HTERM(NBL)*RRP(NBL-1)/DDX(NBL)
         ENDIF
      ENDIF
C  
C     Y-DIRECTION
C
      IF (NY.GT.1) THEN
C
C     EVALUATE THE FLUX
C
         DO 100 I = NX+1,NBL
            HTERM(I) =  - 2.*CRTC*(TEM(I)-TEM(I-NX))/(DDY(I)+DDY(I-NX))
 100     CONTINUE
         DO 110 K = 1,NZ
         DO 110 I = (K-1)*NXNY+1,(K-1)*NXNY+NX
            HTERM(I) = 0.0
 110     CONTINUE
C
C     DIFFERENCE THE FLUX
C
         IF (ICOORD.EQ.4) THEN
C
C-----FLEXI
C
            DO 112 I = 1,NBLM2
               HTERM(I+NX) = HTERM(I+NX)*XZAREA(I)
 112        CONTINUE
            DO 115 I = 1,NBLM2
               DHT(I) = DHT(I) - (HTERM(I+NX)-HTERM(I))
 115        CONTINUE
            DO 118 I = NBLM2+1,NBL
               DHT(I) = DHT(I)+HTERM(I)
 118        CONTINUE
         ELSE
            DO 120 I = 1,NBLM2
               DHT(I) = DHT(I) - (HTERM(I+NX)-HTERM(I))/DDY(I)
120         CONTINUE
            DO 130 I = NBLM2+1,NBL
               DHT(I) = DHT(I)+HTERM(I)/DDY(I)
130         CONTINUE
         ENDIF
      ENDIF
C                *** FLEXI QUESTION ANY X Y Z ???
C
C     HEAT FLUX IN THE Z-DIRECTION USING THE MODEL OF VINSOME-WESTERVELD
C
      DO 200 I = 1,NBL
         TFLUXO(I) = 0.0
         TFLUXU(I) = 0.0
 200  CONTINUE
C
C     FLAG THE BLOCKS WITH TEMP. CHANGE
C
      IF (IHLOS.EQ.1.AND.TCONO.GT.ZERO) THEN
         DO 210 I = 1,NXNY
            IF (ABS(TEM(I)-TEMPOB).GT.ONEM6.AND.TTCHG(I).LT.-ONE) THEN
               TTCHG(I) = T-DT
            ENDIF
 210     CONTINUE
C
C     IGNORE HEAT LOSS FOR WELL BLOCKS
C
         DO 212 M = 1,NWELL
            ID = IDW(M)
            DO 214 IWB = 1,NWBC(ID)                              
               IJK2 = IJKPOS(IWC(ID,IWB),JWC(ID,IWB),KWC(ID,IWB))
               TTCHG(IJK2) = -999
 214        CONTINUE
 212     CONTINUE
C
C     CALCULATE OVERBURDEN HEAT FLUX
C
         DO 220 I = 1,NXNY
            IF (TTCHG(I).GT.-1.0) THEN
               HLTIME = T-TTCHG(I)
               CTD = TCONO/(DENO*CVSPO)
               DN = SQRT(CTD*HLTIME)/TWO
               T1 = 3.0*DN*DN+CTD*DT
               T2 = ONE/DN-CTD*DT/(DN*T1)
               T3 = (DN**3)/(T1*CTD*DT)
               TFLUXO(I) = TCONO*(-TEMPOB*T2-RINO(I)/T1-TEM(I)*T3)
            ENDIF
220      CONTINUE 
      ENDIF
C
C     CALCULATE UNDERBURDEN HEAT FLUX
C
      IF (IHLOS.EQ.1.AND.TCONU.GT.ZERO) THEN
         DO 225 I = NBLM3+1,NBL
            IF (ABS(TEM(I)-TEMPUB).GT.ONEM6.AND.TTCHG(I).LT.-1.0) THEN
               TTCHG(I) = T-DT
            ENDIF
 225     CONTINUE
C
C     IGNORE HEAT LOSS FOR WELL BLOCKS
C
         DO 226 M = 1,NWELL
            ID = IDW(M)
            DO 227 IWB = 1,NWBC(ID)                              
               IJK2 = IJKPOS(IWC(ID,IWB),JWC(ID,IWB),KWC(ID,IWB))
               TTCHG(IJK2) = -999
 227        CONTINUE
 226     CONTINUE
C
         DO 230 I = NBLM3+1,NBL
            IF (TTCHG(I).GT.-1.0) THEN
               HLTIME = T-TTCHG(I)
               CTD = TCONU/(DENU*CVSPU)
               DN = SQRT(CTD*HLTIME)/TWO
               T1 = 3.0*DN*DN+CTD*DT
               T2 = ONE/DN-CTD*DT/(DN*T1)
               T3 = (DN**3)/(T1*CTD*DT)
               TFLUXU(I) = TCONU*(-TEMPUB*T2-RINU(I)/T1-TEM(I)*T3)
            ENDIF
230      CONTINUE
      ENDIF
C                *** END FLEXI QUESTION ANY X Y Z ???
C
C     CALCULATE THE CONDUCTION FLUX IN Z-DIRECTION
C
      IF (ICOORD.EQ.4) THEN
C
C-----FLEXI
C
         IF (NZ.EQ.1) THEN
            DO 240 I = 1,NBL   
               AXY = DDY(I)*0.5*(DDX(I)+DDX(I-1)*ALX1(I-1))
               TFLUXO(I) = TFLUXO(I)*AXY
               TFLUXU(I) = TFLUXU(I)*AXY
 240        CONTINUE
            DO 245 I = 1,NBL
               DHT(I) = DHT(I)-(TFLUXO(I)+TFLUXU(I))
               TQLOS = TQLOS-DT*(TFLUXO(I)+TFLUXU(I))
 245        CONTINUE
         ELSE  
C
C     CALCULATE THE Z FLUX
C
            DO 250 I = NXNY+1,NBL
               HTERM(I) = -2.*CRTC*(TEM(I)-TEM(I-NXNY)) 
     *            *XYAREA(I-NXNY)/(DDZ(I-NXNY)*(ONE+ALZ1(I-NXNY)))
 250        CONTINUE
C
C     -------------------
C     DIFFERENCE THE FLUX
C     --------------------
C     1-TOP LAYER
C
            DO 253 I = 1,NX
            DO 253 J = 1,NY
               K = I + (J-1)*NX
               AXY = HXB(I)*DY(J)
               TFLUXO(K) = TFLUXO(K)*AXY
 253        CONTINUE
            DO 255 I = 1,NXNY
               DHT(I) = DHT(I)-(HTERM(I+NXNY)+TFLUXO(I))
               TQLOS = TQLOS-DT*TFLUXO(I)
 255        CONTINUE
C
C     2-MIDDLE LAYERS
C
             DO 260 I = NXNY+1,NBL-NXNY
                DHT(I) = DHT(I)-(HTERM(I+NXNY)-HTERM(I))
 260         CONTINUE
C
C     3-BOTTOM LAYER
C
             DO 265 I = NBL-NXNY+1,NBL
                TFLUXU(I) = TFLUXU(I)*XYAREA(I)
 265         CONTINUE
C
             DO 270 I = NBL-NXNY+1,NBL
                DHT(I) = DHT(I)-(TFLUXU(I)-HTERM(I))
                TQLOS = TQLOS-DT*TFLUXU(I)
 270         CONTINUE
         ENDIF
      ELSE
         IF (NZ.EQ.1) THEN
            DO 300 I = 1,NBL
               DHT(I) = DHT(I)-(TFLUXO(I)+TFLUXU(I))/DDZ(I)
               TQLOS = TQLOS-DT*(TFLUXO(I)+TFLUXU(I))*DDY(I)*DDX(I)
 300        CONTINUE
         ELSE  
C
C     CALCULATE THE Z FLUX
C
            DO 310 I = NXNY+1,NBL
               HTERM(I) = -2.*CRTC*(TEM(I)-TEM(I-NXNY))/(DDZ(I)+
     *                    DDZ(I-NXNY))
 310        CONTINUE
C
C     -------------------
C     DIFFERENCE THE FLUX
C     --------------------
C     1-TOP LAYER
C
            DO 320 I = 1,NXNY
               DHT(I) = DHT(I)-(HTERM(I+NXNY)+TFLUXO(I))/DDZ(I)
               TQLOS = TQLOS-DT*TFLUXO(I)*DDX(I)*DDY(I)
 320        CONTINUE
C
C     2-MIDDLE LAYERS
C
            DO 330 I = NXNY+1,NBL-NXNY
               DHT(I) = DHT(I)-(HTERM(I+NXNY)-HTERM(I))/DDZ(I)
 330        CONTINUE
C
C     3-BOTTOM LAYER
C
            DO 340 I = NBL-NXNY+1,NBL
               DHT(I) = DHT(I)-(TFLUXU(I)-HTERM(I))/DDZ(I)
               TQLOS = TQLOS-DT*TFLUXU(I)*DDX(I)*DDY(I)
 340        CONTINUE
         ENDIF
      ENDIF   
C
C     **********************************************
C     CALCULATE THE CONVECTIVE FLUXES FOR EACH PHASE
C     **********************************************
C
      DO 500 L = 1,NPHAS
C
C     CALCULATE THE HEAT CONTENT FOR EACH PHASE
C
         DO 510 I = 1,NBL
            WKSP1(I) = FACT1*DEN(I,L)*CVSPL(L)*TEM(I)
510      CONTINUE
C
C     CALCULATE THE X-DIRECTION UPSTREAM AND FLUXES
C
         DO 512 I = 1,NBL-1
            LX(I,L) = I
            IF (VELX(I,L).LT.ZERO) LX(I,L) = I+1
 512     CONTINUE
C
         DO 514 I = NX,NBL,NX
            LX(I,L) = I
 514     CONTINUE
C
         DO 520 I = 1,NBL
            HTERM(I) = WKSP1(LX(I,L))*VELX(I,L)
  520    CONTINUE
C
C     DIFFERENCE X-DIRECTION FLUXES
C
         IF (ICOORD.NE.2) THEN
            IF (ICOORD.EQ.4) THEN
C
C-----FLEXI
C
               DO 521 I = 1,NBL
                  HTERM(I) = HTERM(I)*YZAREA(I)
 521           CONTINUE
               DHT(1) = DHT(1)-HTERM(1)
               DO 525 I = 2,NBL
                  DHT(I) = DHT(I)-(HTERM(I)-HTERM(I-1))
 525           CONTINUE
            ELSE
               DHT(1) = DHT(1)-HTERM(1)/DDX(1)
               DO 530 I = 2,NBL
                  DHT(I) = DHT(I)-(DDY(I)*HTERM(I)-DDY(I-1)*HTERM(I-1))
     *                     /(DDY(I)*DDX(I))
 530           CONTINUE
            ENDIF
         ELSE
            DHT(1) = DHT(1)-2.*RRP(1)*HTERM(1)/DDX(1)
            DO 535 I = 2,NBL
               DHT(I) = DHT(I)-2.*(RRP(I)*HTERM(I)-RRP(I-1)*HTERM(I-1))
     *                  /DDX(I)
 535        CONTINUE
         ENDIF
C
C    CALCULATE Y-DIRECTION UPSTREAM AND FLUXES
C
         IF (NY.GT.1) THEN
            DO 550 I = 1,NBLM2
               LY(I,L) = I
               IF (VELY(I,L).LT.ZERO) LY(I,L) = I+NX
  550       CONTINUE
C
            DO 560 K = 1,NZ
               IBGN = K*NXNY-NZ+1
               IEND = K*NXNY
               DO 570 I = IBGN,IEND
                  LY(I,L) = I
 570           CONTINUE
 560        CONTINUE
C
            DO 600 I = 1,NBL
               HTERM(I) = WKSP1(LY(I,L))*VELY(I,L)
  600       CONTINUE
C
C     DIFFERENCE Y-DIRECTION FLUXES
C
            IF (ICOORD.EQ.4) THEN
C
C-----FLEXI
C
               DO 601 I = 1,NBL
                  HTERM(I) = HTERM(I)*XZAREA(I)
 601           CONTINUE
               DO 604 I = 1,NX
                  DHT(I) = DHT(I)-HTERM(I)
 604           CONTINUE
               DO 606 I = NX+1,NBL
                  DHT(I) = DHT(I)-(HTERM(I)-HTERM(I-NX))
 606           CONTINUE
            ELSE
               DO 610 I = 1,NX
                  DHT(I) = DHT(I)-HTERM(I)/DDY(I)
 610           CONTINUE
               DO 620 I = NX+1,NBL
                  DHT(I) = DHT(I)-(HTERM(I)-HTERM(I-NX))/DDY(I)
 620           CONTINUE
            ENDIF
         ENDIF
C
C     CALCULATE Z-DIRECTION UPSTREAM AND FLUXES
C
         IF (NZ.GT.1) THEN
            DO 630 I = 1,NBLM3
               LZ(I,L) = I
               IF (VELZ(I,L).LT.ZERO) LZ(I,L) = I+NXNY
 630        CONTINUE
C
            DO 640 I = NBLM3+1,NBL
               LZ(I,L) = I
 640        CONTINUE
C
            DO 700 I = 1,NBL
               HTERM(I) = WKSP1(LZ(I,L))*VELZ(I,L)
  700       CONTINUE
C
C     DIFFERENCE Z-DIRECTION FLUXES
C
            IF (ICOORD.EQ.4) THEN
C
C-----FLEXI
C
               DO 701 I = 1,NBL
                  HTERM(I) = HTERM(I)*XYAREA(I)
 701           CONTINUE
C
               DO 705 I = 1,NXNY
                  DHT(I) = DHT(I)-HTERM(I)
 705           CONTINUE
               DO 709 I = NXNY+1,NBL
                  DHT(I) = DHT(I)-(HTERM(I)-HTERM(I-NXNY))
 709           CONTINUE
            ELSE
               DO 710 I = 1,NXNY
                  DHT(I) = DHT(I)-HTERM(I)/DDZ(I)
 710           CONTINUE
               DO 720 I = NXNY+1,NBL
                  DHT(I) = DHT(I)-(HTERM(I)-HTERM(I-NXNY))/DDZ(I)
 720           CONTINUE
            ENDIF
         ENDIF
500   CONTINUE
C
C     **************************
C     ADD SINK AND SOURCE TERMS
C     **************************
C
      DO 800 M = 1,NWELL
         ID = IDW(M)
      DO 800 IWB = 1,NWBC(ID)
         IJK2 = IJKPOS(IWC(ID,IWB),JWC(ID,IWB),KWC(ID,IWB))
         DO 810 L = 1,NPHAS
            IF (Q(ID,IWB,L).GE.PRCSN) THEN
               HEATS = Q(ID,IWB,L)*DEN(IJK2,L)*FACT1*TEMINJ(ID)*
     *                 CVSPL(L)
               CUMHI = CUMHI+HEATS*DT
            ELSE
               HEATS = Q(ID,IWB,L)*DEN(IJK2,L)*FACT1*TEM(IJK2)*
     *                 CVSPL(L)
               CUMHP = CUMHP+HEATS*DT
            ENDIF
C
C-----FLEXI
C
            IF (ICOORD.EQ.4) THEN
               DHT(IJK2) = DHT(IJK2)+HEATS
            ELSE
               DHT(IJK2) = DHT(IJK2)+HEATS/(DDX(IJK2)*DDY(IJK2)
     *                   *DDZ(IJK2))
            ENDIF
810      CONTINUE
800   CONTINUE   
C
C     CALCULATE SOURCE TERMS TO/FROM THE EXTERNAL BOUNDARY FOR THE
C     RADIAL GEOMETRY
C   
      IF (ICOORD.EQ.2) THEN
         M = NWELL+1
         IDW(M) = NWELL+1
         ID = IDW(M)
         DO 812 K = 1,NZ
            IJK2 = K*NX
            DO 814 L = 1,NPHAS
               IF (Q(ID,K,L).GE.PRCSN) THEN
                  HEATS = Q(ID,K,L)*ENTHE(K,L)
                  CUMHI = CUMHI+HEATS*DT
               ELSE
                  HEATS = Q(ID,K,L)*TEM(IJK2)*DEN(IJK2,L)*FACT1
     *                    *CVSPL(L)
                  CUMHP = CUMHP+HEATS*DT
               ENDIF
               DHT(IJK2) = DHT(IJK2)+HEATS/(DDX(IJK2)*DDY(IJK2)*
     *                     DDZ(IJK2))
814         CONTINUE
812      CONTINUE
      ENDIF
C
C     CALCULATE SOURCE TERMS TO/FROM THE EXTERNAL BOUNDARY 
C   
      IF (ICOORD.NE.2) THEN
         IF (IBL.EQ.1) THEN
            DO 820 J = 1,NY
            DO 820 K = 1,NZ
               IL = IJKPOS(1,J,K) 
            DO 820 L = 1,NPHAS
               IF (QL(IL,L).GE.PRCSN) THEN
                  HEATS = QL(IL,L)*FACT1*TEMPI*DEN(IL,L)*CVSPL(L)
                  CUMHI = CUMHI+HEATS*DT
               ELSE
                  HEATS = QL(IL,L)*FACT1*TEM(IL)*DEN(IL,L)*CVSPL(L)
                  CUMHP = CUMHP+HEATS*DT
               ENDIF
               DHT(IL) = DHT(IL)+HEATS/(DDX(IL)*DDY(IL)*DDZ(IL))
820         CONTINUE
         ENDIF
C   RIGHT BOUNDARY
         IF (IBR.EQ.1) THEN
            DO 825 J = 1,NY
            DO 825 K = 1,NZ
               IR = IJKPOS(NX,J,K) 
            DO 825 L = 1,NPHAS
               IF (QL(IR,L).GE.PRCSN) THEN
                  HEATS = QL(IR,L)*FACT1*TEMPI*DEN(IR,L)*CVSPL(L)
                  CUMHI = CUMHI+HEATS*DT
               ELSE
                  HEATS = QL(IR,L)*FACT1*TEM(IL)*DEN(IL,L)*CVSPL(L)
                  CUMHP = CUMHP+HEATS*DT
               ENDIF
cbug in 9.0
               DHT(IR) = DHT(IR)+HEATS/(DDX(IR)*DDY(IR)*DDZ(IR))
825         CONTINUE
         ENDIF
      ENDIF
C
C     ***************************************************************
C     CALCULTE NEW TOTAL VOLUMETRIC HEAT CAPACITY (TERM M AT NEW TIME 
C     LEVEL)
C     ***************************************************************
C
      IF (COMPR.GT.PRCSN) THEN
         DO 900 I = 1,NBL
            COMP1(I) = POR(I)*(1.+COMPR*(P(I,1)-PR(I)))
 900     CONTINUE
      ELSE
         DO 910 I = 1,NBL
            COMP1(I) = POR(I)
 910     CONTINUE        
      ENDIF
C
      DO 920 I = 1,NBL
         WKSP1(I) = 0.0
         DO 930 L = 1,NPHAS
            WKSP1(I) = WKSP1(I)+COMP1(I)*FACT1*DEN(I,L)*S(I,L)*CVSPL(L)
 930     CONTINUE
         THCNEW(I) = WKSP1(I)+(1.-COMP1(I))*DENS*CVSPR
 920  CONTINUE  
C
C     CALCULATE THE THE OLD HEAT CONTENT ( TERM M*T AT NTH TIME LEVEL)
C 
      DO 1000 I = 1,NBL
         HTERM(I) = TVHC(I)*TEM(I)
1000  CONTINUE
C
C     UPDATE THE HEAT CONTENT ( RIGHT HAND SIDE OF THE EQUATION)
C
      IF (ICOORD.EQ.4)THEN
         DO 1009 I = 1,NBL
            VOLUME = XZAREA(I)*DDY(I)
            DHT(I) = DHT(I)*DT/VOLUME
            HTERM(I) = HTERM(I)+DHT(I)
1009     CONTINUE
      ELSE
         DO 1010 I = 1,NBL
            DHT(I) = DHT(I)*DT
            HTERM(I) = HTERM(I)+DHT(I)
1010     CONTINUE
      ENDIF
C
C     UPDATE THE TOTAL HEAT CAPACITIES 
C
      DO 1020 I = 1,NBL
         TVHC(I) = THCNEW(I)
 1020 CONTINUE
C
C     ***********************************************
C     COMPUTE THE IMPLICIT PART OF THE HEATLOSS MODEL
C     ***********************************************
C
      DO 1100 I = 1,NBL
         TFLUXO(I) = 0.0
         TFLUXU(I) = 0.0
         TFLM(I) = 0.0
1100  CONTINUE
C
C     CALCULATE OVERBURDEN HEAT FLUX
C
      IF (IHLOS.EQ.1.AND.TCONO.GT.ZERO) THEN
          DO 1110 I = 1,NXNY
             IF (TTCHG(I).GT.-ONE) THEN
                HLTIME = T-TTCHG(I)
                CTD = TCONO/(DENO*CVSPO)
                DN = SQRT(CTD*HLTIME)/TWO
                T1 = 3.0*DN*DN+CTD*DT
                T2 = ONE/DN-CTD*DT/(DN*T1)
                T3 = (DN**3)/(T1*CTD*DT)
                TFLUXO(I) = TCONO*(T2+T3)
             ENDIF
1110      CONTINUE 
      ENDIF
C
C     CALCULATE UNDERBURDEN HEAT FLUX
C
      IF (IHLOS.EQ.1.AND.TCONU.GT.ZERO) THEN
         DO 1120 I = NBLM3+1,NBL
            IF (TTCHG(I).GT.-ONE) THEN
               HLTIME = T-TTCHG(I)
               CTD = TCONU/(DENU*CVSPU)
               DN = SQRT(CTD*HLTIME)/TWO
               T1 = 3.0*DN*DN+CTD*DT
               T2 = ONE/DN-CTD*DT/(DN*T1)
               T3 = (DN**3)/(T1*CTD*DT)
               TFLUXU(I) = TCONU*(T2+T3)
            ENDIF
1120     CONTINUE
      ENDIF
C
C     CALCULATE THE TERM FOR MODIFYING THE FLUXES DUE TO HEATLOSS
C
      IF (IHLOS.EQ.1) THEN
         IF (ICOORD.EQ.4) THEN
C
C---- FLEXI
C
            IF (NZ.EQ.1) THEN
               DO 1135 J = 1,NY
               DO 1135 I = 1,NX
                  IJ = I + (J-1)*NX
                  DELZ = 0.5*(DDZ(IJ)+HZB(I))
                  TFLM(IJ) = DT*(TFLUXU(IJ)+TFLUXO(IJ))/DELZ
 1135          CONTINUE
            ELSE
               IF (TCONO.GT.ZERO) THEN
                  DO 1145 J = 1,NY
                  DO 1145 I = 1,NX
                     IJ = I + (J-1)*NX
                     DELZ = 0.5*(DDZ(IJ)+HZB(I))
                     TFLM(IJ) = DT*TFLUXO(IJ)/DELZ
 1145             CONTINUE
               ENDIF
C
               IF (TCONU.GT.ZERO) THEN
                  DO 1155 I = NBLM3+1,NBL
                     DELZ = 0.5*(DDZ(I)+DDZ(I-NXNY)*ALZ1(I-NXNY))
                     TFLM(I) = DT*TFLUXU(I)/DELZ
 1155             CONTINUE
               ENDIF
            ENDIF
        ELSE
            IF (NZ.EQ.1) THEN
               DO 1130 I = 1,NXNY
                  TFLM(I) = DT*(TFLUXU(I)+TFLUXO(I))/DDZ(I)
 1130          CONTINUE
            ELSE
               IF (TCONO.GT.ZERO) THEN
                  DO 1140 I = 1,NXNY
                     TFLM(I) = DT*TFLUXO(I)/DDZ(I)
 1140             CONTINUE
               ENDIF
C
               IF (TCONU.GT.ZERO) THEN
                  DO 1150 I = NBLM3+1,NBL
                     TFLM(I) = DT*TFLUXU(I)/DDZ(I)
 1150             CONTINUE
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
C     ******************************************************
C     UPDATE TEMPERATURES AND COMPUTE THE MAX. TEMP. CHANGE
C     ******************************************************
C
      DTTMAX = 0.0
      DO 1160 I = 1,NBL
         DTEMP(I) = HTERM(I)/(TVHC(I)+TFLM(I))-TEM(I)
         TEM(I) = TEM(I)+DTEMP(I)
         DTTMAX = MAX(DTTMAX,ABS(DTEMP(I)))
1160  CONTINUE
C
C     ***************************************
C     UPDATE THE TERM I IN VINSOME MODEL
C     **************************************
C
      IF (IHLOS.EQ.1.AND.TCONO.GT.ZERO) THEN
         DO 1220 I = 1,NXNY
            IF (TTCHG(I).GT.-ONE) THEN
               HLTIME = T-TTCHG(I)
               CTD = TCONO/(DENO*CVSPO)
               DN = SQRT(HLTIME*CTD)/TWO
               T1 = 3.*DN*DN+CTD*DT
               T2 = CTD*DT*(TEM(I)-TEMPOB)/DN+RINO(I)
               T3 = (DN**3)*DTEMP(I)/(CTD*DT)
               PTERM = (T2-T3)/T1
               QTERM = PTERM/DN-(TEM(I)-TEMPOB)/(TWO*DN*DN)+
     *                 DTEMP(I)/(TWO*CTD*DT)
               RINO(I) = (TEM(I)-TEMPOB+PTERM*DN)*DN+2.*QTERM*(DN**3)
            ENDIF
 1220    CONTINUE
      ENDIF
C
      IF (IHLOS.EQ.1.AND.TCONU.GT.ZERO) THEN
          DO 1230 I = NBLM3+1,NBL
             IF (TTCHG(I).GT.-ONE) THEN
                HLTIME = T-TTCHG(I)
                CTD = TCONU/(DENU*CVSPU)
                DN = SQRT(HLTIME*CTD)/TWO
                T1 = 3.*DN*DN+CTD*DT
                T2 = CTD*DT*(TEM(I)-TEMPUB)/DN+RINU(I)
                T3 = (DN**3)*DTEMP(I)/(CTD*DT)
                PTERM = (T2-T3)/T1
                QTERM = PTERM/DN-(TEM(I)-TEMPUB)/(TWO*DN*DN)+
     *                  DTEMP(I)/(TWO*CTD*DT)
                RINU(I) = (TEM(I)-TEMPUB+PTERM*DN)*DN+2.*QTERM*(DN**3)
             ENDIF
 1230    CONTINUE
      ENDIF
C
C     UPDATE TOTAL HEAT LOSS
C
      IF (ICOORD.EQ.4) THEN
C
C-----FLEXI
C
         IF (IHLOS.EQ.1) THEN
            IF (NZ.EQ.1) THEN
               IF (TCONU.GT.ZERO.OR.TCONO.GT.ZERO) THEN
                  DO 1244 I = 1,NXNY
                     TQLOS = TQLOS-DT*TEM(I)*(TFLUXU(I)+TFLUXO(I))
     *                      *DDY(I)*0.5*(DDX(I)+DDX(I-1)*ALX1(I-1))
1244              CONTINUE
               ENDIF
            ELSE
               IF (TCONO.GT.ZERO) THEN
                  DO 1246 I = 1,NX
                  DO 1246 J = 1,NY
                     K = I + (J-1)*NX
                     TQLOS = TQLOS-DT*TFLUXO(K)*TEM(K)*HXB(I)*DY(J)
 1246             CONTINUE
               ENDIF
               IF (TCONU.GT.ZERO) THEN
                  DO 1248 I = NXNY+1,NBL
                     TQLOS = TQLOS-DT*TFLUXU(I)*TEM(I)*XYAREA(I)
 1248             CONTINUE
               ENDIF
            ENDIF
         ENDIF
      ELSE
         IF (IHLOS.EQ.1) THEN
            IF (NZ.EQ.1) THEN
               IF (TCONU.GT.ZERO.OR.TCONO.GT.ZERO) THEN
                  DO 1240 I = 1,NXNY
                     TQLOS = TQLOS-DT*TEM(I)*(TFLUXU(I)+TFLUXO(I))
     *                       *DDX(I)
     *                      *DDY(I)
1240              CONTINUE
               ENDIF
            ELSE
               IF (TCONO.GT.ZERO) THEN
                  DO 1250 I = 1,NXNY
                     TQLOS = TQLOS-DT*TFLUXO(I)*TEM(I)*DDX(I)*DDY(I)
 1250             CONTINUE
               ENDIF
               IF (TCONU.GT.ZERO) THEN
                  DO 1260 I = NXNY+1,NBL
                     TQLOS = TQLOS-DT*TFLUXU(I)*TEM(I)*DDX(I)*DDY(I)
 1260             CONTINUE
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
C     COMPUTE TEMPERATURE IN THE PRODUCER WELLBORE
C
      TW1 = 0.0
      TW2 = 0.0
      DO 1300 M = 1,NWELL
         ID = IDW(M)
         IF (PWF(ID).LT.ZERO) GO TO 1300
         IF (IFLAG(ID).EQ.1.OR.IFLAG(ID).EQ.3) GO TO 1300
         IJK1 = NBL+ID
         DO 1310 IWB = 1,NWBC(ID)
            IJK2 = IJKPOS(IWC(ID,IWB),JWC(ID,IWB),KWC(ID,IWB))
            DO 1320 L = 1,NPHAS
               TW1 = TW1+Q(ID,IWB,L)*CVSPL(L)*DEN(IJK2,L)*TEM(IJK2)  
               TW2 = TW2+Q(ID,IWB,L)*CVSPL(L)*DEN(IJK2,L)
 1320       CONTINUE
 1310    CONTINUE
         TEM(IJK1) = TW1/TW2
 1300 CONTINUE
C
      RETURN
      END
