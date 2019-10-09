      PROGRAM READPP
      IMPLICIT NONE
      INTEGER I,K,NX,NZ,NF1,NF2,NF3,NF4
      PARAMETER (NX=8,NZ=26,NF1=96,NF2=97,NF3=98,NF4=99)
      REAL*8 POR(NZ,NX),KXX(NZ,NX),KZZ(NZ,NX)

      OPEN(UNIT=NF1,FILE='porosity.dat',STATUS='old')
      READ(NF1,*)
      READ(NF1,*) ((POR(K,I),K=1,NZ),I=1,NX)
      CLOSE(NF1)

      OPEN(UNIT=NF2,FILE='permeability.dat',STATUS='old')
      READ(NF2,*)
      READ(NF2,*) ((KXX(K,I),K=1,NZ),I=1,NX)
      READ(NF2,*)
      READ(NF2,*)
      READ(NF2,*) ((KZZ(K,I),K=1,NZ),I=1,NX)
      CLOSE(NF2)

      OPEN(UNIT=NF3,FILE='poro2d.dat',STATUS='unknown')
      WRITE(NF3,*)  'POROSITY1() = '
      WRITE(NF3,15) ((POR(K,I),I=1,NX),K=1,NZ)
      CLOSE(NF3)

      OPEN(UNIT=NF4,FILE='perm2d.dat',STATUS='unknown')
      WRITE(NF4,*)  'XPERM1() = '
      WRITE(NF4,15) ((KXX(K,I),I=1,NX),K=1,NZ)
      WRITE(NF4,*)  
      WRITE(NF4,*)  'YPERM1() = 1.0'
      WRITE(NF4,*)  
      WRITE(NF4,*)  'ZPERM1() = '
      WRITE(NF4,15) ((KZZ(K,I),I=1,NX),K=1,NZ)
      CLOSE(NF4)

  15  FORMAT (2X,7F12.4)

      STOP
      END

