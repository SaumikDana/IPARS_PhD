C EVISUAL.F - PRINT ELASTIC MODEL VISUALIZATION DAT
C
C ROUTINES IN THIS MODULE:
C
C SUBROUTINE EVIS_TRANSL(SCALAR,NUM,NAME)
C SUBROUTINE EVIS_OUTPUT()
C
C CODE HISTORY:
C   XIULI GAI  08/10/2003/  MODIFIED FROM IVISUAL.DF
C    TAMEEM ALMANI   07/27/2016 INCLUDE NECESSARY CHANGES FOR
C                               COUPLING WITH MECHANICS
C*********************************************************************
      SUBROUTINE EVIS_TRANSL(SCALAR,NUM,NAME)
C**********************************************************************
C 1. Routine lookup the <name> in the glossary for the model which is read
C    from ivisual.h
C 2. Set VIS_VARNAMES, VIS_OFFSETS and VIS_IPARS_VARS
C**********************************************************************
      IMPLICIT NONE
      INCLUDE 'visual.h'
      INCLUDE 'evisual.h'
      INCLUDE 'emodel.h'
      INCLUDE 'control.h'
C      INCLUDE 'xmodel.h'

      CHARACTER*20 NAME
      INTEGER NUM
      LOGICAL SCALAR,SUCCESS

      INTEGER I,K,NNUM

      SUCCESS=.FALSE.
      IF(SCALAR) THEN
         VIS_VARNAMES(NUM) = NAME
         VIS_OFFSETS(NUM) = 1
         VIS_IPARS_NAMES (NUM) = NAME
         DO I=1,IPARS_NSCL
            IF(IPARS_SCL_NAMES(1,I).EQ.NAME) THEN
               IF(MBPOROE) THEN ! SAUMIK,BGANIS
                  VIS_SCL_POROHEX = VIS_SCL_POROHEX + 1
                  IF (NUM.EQ.1) STARTMODACT = MODACT
               ENDIF
               VIS_VARNAMES(NUM) = IPARS_SCL_NAMES(2,I)
               VIS_IPARS_NAMES(NUM) = IPARS_SCL_NAMES(3,I)
               VIS_OFFSETS(NUM) = IPARS_SCL_OFFSETS(I)
               VIS_VAL_NODAL(NUM) = 1
               SUCCESS=.TRUE.
               GO TO 1
            ENDIF
         ENDDO
      ENDIF

      IF (.NOT.SUCCESS) THEN
      IF(PEFLOW.EQ.17.OR.MBPOROE) THEN ! SAUMIK,BGANIS
         MODACT = 17
         CALL TVIS_TRANSL(SCALAR,NUM,NAME)
         MODACT = 15
      ENDIF
C       IF(PEFLOW.EQ.16.OR.MBPOROE) THEN ! SAUMIK,BGANIS
C	         MODACT = 16
C          CALL XVIS_TRANSL(SCALAR,NUM,NAME)
C	         MODACT = 15
C       ENDIF

ctm
C      IF(PEFLOW.EQ.18.OR.MBPOROE) THEN ! SAUMIK,BGANIS
C         MODACT = 18
C         CALL HVIS_TRANSL(SCALAR,NUM,NAME)
C         MODACT = 15
C      ENDIF
ctm



cHYDROE_MPFA      IF(PEFLOW.EQ.MGMODEL) THEN
cHYDROE_MPFA         MODACT = MGMODEL
cHYDROE_MPFA         CALL GVIS_TRANSL(SCALAR,NUM, NAME)
cHYDROE_MPFA         MODACT = 15
cHYDROE_MPFA      ENDIF
      ENDIF

      RETURN
c 1    CALL VIS_ARYNUM(NUM)

! SAUMIK,BGANIS
 1    CONTINUE
      IF(MBPOROE) THEN
         CALL VIS_ARYNUMMB(NUM,VIS_SCL_POROHEX)
      ELSE
         CALL VIS_ARYNUM(NUM)
      ENDIF

      END

C**********************************************************************
      SUBROUTINE EVIS_OUTPUT ()
C**********************************************************************
C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'visual.h'
      INCLUDE 'emodel.h'

      IF((VISFLAG.LT.1).OR.(VISFLAG.GT.MAXVISFLAG)) THEN
         GOTO 1
      ENDIF
      IF(PEFLOW.EQ.17.OR.MBPOROE) THEN ! SAUMIK,BGANIS
         MODACT = 17
         CALL TVIS_OUTPUT()
         MODACT = 15
      ENDIF

C      IF(PEFLOW.EQ.16.OR.MBPOROE) THEN ! SAUMIK,BGANIS
C         MODACT = 16
C         CALL XVIS_OUTPUT()
C         MODACT = 15
C      ENDIF

ctm
C      IF(PEFLOW.EQ.18.OR.MBPOROE) THEN ! SAUMIK,BGANIS
C         MODACT = 18
C         CALL HVIS_OUTPUT()
C         MODACT = 15
C      ENDIF
ctm

cHYDROE_MPFA      IF(PEFLOW.EQ.MGMODEL) THEN
cHYDROE_MPFA         MODACT = MGMODEL
cHYDROE_MPFA         CALL GVIS_OUTPUT()
cHYDROE_MPFA         MODACT = 15
cHYDROE_MPFA      ENDIF

C      IF(SEISMIC)  CALL IGASSMAN()
!bw      CALL VIS_OUTPUT()

       IF(MBPOROE) THEN ! SAUMIK,BGANIS
          VIS_NVARS = VIS_SCL_POROHEX
          CURRENT_MODEL = MODACT
          DO I=1, VIS_NVARS
             N_ARRAY = N_VIS_VARS(I,CURRENT_MODEL)
        CALL UPDATE(N_ARRAY,2)
          ENDDO
          IF(VIS_SCL_POROHEX.GT.0) CALL VIS_OUTPUT()
      ENDIF

 1    CONTINUE

      RETURN

      END

C**********************************************************************
      SUBROUTINE EVIS_INIT ()
C**********************************************************************
C      INCLUDE 'msjunk.h'
      INCLUDE 'control.h'
      INCLUDE 'emodel.h'
      IF(PEFLOW.EQ.17.OR.MBPOROE) THEN ! SAUMIK,BGANIS
         MODACT = 17
         CALL TVIS_INIT()
         MODACT = 15
      ENDIF

C      IF(PEFLOW.EQ.16.OR.MBPOROE) THEN ! SAUMIK,BGANIS
C         MODACT = 16
C         CALL XVIS_INIT()
C         MODACT = 15
C      ENDIF

ctm
C      IF(PEFLOW.EQ.18.OR.MBPOROE) THEN ! SAUMIK,BGANIS
C         MODACT = 18
C         CALL HVIS_INIT()
C         MODACT = 15
C      ENDIF
ctm

cHYDROE_MPFA      IF(PEFLOW.EQ.MGMODEL) THEN
cHYDROE_MPFA         MODACT = MGMODEL
cHYDROE_MPFA         CALL GVIS_INIT()
cHYDROE_MPFA         MODACT = 15
cHYDROE_MPFA      ENDIF

      END

