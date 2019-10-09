C  DIVIDEINSERT.F - INSERTIONS to DIVIDE.DF DUE TO METHOD 4

C  ROUTINES IN THIS MODULE:

C  SUBROUTINE DIVIDEINSERT1 
C  SUBROUTINE DIVIDEINSERT2 
C  SUBROUTINE DIVIDEINSERT3 

C  CODE HISTORY:

C  YURI VASSILEVSKI  2/24/00    

C*********************************************************************
      SUBROUTINE DIVIDEINSERT1 (N,NX,NY,NZ,NUMBLK,NUMPRC) 
C*********************************************************************

      INCLUDE 'sprb.h'

      n1(N) = NZ
      n2(N) = NY
      n3(N) = NX
c Predefine the distribution of processors per each block
      PRCBLK(N)    = 1
      IF (NUMBLK.eq.1) PRCBLK(N) = NUMPRC

      RETURN
      END

C*********************************************************************
      SUBROUTINE DIVIDEINSERT2 (NUMBLK,NUMPRC,MAXBLKS,NFOUT,LEVELC,*)
C*********************************************************************

      INCLUDE 'sprb.h'
      LOGICAL LEVELC


       IF (NUMPRC.GT.1) THEN
c Get the distribution of processors per each block
c If there is no respective line, it is already done PRCBLK(N)=1, N=1,..,NUMBLK
        CALL GETVAL('PRCBLK ',PRCBLK,'I4',MAXBLKS,0,0,0,NDUM,NERR)
        IF (NDUM.LT.NUMBLK.and.LEVELC) THEN
          write(NFOUT,*)'Not all PRCBLK where defined explicitly:'
          write(NFOUT,*)'Predefined values will be used for remaining'
        ELSE IF (NDUM.GT.NUMBLK.and.LEVELC) THEN
          write(NFOUT,*)'There are too many PRCBLK entries:'
          write(NFOUT,*)'The last entries are ignored'
        END IF
C Count the number of processors already allocated in previous blocks 
        NPCBLK(1) = 0
        DO 400 N = 2, NUMBLK
 400    NPCBLK(N) = NPCBLK(N-1) + PRCBLK(N-1)
c  Check for correct user defined processor distribution among the blocks
        IF (NPCBLK(NUMBLK)+PRCBLK(NUMBLK).NE.NUMPRC) THEN
          IF (LEVELC) THEN
          WRITE(NFOUT,*)'DIVIDE: NUMPRC.NE.SUM(Procs at Block)'
          WRITE(NFOUT,*)'Discrepancy between NUMPRC and PRCBLK!'
          END IF
          RETURN 1
        END IF
       END IF

      RETURN
      END

C*********************************************************************
      SUBROUTINE DIVIDEINSERT3(MYPRC,NPC,NFC,N0,NX,NY,NZ,
     &                    PRCMAP,NER,NERF,NERP,NFOUT,LEVELC,*)
C*********************************************************************

      INTEGER PRCMAP(*)
      LOGICAL LEVELC

      INCLUDE 'sprb.h'

c Work arrays for getloc,getbnd
      parameter (MXLEV=7)
      integer   P4(MXLEV+1),SPLIT(MXLEV+(4**MXLEV-1)/3),IUFNFC,ILFNFC

c   subroutines inispl,getloc,getbnd are defined in dc2d.f, dc3d.f



c  Check for thickness of the strips
       NCHECK = 4**(1+int(1d-13+log(2d0*PRCBLK(NFC))/log(4d0)))
       IF (NCHECK.gt.NZ) THEN
         IF (LEVELC) THEN 
         WRITE(NFOUT,*)'DIVIDE: For ',PRCBLK(NFC),
     &                 ' prs NZ should be >=',NCHECK
         WRITE(NFOUT,*)'Error: insufficient thickness of the strips'
         END IF
         RETURN 1
       END IF
c  Check for  the number of processors at the current block
       IF (2**int(log(dble(PRCBLK(NFC)))/log(2.d0) + 0.5d0)
     &    .ne.PRCBLK(NFC)) THEN
         IF (LEVELC) THEN 
         WRITE(NFOUT,*)'DIVIDE: The number of processors is not 2**k'
         WRITE(NFOUT,*)'Error: wrong number of processors at block', 
     &                  NFC
         END IF
         RETURN 1
       END IF
       LEV  = 1 + max(int(1d-13 + log(dble(NZ))/log(4.d0)),0)
       if (LEV.gt.MXLEV) then
         IF (LEVELC) THEN 
         WRITE(NFOUT,*)'DIVIDE: Increase MXLEV in divide.df to ', LEV
         WRITE(NFOUT,*)'Error: SPLIT has to be enlarged'
         END IF
         RETURN 1
       end if
       P4(1) = 1
       do k=1,LEV
          P4(k+1) = 4*P4(k)
       end do
       call inispl(NZ,SPLIT,LEV,P4)
       call getloc(locs,ilocf,ilocl,inp,LEV,
     &             PRCBLK(NFC),NPC-NPCBLK(NFC),P4)
       call getbnd(LEV,ilocf,ilb,iub,SPLIT,P4)
       ILFNFC = max(ilb,1)
       call getbnd(LEV,ilocl,ilb,iub,SPLIT,P4)
       IUFNFC = iub-1
       if (MYPRC.eq.NPC) then
          IUF(NFC) = IUFNFC
          ILF(NFC) = ILFNFC
          n1(NFC) = IUF(NFC)-ILF(NFC)+1
       end if
c  Check for  bad result of getbnd
       IF (ILFNFC.GT.IUFNFC) THEN
         IF (LEVELC) THEN 
         WRITE(NFOUT,*) "DIVIDE: ILF.GT.IUF",ILFNFC,IUFNFC
         WRITE(NFOUT,*) 'Bad output from getbnd'
         END IF
         RETURN 1
       END IF
       IF (IUFNFC.GT.NZ) THEN
         IF (LEVELC) THEN 
         WRITE(NFOUT,*) "DIVIDE: IUF.GT.NZ",IUFNFC,NZ
         WRITE(NFOUT,*) 'Bad output from getbnd'
         END IF
         RETURN 1
       END IF
       DO 301 K=ILFNFC,IUFNFC
       NN=N0+K*NY
       DO 301 J=1,NY
       IF (PRCMAP(NN+J).EQ.-1) THEN
         PRCMAP(NN+J)=NPC
         NER=NER-NX
         NERF=NERF-NX
         NERP = -1
       ENDIF
  301  CONTINUE

      RETURN
      END
