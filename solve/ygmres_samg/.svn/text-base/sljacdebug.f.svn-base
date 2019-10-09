c---->------------------------------------------------------------------<
c  Generate a debugging jacobian
c---->------------------------------------------------------------------<
      SUBROUTINE SLJACDEBUG(
     > IDIM,JDIM,KDIM,LDIM,IL1,IL2,JLV1,JLV2,KL1,KL2,KEYOUT,NBLK,
     > NS, NEV, AJ )
c
      IMPLICIT NONE
c---->
      INTEGER IDIM,JDIM,KDIM,LDIM
      INTEGER IL1,IL2,JLV1(KDIM),JLV2(KDIM),KL1,KL2,NBLK
      INTEGER KEYOUT(IDIM,JDIM,KDIM)
      INTEGER NS,NEV
c---->
      REAL*4 AJ(IDIM,JDIM,KDIM,NS,NEV,NEV)
c---->
      REAL*4 D(7,3,3)
c
      INTEGER I,J,K,JL1,JL2,IS,IE,IV,IC
      INTRINSIC ABS
c---->
c  To look at each cell's stencil in a debuggeer
c
      do K = KL1,KL2
        JL1 = JLV1(K)
        JL2 = JLV2(K)
        do J = JL1,JL2
          do I = IL1,IL2
            if ( KEYOUT(I,J,K) .eq. 1 ) then
              do IV = 1,NEV
                do IE = 1,NEV
                  do IS = 1,7
                    D(IS,IE,IV) = AJ(I,J,K,IS,IE,IV)
                  end do
                end do
              end do
              IS = IS
            end if
          end do
        end do
      end do
c---->
      do IV = 1,NEV
        do IE = 1,NEV
          do IS = 1,NS
            do K = 1,KDIM
              do J = 1,JDIM
                do I = 1,IDIM
                  AJ(I,J,K,IS,IE,IV) = 0.0
                end do
              end do
            end do
          end do
        end do
      end do

      IC = 0
      do K = KL1,KL2
        JL1 = JLV1(K)
        JL2 = JLV2(K)
        do J = JL1,JL2
          do I = IL1,IL2
            if ( KEYOUT(I,J,K) .eq. 1 ) then
              IC = IC + 1
              do IS = 1,NS
                do IV = 1,NEV
                  do IE = 1,NEV
c                    AJ(I,J,K,IS,IE,IV) = -1.0 ;
                  end do
                end do
              end do
              do IV = 1,NEV
                AJ(I,J,K,1,IV,IV) = 1.0 
c                AJ(I,J,K,1,IV,IV) = 1000.0 + 100.0 * IV ;
              end do
            end if
          end do
        end do
      end do
      return
      end
c---->------------------------------------------------------------------<

