c---->------------------------------------------------------------------<
c  Generate a debugging jacobian
c---->------------------------------------------------------------------<
      SUBROUTINE SLJACNORM(
     > IDIM,JDIM,KDIM,LDIM,IL1,IL2,JLV1,JLV2,KL1,KL2,KEYOUT,NBLK,
     > NS, NEV, AJ , ANORM )
c
      IMPLICIT NONE
c---->
      INTEGER IDIM,JDIM,KDIM,LDIM
      INTEGER IL1,IL2,JLV1(KDIM),JLV2(KDIM),KL1,KL2,NBLK
      INTEGER KEYOUT(IDIM,JDIM,KDIM)
      INTEGER NS,NEV
c---->
      REAL*8 ANORM
      REAL*4 AJ(IDIM,JDIM,KDIM,NS,NEV,NEV)
      INTEGER I,J,K,JL1,JL2,IS,IE,IV,IC
      INTRINSIC ABS
c---->
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
                    ANORM = ANORM +
     >                AJ(I,J,K,IS,IE,IV) * AJ(I,J,K,IS,IE,IV)
                  end do
                end do
              end do
            end if
          end do
        end do
      end do
c---->
      return
      end
c---->------------------------------------------------------------------<

