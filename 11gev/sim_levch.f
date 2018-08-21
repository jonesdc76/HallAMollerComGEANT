      REAL FUNCTION SIM_LEVCH(N1,N2,N3,K1,K2,K3)
C
C===    Calculates the Levchuk effect 
C===    N1..    - number of simulated events 
C===    K1..    - number of useful events
C       N1 - scattering on external shells 0
C       N2 - on L-shell 1
C       N3 - on K-shell 2
C
      VECTOR EFLEVCH(2)
      VECTOR EFFIM(3)
      VECTOR EREFFIM(3)
C
      IMPLICIT NONE
      INTEGER N1,N2,N3,K1,K2,K3
C
      INTEGER nt(3),kt(3),i
      REAL al(4),r,er,r1,etot,r0,ef(3),eef(3)
C
C     ------------------------------------------------------------------
C
      SIM_LEVCH=0.
C      etot=26.5                ! Supermendur
      etot=26.                 ! Iron
      al(1)=2.216/etot         ! polarized electrons (M shell)
      al(2)=8./etot            ! L-shell
      al(3)=2./etot            ! K-shell 
      al(4)=1.-al(2)-al(3)     ! not L or K shell (free electrons) M shell
C
      nt(1)=N1
      nt(2)=N2
      nt(3)=N3
      kt(1)=K1
      kt(2)=K2
      kt(3)=K3
      DO i=1,3
         ef(i)=0.
         eef(i)=0.
         IF(nt(i).GT.0) THEN
            ef(i)=REAL(kt(i))/REAL(nt(i))
            eef(i)=SQRT(ef(i)*(1.-ef(i))/nt(i))
         ENDIF
         EFFIM(i)=ef(i)
         EREFFIM(i)=eef(i)
      ENDDO
C
      r0=al(1)/(al(4)+al(2)+al(3))
      r1=al(1)*ef(1)/(ef(1)*al(4)+ef(2)*al(2)+ef(3)*al(3))
      r=(r1-r0)/r0


      er=al(1)/(ef(1)*al(4)+ef(2)*al(2)+ef(3)*al(3))**2
     +         *SQRT(((ef(2)*al(2)+ef(3)*al(3))*eef(1))**2
     +               +(ef(1)*al(2)*eef(2))**2
     +               +(ef(1)*al(3)*eef(3))**2)/r0
C
      WRITE(6,1000) r,er
 1000 FORMAT(' Levchuk effect: ',F10.5,' +/- ',F8.5)
C
      EFLEVCH(1)=r
      EFLEVCH(2)=er
      SIM_LEVCH=r
      END