      REAL FUNCTION CNTPR(IPRFL)
C
      INTEGER IPRFL
C
C===    Example: reading the COMGEANT NTUPLE
C===             prints different parts of the data, fills a histogram
C===             ipri=1 - detectors
C                     2 - kinematics, tracks
C                     3 - hits
C                     4 - digitisings 
      INCLUDE ?
C
      INTEGER nhd1,ihd1(100)
C
      INTEGER    mxhh,mxhd
      PARAMETER (mxhh=9000,mxhd=12000)
      INTEGER nhd(mxhh),jhd(mxhh),ihd(mxhd),khd
C
      
      CNTPR=1.
      ipri=IPRFL
      lun=6
C      IF(IDNEVT.NE.1) GO TO 999
C      write(6,*) ' Start '
C      write(6,1000) idnevt,nkine
C
      IF(ipri.EQ.1) THEN
C
        IF(ndet.GT.0) THEN
          WRITE(lun,1000) ndet,mxvs
          DO idet=1,ndet
            WRITE(lun,1100) idet,itydet(idet),iddet(idet),nsetdet(idet)
     +               ,ndetdet(idet)
     +               ,(nvsdet(j,idet),ivsdet(j,idet),j=1,kvsdet(idet))
          END DO
        ENDIF
C 
      ENDIF IF(ipri.EQ.2) THEN
C
        WRITE(lun,2000) idnevt,nver,ntra
        IF(nver.GT.0) THEN
          WRITE(lun,2010) nver
          DO i=1,nver
            WRITE(lun,2100) i,igev(i)
     +                     ,(vert(j,i),j=1,3)
     +                     ,ltimv(i),imov(i),ntdv(i),itdv(i)
          END DO
        ENDIF
C
        IF(ntra.GT.0) THEN
          WRITE(lun,2150) ntra
          DO i=1,ntra
            WRITE(lun,2200) i,iget(i)
     +                     ,(ptra(j,i),j=1,3)
     +                     ,itra(i),itvb(i),itve(i)
          END DO
        ENDIF
C
      ENDIF IF(ipri.EQ.3) THEN
C
        WRITE(lun,3000) idnevt,nhit,ndig
        IF(nhit.GT.0) THEN
C
          khd=0
          DO i=1,nhit
             nhd(i)=0
          END DO
          IF(npdig.GT.0.and.ndig.GT.0) THEN
            DO idi=1,ndig
              i1=1
              IF(idi.GT.1) THEN
                k=ip1dig(idi-1)
                i1=IBITS(k,16,16)
              ENDIF
              k=ip1dig(idi)
              i2=IBITS(k,16,16)
C              WRITE(6,*) i1,i2
              DO j=i1,i2-1
                i=jpdig(j)
                IF(i.GT.0.AND.i.LE.nhit) THEN
                  IF(khd.LT.mxhd) THEN
                    khd=khd+1
                    nhd(i)=nhd(i)+1
                    ihd(khd)=idi
                    IF(nhd(i).EQ.1) jhd(i)=khd
                  ELSE
                    WRITE(6,*) ' *** hit ',i,' too many digitisings'
                  ENDIF
                ELSE
                  WRITE(6,*) ' *** idi=',idi,' ihit is out of range ',i
                ENDIF
              END DO
            END DO
          ENDIF
C
          WRITE(lun,3010) nhit
          nhitsel=0
          nhits1=0
C          DO i=1,50
          DO i=1,nhit
C
            k=ip1hit(i)
            itrh=IBITS(k,0,16)
            iorh=IBITS(k,16,16)
            k=ip2hit(i)
            iddh=IBITS(k,0,16)
            itmh=IBITS(k,16,16)-32768
C
C            WRITE(6,*) '&&&',i,itrh,iorh,k,iddh,itmh,nhd(i)
            IF(nhd(i).EQ.0) GO TO 300
            nhits1=nhits1+1
C            IF(ABS(itmh).GT.10) GO TO 300
            nhitsel=nhitsel+1
            WRITE(lun,3100) i,itrh,iorh,iddh
     +                     ,hit(1,i),hit(2,i),zhit(i),(hitd(j,i),j=1,3)
     +                     ,phit(i)
     +                     ,itmh,(ihd(j+jhd(i)-1),j=1,nhd(i))
 300     END DO
         WRITE(6,3120) nhitsel
         CALL HF1(10,nhits1+.1,2.)
         CALL HF1(11,nhitsel+.1,2.)
        ENDIF
C
      ENDIF IF(ipri.EQ.4) THEN
        WRITE(lun,3000) idnevt,nhit,ndig
        IF(ndig.GT.0) THEN
          WRITE(lun,4010) ndig
          DO i=1,ndig
C
C--           Get the digitisations
C	
            i1=0
            i2=0
            IF(npdig.GT.0) THEN
              i1=1
              IF(i.GT.1) THEN
                k=ip1dig(i-1)
                i1=IBITS(k,16,16)
              ENDIF
              k=ip1dig(i)
              i2=IBITS(k,16,16)
            ENDIF
            k=ip1dig(i)
            iddd=IBITS(k,0,16)
            k=ip2dig(i)
            idg1=IBITS(k,0,16)
            idg2=IBITS(k,16,16)
C
            IF(iddd.GE.67.AND.iddd.LE.174) THEN
               CALL HF1(12,idg2+0.1,1.)
            ENDIF
            WRITE(lun,4100) i,iddd,idg1,idg2
     +                     ,(jpdig(j),j=i1,i2-1)
          END DO
        ENDIF
C 
      ENDIF IF(ipri.EQ.5) THEN
C
        WRITE(lun,5000) idnevt,nlusr,nlund
        IF(nlund.GT.0) THEN
          WRITE(lun,5010) 
          DO i=1,nlund
            WRITE(lun,5100) i,klun1(i),klun2(i)
     +                     ,(klun3(j,i),j=1,3)
     +                     ,(plund(j,i),j=1,4)
          END DO
        ENDIF
C
      ENDIF
C
 999  RETURN
C
 1000 FORMAT(5X,I4,' - detector planes, max number of volumes=',I3
     ,/,'  no type   id    set  det  volu no  volu no ...')
 1100 FORMAT(I4,1X,I4,1X,I4,3X,A4,1X,A4,2X,5(A4,1X,I2,2X))
 2000 FORMAT(5X,'Event:',I5,3X,I4,' - verices, ',I4,' - tracks')
 2010 FORMAT(5X,I4,' - verices',/
     +  ,'  iv GEANT      X        Y        Z    t_ns*10 moth_t '
     +  ,'nt it1')
 2100 FORMAT(I4,1X,I4,2X,F9.2,2F9.3,2X,I5,2X,I3,2X,2I4) 
 2150 FORMAT(5X,I4,' - tracks ',/
     +  ,'  it GEANT      Px       Py       Pz     type'
     +  ,' orig_v end_v') 
 2200 FORMAT(I4,1X,I4,2X,F9.2,2F9.3,2X,I5,2I5)
 3000 FORMAT(5X,'Event:',I5,3X,I4,' - hits   , ',I4,' - digitisings')
 3010 FORMAT(5X,I4,' - hits ',/
     +  ,'  no track ori  idet      X        Y        Z       '
     +  ,'XDet     YDet     ZDet'
     +  ,'   P entry  ns*10'
     +  ,'  digitisations..')  
 3100 FORMAT(I4,2X,I4,1X,I3,1X,I4,2X,2F9.3,F9.1,1X,3F9.3,1X,F8.3,I6
     +     ,2X,(20I5))
 3120 FORMAT(' Selected hits:',I5)
 4010 FORMAT(5X,I4,' - digitisations',/
     +  ,'  no idet  wire  width/time    reference to hits...')
 4100 FORMAT(I4,1X,I4,2X,2I6,4X,(20I5))
 5000 FORMAT(5X,'Event:',I5,3X,I4,' - user words, ',I4,' - LUND')
 5010 FORMAT('  #    ks      kf  k: 3   4   5      PX'
     +      ,'        PY        PZ    mass') 
 5100 FORMAT(I4,1X,I4,2X,I6,2X,3I4,4F10.5)
C 1000 format(/' eve,nkine=',2I10)
C 1100 format(' #,mom=',I4,3F12.4)
      END

