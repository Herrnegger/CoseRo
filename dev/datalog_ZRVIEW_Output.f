!***********************************************************************
!> # cos::datalog 
!>    - COSERO (main)
!>    - Write non-binary output for ZRVIEW
!>        + precipitation and .plus files
!> ** maintenance**
!>    - author: Herrnegger (11/2009)
!>    - Adaption of NB numbering to I4.4 (DJ 02/2016)
!> ***
!***********************************************************************

      Subroutine ZRVIEW_OUTPUT (ND, TGEB_B, BW0GEB_B, QAB1GEB_B,
     +              QAB2GEB_B, QAB3GEB_B, BW3GEB_B, SWWGEB_B,
     +              PRAINGEB_B, PSNOWGEB_B, QABGEB_B_SUM,
     +              ETATGEB_B_SUM, PGEB_B_SUM, pgeb_b)
c ----------------------------------------------------------------------
c pre definitions:
      use allvar
      implicit none
      
c     external variables:
      integer ND

      real TGEB_B(MAXBASIN)
      real BW0GEB_B(MAXBASIN)
      real QAB1GEB_B(MAXBASIN)
      real QAB2GEB_B(MAXBASIN)
      real QAB3GEB_B(MAXBASIN)
      real BW3GEB_B(MAXBASIN)
      real swwgeb_b(maxbasin)
      real PRAINGEB_B(MAXBASIN)
      real PSNOWGEB_B(MAXBASIN)
      real qabgeb_b_sum(maxbasin)
      real etatgeb_b_sum(maxbasin)
      real pgeb_b_sum(maxbasin)
      real pgeb_b(maxbasin)
c     internal variables:
      integer NBTATSMAX
      integer NB
      INTEGER RECNR
      INTEGER RECNR1
      INTEGER RECNR2
      INTEGER NVARCOUNT
      
      SAVE RECNR, RECNR1, RECNR2
c ----------------------------------------------------------------------

c ----------------------------------------------------------------------
c code:
c     Write header at first timestep
      if (ND > IDAY) then 
        stop 'ERROR: ND > IDAY!'
      end if
      if (ND.eq.1) then
        NBTATSMAX = 0
        do NB=1, NBASIN
          if (BASIN_SIM(NB).eq.1) then
            NBTATSMAX = NBTATSMAX + 1
          endif 
        enddo
c       file for .precB-file
        open (unit=1090, file='output/COSERO.precB',
     +        form='unformatted',
     +        access='direct', Recl=4)
        RECNR = 1
        write (1090, Rec=1) NBTATSMAX
c       file for .plus1B-file (BW)
        open (unit=1091, file='output/COSERO.plus1B',
     +        form='unformatted',
     +        access='direct', Recl=4)
        RECNR2 = 2
        NVARCOUNT = 6
        write (1091, Rec=1) NBTATSMAX 
        write (1091, Rec=2) NVARCOUNT
c       file for .plusB -file (QAB)
        open (unit=1092, file='output/COSERO.plusB',
     +        form='unformatted',
     +        access='direct', Recl=4)
        RECNR1 = 2
        NVARCOUNT = 4
        write (1092, REC=1) NBTATSMAX
        write (1092, REC=2) NVARCOUNT
c       +++ASCII output-files for ZRVIEW ++++
c       file for .prec-file
        open (unit=1085, file='output/COSERO.prec')
        write (1085,fmt='(5A,$)')
     +      ' yyyy ', 'mm ', 'dd ', 'hh ', 'mm '
        do NB=1, NBASIN
          if (BASIN_SIM(NB) .gt. 0) then
            write (1085,fmt='(2(A,I4.4),$)')
     +          ' PRAINGEB_', NB,
     +          ' PSNOWGEB_', NB
          endif
        enddo
        write (1085,*)

c       file for .plus1 (BW) file !mathew
        open (unit=1086, file='output/COSERO.plus1')
        write (1086,*) '6'
        write (1086,fmt='(5A,$)')
     +      ' yyyy ', 'mm ', 'dd ', 'hh ', 'mm '
        do NB=1, NBASIN
          if (BASIN_SIM(NB) .gt. 0) then
            write (1086,fmt='(6(A,I4.4),$)')
     +     ' BW0GEB_', NB,
     +     ' BW3GEB_', NB,
     +     ' SWWGEB_', NB,
     +     ' PGEB_SUM_', NB,
     +     ' ETAGEB_SUM_', NB,
     +     ' QABGEB_SUM_', NB
          endif
        enddo
        write (1086,*)

c       file for .plus (QAB) !mathew
        open (unit=1087, file='output/COSERO.plus')
        write (1087,*) '4'
        write (1087,fmt='(5A,$)')
     +      ' yyyy ', 'mm ', 'dd ', 'hh ', 'mm '
        do NB=1, NBASIN
          if (BASIN_SIM(NB) .gt. 0) then
            write (1087,fmt='(4(A,I4.4),$)')
     +          ' QAB123GEB_', NB,
     +          ' QAB23GEB_', NB,
     +          ' QAB3GEB_', NB,
     +          ' TGEB_', NB
          endif
        enddo
        write (1087,*)
        
!c testfile
!        open(8888, file='output/test_parallel.txt')
!        write (8888,*) '4'
!        write (8888,fmt='(5A,$)')
!     +      ' yyyy ', 'mm ', 'dd ', 'hh ', 'mm '
!        do NB=1, NBASIN
!          if (BASIN_SIM(NB) .gt. 0) then
!            write (8888,fmt='(2(A,I4.4),$)')
!     +          ' PGEB_', NB,
!     +          ' PGEB_SUM_', NB
!          endif
!        enddo
!        write (8888,*)
        
        
      endif !Header schreiben beim 1. Zeitschritt

c     +++Variables for Binary files for ZRVIEW+++
c     ### .precB-variables       
      do NB=1, NBASIN
       if (BASIN_SIM(NB) .gt. 0) then
         RECNR=RECNR+1
         write(1090, REC=RECNR)  PRAINGEB_B(NB)
         RECNR=RECNR+1
         write(1090, REC=RECNR) PSNOWGEB_B(NB)
       endif
      enddo

c     ### .plus1B-variables
      do NB=1, NBASIN
       if (BASIN_SIM(NB) .gt. 0) then
         RECNR2=RECNR2+1
         write (1091, REC=RECNR2) BW0GEB_B(NB)
         RECNR2=RECNR2+1
         write (1091, REC=RECNR2) BW3GEB_B(NB)
         RECNR2=RECNR2+1
         write(1091, REC=RECNR2) SWWGEB_B(NB)
         RECNR2=RECNR2+1
         write (1091, REC=RECNR2) pgeb_b_sum(NB) 
         RECNR2=RECNR2+1
         write (1091, REC=RECNR2) etatgeb_b_sum(NB) 
         RECNR2=RECNR2+1
         write (1091, REC=RECNR2) qabgeb_b_sum(NB) 
       endif
      enddo
      
c     ### .plusB-variables !mathew
      do NB=1, NBASIN
       if (BASIN_SIM(NB) .gt. 0) then
         RECNR1=RECNR1+1
         write (1092, REC=RECNR1) QAB1GEB_B(NB)+QAB2GEB_B(NB)
     +          + QAB3GEB_B(NB)
         RECNR1=RECNR1+1
         write (1092, REC=RECNR1) QAB2GEB_B(NB)+QAB3GEB_B(NB)
         RECNR1=RECNR1+1
         write(1092, REC=RECNR1) QAB3GEB_B(NB)
         RECNR1=RECNR1+1
         write (1092, REC=RECNR1) TGEB_B(NB) 
       endif
      enddo
      
c     +++ Variables for ASCII files for ZRVIEW+++
c     ### .prec-variables !mathew
      write (1085,fmt='(5I5,$)')
     +      NYEAR(ND), NMONTH(ND), NDAY(ND), NHOUR(ND), NMINUTE(ND)
      do NB=1, NBASIN
       if (BASIN_SIM(NB) .gt. 0) then
         write (1085,fmt='(2F10.3,$)')
     +          PRAINGEB_B(NB),
     +          PSNOWGEB_B(NB)
       endif
      enddo
      write (1085,*)

c     ### .plusQAB-variables !mathew
      write (1087,fmt='(5I5,$)')
     +      NYEAR(ND), NMONTH(ND), NDAY(ND), NHOUR(ND), NMINUTE(ND)
      do NB=1, NBASIN
       if (BASIN_SIM(NB) .gt. 0) then
         write (1087,fmt='(4F10.3,$)')
     +          QAB1GEB_B(NB)+QAB2GEB_B(NB)+QAB3GEB_B(NB),
     +          QAB2GEB_B(NB)+QAB3GEB_B(NB),
     +          QAB3GEB_B(NB),
     +          TGEB_B(NB)
       endif
      enddo
      write (1087,*)

!c     ### testfile
!      write (8888,fmt='(5I5,$)')
!     +      NYEAR(ND), NMONTH(ND), NDAY(ND), NHOUR(ND), NMINUTE(ND)
!      do NB=1, NBASIN
!       if (BASIN_SIM(NB) .gt. 0) then
!         write (8888,fmt='(2F10.3,$)')
!     +          PGEB_B(NB),
!     +          pgeb_b_sum(nb)
!       endif
!      enddo
!      write (8888,*)

c     .plus1-variables !mathew
      write (1086,fmt='(5I5,$)')
     +      NYEAR(ND), NMONTH(ND), NDAY(ND), NHOUR(ND), NMINUTE(ND)
      do NB=1, NBASIN
       if (BASIN_SIM(NB) .gt. 0) then
         write (1086,fmt='(6F10.3,$)')
     +          BW0GEB_B(NB),
     +          BW3GEB_B(NB),
     +          swwgeb_b(NB),
     +          pgeb_b_sum(NB),
     +          etatgeb_b_sum(NB),
     +          qabgeb_b_sum(NB)
       endif
      enddo
      write (1086,*)
      if (ND.eq.IDAY) then
        close(1090)
        close(1091)  
        close(1092)
        close(1085) 
        close(1087)
        close(1086) !§§ probably an error, here [10]84 was closed
!        close(8888)
      endif
      end subroutine
      