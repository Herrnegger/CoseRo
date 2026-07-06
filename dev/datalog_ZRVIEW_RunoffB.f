!***********************************************************************
!> # cos::datalog 
!>    - this file containes 2 subroutines 
!>      + ZRVIEW_RunoffB: 
!!         write binary runoff output for ZRVIEW
!>      + ZRVIEW_RunoffB_mm:
!!         write binary runoff output for ZRVIEW
!> ** maintenance**
!>     - author: Herrnegger (11/2009)
!> ***
!***********************************************************************

      Subroutine ZRVIEW_RunoffB()
c-----------------------------------------------------------------------
c Write binary runoff output for ZRVIEW
c-----------------------------------------------------------------------
c ****
c pre definitions:
      use allvar
      implicit none
      
c     internval variables:
      integer NB, ND
      integer NBTATSMAX
      integer RECNR3
c ****

c code:
c     +++ calculate number of simulated basins +++
      NBTATSMAX = 0
      do NB=1, NBASIN
       if (BASIN_SIM(NB).eq.1) then
         NBTATSMAX = NBTATSMAX + 1
       endif 
      enddo
c     +++ Binary Output-file +++
c     +++ Runoff +++
      open (unit=1095, file='output/COSERO.runoffB', 
     +      form='unformatted',
     +      access='direct', Recl=4)
      RECNR3=1
      write (1095, Rec=RECNR3) NBTATSMAX
      do ND= 1, IDAY
        RECNR3= RECNR3 + 1
        write (1095, REC=RECNR3) NYEAR(ND) 
        RECNR3= RECNR3 + 1
        write (1095, REC=RECNR3) NMONTH(ND)
        RECNR3= RECNR3 + 1
        write (1095, REC=RECNR3) NDAY(ND)
        RECNR3= RECNR3 + 1
        write (1095, REC=RECNR3) NHOUR(ND)
        RECNR3=RECNR3+1
        write (1095, REC=RECNR3) NMINUTE(ND)
        do NB=1, NBASIN
          if (BASIN_SIM(NB) .gt. 0) then
            RECNR3=RECNR3+1
            write (1095, REC=RECNR3) QOBS_T(NB,ND)
            RECNR3=RECNR3+1
            write (1095, REC=RECNR3) QSIM_T(NB,ND)
          endif
        end do
      end do
      close (1095)
      end subroutine



      Subroutine ZRVIEW_RunoffB_mm (ND,QOBSGEB_B,QSIMGEB_B)
c-----------------------------------------------------------------------
c Write binary runoff output for  ZRVIEW
c-----------------------------------------------------------------------
c ****
c pre definitions:
      use allvar
      implicit none
      
c     external variables: 
      integer NB, ND
      real QOBSGEB_B(MAXBASIN)
      real QSIMGEB_B(MAXBASIN)
c     internal variables:
      integer NBTATSMAX
      integer RECNR4
      
      save RECNR4
c ****

c code:
c+++ calculate number of simulated basins +++
        NBTATSMAX = 0
        do NB=1, NBASIN
         if (BASIN_SIM(NB).eq.1) then
           NBTATSMAX = NBTATSMAX + 1
         endif 
        enddo
        if (ND == 1) then
          open (unit=1303, file='output/CosReg_mm.runoffB',
     +           form='unformatted',
     +           access='direct', Recl=4)
          RECNR4=1
          write (1303, REC=RECNR4) NBTATSMAX
        endif
c     +++ Binary Runoff in mm +++
!      do ND = 1, IDAY
        RECNR4=RECNR4+1  
        write (1303, REC=RECNR4) NYEAR(ND) 
        RECNR4=RECNR4+1
        write (1303, REC=RECNR4) NMONTH(ND)     
        RECNR4=RECNR4+1
        write (1303, REC=RECNR4) NDAY(ND)
        RECNR4=RECNR4+1
        write (1303, REC=RECNR4) NHOUR(ND)
        RECNR4=RECNR4+1
        write (1303, REC=RECNR4) NMINUTE(ND)      
        
        do NB = 1, NBASIN
          if (BASIN_SIM(NB) .gt. 0) then
            RECNR4=RECNR4+1
            write (1303, REC=RECNR4) QOBSGEB_B(NB)
            RECNR4=RECNR4+1
            write (1303, REC=RECNR4) QSIMGEB_B(NB)
          endif          
        end do
      if (ND.eq.IDAY) then
        close (1303)
      endif
!      end do
      
      end subroutine