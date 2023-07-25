!> @file user_init_3d_model.f90
!------------------------------------------------------------------------------!
! This file is part of the PALM model system.
!
! PALM is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License as published by the Free Software
! Foundation, either version 3 of the License, or (at your option) any later
! version.
!
! PALM is distributed in the hope that it will be useful, but WITHOUT ANY
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
! A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with
! PALM. If not, see <http://www.gnu.org/licenses/>.
!
! Copyright 1997-2018 Leibniz Universitaet Hannover
!------------------------------------------------------------------------------!
!
! Current revisions:
! -----------------
! 
! 
! Former revisions:
! -----------------
! $Id: user_init_3d_model.f90 2718 2018-01-02 08:49:38Z maronga $
! Corrected "Former revisions" section
! 
! 2696 2017-12-14 17:12:51Z kanani
! Change in file header (GPL part)
!
! 2618 2017-11-16 15:37:30Z suehring
! Provide example for user-defined initialization of surface-related quantities
! 
! 2233 2017-05-30 18:08:54Z suehring
!
! 2232 2017-05-30 17:47:52Z suehring
! +surface_mod
! 
! 2000 2016-08-20 18:09:15Z knoop
! Forced header and separation lines into 80 columns
! 
! 1682 2015-10-07 23:56:08Z knoop
! Code annotations made doxygen readable 
!
! 1320 2014-03-20 08:40:49Z raasch
! small changes in layout
!
! 1036 2012-10-22 13:43:42Z raasch
! code put under GPL (PALM 3.9)
!
! 211 2008-11-11 04:46:24Z raasch
! Former file user_interface.f90 split into one file per subroutine
!
! Description:
! ------------
!> Allows the complete initialization of the 3d model.
!>
!> @attention The user is responsible to set at least all those quantities which
!>            are normally set within init_3d_model!
!------------------------------------------------------------------------------!
 SUBROUTINE user_init_3d_model
 

    USE arrays_3d
    
    USE control_parameters
    
    USE indices
    
    USE kinds

    USE surface_mod
    
    USE user

    IMPLICIT NONE

    INTEGER(iwp) ::  l !< running index surface orientation
    INTEGER(iwp) ::  m !< running index surface elements
    INTEGER(iwp) ::  i
    INTEGER(iwp) ::  j
    INTEGER(iwp) ::  k

    DO  i = nxlg, nxrg
       DO  j = nysg, nyng
          DO k = nzb, nzt
!!!! Canadian Basin - DJF WINTER TEMP PROFILE 75N 221E - Erin Thomas (ethomas@lanl.gov) !!!!
             IF (abs(zu(k)).le.33.0) THEN ! winter ML depth + temp
                pt(k,j,i) = -1.47 + 273.15
             ELSE
                pt(k,j,i) = 273.15                                       & 
                     - (5.8977)                                           &
                     - (0.19662)*zu(k)                                      &
                     - (0.0018831)*zu(k)*zu(k)                              &
                     - (-4.4247e-06)*zu(k)*zu(k)*zu(k)                       &
                     - (-1.5372e-07)*zu(k)*zu(k)*zu(k)*zu(k)                 &
                     - (-7.979e-10)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)          &
                     - (-1.3313e-12)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)
             ENDIF
!!!! Canadian Basin JJA SUMMER TEMP Profile 75N 221E- Erin Thomas (ethomas@lanl.gov)!!!!
!             IF (abs(zu(k)).le.38.0) THEN ! summer ML depth + temp
!                pt(k,j,i) = -1.39 + 273.15
!             ELSE
!                pt(k,j,i) = 273.15                                       & 
!                     - (24.687)                                           &
!                     - (1.3353)*zu(k)                                      &
!                     - (0.028089)*zu(k)*zu(k)                              &
!                     - (2.9655e-04)*zu(k)*zu(k)*zu(k)                       &
!                     - (1.6961e-06)*zu(k)*zu(k)*zu(k)*zu(k)                 &
!                     - (5.0316e-09)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)          &
!                     - (6.0794e-12)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)
!             ENDIF
          ENDDO
          u(:,j,i)  = 0.0_wp
          v(:,j,i)  = 0.0_wp
       ENDDO
    ENDDO
! FIX pt_init 
    DO k = nzb, nzt
!!!! Canadian Basin - DJF WINTER TEMP PROFILE 75N 221E - Erin Thomas (ethomas@lanl.gov) !!!!
       IF (abs(zu(k)).le.33.0) THEN ! winter ML depth + temp
          pt_init(k) = -1.47 + 273.15
       ELSE
          pt_init(k) = 273.15                                       & 
                     - (5.8977)                                           &
                     - (0.19662)*zu(k)                                      &
                     - (0.0018831)*zu(k)*zu(k)                              &
                     - (-4.4247e-06)*zu(k)*zu(k)*zu(k)                       &
                     - (-1.5372e-07)*zu(k)*zu(k)*zu(k)*zu(k)                 &
                     - (-7.979e-10)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)          &
                     - (-1.3313e-12)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)
       ENDIF
!!!! Canadian Basin JJA SUMMER TEMP Profile 75N 221E- Erin Thomas (ethomas@lanl.gov)!!!!
!      IF (abs(zu(k)).le.38.0) THEN ! summer ML depth + temp
!          pt_init(k) = -1.39 + 273.15
!      ELSE
!         pt_init(k) = 273.15                                       & 
!                     - (24.687)                                           &
!                     - (1.3353)*zu(k)                                      &
!                     - (0.028089)*zu(k)*zu(k)                              &
!                     - (2.9655e-04)*zu(k)*zu(k)*zu(k)                       &
!                     - (1.6961e-06)*zu(k)*zu(k)*zu(k)*zu(k)                 &
!                     - (5.0316e-09)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)          &
!                     - (6.0794e-12)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)
!      ENDIF
    ENDDO
!
!--       Mask topography
    u = MERGE( u, 0.0_wp, BTEST( wall_flags_0, 1 ) )
    v = MERGE( v, 0.0_wp, BTEST( wall_flags_0, 2 ) )
!
    IF ( ibc_uv_b /= 1  .AND.  .NOT.  spinup )  THEN
       DO  i = nxlg, nxrg
          DO  j = nysg, nyng
             DO  k = nzb, nzt
                u(k,j,i) = MERGE( u(k,j,i), 0.0_wp,                      &
                     BTEST( wall_flags_0(k,j,i), 20 ) )
                v(k,j,i) = MERGE( v(k,j,i), 0.0_wp,                      &
                     BTEST( wall_flags_0(k,j,i), 21 ) )
             ENDDO
          ENDDO
       ENDDO
    ENDIF
!
    IF ( ocean )  THEN
       DO  i = nxlg, nxrg
          DO  j = nysg, nyng
             DO k = nzb, nzt
!!!! Canadian Basin - DJF WINTER SALT PROFILE 75N 221E - Erin Thomas (ethomas@lanl.gov) !!!!
             IF (abs(zu(k)).le.30.0) THEN ! winter salt mixed layer
                sa(k,j,i) = 27.04
             ELSE
                sa(k,j,i) = 22.845                                   &
                     - (0.10595)*zu(k)                                  &
                     - (-0.0026536)*zu(k)*zu(k)                              &
                     - (-6.5508e-05)*zu(k)*zu(k)*zu(k)                       &
                     - (-5.5721e-07)*zu(k)*zu(k)*zu(k)*zu(k)                 &
                     - (-2.142e-09)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)          &
                     - (-3.1388e-12)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)
             ENDIF
!!!! Canadian Basin JJA SUMMER TEMP Profile 75N 221E- Erin Thomas (ethomas@lanl.gov)!!!!
!             IF (abs(zu(k)).le.35.0) THEN ! summer salt mixed layer 
!                sa(k,j,i) = 28.11
!             ELSE
!                sa(k,j,i) = 18.802                                          &
!                     - (0.40328)*zu(k)                                      &
!                     - (0.0046778)*zu(k)*zu(k)                              &
!                     - (2.1635e-05)*zu(k)*zu(k)*zu(k)                       &
!                     - (-9.9477e-09)*zu(k)*zu(k)*zu(k)*zu(k)                 &
!                     - (-3.8883e-10)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)          &
!                     - (-8.7885e-13)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)
!             ENDIF
             ENDDO
          ENDDO
       ENDDO
! FIX sa_init
       DO k = nzb, nzt
!!!! Canadian Basin - DJF WINTER SALT PROFILE 75N 221E - Erin Thomas (ethomas@lanl.gov) !!!!
          IF (abs(zu(k)).le.30.0) THEN ! winter salt mixed layer
             sa_init(k) = 27.04
          ELSE
             sa_init(k) = 22.845                                   &
                  - (0.10595)*zu(k)                                  &
                  - (-0.0026536)*zu(k)*zu(k)                              &
                  - (-6.5508e-05)*zu(k)*zu(k)*zu(k)                       &
                  - (-5.5721e-07)*zu(k)*zu(k)*zu(k)*zu(k)                 &
                  - (-2.142e-09)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)          &
                  - (-3.1388e-12)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)
          ENDIF
!!!! Canadian Basin JJA SUMMER TEMP Profile 75N 221E- Erin Thomas (ethomas@lanl.gov)!!!!
!         IF (abs(zu(k)).le.35.0) THEN ! summer salt mixed layer 
!             sa_init(k) = 28.11
!         ELSE
!             sa_init(k) = 18.802                                          &
!                 - (0.40328)*zu(k)                                      &
!                 - (0.0046778)*zu(k)*zu(k)                              &
!                 - (2.1635e-05)*zu(k)*zu(k)*zu(k)                       &
!                 - (-9.9477e-09)*zu(k)*zu(k)*zu(k)*zu(k)                 &
!                 - (-3.8883e-10)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)          &
!                 - (-8.7885e-13)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)
!         ENDIF
       ENDDO        
    ENDIF

    IF ( passive_scalar )  THEN
       DO  i = nxlg, nxrg
          DO  j = nysg, nyng
             DO k = nzb, nzt
!!!! Eurasian Basin !!!!
                IF(abs(zu(k)).le.15.0) THEN
                   s(k,j,i) = 0.0
                ELSE
                   s(k,j,i) = -0.90 - (1.04e-1)*zu(k) - (2.75e-3)*zu(k)*zu(k)       &
                        - (3.95e-5)*zu(k)*zu(k)*zu(k)                         &
                        - (3.13e-7)*zu(k)*zu(k)*zu(k)*zu(k)                   &
                        - (1.29e-9)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)            &
                        - (2.15e-12)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)
                ENDIF
!!!! Canadian Basin !!!!
!                IF(abs(zu(k)).le.25.0) THEN
!                   s(k,j,i) = 0.0
!                ELSE
!                   s(k,j,i) = 0.59 - (1.72e-2)*zu(k) - (3.82e-4)*zu(k)*zu(k)       &
!                        - (5.16e-6)*zu(k)*zu(k)*zu(k)                         &
!                        - (4.14e-8)*zu(k)*zu(k)*zu(k)*zu(k)                   &
!                        - (1.79e-10)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)            &
!                        - (3.16e-13)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)*zu(k)
!                ENDIF
             ENDDO
          ENDDO
       ENDDO
    ENDIF
!
!-- Initialization of surface-related quantities.
!-- The following example shows required initialization of surface quantitites
!-- at default-type upward-facing surfaces.  
!   DO  m = 1, surf_def_h(0)%ns
!      surf_def_h(0)%ol(m)   = ...    ! Obukhov length
!      surf_def_h(0)%us(m  ) = ...    ! friction velocity
!      surf_def_h(0)%usws(m) = ...    ! vertical momentum flux, u-component
!      surf_def_h(0)%vsws(m) = ...    ! vertical momentum flux, v-component
!      surf_def_h(0)%z0(m)   = ...    ! roughness length for momentum
!      IF ( .NOT. neutral )  THEN
!         surf_def_h(0)%ts(m)   = ... ! scaling parameter
!         surf_def_h(0)%shf(m)  = ... ! surface sensible heat flux
!         surf_def_h(0)%z0h(m)  = ... ! roughness length for heat
!      ENDIF
!      IF ( humditiy )  THEN
!         surf_def_h(0)%qs(m)   = ... ! scaling parameter
!         surf_def_h(0)%qsws(m) = ... ! surface latent heat flux
!         surf_def_h(0)%z0q(m)  = ... ! roughness length for moisture
!      ENDIF
!      IF ( passive_scalar )  THEN
!         surf_def_h(0)%ss(m)   = ... ! scaling parameter
!         surf_def_h(0)%ssws(m) = ... ! surface latent heat flux
!      ENDIF
!   ENDDO 
!
!-- Same for natural and urban type surfaces
!   DO  m = 1, surf_lsm_h%ns
!      ...
!   ENDDO 
!   DO  m = 1, surf_usm_h%ns
!      ...
!   ENDDO
!
!-- Also care for vertically aligned surfaces (default-, natural-, and 
!-- urban-type).
!   DO  l = 0, 3
!      DO  m = 1, surf_def_v(l)%ns
!         ...
!      ENDDO
!      DO  m = 1, surf_lsm_v(l)%ns
!         ...
!      ENDDO
!      DO  m = 1, surf_usm_v(l)%ns
!         ...
!      ENDDO
!   ENDDO
!
!
!-- In the following, initialize 3D quantities, e.g. u, v, w, pt, etc..

 END SUBROUTINE user_init_3d_model

