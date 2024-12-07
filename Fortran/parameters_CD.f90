! WHAT   : Consumer problem -> parametrization for CD utility function 
! TAKE   : unchanged in routine:   switch_two_rates - 0 = one interest rate, 1 = two interest rates; switch_bequest -  0 = bequests collected and spreaded uniformly; 1 = bequests remain in respective cohort  
!          changed   in routine:   time-consistent (exponential) preferences [[beta]], capital depreciation [[depr]], the standard discount factor [[delta]], replacement ratio in old  [[rho_1]] and new [[rho_2]] PAYG pension system
!          changed   in routine:   contribution to 1st pillar [[t1_ss]], the inverse of Frisch elasticity of labor [[ksi]], the disutility of labor [[psi]], preference for leisure [[phi]]
! DO     : called in globals subroutine to clean parameters
! RETURN : calibration of the model

read(1,*) ! Social Security params
    
!           case 0 - omega = 1 and beta = 1        
read(1,*) beta != 1.00_dp

read(1,*) rho1 != 0*0.25d0
read(1,*) rho2 != 0*0.3d0
!rho1 = rho1 + rho2     ! common soc sec
!rho2 = 0
rho1 = 0.24
if (theta == 2) rho2 = 0.175    !0.155
if (theta == 4) rho2 = .175 !0.16    !0.155

!if (bigJ == 4) then      
!        depr = (1.0_dp + 0.0095_dp)**zbar - 1.0_dp    
!        delta =  (1.01_dp)**(zbar) 
!        rho_1 = 1.20_dp
!        rho_2 = 1.20_dp
!        phi = 0.22_dp
!        t1_ss_old =  0*0.0780_dp 
!        t1_ss_new = t1_ss_old 
!        t2_ss_old = 0d0
!        t2_ss_new = 0d0
!        !progression_param = 0.05d0 
!        
!        if (swich_cohort_ps == 1) then
!            rho_1 = 0*0.128d0 !0.225_dp!*0.0d0
!            rho_2 = 0*0.128d0 !0.225_dp!*0.0d0    
!        endif
!        
!    elseif ((bigJ == 16) .or. (bigJ == 20)) then  
!        ! pension sys
!        rho_1 = 0.2 !0.465d0 !0.225_dp!*0.0d0
!        rho_2 = 0.465d0 !0.225_dp!*0.0d0
!        t1_ss_old =  0.078_dp!*0.5d0 0d0 !
!        t1_ss_new =  0.078d0
!        t2_ss_old = 0d0 !0.077_dp*0.5d0 !
!        t2_ss_new = 0.0d0 !39d0
!        
!        depr = (1.0_dp + 0.02_dp)**zbar - 1.0_dp    
!        delta =  1.5d0 !1.00d0**(zbar)   !(0.9862_dp)
!        phi = 0.355_dp
!
!         if (theta == 2) then 
!                ! r = 4.5, alpha = 0.3
!                depr = 0.05d0 !(1.0_dp + 0.0145_dp)**zbar - 1.0_dp 
!                delta = 1.06d0 !1.01480d0**(zbar)   !(0.9862_dp)
!                phi = 0.339_dp
!                rho_1 = 0.05d0 !0.2d0 !0.4415d0 !0.225_dp!*0.0d0
!                rho_2 = 0.05d0  !0.4415d0 !0.225_dp!*0.0d0
!                t1_ss_old =  0.086_dp !0.085_dp !0.0745_dp!*0.5d0 0d0 !
!                t1_ss_new =  0.086_dp !0.0745d0
!     
!            if ( switch_reduce_pension == 1) then        
!        !         r = 6.25, alpha = 0.3
                read(1,*) !
                depr = (1.0_dp + 0.058_dp)**zbar - 1.0_dp
                read(1,*) !delta ! 0.987d0**(zbar)   !(0.9862_dp)
                if (theta == 2) delta = .97285d0 !.995d0 !.995d0 !0.9537d0 !0.9825d0   !0.982095955798096d0 !<- mean delta !0.9825d0 !0.9371d0 !<- deltam2 !0.991d0 !<- deltam   !0.9915d0 <- with benefits    !0.9885d0 <- no benefits
                if (theta == 4) delta = .9425d0 !.97285d0 !.995d0 !.995d0 !0.9537d0 !0.9825d0   !0.982095955798096d0 !<- mean delta !0.9825d0 !0.9371d0 !<- deltam2 !0.991d0 !<- deltam   !0.9915d0 <- with benefits    !0.9885d0 <- no benefits
                delta = delta**(zbar)   !(0.9862_dp)
                read(1,*) phi != 0.355_dp
                read(1,*) !rho_1 != 0.461d0 
                read(1,*) !rho_2 != 0.461d0 
                read(1,*) !t1_ss_old !=  0.0742_dp
                read(1,*) !t1_ss_new !=  0.0742d0
                if (theta == 2) t1_ss_old = .105d0 !.155d0 !.1d0 !.1791d0 !0.116556d0
                if (theta == 4) t1_ss_old = .095d0 !.155d0 !.1d0 !.1791d0 !0.116556d0
                t1_ss_new = t1_ss_old
                !rho_1 = 1.1d0 !1.695d0 !<- mean delta !1.525d0 <- deltam2 !1.8d0 <- deltam   !1.83702d0 <- with benefits   !1.815d0 <- no benefits
                !rho_2 = rho_1
                !rho = 1.1d0
!            endif
!        endif 
!
!       if (swich_cohort_ps == 1) then
!            rho_1 = 0.05d0 !0.03212d0 !0.22d0 
!            rho_2 = 0.05d0 !0.22d0
!        endif
!
!elseif (bigJ == 80)  then       
!    depr = 0.055_dp
!    delta =  0.980_dp 
!    rho_1 = 0.2420_dp
!    rho_2 = 0.2420_dp
!    phi = 0.5240_dp
!    t1_ss_old =  0.0602_dp !1953_dp
!    t1_ss_new = t1_ss_old 
!    t2_ss_old = 0d0
!    t2_ss_new = 0d0
!endif