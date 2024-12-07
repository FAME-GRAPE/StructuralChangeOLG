! WHAT   :  read initial data and inital values from world without the reform
! TAKE   :  data files and output files from base scenario (without reform) 
! DO     :  read data from files to variables and parameters 
! RETURN :  base variable CRUCIAL to the next run on the path 

MODULE get_data
use global_vars
use csv_module
use iso_fortran_env, only: wp => real64
use csv_2d
use useful_functions

IMPLICIT NONE
CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine read_data(omega_d, gam_d, gam_cum_d, zet_d, pi_d, Nn_d, jbar_d, omega_share_d, alpha_d, gam_corrector, delta_mult_d, rho_d)
      integer :: bigJT, ow, last_N, last_pi, last_s, last_o, last_gam, last_dm, last
      real(dp)::  sum_N_temp , N_temp, che ! dp zliczenia populacji USA
      real(dp), dimension(bigJ, 1:omega_dim, 1:bigT), intent(out) :: omega_share_d
      real(dp), dimension(bigJ, 1:omega_dim, 1:bigT), intent(out) :: omega_d 
      real(dp), dimension(bigT), intent(out) :: gam_d, gam_cum_d, zet_d, alpha_d, rho_d
      real*8, dimension(omega_dim, bigT), intent(out) :: delta_mult_d
      real(dp), dimension(bigJ, 1:omega_dim, 1:bigT) :: omega_share_d2, omega_d2
      real(dp), dimension(bigT):: gam_d2, gam_d_diff, rho_d2
      real(dp), dimension(bigT) :: gam_corrector
      real(dp), dimension(bigJ, bigT), intent(out) :: Nn_d, pi_d
      real(dp), dimension(bigJ, bigT) :: Nn_d2, pi_d2
      real(dp), dimension(omega_dim, bigT) :: pop_share
      real*8, dimension(1:2, omega_dim, bigT) :: employment_mat2
      integer, dimension(bigT), intent(out) :: jbar_d

call chdir(cwd_r)

omega_d = 0         ! clear if already loaded
omega_share_d = 0   ! clear if already loaded
alpha_d = 0         ! clear if already loaded
Nn_d = 0            ! clear if already loaded
pi_d = 0            ! clear if already loaded
gam_d = 0           ! clear if already loaded
rho_d = 0
 
OPEN (unit=4, FILE = "_data_gamma.txt") 
OPEN (unit=5, FILE = "_data_alpha_4.txt") 
OPEN (unit=6, FILE = "_data_alpha_16.txt")
OPEN (unit=7, FILE = "_data_alpha_80.txt")
OPEN (unit=9, FILE = "_data_jbar.txt")
OPEN (unit=11, FILE = "_data_omega_jeden.txt")   
OPEN (unit=12, FILE = "_data_omega_dwa.txt")   
OPEN (unit=13, FILE = "_data_omega_trzy.txt")   
OPEN (unit=14, FILE = "_data_omega_cztery.txt")  
OPEN (unit=21, FILE = "_data_omega_share_jeden.txt")  
OPEN (unit=22, FILE = "_data_omega_share_dwa.txt")  
OPEN (unit=23, FILE = "_data_omega_share_trzy.txt")  
OPEN (unit=24, FILE = "_data_omega_share_cztery.txt")  

! -------------------------------- OMEGA -------------------------------

select case (omega_dim)
case(1)
    omega_d = 1d0
    omega_share_d = 1d0
case(2)
    omega_d = 1d0/omega_dim
    omega_share_d = 1d0/omega_dim
case(4)
    select case (bigJ)
    case(4)
        do  i_o = 1, omega_dim
            omega_share_d(:, i_o, :) = 1.1d0
            omega_d(:, i_o, :) = 0.95d0**i_o
        enddo
        call csv_2d_main_read('_data_omega_share_20y_o1_test.csv', omega_share_d(:, 4, :))
        call csv_2d_main_read('_data_omega_share_20y_o2_test.csv', omega_share_d(:, 3, :))
        call csv_2d_main_read('_data_omega_share_20y_o3_test.csv', omega_share_d(:, 2, :))
        call csv_2d_main_read('_data_omega_share_20y_o4_test.csv', omega_share_d(:, 1, :))
    case(16) 
        do j = 1, bigJ
            call csv_2d_main_read('_data_omega_5y.csv', omega_d(j, :, :)) !, len('_data_omega.csv')+1
            call csv_2d_main_read('_data_omega_share_5y.csv', omega_share_d(j, :, :)) !, len('_data_omega_share.csv')+1
        enddo
    case(80)
        do j = 1, bigJ
            call csv_2d_main_read('_data_omega_1y_v2.csv', omega_d(j, :, :)) !, len('_data_omega.csv')+1
            !call csv_2d_main_read('_data_omega_share_1y.csv', omega_share_d(j, :, :)) !, len('_data_omega_share.csv')+1
        enddo

        call csv_2d_main_read('_data_omega_share_o1_v6.csv', omega_share_d(:, 1, :)) !, len('_data_omega_share.csv')+1
        call csv_2d_main_read('_data_omega_share_o2_v6.csv', omega_share_d(:, 2, :))
        call csv_2d_main_read('_data_omega_share_o3_v6.csv', omega_share_d(:, 3, :))
        call csv_2d_main_read('_data_omega_share_o4_v6.csv', omega_share_d(:, 4, :))

    end select
end select
!omega_share_d = 0.25d0 !UBUG
!omega_d = 1d0
do i = 1, bigT 
    if (omega_d(1, 1, i) /= 0) last_o = i
    if (omega_share_d(1, 1, i) /= 0) last_s = i
    if (omega_d(1, 1, i) == 0) omega_d(:, :, i) = omega_d(:, :, last_o)
    if (omega_share_d(1, 1, i) == 0) omega_share_d(:, :, i) = omega_share_d(:, :, last_s)
enddo

! First 10 years are SS (1980 - 1989)
omega_d2 = omega_d
omega_share_d2 = omega_share_d
omega_d(:, :, 10:bigT) = omega_d2(:, :, 1:bigT - 9)
omega_share_d(:, :, 10:bigT) = omega_share_d2(:, :, 1:bigT - 9)
do i = 1, 9
    omega_d(:, :, i) = omega_d(:, :, 10)
    omega_share_d(:, :, i) = omega_share_d(:, :, 10)
enddo

! Checking whether shares sum up to 1 & diagonals are ok
do i = 1, bigT
    do j = 1, bigJ
        che = sum(omega_share_d(1,1:4,1))
        che = nint(che * 100000000.0) * 1E-8
        if (che /= 1) pause
        if (i < bigT .and. j < bigJ) then
            do i_o = 1, omega_dim
                if (omega_share_d(j, i_o, i) /= omega_share_d(j+1, i_o, i+1)) pause
            enddo
        endif
    enddo
enddo
    
!print *, extend_data(omega_d)
    
!real(dp), dimension(bigJ, omega_dim) :: omega_share = (/0.136d0, 1.864d0/) ! In 1995 6,8% of population had higher education, ref: https://web.archive.org/web/20070809012430/http://www.cepes.ro/publications/pdf/hee_eng_pdf/he3_01.pdf
close(11)
close(12)
close(13)
close(14)
close(21)
close(22)
close(23)
close(24)

! -------------------------------- DELTA_MULT -------------------------------

if (theta == 2) call csv_2d_main_read('_data_delta_mult_v3.csv', delta_mult_d(:, :))
if (theta == 4) call csv_2d_main_read('_data_delta_mult_v4.csv', delta_mult_d(:, :))
do i = 1, bigT 
    if (delta_mult_d(1, i) /= 0) last_dm = i
    if (delta_mult_d(1, i) == 0) delta_mult_d(:, i) = delta_mult_d(:, last_dm)
    do i_o = 1, omega_dim
        !if (delta_mult_d(i_o, i)*delta >= 1d0) pause !print *, i_o
    enddo
enddo
if (delta_mult_switch == 0) delta_mult_d = 1d0
!pause
! -------------------------------- JBAR -------------------------------
      do i = 1, bigT,1
        read(9,*) jbar_d(i)      
      enddo

    if (switch_fix_retirement_age > 0) then 
      jbar_d = switch_fix_retirement_age
    endif 
    
! -------------------------------- ALPHA -------------------------------
    if (bigJ == 4) then
        do i = 1, bigT
            read(5,*)alpha_d(i)
        enddo
    elseif (bigJ == 16) then
        do i = 1, bigT
            read(6,*)alpha_d(i)
        enddo
    elseif (bigJ == 80) then
        do i = 1, bigT
            !read(7,*)alpha_d(i)
            alpha_d = 0.33d0
        enddo
    else
        print *, "No input file for alpha"
    endif

! -------------------------------- N & PI -------------------------------
    bigJT = bigJ*bigT
    
! -------------------------------- BIGJ = 4    
    if (bigJ == 4) then 
        do i = 1, bigT,1 
            do j = 1, bigJ
            Nn_d(j,i) = 0.950_dp**(j-1)
            pi_d(j,i) = 0.950_dp**(j-1)
            
            enddo
        enddo 
    endif
    !do i=2, bigT
    !pi_d(:, i) = pi_d(:,1)**.9
    !enddo
    !pi_d = 1d0
    if (bigJ == 2 ) then 
        do i = 1, bigT,1   
            Nn_d(1,i) = 1.0_dp
            pi_d(1,i) = 1.0_dp
            
            Nn_d(2,i) = 1.0_dp
            pi_d(2,i) = 1.00_dp
        enddo 
     
    endif
    
 ! -------------------------------- BIGJ = 16 - US
    if (bigJ == 16) then   
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! read the file
  
    call csv_2d_main_read('_data_pi_cond_pl_5y.csv', pi_d) !, len('_data_pi_pl.csv')+1
    call csv_2d_main_read('_data_N_pl_5y.csv', Nn_d) !, len('_data_N_pl.csv')+1
    
    do i = 1, bigT 
        !if (Nn_d(1, i) /= 0) last_N = i
        if (pi_d(1, i) /= 0) last_pi = i
        !if (Nn_d(1, i) == 0) Nn_d(:, i) = Nn_d(:, last_N)
        if (pi_d(1, i) == 0) pi_d(:, i) = pi_d(:, last_pi)
    enddo

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !Open(unit = 121, FILE = "_data_pi_vec1.txt") !"_data_pi_cond_US.txt")
    !Open(unit = 122, FILE = "_data_Nn_PL_1995.txt")
!    Czytamy tylko kawa³ek który mamy z predykcji, resztê nadpiszemy 
                !do i = 1, 13 !17,1 
                !    do j = 1,bigJ,1 
                !        read(121,*) pi_d(j,i)
                !    enddo
                !enddo
                !
                !do j = 1, bigJ      
                !    read(122,*) N_ss_old(j)
                !enddo
    
! Z PREDYKCJI BIERZEMY TYLKO 20 LAKOW
    !!sum_N_temp = 0.0_dp
    !!do i = 1, 47, 1
    !!    
    !!    ! biezemy tylko co 5 wartosc tzn 1 , 6,       
    !!    if (mod(i,5) == 0) then
    !!        read(122,*) N_temp
    !!        sum_N_temp = sum_N_temp + N_temp
    !!        Nn_d(1,i/5) = sum_N_temp ! sumujemy po 5 kratek 
    !!        sum_N_temp = 0.0_dp
    !!    else 
    !!        read(122,*) N_temp
    !!        sum_N_temp = sum_N_temp + N_temp 
    !!    endif
    !!enddo
    !close(121) 
    !close(122)
    !!! musimy uzupe³niæ ogonki macierzy Pi i N ostatnimi wartoœciami jakie mamy
    !!Nn_d(1,9:) = Nn_d(1, 9) 
                !Nn_d  = 1.0_dp ! to do to do
                !do i = 14, bigT, 1
                ! pi_d(:,i)=  pi_d(:,13) ! 30?
                !enddo

    ! potrzebujemy przeliczyc pi na bezwarunkowe bo cay model na takim chodzi 

    do i = 1,bigT, 1
        pi_d(1,i) = 1.0_dp
        do j = 2, bigJ
            pi_d(j,i) = pi_d(j-1,max(i-1,1))*pi_d(j,i)
        enddo
    enddo      
    !pi_d = 1.0_dp
endif
 ! -------------------------------- BIGJ = 20     
 if (bigJ == 20) then 
    Open(unit = 121, FILE = "_data_pi_5periods.txt")
    Open(unit = 122, FILE = "_data_N_5periods.txt")
    do i = 1,bigT,1 
        do j = 1,bigJ,1 
            read(121,*) pi_d(j,i)
            read(122,*) Nn_d(j,i)
        enddo
    enddo
    close(121) 
    close(122)
 endif   
 
 ! -------------------------------- BIGJ = 80  
if (bigJ == 80) then
    
    call csv_2d_main_read('_data_pi_pl_1y.csv', pi_d) 
    call csv_2d_main_read('_data_N_pl_1y.csv', Nn_d) 
    do i = 1, bigT 
        if (Nn_d(1, i) /= 0) last_N = i
        if (pi_d(1, i) /= 0) last_pi = i
        if (Nn_d(1, i) == 0) Nn_d(1, i) = Nn_d(1, last_N)
        if (pi_d(1, i) == 0) pi_d(:, i) = pi_d(:, last_pi)
    enddo
    ! First 10 years are SS (1980 - 1989)
    Nn_d2 = Nn_d
    pi_d2 = pi_d
    !Nn_d(:, 10:bigT) = Nn_d2(:, 1:bigT - 9)
    pi_d(:, 10:bigT) = pi_d2(:, 1:bigT - 9)
    do i = 1, 9
        !Nn_d(:, i) = Nn_d(:, 10)
        pi_d(:, i) = pi_d(:, 10)
    enddo
    
      ! potrzebujemy przeliczyc pi na bezwarunkowe bo cay model na takim chodzi

    do i = 1,bigT, 1
        pi_d(1,i) = 1.0_dp
        do j = 2, bigJ
            pi_d(j,i) = pi_d(j-1,max(i-1,1))*pi_d(j,i)
        enddo
    enddo      
endif


    ! recalculate pi to express as the survival probability at the beginning of the period (from the end of period value)
    do i = 1,bigT, 1
        do j = 2, bigJ
            pi_d(j,i) = pi_d(j,max(i,1))/pi_d(1,max(i-j+1,1))
        enddo
        pi_d(1,i) = 1.0_dp
    enddo

     

!pi_d = 1d0
!do i = 1, bigT
!    !pi_d(:, i)= pi_d(:, 1)
!    Nn_d(1, i)= Nn_d(1, 1)
!enddo

! -------------------------------- unstable demography
if (switch_unstable_dem_ss == 1) then 
!! at period 55 we start concerge to stationary population
!! number of 21 year old has to increase - in contrast to data (?)
!! since 54 we overwrite population 21years old as it stock at 54 multiply by growth rate (N_55/N_54)
!! normaplization
!! do i = 1, bigT, 1
!!    Nn_d(1,i) = Nn_d(1,i)/Nn_d(1,i)
!!enddo
    nu_ss_old = 0.995
    nu_ss_new = 1.01_dp 
else 
    nu_ss_old = 1.0_dp
    nu_ss_new = 1.0_dp   
endif
!    do i = 10, bigT, 1
!        Nn_d(1,i) = Nn_d(1,i-1)*nu_ss_new
!    enddo
!
!!Nn_d(1, :) = 1.00_dp !!!! to do temp demography 
if (switch_N_from_pi == 1) then
    do i = 2, bigT, 1
        do j = 2, bigJ
            !Nn_d(j,i) = pi_d(j,max(i,1))*Nn_d(1,max(i-j+1,1))
            Nn_d(j,i) = pi_d(j,i)/pi_d(j-1,i-1)*Nn_d(j-1,i-1)
        enddo
    enddo
endif
! -------------------------------- there is no mortality
if (switch_mortality == 0) then 
    !pi_d = 1.0_dp
    do i = 1,bigT, 1
        do j = 2, bigJ
            Nn_d(j,i) = Nn_d(j-1,max(i-1,1))
        enddo
    enddo
elseif (switch_mortality == 3) then 
    do i = 2, bigT,1
        do j = 2, bigJ, 1   
            !  Nn_d(j,i) = Nn_d(j,1)
            pi_d(j,i) = pi_d(j,1)
            Nn_d(j,i) = pi_d(j,max(i,1))*Nn_d(1,max(i-j+1,1))
        enddo    
    enddo
endif


if (demography_switch == 0) then    !UBUG
    do i = 1, bigT
        pi_d(:, i) = pi_d(:, 10)
        Nn_d(:, i) = Nn_d(:, 10)
    enddo
endif   !UBUG

! -------------------------------- UNEMPLOYMENT -------------------------------
call csv_2d_main_read('_data_job_finding_rate_v2.csv', employment_mat(1, :, :)) !v2
do i = 1, bigT 
    if (employment_mat(1, 1, i) /= 0) last = i
    if (employment_mat(1, 1, i) == 0 .and. i > 30) employment_mat(1, :, i) = employment_mat(1, :, last)
enddo
    
call csv_2d_main_read('_data_separation_rate_v3.csv', employment_mat(2, :, :)) !v3
do i = 1, bigT 
    if (employment_mat(2, 1, i) /= 0) last = i
    if (employment_mat(2, 1, i) == 0 .and. i > 30) employment_mat(2, :, i) = employment_mat(2, :, last)
enddo

! First 10 years are SS (1980 - 1989)
employment_mat2 = employment_mat
employment_mat(:, :, 10:bigT) = employment_mat2(:, :, 1:bigT - 9)
do i = 1, 9
    employment_mat(:, :, i) = employment_mat(:, :, 10)
enddo
!employment_mat(1, :, 1:20) = 0d0   !UBUG

! -------------------------------- RHO -------------------------------
if (theta == 2) call csv_1d_main_read('_data_rho_v2.csv', rho_d) 
if (theta == 4) call csv_1d_main_read('_data_rho_v2.csv', rho_d) 

do i = 1, bigT 
    if (rho_d(i) /= 0) last = i
    if (rho_d(i) == 0) rho_d(i) = rho_d(last)
enddo
! First 10 years are SS (1980 - 1989)
rho_d2 = rho_d
rho_d(10:bigT) = rho_d2(1:bigT - 9)
do i = 1, 9
    rho_d(i) = rho_d(10)
enddo
if (name == 'alt') rho_d = 1d0
print *, rho_d(66)
! -------------------------------- GAMMA -------------------------------
    if (bigJ == 4) then 
        gam_d(1) = 2.046208689_dp
        gam_d(2) = 2.046208689_dp !1.646255157_dp
        gam_d(3:bigT) = 2.046208689_dp !1.400938461_dp
    elseif (bigJ == 20) then 
        gam_d(1) = 1.160885573_dp
        gam_d(2) = 1.160885573_dp
        gam_d(3) = 1.159767186264_dp
        gam_d(4) = 1.14973936308_dp
        gam_d(5) = 1.138677866832_dp
        gam_d(6) = 1.12988502456_dp
        gam_d(7) = 1.12988502456_dp
        gam_d(8) = 1.113536470124_dp
        gam_d(9) = 1.088806684672_dp
        gam_d(10) = 1.073966396652_dp
        gam_d(11:bigT) = 1.069753735521_dp
    elseif (bigJ == 16) then 
        !gam_d(1:) = (1.02_dp)**(zbar/(1.0_dp - alpha_d(1:))) ! ^(1/(1-alpha)) is changing TFP from Hicks-neutral to Harrod-neutral
        !if (switch_go_to_lower_gamma == 1) then 
        !    do i = 10, 19, 1
        !       gam_d(i) = (1.02_dp- 0.005_dp*float(i-9)/float(10))**(zbar/(1.0_dp - alpha_d(i)))    
        !    enddo
        !    gam_d(20:) = (1.015_dp)**(zbar/(1.0_dp - alpha_d(20:)))   
        !endif
        call csv_1d_main_read('_data_gam_1y.csv', gam_d) 
    elseif (bigJ == 80) then 
        !do i = 1, bigT,1
        !    read(4,*) gam_d(i)
        !    gam_d(i)=gam_d(i)**(zbar/(1.0_dp - alpha_d(i)))  ! ^(1/(1-alpha)) is changing TFP from Hicks-neutral to Harrod-neutral
        !enddo
        if (gamma_hp_switch == 0) then
            if (gamma_corrector == 0) call csv_1d_main_read('_data_gamma_corrected2.txt', gam_d) ! _data_gamma_corrected.txt    ! _data_gamma2.csv
            if (gamma_corrector == 1) call csv_1d_main_read('_data_gamma2.csv', gam_d) 
        elseif (gamma_hp_switch == 1) then
            if (gamma_corrector == 0) call csv_1d_main_read('_data_gamma_corrected_hp_1989.txt', gam_d)
            if (gamma_corrector == 1) call csv_1d_main_read('_data_gamma_hp_1989.csv', gam_d)
        endif
    endif
    !gam_d = 0d0
    !gam_d = gam_d + 1d0
    do i = 1, bigT 
        if (gam_d(i) /= 0) last_gam = i
        if (gam_d(i) == 0) gam_d(i) = gam_d(last_gam)
    enddo
    !gam(11:41) = gam(11:41) + 0.01
    !gam(1) = 1.02d0
    ! First 10 years are SS (1980 - 1989)
    gam_d2 = gam_d
    gam_d(10:bigT) = gam_d2(1:bigT - 9)
    do i = 1, 9
        gam_d(i) = gam_d(10)
    enddo
    
     
    if (gamma_switch == 0) then !param_switch == -1 .or. param_switch == -2
        select case (bigJ)
        case(4)
            gam_d = 2d0
        case(16)
            gam_d = 1.18d0
        case(80)
            !gam_d = 0.03d0
            do i = 2, bigT
                gam_d(i) = gam_d(1)
            enddo
        end select
    endif

    gam_d2 = gam_d
    do i = 2, bigT
        if (omega_switch == 1) omega_d(:, :, i) = omega_d(:, :, 1)
        if (omega_switch == 2) omega_share_d(:, :, i) = omega_share_d(:, :, 1)
        if (omega_switch == 3) then
            omega_share_d(:, :, i) = omega_share_d(:, :, 1)
            omega_d(:, :, i) = omega_d(:, :, 1) 
        endif
    enddo 
            
if (gamma_corrector == 1) then
    do i = 1, bigT
        do i_o = 1, omega_dim
            pop_share(i_o, i) = sum(omega_d(:jbar_t(i)-1, i_o, i)*omega_share_d(:jbar_t(i)-1, i_o, i)*Nn_d(:jbar_t(i)-1, i)/sum(Nn_d(:jbar_t(i)-1, i)))
        enddo
        gam_corrector(i) = sum(pop_share(:, i))
        gam_d(i) = gam_d(i)/gam_corrector(i)
        !omega_d(:, :, i) = omega_d(:, :, i)/gam_corrector(i)
    enddo    
    gam_d_diff = gam_d - gam_d2
    call chdir(cwd_g)
    open (unit = 89, FILE = "gam_diff.txt")
        do i = 1, bigT
            write(89, *) gam_d_diff(i)
        enddo    
    close(89)
    gam_d = gam_d + 1d0
    gam_d = gam_d**(1d0/(1.0d0 - alpha_d))  ! ^(1/(1-alpha)) is changing TFP from Hicks-neutral to Harrod-neutral
    
    open (unit = 84, FILE = "omega_mean.txt")
    if (gamma_hp_switch == 0) open (unit = 86, FILE = "_data_gamma_corrected2.txt")
    if (gamma_hp_switch == 1) open (unit = 86, FILE = "_data_gamma_corrected_hp_1980_v2.txt")
    write(86, *) "gamma"
    do i = 1, bigT
        write(84, *) gam_corrector(i)
        write(86, *) gam_d(i)
        !write(85, *) gam_d(i)**(zbar/(1.0_dp - alpha_d(i))) !**(1/(1-alpha)) !is changing TFP from Hicks-neutral to Harrod-neutral
        
    enddo    
    close(84)
    close(86)
    call chdir(cwd_r)
endif

call chdir(cwd_w)
    OPEN (unit=534,  FILE = "gam_cum.txt")
    gam_cum_d(1) = gam_d(1)
    write(534, '(F20.10)') gam_cum_d(1)
    do i = 2,bigT,1
        gam_cum_d(i) = gam_cum_d(i-1)*gam_d(i) ! gam cum dif graph
        write(534, '(F20.10)') gam_cum_d(i)        
    enddo
    close(534)

   zet_d(1) = 1
    do i = 2,bigT,1
        zet_d(i) = zet_d(i-1)*gam_d(i)    
    enddo


CLOSE(3)
CLOSE(4)
CLOSE(5)
CLOSE(6)
CLOSE(7)
CLOSE(9)

OPEN (unit=535,  FILE = "N_sector.csv")
do i = 1, bigT
    write(535,  '(F20.10, A, F20.10, A, F20.10, A, F20.10, A, I3)') sum(Nn_d(2:switch_fix_retirement_age-2, i)*omega_share_d(2:switch_fix_retirement_age-2 , 1, i)), ";", sum(Nn_d(2:switch_fix_retirement_age-2, i)*omega_share_d(2:switch_fix_retirement_age-2 , 2, i)), ";", sum(Nn_d(2:switch_fix_retirement_age-2, i)*omega_share_d(2:switch_fix_retirement_age-2 , 3, i)), ";", sum(Nn_d(2:switch_fix_retirement_age-2, i)*omega_share_d(2:switch_fix_retirement_age-2 , 4, i)), ";", i
    !if (i < 30) write(*,  '(F20.10, A, I3)') sum(Nn_d(2:42, i)), ";",  i
enddo
close(535)

OPEN (unit=534,  FILE = "N_post.txt")
do i = 1, bigT
    do j = 1, bigJ
            write(534,  '(F20.10, A, I3, A, I3)') Nn_d(j, i), ";",  j, ";", i
    enddo
    !if (i < 30) write(*,  '(F20.10, A, I3)') sum(Nn_d(2:42, i)), ";",  i
enddo
close(534)
OPEN (unit=533,  FILE = "pi_post.txt")
do i = 1, bigT
    do j = 1, bigJ
            write(533,  '(F20.10, A, I3, A, I3)') pi_d(j, i), ";",  j, ";", i
    enddo
enddo
close(533)
    print *, cwd_w
call chdir(cwd_r)
    !pi_d = 1d0
    !Nn_d = 1d0
end subroutine read_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module  get_data