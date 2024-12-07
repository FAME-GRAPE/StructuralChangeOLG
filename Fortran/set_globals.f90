! WHAT   : declare values for switches, ATTENTION! : they are overwritten in some subroutines (e.g. ret_age_dem_main etc.) !!!!to do in close future!!!!, clear (reset) global variables before next run of code 
! DO     : read initial values of switches and base (from base scenario run) values of basic variables
! RETURN : clean values for next run 
!include "main_params.f90"

MODULE global_vars2
USE global_vars
USE get_data
use pfi_trans


IMPLICIT NONE
CONTAINS

subroutine globals 

!real, dimension(bigJ, bigT) :: ones
real*8 :: pi_i_6, pi_6_6, pi_6_7, pi_7_7 ! super stars     
param_switch = 4

call chdir(cwd_p)

select case (param_switch) ! Description given below is not valid
case(-2) ! Everything on + transition is between identical steady states
    OPEN (unit=1, FILE = "_read_globals_all_on.txt")
case(-1) ! Everything off + transition is between identical steady states
    OPEN (unit=1, FILE = "_read_globals_all_off.txt")
case(0) ! Everything off
    OPEN (unit=1, FILE = "_read_globals_all_off.txt")
case(1) ! No government
    OPEN (unit=1, FILE = "_read_globals_all_off.txt")
case(2) 
    OPEN (unit=1, FILE = "_read_globals_all_off.txt")
case(3) ! All on
    OPEN (unit=1, FILE = "_read_globals_all_on.txt")
case(4) ! Finals
    OPEN (unit=1, FILE = "_read_globals.txt")
end select

        read(1,*) !read switches
        read(1,*) switch_reform ! = 0           ! 0 = base transition, 1 = main LSRA (baseline + reform + welfare change)
        read(1,*) switch_partial_eq_cal != 0   ! 0 = full transition model, 1 = decomposition of variance and expected value effect for welafare 2 (see file partial_eq_decomposition)
    
        read(1,*) switch_run_1 != 1            ! 0 = don't run old steady state; 1 = run old steady state
        read(1,*) switch_run_2 != 1            ! 0 = don't run new steady state; 1 = run new steady state
        read(1,*) switch_run_t != 1            ! 0 = don't run transition; 1 = run transition
    
        read(1,*) switch_type_1 != 0           ! initial ss: 0 = DB; 1 = DC 
        read(1,*) switch_type_2 != 0           ! final   ss: 0 = DB; 1 = DC
        switch_pension = abs(switch_type_1 - switch_type_2)         ! 0 = all are in new pension scheme in transitionFF; 1 = old cohorts remain in the old system in transitionFF

    ! note: transition path is run only if the second steady state is run
        read(1,*) switch_param_1 != 0          ! 0 = with old parameters; 1 = with new parameters  
        read(1,*) switch_param_2 != 0          ! 0 = with old parameters; 1 = with new parameters  

    ! note: parameters on the transition path are determined by the parameters on the second steady state     
        read(1,*) switch_vf      != 1         ! 0 = analitical solution, 1 endogenous grid

    !!! DEBUG_SWITCH
       read(1,*) switch_labor_choice    != 0         ! 0 = no labor choice (phi = 1) , 1 =  labor choice determined by 0<phi<1
       read(1,*) switch_mortality       != 0         ! 0 = no mortality on transition, 1 mortality according to data 
       read(1,*) switch_unstable_dem_ss != 0         ! 0 = demography  in steady state is stable (fertility rate = 2), unstable demography in steady state     
       read(1,*) swich_cohort_ps        != 1         ! 0 = points pension system like us, 1 = the same benefits within a whole cohorts  
       read(1,*) switch_see_ret != 1                 ! 0 = agent sees no tax-benefit link; 1 = agent sees implicit savings
       read(1,*) swich_fix_labor != 0d0            ! 0 = endogenous labor, other number (=0.33 for US) fix labor force participation
       read(1,*) swich_g_const != 0                  ! 0 = in bseline share GDP in ref constant per eff unit of labor, 1 = in per capita,        
                                          ! 2 = as a share of gdp, 3 in bseline share GDP in ref constant as a number
       read(1,*) gamma_switch != 0       ! 0 = gamma = const = 2%, 1 =  gamma = 2 for i< 20 & gamma = 1.5 for i > 40 7 linear in the meantime 
       read(1,*) !gamma_corrector
       gamma_corrector = 0
       gamma_hp_switch = 1                         ! 0 = regular gamma, 1 = gamma with HP filter
       read(1,*) switch_reduce_pension != 0
       read(1,*) j_young_bar                        ! number of cohorts which do not save
       read(1,*) switch_N_from_pi                   ! 1 = generate population from mortality rates
       read(1,*) omega_switch
       read(1,*) demography_switch
       read(1,*) bequest_all_switch                 ! 0 = bequests are devided among all kohorts, 1 = no bequsts for j == 1
       delta_mult_switch = 1
    
    !write(*,*) 'Is there longevity? 1 - yes, 3 - no'
    !read(*,*)   switch_mortality
   !switch_mortality = 3
    !
   if (bigJ == 4) then ! 0 = retirement age from data file, retirement age equal to value of switch_fix_retirement_age. ex switch  = 45 means jbar = 45 
        switch_fix_retirement_age = 3 
    elseif (bigJ == 16) then
        switch_fix_retirement_age = 9         
    elseif (bigJ == 20) then
        switch_fix_retirement_age = 11 
    elseif  (bigJ == 80)  then
        switch_fix_retirement_age = 43 ! thus retirement age = 42
     elseif  (bigJ == 2)  then
        switch_fix_retirement_age = 2 
   endif

  
!!!!!!!!!!!!!!!!!!!
    err_tol = 1e-7 !1e-7 !! 0.05_dp !! 
    err_ss_tol = 1e-8
    !ones = 1 

!!!!!!!! choose closure!!!!!!!!!!
        read(1,*) switch_residual_read != 1
        read(1,*) ! For now only 1 & 9 work
    select case (switch_residual_read)  
    case(0)
        closure = 'upsil_'
        switch_residual_1 = 0
        switch_residual_2 = 0 
        switch_residual_t = 0
    case(1)
        closure = 'taxC__' 
        switch_residual_2 = 1     
        switch_residual_1 = 1
        switch_residual_t = 1
    case(2)
        closure = 'taxL__' 
        switch_residual_2 = 2  
    case(3)
        closure = 'taxK__' 
        switch_residual_2 = 3
    case(4)
        closure = 'cont__' 
        switch_residual_2 = 4
    case(6)
        closure = 'frlC__' 
        switch_residual_2 = 1
    case(7)
        closure = 'frlk__' 
        switch_residual_2 = 3
    case(8)
        closure = 'btax__' 
        switch_residual_2 = 5
    case(9)
        closure = '_G____' 
        switch_residual_1 = 9
        switch_residual_2 = 9
        switch_residual_t = 9
    case(10)
        closure = 'G_taxC' 
        switch_residual_1 = 9
        switch_residual_2 = 1
        switch_residual_t = 1
    end select   
    
        read(1,*) ! params
        read(1,*) ! gov
    
        read(1,*) valor_share != 1.0_dp ! % of growth rate, to ma być 25%, jednak w REV mielismy 0.2 wiec na razie jest tyle

    ! r = 4.5, alpha = 0.3
        read(1,*) !tL_ss != 0*0.11_dp !(-0.03_dp) ! marked to data, includes excise, with ref. to PIT share in labour income in NA data
        read(1,*) !tk_ss != 0*0.15_dp !0.195_dp   
        read(1,*) !tc_ss != 0*0.0455_dp ! marked to data, includes excise, with ref. to individual consumption
    
    
    !if (switch_reduce_pension == 1) then 
    !!     r = 6.25, alpha = 0.3
    !    tL_ss = 0.028_dp ! marked to data, includes excise, with ref. to PIT share in labour income in NA data
    !    tk_ss = 0.24250_dp   
    !    tc_ss = 0.046_dp ! marked to data, includes excise, with ref. to individual consumption
    !endif


        read(1,*) debt_constr != 0*1.10_dp/zbar
    debt_constr = debt_constr/zbar
    !debt_constr = 0d0

        read(1,*) tc_growth != 0.20d0
        read(1,*) up_tc != 0.85d0 
    
        read(1,*) g_share_ss !=  0*0.17d0 !0.17_dp
        read(1,*) g_share_ss_2 !=  0*0.17d0*1.00d0 !0.17_dp
    g_share = g_share_ss_2 !0.17_dp
    
        read(1,*) ! others
        read(1,*) theta != 2.0_dp
        theta = 2d0
        read(1,*) up_t != 0.6d0 
    
    include 'parameters_CD.f90'
    
close(1)

    
if (theta == 2) then
    tL_ss = .12175d0 !0.1233365d0
    tk_ss = .158d0 !.1615d0 !.165d0 !.178d0 !0.14d0 !0.163555d0 !0.1752035d0
    tc_ss = .16072d0 !.155d0 !.166d0 !0.15d0 !0.16053d0 !0.183685d0    
elseif (theta == 4) then
    tL_ss = .1207d0 !.12175d0 !0.1233365d0
    tk_ss = .13d0 !.158d0 !.1615d0 !.165d0 !.178d0 !0.14d0 !0.163555d0 !0.1752035d0
    tc_ss = .1455d0 !.16072d0 !.155d0 !.166d0 !0.15d0 !0.16053d0 !0.183685d0    
endif 

    tL = tL_ss  ! marked to data, includes excise, with ref. to PIT share in labour income in NA data
    tk = tk_ss 
    tc = tc_ss  ! marked to data, includes excise, with ref. to individual consumption
    
    
    call chdir(cwd_r)

    if (switch_labor_choice == 0) then
        phi  = 1.00_dp 
    endif

    call read_data(omega, gam_t, gam_cum, zet, pi, Nn_, jbar_t, omega_share, alpha, gam_corrector, delta_mult, rho)
    include 'shocks_parameters.f90'
        
    alpha = 0.33_dp
    alpha_ss_old = alpha(1)
    alpha_ss_new = alpha(bigT)
    
    if (com == 0) rho(2:) = rho(1)
    !rho(2:) = rho(1)    ! alternative
    !rho = .6d0 !.6614d0
    
    rho_1 = rho(1)
    rho_2 = rho(bigT)

    if (param_switch == -1 .or. param_switch == -2) then
        
        do i = 1, bigT
            !omega_share(:, :, i) = omega_share(:, :, 1) !1d0/omega_dim
            !omega(:, :, i) = omega(:, :, 1) ! 1d0/omega_dim
            !omega_share(:, :, i) = 1d0/omega_dim
            !omega(:, :, i) = 1d0/omega_dim
        enddo
    endif
    
    omega_share_ss_old = omega_share(:, :, 1)
    omega_share_ss_new = omega_share(:, :, bigT)
    omega_ss_1(:, :) = omega(:, :, 1) !0.6d0
    omega_ss_2(:, :) = omega(:, :, bigT) !0.4d0
    
    t1_ss_contrib = t1_ss_old
    t1 = t1_ss_old

    jbar_ss_old = jbar_t(1)
    jbar_ss_new = jbar_t(bigT)

    gam_ss_old = gam_t(1)
    gam_ss_new = gam_t(bigT)

    pi_ss_old = pi(:,1) !8 / 13
    pi_ss_new = pi(:,bigT)!pi_ss_old !pi(:,13) !pi(:, bigT)

    N_ss_old = Nn_(:,1)
    N_ss_new = Nn_(:,bigT)
    
    if (param_switch == -1 .or. param_switch == -2) then
        do i = 1, bigT
            !pi(:, i) = pi_ss_old
            !Nn_(:, i) = N_ss_old
        enddo
        !pi_ss_new = pi_ss_old
        !N_ss_new = N_ss_old
    endif
    
    if (bigJ /= 80) then
        up_t =  0.6_dp
        up_ss = 0.6d0
    else
        up_t =  0.7_dp 
        if (theta == 1d0) then
            up_ss = 0.9d0
        else 
            up_ss = 0.8d0
        endif
    endif
    !z = 2**(1-alpha(1))
    z = 1d0
    ss = 0
call chdir(cwd_w)
call csv_2d_main_write("N_ss_old.txt", Nn_) 
    
end subroutine globals

subroutine clear_globals
    k_ss_1 = 0
    r_ss_1 = 0
    w_bar_ss_1 = 0
    upsilon_r_ss_1 = 0
    l_ss_j_1 = 0
    w_ss_j_1 = 0
    s_ss_j_1 = 0
    c_ss_j_1 = 0
    b_ss_j_1 = 0
   
    k_ss_2 = 0
    r_ss_2 = 0
    w_bar_ss_2 = 0
    upsilon_r_ss_2 = 0
    l_ss_j_2 = 0
    w_ss_j_2 = 0
    s_ss_j_2 = 0
    c_ss_j_2 = 0
    b_ss_j_2 = 0

    c_db = 0
    l_db = 0
    tax_c_db = 0
    r_db = 0 
    
    gam_t = 0d0
end subroutine clear_globals

end module global_vars2
