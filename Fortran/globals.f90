! WHAT  : declaration of global (ALL subroutines and functions MAY USE them) parameters and variables
! TAKE  : none 
! DO    : definition of a switch in comments 
! RETURN: nothing
MODULE global_vars
!use params
IMPLICIT NONE
   save
    !integer :: n_iter_ss
    !integer :: n_iter_t
    !integer :: bigJ
    !integer :: omega_dim
    !integer :: n_sp
    !
    integer, parameter :: n_iter_ss = 200
    integer, parameter :: n_iter_t = 40
    integer, parameter :: bigJ = 80d0
    integer, parameter :: omega_dim = 4d0
    integer, parameter :: n_sp = 6
    integer, parameter :: n_sp_disc = n_sp - 1  ! disc = discretise
    !call read_main_params() 

    
    integer, parameter :: dp = kind(1.0d0)
    integer, parameter :: n_p = 300, n_debt  = 60, forward = 1 ! id does not work :( 
    real(dp), parameter :: forward_smoothing = 1d0/real(forward)
	integer, parameter :: bigT = n_p+bigJ+1
    integer, parameter :: ofe_u = 1
    real(dp), parameter :: zbar = real(80/bigJ) ! J periods model scale parameter
    integer :: iter, i, j, s, m, cl, ss, com
    character(6) :: version
    character(6) :: closure
    character(128) :: cwd, cwd_r, cwd_w, cwd_g, cwd_p, name, name2, clear_w, clear_g1, clear_g2
    character(256) :: db_g, db_o, cwd_db_g, cwd_db_o, cwd_db_g2, stata
    
 
    integer :: switch_run_1, switch_run_2, switch_run_t     ! 0 = don't run the first/second steady state/transition path; 1 = run the first/second steady state/transition path
    integer :: switch_param_1, switch_param_2               ! 0 = with old parameters; 1 = with new parameters (pi,gam,N,jbar) 
    integer :: switch_type_1, switch_type_2                 ! 0 = payg; 1 = ff
    integer :: switch_residual_1                            ! old steady state; 0 = upsilon is residual, there are no more options for the first steady state
    integer :: switch_residual_2                            ! new steady state; 0 = upsilon is residual; 1 = tC is residual; 2 = tL is residual                
    integer :: switch_residual_t                            ! transition path;  0 = upsilon is residual; 1 = tC is residual; 2 = tL is residual
    integer :: switch_residual_read                         ! set scheme for residuals in both ss and on transition
    

    integer :: switch_print
    integer :: switch_vf
    integer :: switch_labor_choice                         ! 0 = no labor choice (phi = 1) , 1 =  labor choice determined by 0<phi<1
    integer :: switch_mortality                            ! 0 = no mortality on transition, 1 mortality according to data 
    integer :: switch_fix_retirement_age                   ! 0 = retirement age from data file, retirement age equal to value of switch_fix_retirement_age
    integer :: switch_unstable_dem_ss                      ! 0 = demography  in steady state is stable (fertility rate = 2), unstable demography in steady state 
    integer :: switch_reform                               ! 0 = base transition, 1 = main LSRA (baseline + reform + welfare change)
    integer :: switch_partial_eq_cal                       ! 0 = full transition model, 1 = decomposition of variance and expected value effect for welafare 2 (see file partial_eq_decomposition)
    integer :: swich_cohort_ps                             ! 0 = points pension system like us, 1 = the same benefits within a whole cohorts 
    integer :: switch_pension                              ! 0 = all are in new pension scheme in transitionFF; 1 = old cohorts remain in the old system in transitionFF
    integer :: switch_see_ret                              ! 0 = agent sees no tax-benefit link; 1 = agent sees implicit savings
 
    
    REAL    :: swich_fix_labor                             ! 0 = endogenous labor, other number (=0.33 for US) fix labor force participation
    integer :: switch_partial_eq                           ! 0 = full transition model, 1 = decomposition of variance and expected value effect for welafare 2 (see file partial_eq_decomposition)
    integer :: swich_g_const                               ! 0 = baseline g keept as a fixed share of gdp, reform keept as a fixed number form baseline scanario
                                                           ! 1 = keept as a fixed in per capita terms 
    integer :: switch_ref_run_now 
    integer :: gamma_switch                                ! 0 = constant gamma, 1 = gamma from data                         ! 0 = gamma = const = 2%, 1 =  gamma = 2 for i< 20 & gamma = 1.5 for i > 40 7 linear in the meantime     
    integer :: gamma_corrector
    integer :: switch_reduce_pension
    integer :: j_young_bar
    integer :: switch_N_from_pi
    integer :: omega_switch 
    integer :: demography_switch
    integer :: param_switch
    integer :: bequest_all_switch
    integer :: delta_mult_switch
    integer :: gamma_hp_switch


    ! heterogenity ex-ante
    real(dp), dimension(bigJ, omega_dim) :: omega_share_ss, omega_share_ss_old, omega_share_ss_new
    real(dp), dimension(bigJ, omega_dim, bigT) :: omega_share
    !real(dp), dimension(omega_dim) :: bigL_o_ss
    integer :: i_o
    
    ! Deklaracja zmiennych wczytywanych
    real(dp), dimension(bigJ, omega_dim) :: omega_ss_2, omega_ss_1
    !real(dp), dimension(bigJ, omega_dim, bigT) :: omega
    real(dp), dimension(bigJ, omega_dim, bigT) :: omega 
    real(dp), dimension(n_p)  :: gam

! Deklaracje zmiennych, ktore nam zostaja po steady state'ach
    real(dp) :: k_ss_1, r_ss_1, r_bar_ss_1, w_bar_ss_1, upsilon_r_ss_1, t1_ss_1, g_per_capita_ss_1 !, rho_ss
    real(dp) :: k_ss_2, r_ss_2, r_bar_ss_2, w_bar_ss_2, upsilon_r_ss_2, t1_ss_2, g_per_capita_ss_2
    real(dp), dimension(bigJ, omega_dim) :: l_ss_j_1, w_ss_j_1, s_ss_j_1, c_ss_j_1, b_ss_j_1, l_ss_pen_j_1, labor_tax_ss_j_1
    real(dp), dimension(bigJ, omega_dim) :: l_ss_j_2, w_ss_j_2, s_ss_j_2, c_ss_j_2, b_ss_j_2, l_ss_pen_j_2, labor_tax_ss_j_2
    
! Parametry
    real(dp) :: beta, delta, depr, theta, phi, up_ss, up_t, rho_1, rho_2, err_tol, err_ss_tol, rho1, rho2, alpha_ss_old, alpha_ss_new, rho(bigT)
    real(dp) :: g_share_ss, g_share_ss_2, tk_ss, tL_ss, tc_ss, t1_ss_old, t1_ss_new, t2_ss_old, t2_ss_new, valor_share, debt_constr, tc_new, tl_new, tk_new
    real(dp) :: jbar_ss_old, jbar_ss_new, gam_ss_old, gam_ss_new, nu_ss_old, nu_ss_new
    real(dp) :: tc_growth, up_tc, z
    real(dp), dimension(bigJ) :: pi_ss_old, pi_ss_new, N_, N_ss_old, N_ss_new 

! transition variables
    integer, dimension(bigT) :: jbar_t
	real(dp), dimension(bigT):: g_share, tk, tL, tc, gam_cum, zet, feasibility, alpha
    real(dp), dimension(bigT):: gam_t, gam_corrector
    real(dp), dimension(bigJ, bigT) :: Nn_, pi, t1
  
! LSRA
    real(dp), dimension(bigJ, bigT) :: better_j !  c_base, l_base,  c_ref, l_ref
    real(dp), dimension(bigJ, omega_dim, bigT) :: c_db, l_db
    real(dp), dimension(- bigJ: bigT) ::  V_20_years_old_db !  V_20_years_old_base, V_20_years_old_ref
    real(dp), dimension(bigT) :: r_db, tax_c_db, g_per_capita_db, better !  r_base, tax_c_base, g_per_capita_base, r_ref, tax_c_ref, g_per_capita_ref
    real(dp), dimension(bigJ, bigT) :: x_j_pro,  x_unif_pro, sum_x_pro, x_c_j_pro, eq_unif_pro, sum_eq_pro
    real(dp) :: LS_pro , g_trans(bigT), g_ss_2, upsilon_r_ss_nr



 ! pfi 
    real*8, parameter  :: fi = (5d0**(1d0/2d0)-1d0)/2d0
    integer, parameter :: n_a = 100, n_aime = 0, switch_a_min = 1 ! n_a = 300
    real*8, parameter  :: a_min = 0d0
    real*8 ::  a_l, a_u, a_grow, aime_l, aime_u, aime_grow, poss_ass_sum_ss(bigJ), sigma_nu_p(omega_dim), n_sp_initial(omega_dim), sigma_nu_r,&
               zeta_p(omega_dim), zeta_r, r_ss_, zeta_d, sigma_nu_d, &
               pi_ip(n_sp, n_sp, omega_dim, bigT), pi_ip_disc(n_sp, n_sp, omega_dim), pi_ip_ss(n_sp, n_sp, omega_dim), n_sp_value(n_sp, omega_dim), employment_mat(1:2, omega_dim, bigT)
               !, pi_ir(n_sr,n_sr), n_sr_value(n_sr), pi_id(n_sd,n_sd), n_sd_value(n_sd), prob_norm(n_sr) 
    real*8 :: aime_cap, aime_cap_ge
    real*8 :: unemployment_rate(bigT) !, unemployment_rate2(bigT)
    real*8, dimension(omega_dim, bigT) :: delta_mult
  
 ! pension system 
    real*8 :: sum_b_weight_ss, b_scale_factor_old, b_scale_factor_new, priv_share, t1_ss_contrib
    real*8, dimension(bigJ) :: b1_ss_j_1, b2_ss_j_1, b1_ss_j_2, b2_ss_j_2
    real*8, dimension(bigJ, omega_dim) ::  pillar1_ss_j_1, pillar2_ss_j_1, pillar1_ss_j_2, pillar2_ss_j_2
    real*8, dimension(bigJ, bigT) :: t1_contrib(bigJ, bigT), t2(bigJ, bigT)
    real*8, dimension(bigJ, omega_dim, bigT) :: sum_b_trans
    !real*8, dimension(omega_dim, bigT) ::  sum_b_weight_trans_om
    real*8, dimension(bigT) :: sum_b1_help, avg_ef_l_suply_trans, subsidy, contribution, sum_b
    real*8, dimension(bigJ, n_sp, omega_dim) :: b1_ss, b2_ss
    real*8, dimension(n_sp, omega_dim) :: aime_ss
    real*8, dimension(bigJ, n_sp, omega_dim, bigT) :: b1_t, b2_t
    real*8, dimension(n_sp, omega_dim, bigT) :: aime_t
    !real*8, dimension(omega_dim, bigT) :: 

! implicit tax
    real(dp), dimension(bigj) :: tau1_ss_1, tau1_a_ss_1, tau2_ss_1, &
                                 tau1_ss_2, tau1_a_ss_2, &
                                 tau2_ss_2, &
                                 transfer_pfi, bequest_left_ss_j_1, bequest_left_ss_j_2
    real(dp) :: ret1_help, ret2_help, savings_ss_pom, pom2
    
    real(dp), dimension(bigJ, omega_dim) :: s_pom_ss_j_1, s_pom_ss_j_2, w_pom_ss_j_1, w_pom_ss_j_2, & 
                                            b_pom_ss_j_1, b_pom_ss_j_2
    
 ! progression
    real*8  :: lambda_old = 0*0.15d0, lambda_new = 0*0.15d0, progression_param= 0.15d0
    real*8  :: tau, lambda, lambda_trans(bigT), labor_tax_j_vfi_ss_1(bigJ), labor_tax_j_vfi_ss_2(bigJ)
    integer :: i_temp
    real*8  :: tl_com, lambda_com
    
! partial 
    real*8  :: avg_aime_replacement_rate(bigJ, bigT)
    integer :: switch_partial_efficiency = 0d0 , iter_theta, if_border
    
! elasticity
    real(dp), dimension(bigT) ::  savings_el, k_el, elasticity
    real(dp), dimension(bigJ, bigT) :: sv_j_el
    real(dp) :: temp_test
    
    real(dp), dimension(bigT) :: delta_K, K_semi_elasticity_reform, K_semi_elasticity_base, delta_proc_r, r_semi_elasticity_reform , r_semi_elasticity_base,  semi_elasticity
    real(dp) :: pop_denom
    integer :: switch_partial_semi = 0
    
! print
    character(30) c_name, s_name, f_name, f_name2, f_name3, f_name4, f_name5, f_name6, theta_string
    integer :: i_n_sp
    character(8) c_n
    character(8)  :: date
    character(10) :: time
    
    real(dp) :: bigL_ss_j, avg_ef_l_suply
    real(dp), dimension(omega_dim) :: sum_b_weight_ss_om
    
! temps
    
    real(dp) :: g_ss2
    real(dp), dimension(1:bigT) :: g_tr
    
end module global_vars