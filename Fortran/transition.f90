! WHAT   : transition path for PAYG  
! TAKE   : initial parametrizations and values of the basic variables as a guess on the path,             
! DO     : start iteration, variable_new = f(variable_old) etc. until sum|variable_new(i) - variable_old(i)|< err_tol
! RETURN : generated transition path

MODULE transition_DB
use get_data
use global_vars
use pfi_trans
use useful_functions

IMPLICIT NONE 
CONTAINS
subroutine transition_path_DB(switch_residual, param, l_j, c_j, tax_c, r_f, V_20_years_old, g_per_capita)

    integer, parameter :: dp = kind(1.0d0)
    real(dp) :: pom
    integer :: i_mark
    real(dp), dimension(n_iter_t) :: cum_err, cum_feas
    real(dp), dimension(bigj+n_p) :: u_all
    real(dp), dimension(bigj) ::  u_init_old
    real(dp), dimension(bigT) :: k_new, k_total, k_star, i_star,  err, sv_flow, debt_share, r_bar, r, u, feas
    real(dp), dimension(bigT) :: upsilon, upsilon_r, upsilon_old, Tax, debt, replacement, replacement2, bequest, income, nu, sum_b_pom
	real(dp), dimension(bigT) :: bigL,  bigl_aux, average_l, average_w, subsidy_pom, consumption, consumption_gross, &
                                 consumption_gross_pom, consumption_gross_new, savings, savings_pom, y, k, w_bar, N_t, rI, g, g_new, deficit, &
                                 sum_priv_sv, contribution_pom, gap, valor_mult, r_, bigK, avg_hours, avg_w
    real(dp), dimension(bigj, bigT) :: savings_j, lti_j, consumption_gross_j, bequest_j, bequest_j_old, bequest_left_j
	real(dp), dimension(bigj, bigT) :: denominator_j, N_t_j, bigL_j, bigl_j_aux, savings_rate_j, labor_tax_j_agr
    real(dp), dimension(bigJ, omega_dim, bigT) :: b_j, sv_j, b_pom_j, b_j_old, sv_pom_j, sv_old_j, sv_old_pom_j, income_j, & 
                                                  w_j, l_new_j, u_j, subsidy_j, contribution_j, labor_tax_j
    real(dp), dimension(omega_dim, bigT) :: bigL_pom

    integer, intent(in) :: switch_residual	
    integer, intent(in) :: param
    real(dp), dimension(bigT), intent(out) :: r_f, tax_c, g_per_capita
    real(dp), dimension(-bigJ:bigT), intent(out) :: V_20_years_old
    real(dp), dimension(bigj, omega_dim, bigT), intent(out) :: c_j, l_j
    
    ! partial equilibrum stohastic vs deterministic model 
    real(dp), dimension(bigj) ::  u_init_old_higher_lambda, u_init_old_const_lambda
    real(dp), dimension(bigT) ::  u_higher_lambda, u_const_lambda, x_c_higher_lambda, c_higher_lambda_tot, disc_higher_lambda, zz
    real(dp), dimension(bigj, bigT) :: u_j_higher_lambda, mult_partial, x_j_higher_lambda, x_c_j_higher_lambda, sum_eq_higher_lambda, l_pen_j_vfi
    real(dp) ::  LS_higher_lambda, S_C_higher_lambda, unif_higher_lambda
    real(dp), dimension(bigJ, bigT) :: c_j_higher_lambda, l_j_higher_lambda, sv_j_higher_lambda, sv_pom_j_higher_lambda
    
    
    real(dp), dimension(bigJ, omega_dim, bigT) :: pillarI_j, pillarII_j, pillarI_old_j, pillarII_old_j, contributionI_j, contributionII_j, l_pen_j
    real(dp), dimension(bigJ, bigT) :: b1_j, b2_j
    real(dp),	dimension(bigJ,-bigJ:bigT)	:: life_exp ! -bigJ:bigT is needed for implicit tax when we want to perwfome DC- DC with changing mortality
    real(dp), dimension(bigT) :: b_scale_factor, pillarI, pillarII, contributionI, contributionII
    real(dp) :: accountI, accountII, sv_help, nom1, denom1, nom2, denom2
    
    !real(dp) :: avg_wl(bigT)
    
    real(dp),	dimension(bigJ,-bigJ:bigT)	:: tau1_s_t, tau1_a_s_t, tau2_s_t, tau1_s_t_old, tau1_a_s_t_old, tau2_s_t_old
    real(dp),	dimension(bigJ, omega_dim, -bigJ:bigT)	:: w_pom_j
    integer :: is, ii, si, i_o2
    
    !real(dp) :: unemployment_rate_om(omega_dim, bigT)
    !real(dp) :: temp_prob(switch_fix_retirement_age-1, 0:n_a, 0:n_aime, n_sp, omega_dim, bigT)
    real(dp) :: temp_prob(bigT), N_t_working(bigT), N_t2(bigT)  ! , temp_prob2(omega_dim, bigT)

    !logical, dimension(bigJ, 0:n_a, 0:n_aime, n_sp, 1:omega_dim, bigT) :: ones

    if (param == 0) then    ! 0 = with old parameters (i.e. overwriting);  1 = with default (transition) parameters
        do i = 1,bigT,1       
            omega(:, :, i) = omega_ss_1
            pi(:,i) = pi_ss_old
            Nn_(:,i) = N_ss_old
            !gam_t(i) = gam_ss_old
            jbar_t(i) = jbar_ss_old
            !alpha(i) = alpha_ss_old
        enddo

        gam_cum(1) = gam_t(1)
        do i=2,bigT,1
            gam_cum(i) = gam_cum(i-1)*gam_t(i)
        enddo
    endif

!gam_t(2) = 2d0
!ones = .true.
    
t1 = t1_ss_new
!t2 =  t2_ss_new
 
do i = 1,n_p,1
    do j =1, bigj, 1
        if (j-i+3 > ofe_u) then 
            t1(j,i) = t1_ss_old
            !t2(j,i) = t2_ss_old  
        endif
    enddo
enddo 
t1(:,1) = t1_ss_old
!t2(:,1) = t2_ss_old
t1_contrib = t1

if (switch_residual_t == 4) then ! we use contribution closure  
    t1(:,1)  = t1_ss_1
    t1(:,2:) = t1_ss_2
endif 


life_exp = 0
! initial stady state
do j = 1,bigJ,1       
	do s = 0,bigJ-j,1
        is = max(1+s,1)
	        if (s /= bigJ-j) then
	            life_exp(j,1) = life_exp(j,1) + (s+1)*(pi(j+s,1)/pi(j,1))*(1-pi(j+s+1,1)/pi(j+s,1))
	        else
	            life_exp(j,1) = life_exp(j,1) + (s+1)*pi(j+s,1)/pi(j,1) 
        endif
    enddo
enddo 
do i = 2,bigT-bigJ,1
    do j = 1,bigJ,1       
	    do s = 0,bigJ-j,1
            is = max(i+s,1)
	            if (s /= bigJ-j) then
	                life_exp(j,i) = life_exp(j,i) + (s+1)*(pi(j+s,is)/pi(j,max(i,1)))*(1-pi(j+s+1,is+1)/pi(j+s,is))
	            else
	                life_exp(j,i) = life_exp(j,i) + (s+1)*pi(j+s,is)/pi(j,max(i,1)) 
            endif
        enddo
    enddo 
enddo
do i = bigT-bigJ,bigT,1
    life_exp(:,i) = life_exp(:,bigT-bigJ)
enddo


           !!!INITIAL VALUES
include 'Initial_values_db.f90'

print *, "k_ss diff", k(bigT) - k(1)
print *, ' '

    N_t_j = Nn_
    N_t = sum(N_t_j, dim=1)
!print *, N_t(:15)
!print *,' '
    N_t2 = sum(N_t_j(2:switch_fix_retirement_age-1, :), dim=1)
!print *, N_t2(:15)
    
    do i_o = 1, omega_dim
        do i = 1, bigT
            bigL_j(:, i) = N_t_j(:, i)*l_j(:, i_o, i)*(omega_share(:, i_o, i)/sum(omega_share(:, :, i), dim = 2)) ! It doesn't contain omega 'cause it's already in l_j   !bigL_j = omega(:, i_o, :)*N_t_j*l_j
        enddo
        !bigl_j_aux = N_t_j*l_j     
        bigL_pom(i_o, :) = sum(bigL_j, dim=1)
        bigL = bigL + bigL_pom(i_o, :)
    enddo
call csv_2d_main_write("bigL_type.csv", bigL_pom)
    nu(1) = 1
    do i = 2,bigT,1
        nu(i) = bigL(i)/bigL(i-1)
    enddo 

    r_bar = zbar*alpha*k**(alpha - 1) - depr
    w_bar = zbar*(1 - alpha)*k**alpha
    y = zbar*k**(alpha)

    do i = n_p+2,bigT,1
        tc(i) = tc_new
        tl(i) = tl_new
    enddo

    lambda_trans(1) = lambda_old
    do i = 2,bigT,1
        lambda_trans(i) = lambda_new
    enddo
    
    valor_mult(1) = (1 + valor_share*(gam_t(1)*nu(1) - 1))/gam_t(1)
    valor_mult(2) = (1 + valor_share*(gam_t(1)*nu(1) - 1))/gam_t(2)
    do i = 3,bigT,1
        valor_mult(i) = (1 + valor_share*(gam_t(i-1)*(w_bar(i-1)*bigL(i-1))/(w_bar(i-2)*bigL(i-2))-1))/gam_t(i)
    enddo    

    r = 1 + (1 - tk)*r_bar
    r_f = 1 + r_bar    
    
include 'aggr_trans.f90'

include 'bequest.f90'
    
do i = 1, n_p, 1
    labor_tax_j(:, :, i) = labor_tax_ss_j_1
enddo
do i = n_p +1, bigT, 1
    labor_tax_j(:, :, i) = labor_tax_ss_j_2
enddo
avg_aime_replacement_rate = 0.33d0 ! May cause problems?

! calculate the distribution of households over state space
!do i_o = 1, omega_dim
!    do i = 2, bigT
!        call get_distribution_pre_trans(i)
!    enddo
!    prob_trans_u(:, :, i_o, :) = prob_trans_vfi_u
!enddo

!!!!!!!!!!!!!!!!! iterations start 
    include 'transition_iterations.f90' 

!!!!!!!!!!!!!!!!! iterations end

    do i = 2,bigT,1
        do j = 1,bigJ,1
            if (j == 1) then
                income_j(j, :, i) = w_j(j, :, i)*l_j(j, :, i) + b_j(j, :, i)  - upsilon(i) + bequest_j(j,i)
            else
                income_j(j, :, i) = r(i)*sv_j(j-1, :, i-1)/gam_t(i) + w_j(j, :, i)*l_j(j, :, i) + b_j(j, :, i)  - upsilon(i) + bequest_j(j,i)
            endif
        enddo
        savings_rate_j(:,i) = sum(sv_j(:, :, i)/income_j(:, :, i), dim=2) ! dim problem
    enddo
    
    income = sum(N_t_j*sum(income_j, dim=2), dim=1)/bigL ! should be weighted by omega_wieght ?
    tax_c = tc

 include 'utility_trans.f90' 


if (switch_print == 1 .and. (com /= 3 .or. com /= 5 .or. com /= 7)) then    ! .or. com /= 6 
    ! printing on screen
    include 'print_iter.f90'
    ! PRINTING TO FILES
    include 'Print_db.f90'
    print *, '1'
    call chdir(db_o)
    print *, '2'
    include 'Print_db.f90'
    print *, '3'

    call output     ! in pfi_print.f90

    call chdir(cwd_g)

    call execute_command_line ('"F:\janek\Stata16\StataIC-64.exe" /e do ".\k-draw path.do"')
    cwd_db_g2 = '"F:\janek\Stata16\StataIC-64.exe" /e do "F:\janek\code trans\SCENARIOS\k-draw path.do "'//trim(db_g)//'""'
    call execute_command_line (cwd_db_g2)
    !call execute_command_line ('".\k_path.pdf"') 
endif

end subroutine transition_path_DB

END MODULE transition_DB