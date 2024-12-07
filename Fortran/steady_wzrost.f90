! WHAT   : stady state routine for PAYG 
! TAKE   : data on mortality [[pi]], retirement age [[jbar]], change in technological progress [[gamma = z_t/z_(t-1)]], the size of the cohort [[N_ss_j]], pension system parameters [[t1 - ZUS, [[t2 - OFE]]  <- constant during iteration
!          and guess for capital, labor force participation [[l_]], consumption, bequest , nu_ss population growth rate in steady state, N_ss_j population age structure
! DO     : during iterations update guess values to new ones in order to find optimal allocation in steady state; iterations end when difference between old and new value is really small (err_ss < 1e-8)
! RETURN : k_ss, c_ss, r_ss etc. that fullfil the condition of optimal allocation. It is one of the edge of transition path

MODULE steady_state
use global_vars
!use individual_vf
use pfi_trans
use csv_2d
use to_str
use useful_functions

IMPLICIT NONE

CONTAINS

subroutine steady(switch_residual, param_ss, switch_type, omega_ss, omega_share_ss, alpha_ss, rho_ss, k_ss_o, r_ss, r_bar_ss,  w_bar_ss, l_ss_j, w_ss_j, s_ss_j, c_ss_j, b_ss_j, upsilon_r_ss, t1_ss, g_per_capita_ss, b1_ss_j, b2_ss_j, pillarI_ss_j, pillarII_ss_j)
    real(dp) :: k_ss, k_ss_new,  k_total_ss, k_star_ss, i_star_ss, err_ss, u_ss, &
                jbar_ss, gam_ss, N_ss, nu_ss, bigL_ss, bigK_ss, subsidy_ss, y_ss,  consumption_ss_gross, &
                savings_ss, average_l_ss, average_w_ss, upsilon_ss, bequest_ss, income_ss, &
                deficit_ss, debt_ss, Tax_ss, g_ss, g_ss_new, sum_priv_sv_ss, valor_mult_ss, &
                consumption_ss_gross_pom, savings_ss_pom, subsidy_ss_pom, alpha_ss, rho_ss, sum_b_ss_pom, contribution_ss_pom, contribution_ss, avg_hours_ss, avg_w_ss !, alpha_ss_old, alpha_ss_new
    real(dp), dimension(bigj) :: pi_ss, life_exp
    real(dp), dimension(bigj) :: lti_ss_j,  consumption_ss_gross_j,  savings_ss_rate_j
	real(dp), dimension(bigj) :: denominator_j, N_ss_j, l_ss_pen_j_vfi
    real(dp), dimension(bigj) :: bequest_ss_j, bequest_ss_j_old, bequest_left_ss_j

    !real(dp), intent(in)  :: rho_ss	
    integer, intent(in)   :: param_ss
    integer, intent(in)   :: switch_residual, switch_type			
    real(dp), dimension(bigJ, omega_dim) ::  omega_ss
    real(dp), intent(out) :: k_ss_o, r_ss, r_bar_ss, w_bar_ss, upsilon_r_ss, t1_ss, g_per_capita_ss
    real(dp), dimension(bigj, omega_dim), intent(out) :: l_ss_j, w_ss_j, s_ss_j, c_ss_j
    real(dp), dimension(bigj, omega_dim), intent(out) :: b_ss_j ! , n_sp
    real(dp), dimension(bigj, omega_dim) :: omega_share_ss, sum_b_ss_omega, sum_b2_ss_omega !, omega_share_ss_old, omega_share_ss_new
    real(dp), dimension(omega_dim) :: delta_mult_ss
    ! pension system 
     real(dp), dimension(bigj) :: b1_ss_j, b2_ss_j, pillarI_ss_j, pillarII_ss_j, &
                                 contributionI_ss_j, contributionII_ss_j, labor_tax_ss_j_agr, savings_ss_j_agr
    
     real(dp) ::  accountI_ss, accountII_ss, pillarI_ss, pillarII_ss, rI_ss, b_scale_factor_ss, t2_ss, &
                nom1, denom1, nom2, denom2, sum_b_ss 
     real(dp), dimension(bigj, omega_dim) :: subsidy_ss_j, tau1_ss, tau1_a_ss, tau2_ss, b_pom_ss_j, & 
                                            w_pom_ss_j, s_pom_ss_j, income_ss_j, savings_ss_j, u_ss_j, & 
                                            labor_tax_ss_j, lab_ss_j, l_ss_pen_j
    
    real(dp), dimension(bigj, n_a) :: V_ss_j
    integer :: time
    
    !omega_ss = omega_ss
    if (param_ss == 0) then ! 0 = with old parameters;  1 = with new parameters
        gam_ss = gam_ss_old
        pi_ss = pi_ss_old
        N_ss_j =  N_ss_old
        jbar_ss = jbar_ss_old
        nu_ss =  nu_ss_old
        t1_ss = t1_ss_old
        t2_ss = t2_ss_old
        lambda = lambda_old
        !omega_ss = omega_ss_1
        omega_share_ss = omega_share_ss_old
        alpha_ss = alpha_ss_old 
        pi_ip_ss = pi_ip(:, :, :, 1)
        delta_mult_ss = delta_mult(:, 1)
        rho_ss = rho_1
    else 
        gam_ss = gam_ss_new
        pi_ss = pi_ss_new
        N_ss_j =  N_ss_new
        jbar_ss = jbar_ss_new
        nu_ss = nu_ss_new
        t1_ss = t1_ss_new
        t2_ss = t2_ss_new
        lambda = lambda_new
        !omega_ss = omega_ss_2
        omega_share_ss = omega_share_ss_new
        alpha_ss = alpha_ss_new
        pi_ip_ss = pi_ip(:, :, :, bigT)
        delta_mult_ss = delta_mult(:, bigT)
        rho_ss = rho_2
    endif
    !if (switch_run_1 == 0) then
    !    !pi_ss = pi_ss_new
    !    gam_ss = gam_ss_new
    !    alpha_ss = alpha_ss_new
    !    !z = 1.01d0
    !endif
!OPEN (unit=121, FILE = version//closure//"test_k_NEW.txt")
!open(unit = 64, file= "k_ss.csv")
!write(64, '(A)') "k_ss"
!close(64)
 ss = ss + 1
 if (ss == 1) then 
     time = ss
 elseif (ss == 2) then
     time = bigT
endif
!if (switch_run_1 == 1) then
!    omega_ss = omega_ss_1
!else
!    omega_ss = omega_ss_2
!endif

! normalized structure of population such as N_ss_j(1) = 1 (number of 20 years old)
!N_ss_j(1) = 1.0_dp ! delete
!do j = 2, bigJ
!    N_ss_j(j) = pi_ss(j)/pi_ss(1)
!enddo
life_exp = 0
do j = 1,bigJ,1       
    do s = 0,bigJ-j,1
        if (s /= bigJ-j) then
            life_exp(j) = life_exp(j) + (s+1)*(pi_ss(j+s)/pi_ss(j))*(1-pi_ss(j+s+1)/pi_ss(j+s))
        else ! i.e. s==bigJ-j
            life_exp(j) = life_exp(j) + (s+1)*pi_ss(j+s)/pi_ss(j)  
        endif
    enddo
enddo  
b_scale_factor_ss = 1d0
avg_ef_l_suply = 1 !0.33

    valor_mult_ss = (1 + valor_share*(nu_ss*gam_ss - 1))/gam_ss 
    rI_ss = gam_ss*nu_ss - 1
    N_ss = sum(N_ss_j(1:bigJ))       

    ! guess 
    
    r_bar_ss = (1 + 0.03_dp)**(zbar) - 1 !(1 + 0.078_dp)**(zbar) - 1
    k_ss = ((r_bar_ss + depr)/(alpha_ss*zbar))**(1/(alpha_ss - 1))
    
    ! upsilon gess residual closure ( we need only upsil, other parameters are given in set globals)
    select case (switch_residual)
    case(0)
        upsilon_ss = 0d0 ! 0.0686986 !0.053_dp
    endselect

    bequest_ss = 0.0_dp
    bequest_ss_j = 0
    bequest_left_ss_j = 0
    bequest_ss_j_old = 0 
    
    do i_o = 1, omega_dim
        bigL_ss_j = sum(N_ss_j*(omega_share_ss(:, i_o)/sum(omega_share_ss, dim = 2)))
        bigL_ss = bigL_ss + bigL_ss_j
    enddo
!open(unit = 114, file= "assets_iter.csv")    
!write(114, '(A)') "s_ss; omega; o; j; iter"
!!! ITERATIONS STARTS     
do iter = 1, n_iter_ss, 1
        
    include 'denominator_ss.f90'      

    bequest_ss = 0
    consumption_ss_gross_pom = 0
    savings_ss_pom = 0
    subsidy_ss_pom = 0
    contribution_ss_pom = 0d0
    labor_tax_ss_j_agr = 0d0
    sum_b_ss = 0d0
    sum_b_ss_omega = 0d0


    r_bar_ss = z*zbar*alpha_ss*k_ss**(alpha_ss - 1) - depr
    w_bar_ss = z*zbar*(1 - alpha_ss)*k_ss**alpha_ss
    y_ss = z*zbar * k_ss**(alpha_ss)
    !w_bar_ss = 4d0
    !r_bar_ss = 1/delta - 1 
    if (r_bar_ss < 0) then
        r_bar_ss = 0
    endif
    r_ss = 1 + (1 - tk_ss)*r_bar_ss  
    !r_ss = 1/delta - 1 

    avg_hours_ss = bigL_ss/sum(N_ss_j(1:jbar_ss-1))
    avg_w_ss = w_bar_ss*avg_hours_ss
    
    do i_o = 1, omega_dim
        do j = 1, jbar_ss
            w_ss_j(j, i_o) = (1-tl_ss)*(1 - t1_ss-t2_ss)*omega_ss(j, i_o)*w_bar_ss
            !w_ss_j(j, i_o) = 4d0
        enddo
    enddo
    
    valor_mult_ss = (1 + valor_share*(gam_ss*nu_ss  - 1))/gam_ss
    
    do i_o = 1, omega_dim
        do ip = 1, n_sp
            aime_ss(ip, i_o) = omega_ss(jbar_ss-1, i_o)*n_sp_value(ip, i_o)*avg_w_ss 
        enddo
    enddo
    b1_ss(jbar_ss, :, :) = rho_ss*rho1*avg_w_ss
    b2_ss(jbar_ss, :, :) = rho_ss*rho2*aime_ss
    do j = jbar_ss+1, bigJ
        b1_ss(j, :, :) = b1_ss(j-1, :, :)*valor_mult_ss   
        b2_ss(j, :, :) = b2_ss(j-1, :, :)*valor_mult_ss   
    enddo
    b1_ss(:jbar_ss-1, :, :) = 0d0  
    b2_ss(:jbar_ss-1, :, :) = 0d0
    
    !b1_ss(jbar_ss) = rho1*w_bar_ss
    !b2_ss(jbar_ss, :, :) = rho2*aime_ss  
    !do j = jbar_ss+1, bigJ
    !    b1_ss(j) = b1_ss(j-1)
    !    b2_ss(j, :, :) = b2_ss(j-1, :, :)
    !enddo
    
    if (iter == 1) g_ss = 0.2*y_ss
    if (switch_residual /= 9) then
        if (switch_run_1 == 1) then 
            g_ss = g_share_ss*y_ss
            g_per_capita_ss = g_ss*bigL_ss/N_ss
        else 
            if (swich_g_const == 1) then
                g_per_capita_ss = g_per_capita_ss_1 
                if (iter == 1) then ! we do not have big l in first iter 
                    g_ss = g_share_ss_2*y_ss
                else
                    g_ss = g_per_capita_ss*N_ss/bigL_ss 
                endif
            elseif (swich_g_const == 0) then
                if (switch_ref_run_now == 0 ) then ! we are running baseline scenario where g is kept as a share of gdp
                    g_ss = g_share_ss_2*y_ss
                    g_ss_2 = g_ss
                else ! here we are keeping g constant in per eff unit of labor 
                    g_ss = g_ss_2
                endif
            elseif (swich_g_const == 2) then
                g_ss = g_share_ss_2*y_ss
             elseif (swich_g_const == 3) then
                if (switch_ref_run_now == 0 ) then ! we are running baseline scenario where g is kept as a share of gdp
                    g_ss = g_share_ss_2*y_ss
                    g_ss_2 = g_ss*bigL_ss
                else ! here we are keeping g constant in per eff unit of labor 
                    if (iter == 1) then ! we do not have big l in first iter 
                        g_ss = g_share_ss_2*y_ss
                    else
                        g_ss = g_ss_2/bigL_ss 
                    endif
                endif
            endif
        endif
    endif
    !if (param_ss == 1) g_ss = g_ss2         ! temp
    debt_ss = debt_constr*y_ss
    sum_priv_sv_ss = k_ss*gam_ss*nu_ss + debt_ss !- PillarII_ss

    include 'bequest_ss.f90'

    bigL_ss = 0
    do i_o = 1, omega_dim, 1
        !include 'implicit_tax_ss.f90'
        if (switch_vf == 0) then
            !sum_b_weight_ss = 1d0
            !include 'lti_ss.f90'
            !l_ss_pen_j = l_ss_j(:, i_o)
            !labor_tax_ss_j_vfi(1:bigJ) = tl_ss*(1 - t1_ss-t2_ss)*omega_ss*w_bar_ss*l_ss_j(:)
        elseif (switch_vf > 0) then
            w_pom_ss_vfi(:) = (1.0_dp - t1_ss - t2_ss)*omega_ss(:, i_o)*w_bar_ss 
            w_bar_ss_vfi = w_bar_ss
            b1_ss_vfi = b1_ss(:,:,i_o)
            b2_ss_vfi = b2_ss(:,:,i_o)
            !w_pom_ss_implicit_vfi(:, i_o) = (t1_ss*tau1_ss(:, i_o) +  t2_ss*tau2_ss(:, i_o))*omega_ss(:, i_o)*avg_w_ss
            r_ss_vfi = (1d0 - tk_ss)*r_bar_ss
            !delta = 1/(1+r_ss_vfi)
            omega_share_ss_vfi = omega_share_ss
            omega_ss_vfi = omega_ss(:, i_o)
            n_sp_initial_vfi = n_sp_initial(i_o)
            pi_ip_ss_vfi = pi_ip_ss(:, :, i_o)
            n_sp_value_vfi = n_sp_value(:, i_o)
            delta_mult_ss_vfi = delta_mult_ss(i_o)
            tc_ss_vfi = 1_dp + tc_ss
            gam_ss_vfi = gam_ss
            pi_ss_vfi = pi_ss
            !b_ss_j_vfi(:, :) = b_ss_j(:, :, i_o)
            b_ss_j_vfi(:) = b_ss_j(:, i_o)
            bequest_ss_j_vfi(:) =  bequest_ss_j(:)!*(omega_share(:, i_o)/sum(omega_share, dim = 2)) 
            bequest_ss_j_vfi_dif(:) = (bequest_ss_j - bequest_ss_j_old)!*(omega_share(:, i_o)/sum(omega_share, dim = 2)) 
            jbar_ss_vf = switch_fix_retirement_age !ceiling(jbar_ss)
            upsilon_ss_vf = upsilon_ss
            upsilon_dif_ss = upsilon_ss - upsilon_old_ss
            N_ss_j_vfi =  N_ss_j
            !l_cap_trans_vfi(:, :, :, :, time) = l_cap_trans(:, :, : ,: , i_o, time)
            iter_com = iter
            call agent_vf() !
            c_ss_j(:, i_o) = c_ss_j_vfi(:)
            c_ss_g(:,:,:,:,i_o) = c_ss
            !l_cap_trans(:, :, : ,: , i_o, time) = l_cap_trans_vfi(:, :, :, :, time)
            !s_ss_g(:,:,:,:,i_o) = s_ss
            l_ss_j(:, i_o) = l_ss_j_vfi(:)*omega_ss(:, i_o)
            lab_ss_j(:, i_o) = lab_ss_j_vfi(:)
            l_ss_pen_j(:, i_o) = l_ss_pen_j_vfi 
            s_pom_ss_j(1:bigJ-1, i_o) = s_pom_ss_j_vfi(1:bigJ-1) 
            !sum_b_weight_ss_om(i_o) = sum_b_weight_ss
            labor_tax_ss_j(:, i_o) = labor_tax_ss_j_vfi
            sum_b2_ss_omega(:, i_o) = sum_b2_ss_vfi
            sum_b_ss_omega(:, i_o) = sum_b_ss_vfi
            !if ((switch_type == 1) .and. (switch_see_ret == 1)) then ! 
                !do j = 1, bigJ, 1  
                !    if (j == 1) then    
                !        s_ss_j(j, i_o) = w_pom_ss_vfi(j)*l_ss_j(j, i_o)/omega_ss(j, i_o) - labor_tax_ss_j_vfi(j) - c_ss_j(j, i_o)*(1+tc_ss) - upsilon_ss + bequest_ss_j(j)!*(omega_share(j, i_o)/sum(omega_share(j, :))) 
                !    else
                !        s_ss_j(j, i_o) = r_ss*s_ss_j(j-1, i_o)/gam_ss + w_pom_ss_vfi(j)*l_ss_j(j, i_o)/omega_ss(j, i_o) - labor_tax_ss_j_vfi(j) + b_ss_j(j, i_o) - c_ss_j(j, i_o)*(1d0+tc_ss) - upsilon_ss + bequest_ss_j(j)!*(omega_share(j, i_o)/sum(omega_share(j, :))) 
                !    endif
                !enddo
            !else
               s_ss_j =  s_pom_ss_j
            !endif
        endif
    enddo
    avg_ef_l_suply = 0d0
    do i_o = 1, omega_dim
        avg_ef_l_suply = avg_ef_l_suply + sum(N_ss_j(1:jbar_ss-1)*l_ss_j(1:jbar_ss-1, i_o)*(omega_share_ss(:, i_o)/sum(omega_share_ss, dim = 2)))/sum(N_ss_j(1:jbar_ss-1)) ! *omega_ss(:, i_o)       
    enddo   
    
    savings_ss_j_agr = 0d0
    
    do i_o = 1, omega_dim
    
        bigL_ss_j = sum(N_ss_j*l_ss_j(1:jbar_ss-1, i_o)*(omega_share_ss(:, i_o)/sum(omega_share_ss, dim = 2)))
        bigL_ss = bigL_ss + bigL_ss_j
        
        labor_tax_ss_j_agr = labor_tax_ss_j_agr + labor_tax_ss_j(:, i_o)*(omega_share_ss(:, i_o)/sum(omega_share_ss, dim = 2))

        consumption_ss_gross_j  = c_ss_j(:, i_o)
        consumption_ss_gross_pom = consumption_ss_gross_pom + sum(consumption_ss_gross_j(1:bigJ)*N_ss_j(1:bigJ)*(omega_share_ss(:, i_o)/sum(omega_share_ss, dim = 2))) 

        savings_ss_j = s_ss_j !+ pillarII_ss_j
        savings_ss_j_agr = savings_ss_j_agr + savings_ss_j(:, i_o)*omega_share_ss(:, i_o)/sum(omega_share_ss, dim = 2)
        savings_ss_pom = savings_ss_pom + sum(N_ss_j(1:bigJ)*savings_ss_j(1:bigJ, i_o)*(omega_share_ss(:, i_o)/sum(omega_share_ss, dim = 2))) 
        
    enddo
        
    do i_o = 1, omega_dim
        include 'pension_system_ss.f90'      
    enddo

    consumption_ss_gross = consumption_ss_gross_pom/bigL_ss
    savings_ss = savings_ss_pom/bigL_ss
    sum_b_ss = sum_b_ss_pom/bigL_ss
    subsidy_ss = subsidy_ss_pom/bigL_ss
    contribution_ss = contribution_ss_pom/bigL_ss
    
    include 'closure_ss.f90'
    
    k_ss_new = (savings_ss - debt_ss)/(gam_ss*nu_ss)
    err_ss = abs(k_ss_new - k_ss)
    k_ss = up_ss*k_ss + (1 - up_ss)*k_ss_new
    bigK_ss = k_ss*bigL_ss
    


!do i_o = 1, omega_dim
!    do j = 1, bigJ, 1
!        write(114, '(F20.10,A,F20.10,A,I1,A,I2,A,I3)')s_ss_j(j, i_o), ";", omega_ss(j, i_o), ";", i_o, ";", j, ';', iter
!    enddo  
!enddo

    
    if (switch_vf > 0) then
        if (mod(iter,1) == 0 .and. iter >= 75) then
            print*, iter, 'err_ss:', err_ss, 'feas_ss:', ((y_ss - consumption_ss_gross - g_ss)/y_ss - ((nu_ss*gam_ss+depr-1)*k_ss)/y_ss)
            !write(64, '(F20.10)')k_ss   
        endif
        if (err_ss < err_ss_tol) then
            if (iter < 75) print*, iter, 'err_ss:', err_ss, 'feas_ss:', ((y_ss - consumption_ss_gross - g_ss)/y_ss - ((nu_ss*gam_ss+depr-1)*k_ss)/y_ss)
            !close(64)
            exit
        endif
    endif 
enddo 
print *, "subsidy:", subsidy_ss_pom
print *, "savings_agr/bigL:", savings_ss
print *, "debt:", debt_ss
print *, zbar*alpha_ss*k_ss**(alpha_ss - 1)
print *, "debt/GDP", debt_ss/y_ss
!close(114)
! End of main loop
!print *, "1 ", savings_ss_j(:, 1)
!print *, " "
!print *,  "4 ", savings_ss_j(:, 4)
!pause
!close (121)
!print *, consumption_ss_gross, avg_ef_l_suply !c_ss_j(1:2, :)
    !do j = 1,bigJ,1
    !    if (j == 1) then ! TODO przeważyc emerytury rozkladem
    !        income_ss_j(j, :) = (1 - tl_ss)*w_ss_j(j, :)*l_ss_j(j, :) + sum_b_ss*b_ss_j(j, :) - upsilon_ss + bequest_ss_j(j) !, :)
    !    else
    !        income_ss_j(j, :) = r_ss*s_ss_j(j-1, :)/gam_ss + (1 - tl_ss)*w_ss_j(j, :)*l_ss_j(j, :) + sum_b_ss*b_ss_j(j, :)  - upsilon_ss + bequest_ss_j(j) !, :)
    !    endif
    !enddo
    !
    !income_ss = sum(N_ss_j*sum(income_ss_j(1:bigJ, :), dim=2))/bigL_ss ! should be weighted by omega_wieght ?
    savings_ss_rate_j = sum(s_ss_j/income_ss_j, dim=2) ! or: savings_ss_rate_j = sum(s_ss_j, dim=2)/sum(income_ss_j, dim=2)
    
    if (switch_run_1 == 1) then    
        l_ss_pen_j_1(:, :) = l_ss_pen_j
        sum_b_trans(:, :, 1) = sum_b_ss_omega
        b2_t(:, :, :, 1) = b2_ss
        b1_t(:, :, :, 1) = b1_ss
        tc(1) = tc_ss
        subsidy(1) = subsidy_ss
        contribution(1) = contribution_ss
        sum_b(1) = sum_b_ss
    else
        l_ss_pen_j_2(:, :) = l_ss_pen_j
        b2_t(:, :, :, bigT) = b2_ss
        b1_t(:, :, :, bigT) = b1_ss
        do i = 2, bigT
            sum_b_trans(:, :, i) = sum_b_ss_omega
        enddo
    endif

    tc_new = tc_ss
    tl_new = tl_ss
    
if (switch_run_1 == 1) then    
    s_pom_ss_j_1 = s_pom_ss_j
    !tau1_ss_1 = sum(tau1_ss, dim=2)
    !tau2_ss_1 = sum(tau2_ss, dim=2)
    b_ss_j_1 = b_ss_j 
    w_pom_ss_j_1 = w_ss_j
    bequest_left_ss_j_1 = bequest_left_ss_j
    labor_tax_ss_j_1 = labor_tax_ss_j
    b1_ss_j_1 = b1_ss_j
    if (switch_residual_read == 10) then
        g_share_ss_2 = g_ss/y_ss
        g_share = g_ss/y_ss
        !g_ss2 = g_ss        ! temp
        !g_tr = g_ss         ! temp
    endif
    !labor_tax_j_vfi_ss_1 = labor_tax_ss_j_vfi ! ?
else
    s_pom_ss_j_2 = s_pom_ss_j
    !tau1_ss_2 = sum(tau1_ss, dim=2)
    !tau2_ss_2 = sum(tau2_ss, dim=2)
    b_ss_j_2 = b_ss_j ! b_pom_ss_j ; why pom?  
    w_pom_ss_j_2 = w_ss_j
    bequest_left_ss_j_2 = bequest_left_ss_j 
    labor_tax_ss_j_2 = labor_tax_ss_j
    b1_ss_j_2 = b1_ss_j
    !labor_tax_j_vfi_ss_2 = labor_tax_ss_j_vfi ! ?
    
    tc_new = tc_ss
    tk_new = tk_ss
    tl_new = tl_ss
endif

    include 'utility_ss.f90' 

    if (switch_print == 1) then
        include 'Print_steady_db.f90'
    endif
    !if (switch_run_1 /= 1) pause
    k_ss_o = k_ss
     !zbar = 2**(1-alpha_ss)
end subroutine steady

END MODULE steady_state