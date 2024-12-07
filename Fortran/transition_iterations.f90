! WHAT   : iteration for transition path for PAYG 
! TAKE   : unchanged in routine: productivity (omega), the size of each cohort [[N_t_j]], contribution to the pension system rate [[t1, t1_a, t2]], replacement rate [[rho1, [[rho2]]
!          unchanged in routine: percentage of change in technological progress used to indexation of pension benefit (valor_share)
!          changed   in routine: change in technological progress [[gam = z_t/z_(t-1)]], tax rate (labor, capital, consumption), multiplier for indexation in 1st pillar of pension system [[valor_mult]],
!          changed   in routine: wage (w), interest rate (r), government subsidy to pension system (subsidy_j),  capital (k),  
! DO     : start iteration, variable_new = f(variable_old) etc. since sum|variable_new(i) - variable_old(i)|> err_tol
! RETURN : generated transition path 


do iter = 1,n_iter_t,1
    
include 'denominator_trans.f90'
      if (switch_print == 1) then
        if ((switch_vf == 0)) then 
            if  (MOD(iter,50) == 0) then            
                include 'print_iter.f90'            
            endif
        else 
            include 'print_iter.f90'
        endif
      endif
      

    consumption_gross_pom = 0
    savings_pom = 0
    savings_j = 0
  
    !pillarI_old_j = pillarI_j
    !pillarII_old_j = pillarII_j   
    bequest_j_old  = bequest_j 
    b_j_old    = b_j
    if (iter == 1) then
        bigL = 0
        do i_o = 1, omega_dim
            do i = 1, bigT
                bigL_j(:, i) = N_t_j(:, i)*l_j(:, i_o, i)*(omega_share(:, i_o, i)/sum(omega_share(:, :, i), dim = 2)) ! It doesn't contain omega 'cause it's already in l_j   !bigL_j = omega(:, i_o, :)*N_t_j*l_j
            enddo
            !bigl_j_aux = N_t_j*l_j     
            bigL_pom(i_o, :) = sum(bigL_j, dim=1)
            bigL = bigL + bigL_pom(i_o, :)
        enddo
   
        !bigl_aux = sum(bigl_j_aux, dim=1)
        nu(1) = 1
        do i = 2,bigT,1
            nu(i) = bigL(i)/bigL(i-1)
        enddo
    endif
    sv_old_j = sv_j
    sv_old_pom_j = sv_pom_j
    tau1_s_t_old = tau1_s_t
    tau2_s_t_old = tau2_s_t
do i = 1, bigT
zz(i) = 1d0
if (i < 9) zz(i) = 1d0
enddo
    if (bigJ /= 80) then 
        r_bar = zz*zbar*alpha*k**(alpha - 1) - depr
        w_bar = zz*zbar*(1 - alpha)*k**alpha
        y = zz*zbar*k**(alpha)
    else
        r_bar = alpha*k**(alpha - 1) - depr
        w_bar = (1 - alpha)*k**alpha
        y = k**(alpha)
    endif
    do i = 1,bigT,1
        if (r_bar(i) < 0) then
            r_bar(i) = 0.0_dp
        endif
    enddo
    
    do i = 2, bigT
        avg_hours(i) = bigL(i)/sum(Nn_(1:jbar_t(i)-1, i))
    enddo
    avg_w = w_bar*avg_hours
    
    do i_o = 1, omega_dim
        do j = 1,bigJ,1
            w_j(j, i_o, :) = (1-tl(:))*(1 - t1(j,:)-t2(j,:))*omega(j, i_o, :)*w_bar(:)
        enddo
    enddo
    r = 1 + (1 - tk)*r_bar
    r_f = 1 + r_bar
    
    valor_mult(1) = (1 + valor_share*(gam_t(1)*nu(1)  - 1))/gam_t(1)
    valor_mult(2) = (1 + valor_share*(gam_t(1)*nu(1)  - 1))/gam_t(2)
    do i = 3,bigT,1
        valor_mult(i) = (1 + valor_share*(gam_t(i-1)*(w_bar(i-1)*bigL(i-1))/(w_bar(i-2)*bigL(i-2))-1))/gam_t(i)
    enddo 
    
    do i = 2, bigT
        do i_o = 1, omega_dim
            do ip = 1, n_sp
                aime_t(ip, i_o, i) = omega(jbar_t(i)-1, i_o, i)*n_sp_value(ip, i_o)*avg_w(i)    ! include taxes?
            enddo
        enddo
    
        !b1_t(:, :, :, i) = rho1*w_bar(i)*valor_mult(i)
        b1_t(jbar_t(i), :, :, i) = rho(i)*rho1*avg_w(i)
        b2_t(jbar_t(i), :, :, i) = rho(i)*rho2*aime_t(:, :, i)
        do j = jbar_t(i) + 1, bigJ
            b2_t(j, :, :, i) = b2_t(j-1, :,:,i-1)*valor_mult(i)
            b1_t(j, :, :, i) = b1_t(j-1, :,:,i-1)*valor_mult(i)
        enddo
        b1_t(:jbar_t(i)-1, :, :, i) = 0d0
        b2_t(:jbar_t(i)-1, :, :, i) = 0d0
    enddo

   if ((switch_residual .NE. 6) .OR. (switch_residual .NE. 7)) then
    ! we are not using debt adjustment to smooth tax adjustment 
    debt = debt_constr*y
    sum_priv_sv(1) = k(1)*gam_t(1)*nu(1) + debt(1) !- PillarII(1)
 	do i = 2,bigT,1 
 	    if (i > n_p+2) then 
            sum_priv_sv(i) = k(i)*gam_t(i)*nu(i) + debt(i) !- PillarII(i)
        else 
 	        sum_priv_sv(i) = k(i+1)*gam_t(i+1)*nu(i+1) + debt(i) !- PillarII(i)
        endif 
    enddo 
   endif
  
if (switch_residual /= 9) then
    if (swich_g_const == 1) then 
        g = g_per_capita*N_t/bigL
    elseif (swich_g_const == 0) then 
        if (switch_ref_run_now == 0 ) then ! we are renning bseline scanario where g is kept as a share of gdp
            g = g_share * y 
            g_trans = g
        else
            g = g_trans
        endif
    elseif (swich_g_const == 2) then  
        g = g_share * y 
    elseif (swich_g_const == 3) then 
        if (switch_ref_run_now == 0 ) then ! we are renning bseline scanario where g is kept as a share of gdp
            g = g_share * y 
            g_trans = g*bigL
        else
            g = g_trans/bigL
        endif
    endif
endif
   !g = g_tr

subsidy_pom = 0d0
contribution_pom = 0d0
labor_tax_j_agr = 0d0
sum_b_pom = 0d0
include 'pension_system.f90'
do i_o = 1, omega_dim   
    do i = 2, bigT
        labor_tax_j_agr(:, i) = labor_tax_j_agr(:, i) + labor_tax_j(:, i_o, i)*(omega_share(:, i_o, i)/sum(omega_share(:, :, i), dim = 2))
    enddo
enddo
sum_b = sum_b_pom/bigL
subsidy = subsidy_pom/bigL !
contribution = contribution_pom/bigL
include 'closures.f90'
!include 'implicit_tax_trans.f90'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Start here with omega loop
do i_o = 1, omega_dim

    if (switch_vf == 0) then
            !include 'lti_trans.f90'
            !l_pen_j = l_j
            !do i = 1, bigT, 1
            !    labor_tax_j_vfi(:,i) = tl(i)*(1 - t1(:,i)-t2(:,i))*omega(:,i)*w_bar(i)*l_j(:,i)
            !enddo
    elseif (switch_vf > 0) then

        do i = 1, bigT,1
            w_pom_trans_vfi(:,i) = (1 - t1(:,i) - t2(:,i))*omega(:, i_o, i)*w_bar(i) !w_j(:,1:bigT)
            !w_pom_trans_implicit_vfi(:,i) = (t1_contrib(:,i)*tau1_s_t(:,i) + t2(:,i)*tau2_s_t(:,i))*omega(:,i)*w_bar(i)
        enddo
        w_pom_trans_implicit_vfi = 0d0
        
        r_vfi = (1 - tk)*r_bar 
            
        n_sp_initial_vfi = n_sp_initial(i_o)
        pi_ip_vfi = pi_ip(:, :, i_o, :)
        n_sp_value_vfi = n_sp_value(:, i_o)
        delta_mult_vfi = delta_mult(i_o, :)
        b1_t_vfi = b1_t(:,:,i_o, :)
        b2_t_vfi = b2_t(:,:,i_o, :)
        
        c_trans_vfi = c_trans(:, :, :, :, i_o, :)
        prob_trans_vfi = prob_trans(:, :, :, :, i_o, :)
        svplus_trans_vfi = svplus_trans(:, :, :, :, i_o, :)
        aime_plus_trans_vfi = aime_plus_trans(:, :, :, :, i_o, :)
        V_trans_vfi = V_trans(:, :, :, :, i_o, :)
        !l_cap_trans_vfi = l_cap_trans(:, :, : ,: , i_o, :)
        
        tc_vfi = tc + 1.0_dp
        gam_vfi = gam_t
        N_t_j_vfi = N_t_j
        b_j_vfi= b_j(:, i_o, :) !b_pom_j
        bequest_j_vfi =  bequest_j
        bequest_j_vfi_dif = bequest_j - bequest_j_old
        jbar_t_vfi = switch_fix_retirement_age
        upsilon_vfi = upsilon
        upsilon_dif = upsilon - upsilon_old
        b_pom_j_dif = b_j - b_j_old
        labor_tax_j_vfi = labor_tax_j(:, i_o, :)
        !l_pen_j_vfi = l_pen_j(:, i_o, :)
        iter_com = iter
        call agent_vf_trans()
        !write (*,*)"vf ", iter
        do i = 2, bigT
            c_j(:, i_o, i) = c_j_vfi(:,i)
            sum_b_trans(:, i_o, i) = sum_b_trans_vfi(:, i)
            sv_pom_j(1:bigJ-1, i_o, i) = up_t*sv_old_pom_j(1:bigJ-1, i_o, i) + (1-up_t)*s_pom_j_vfi(1:bigJ-1, i)
            !write(*,*)sum(sv_old_pom_j(1:bigJ-1, i_o, i) - s_pom_j_vfi(1:bigJ-1, i))
        enddo
        !write(*,*)"er_sv: ", sum(sv_old_pom_j(1:bigJ-1, i_o, :) - s_pom_j_vfi(1:bigJ-1, :))
        l_new_j(:, i_o, :) = l_j_vfi*omega(:, i_o, :)
        l_pen_j(:, i_o, :) = l_pen_j_vfi
        labor_tax_j(:, i_o, :) = labor_tax_j_vfi
        V_j(:, i_o, :) = V_j_vfi
        !sum_b_weight_trans_om(i_o, 2:) = sum_b_weight_trans(2:)
        !sum_b_trans(:, i_o, 2:) = sum_b_trans_vfi(:, 2:)
        sv_j = sv_pom_j
        
        c_trans(:, :, :, :, i_o, :) = c_trans_vfi
        prob_trans(:, :, :, :, i_o, :) = prob_trans_vfi
        svplus_trans(:, :, :, :, i_o, :) = svplus_trans_vfi
        aime_plus_trans(:, :, :, :, i_o, :) = aime_plus_trans_vfi
        V_trans(:, :, :, :, i_o, :) = V_trans_vfi
        !l_cap_trans(:, :, : ,: , i_o, :) = l_cap_trans_vfi
        !endif
    endif      
enddo

avg_ef_l_suply_trans(2:) = 0d0
do i = 2, bigT
    do i_o = 1, omega_dim
        avg_ef_l_suply_trans(i) = avg_ef_l_suply_trans(i) + sum(N_t_j(:,i)*l_new_j(:, i_o ,i)*(omega_share(:, i_o, i)/sum(omega_share(:, :, i), dim = 2)))/sum(N_t_j(1:switch_fix_retirement_age-1,i))
    enddo
enddo

bigL = 0d0
do i_o = 1, omega_dim
    do i = 1, bigT
        bigL_j(:, i) = N_t_j(:, i)*l_j(:, i_o, i)*(omega_share(:, i_o, i)/sum(omega_share(:, :, i), dim = 2)) ! It doesn't contain omega 'cause it's already in l_j   !bigL_j = omega(:, i_o, :)*N_t_j*l_j
    enddo
    bigL_pom(i_o, :) = sum(bigL_j, dim=1)
    bigL = bigL + bigL_pom(i_o, :)
enddo

nu(1) = 1
do i = 2,bigT,1
    nu(i) = bigL(i)/bigL(i-1)
enddo !bylo 2 raz to samo

!do i = 1, bigT
!    !if (i <= 10) print *, sum(l_cap_trans(:switch_fix_retirement_age-1, :, :, :, :, i)*prob_trans(:switch_fix_retirement_age-1, :, :, :, :, i)), what_type(sum(l_cap_trans(:switch_fix_retirement_age-1, :, :, :, :, i)))
!    unemployment_rate(i) = sum(l_cap_trans(:switch_fix_retirement_age-1, :, :, :, :, i)*prob_trans(:switch_fix_retirement_age-1, :, :, :, :, i))/sum(prob_trans(:switch_fix_retirement_age-1, :, :, :, :, i)) ! N & share !/count(ones(:switch_fix_retirement_age-1, :, :, :, :, i))
!    !if (i <= 10) print *, unemployment_rate(i), what_type(unemployment_rate(i))
!enddo
temp_prob = 0d0
!temp_prob2 = 0d0
do i = 1, bigT
    do i_o = 1, omega_dim
        do ia = n_a, 0 , -1
            do i_aime = n_aime, 0 , -1
                do j = 2, switch_fix_retirement_age-1 
                    do ip = 1, n_sp
                        temp_prob(i) = temp_prob(i) + prob_trans(j, ia, i_aime, ip, i_o, i)*omega_share(j, i_o, i)*N_t_j(j, i) 
                        !temp_prob2(i_o, i) = temp_prob2(i_o, i) + prob_trans(j, ia, i_aime, ip, i_o, i)
                    enddo
                enddo
            enddo
        enddo  
    enddo       
enddo
do i=1, bigT
    N_t_working(i) = sum(N_t_j(:switch_fix_retirement_age-1, i)) 
enddo
unemployment_rate = 0d0
!unemployment_rate2 = 0d0
!unemployment_rate_om = 0d0
do i = 1, bigT
    do i_o = 1, omega_dim
        do ia = n_a, 0 , -1
            do i_aime = n_aime, 0 , -1
                do j = 2, switch_fix_retirement_age-1
                    unemployment_rate(i) = unemployment_rate(i) + N_t_j(j, i)*omega_share(j, i_o, i)*prob_trans(j, ia, i_aime, n_sp, i_o, i)/temp_prob(i) !sum(prob_trans(:switch_fix_retirement_age-1, :, :, :, :, i)) ! *(N_t_j(j, i)/N_t(i))   *omega_share(:switch_fix_retirement_age-1, :, i)
                    !unemployment_rate2(i) = unemployment_rate2(i) + omega_share(j, i_o, i)*prob_trans(j, ia, i_aime, n_sp, i_o, i)/temp_prob2(i_o, i) !sum(prob_trans(:switch_fix_retirement_age-1, :, :, :, :, i)) ! *(N_t_j(j, i)/N_t(i))   *omega_share(:switch_fix_retirement_age-1, :, i)

                    !unemployment_rate_om(i_o, i) = unemployment_rate_om(i_o, i) + prob_trans(j, ia, i_aime, n_sp, i_o, i)/sum(prob_trans(:switch_fix_retirement_age-1, :, :, :, i_o, i)) *omega_share(j, i_o, i)
                    !temp_prob= sum(prob_trans(:switch_fix_retirement_age-1, :, :, :, i_o, i))
                enddo
            enddo
        enddo  
    enddo       
    !if (i> 10 .and. i < 20) print*, unemployment_rate_om(:, i)
enddo
!print *, N_t_working
open(unit = 104, file= "u_N1.csv")
    write(104, '(A)') "un; i"
    do i = 2, bigT
        write(104, '(F20.10,A,I3)') unemployment_rate(i)*N_t_working(i), ";", i  
    enddo
close(104)
OPEN (unit=33, FILE = version//closure//"_unemployment_rate.txt")
    do i = 1,n_p+3,1 
        write(33, '(F20.10)') unemployment_rate(i)
    enddo
close(33)

include 'aggr_trans.f90'
    
include 'bequest.f90'

    k_new(1) = k(1)
    k_new(n_p+1) = (savings(n_p+1) - debt(n_p+1))/(nu(n_p+1)*gam_t(n_p+1))
    do i = 2,n_p+1,1
        k_new(i) = (savings(i-1) - debt(i-1))/(nu(i)*gam_t(i))
        err(i) = abs(k_new(i) - k(i))
        feas(i) = ((y(i) - consumption_gross_new(i) - g(i))/y(i) - (gam_t(i+1)*nu(i+1)*k(i+1) - (1 - depr)*k(i))/y(i))
        !    feasibility(i) = ((y(i) - consumption_gross(i) - g(i))/y(i) - (gam_t(i+1)*nu(i+1)*k(i+1) + (depr-1)*k(i))/y(i))
        k(i) = up_t*k(i) + (1 - up_t)*k_new(i)
        l_j(:, :, i) = up_t*l_j(:, :, i) + (1 - up_t)*l_new_j(:, :, i)
    enddo    
    bigK = k*bigL    
    
    !up_t = 0.999d0*up_t
    cum_err(iter) = sum(err)
    cum_feas(iter) = sum(feas)

    if (iter < n_iter_t+1) then          
        print*, 'iteration: ', iter 
        print*, 'subsidy(18)/y(18)', subsidy(18)/y(18), '   should be 0%'
        !print*, 'subsidy(10)/y(10)', subsidy(10)/y(10)
        !print*, 'subsidy(11)/y(11)', subsidy(11)/y(11)
        print*, 'subsidy(30)/y(30)', subsidy(30)/y(30), '   should be 2%'
        !print*, 'sum_b(10)/y(10)', sum_b(10)/y(10)
        print*, 'sum_b(11)/y(11)', sum_b(11)/y(11), '   should be 7.5%'
        !print*, 'subsidy(29)/y(29)', subsidy(29)/y(29)
        print*, char(9), 'err:', maxval(abs(err),1), 'at period', maxloc(abs(err),1) !cum_err(iter)
        print*, char(9), 'feas:', maxval(abs(feas),1), 'at period', maxloc(abs(feas),1)
        if ((maxval(abs(err),1) < err_tol .and. iter >= 20) .or. iter == n_iter_t .or. (com == 3 .or. com == 5)) then  ! .or. com == 0  !  .or. com == 6 
            write (*,*) 'We`re leaving the iter loop.' 
            print *, err(:n_p+2)
            print *, 'feas'
            print *, feas(:n_p+2)
            !print *, 'k'
            !print *, k(:n_p+2)
            !pause
            exit ! iterations end
        endif
    endif
     
    ! !!! Go back to it
    !replacement(1) = b_j(jbar_t(1),1)/(w_j(jbar_t(1)-1,1)*l_pen_j(jbar_t(1)-1, 1))    
    !do i = 2,bigT,1
    !    if (jbar_t(i-1) < jbar_t(i)) then
    !        replacement(i) = b_j(jbar_t(i-1),i-1)/(w_j(jbar_t(i-1)-1,i-1)*l_pen_j(jbar_t(i-1)-1, i-1)) 
    !    else
    !        replacement(i) = b_j(jbar_t(i),i)/(w_j(jbar_t(i)-1,i)*l_pen_j(jbar_t(i)-1, i)) 
    !    endif    
    !enddo
enddo 