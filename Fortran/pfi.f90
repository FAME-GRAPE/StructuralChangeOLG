!|==============================================================================================================================================|
!| module solove consumer problem (given rbar, wbar, taxes and other parameters) using policy iterations function, Right Hand Side (RHS) method |                         
!|functions: euler_trans, euler_transsec, euler_ss, eulersec_ss - finding optimal futur assets for given grid point                             |
!|function margu - marginal utility for consumption                                                                                             |
!|function year - find year in which consumer will have ijj years, having in year it, ij years                                                  |    
!|subroutines: linear_int - linear evaluation assets on the asset grid point                                                  |
!|subroutines: household_ss, household_trans - find futur assets for every age, assets grid point                                               |
!|subroutines: interpolate_ss, interpolate_trans - interpolate RHS                                                                              |
!|subroutines: get_distribution_ss, get_distribution_trans - get distribution of consumers on age, gridpoints                                   |    
!|subroutines: aggregation_ss, aggregation_trans - agreggate consumption, labour, savings                                                       |
!|subroutine output - print to CSV                                                                                                              |
!|==============================================================================================================================================|
module pfi_trans
    ! modules
    use assertions
    use errwarn
    use gaussian_int
    use linint
    use matrixtools
    use minimization
    use normalProb
    use polynomial
    use simplex
    !use clock
    use rootfinding
    use splines
    use AR_discrete
    use global_vars
    
    
implicit none

!definition of variables
integer :: ia, i_aime, ip, ip_p !,ir, id, ir_r, id_d
real*8 :: sv_grid(0:n_a), sv(0:n_a)
real*8 :: aime(0:n_aime), aime_replacement_rate(0:n_aime) ! to comment out
real*8, dimension(bigJ, n_sp) :: b1_ss_vfi, b2_ss_vfi
real*8, dimension(bigJ, n_sp, bigT) :: b1_t_vfi, b2_t_vfi
real*8 :: omega_ss_vfi(bigJ), w_bar_ss_vfi
integer ::  n_a_1, n_a_2, iter_com, iaimel, iaimer

!transition variables


real*8, dimension(:,:,:,:,:), allocatable ::  c_trans_vfi, labor_tax_trans, l_trans_vfi, &
                                              RHS_trans, ERHS_trans, sv_tempo_trans, EV_trans, &
                                              prob_trans_vfi, svplus_trans_vfi, aime_plus_trans_vfi, V_trans_vfi

real*8, dimension(:,:,:,:,:,:), allocatable ::  prob_trans, svplus_trans, aime_plus_trans, V_trans, c_trans, l_trans

!integer, dimension(:,:,:,:,:), allocatable :: l_cap_trans_vfi
!integer, dimension(:,:,:,:,:,:), allocatable :: l_cap_trans

real*8, dimension(bigJ, n_sp, bigT) :: prob_trans_vfi_u
real*8, dimension(bigJ, n_sp, omega_dim, bigT) :: prob_trans_u

real*8, dimension(bigJ, bigT) :: bequest_j_vfi, bequest_j_vfi_dif, check_e, w_pom_trans_vfi, w_pom_trans_implicit_vfi, &
                                 check_euler_trans, V_j_vfi_const_lambda, V_j_vfi_higher_lambda, V_j_vfi,&
                                 pi_trans_vfi_cond,  b_j_vfi,  N_t_j_vfi, &
                                 c_j_vfi,  s_pom_j_vfi, l_j_vfi, labor_tax_j_vfi, lw_j_vfi, lw_lambda_j_vfi, l_pen_j_vfi

real*8, dimension(bigT) ::   r_vfi, tc_vfi, upsilon_vfi, upsilon_dif
real*8, dimension(bigT) ::   gam_vfi
integer :: jbar_t_vfi(bigT)

real(dp), dimension(bigJ, omega_dim, bigT) :: l_pen_j, b_pom_j_dif, V_j

!steady state variables
real*8, dimension(bigJ, 0:n_a, 0:n_aime, n_sp) :: V_ss, EV_ss, RHS_ss,  svplus_ss, l_ss, c_ss, sv_tempo, labor_tax, prob_ss, &
                                                             aime_plus_ss, aime_tempo

real(dp), dimension(bigJ, 0:n_a, 0:n_aime, n_sp, omega_dim) :: aime_plus_ss_om, c_ss_g, s_ss_g

real(dp), dimension(bigJ, 0:n_a, 0:n_aime, n_sp, omega_dim, bigT) :: aime_plus_trans_om

real*8, dimension(bigJ, 0:n_a, 0:n_aime, n_sp, omega_dim) :: gini_weight_consumption

real*8, dimension(bigJ) :: V_ss_j_vfi, c_ss_j_vfi, s_pom_ss_j_vfi, l_ss_j_vfi, lab_ss_j_vfi, l_ss_pen_j_vfi, &
                           bequest_ss_j_vfi, bequest_ss_j_vfi_dif, pi_ss_vfi, pi_ss_vfi_cond, &
                           labor_tax_ss_j_vfi, lw_ss_j_vfi, lw_lambda_ss_j_vfi, w_pom_ss_vfi, w_pom_ss_implicit_vfi, lab_high

real*8, dimension(bigJ) :: b_ss_j_vfi, sum_b_ss_vfi, sum_b2_ss_vfi   ! n_sp, 
real*8, dimension(bigJ, bigT) :: sum_b_trans_vfi

real*8 ::   r_ss_vfi, tc_ss_vfi, gam_ss_vfi, upsilon_ss_vf, upsilon_old_ss, upsilon_dif_ss, available_temp, l_temp,c_temp, sv_temp
real*8 ::   gini_weight_sv(bigJ, 0:n_a, omega_dim)
integer ::  jbar_ss_vf

real(dp) :: n_sp_initial_vfi, pi_ip_ss_vfi(n_sp, n_sp), pi_ip_vfi(n_sp, n_sp, bigT), n_sp_value_vfi(n_sp)
real(dp) :: delta_mult_vfi(bigT), delta_mult_ss_vfi, w_bar_ss

real(dp), dimension(bigj, omega_dim) :: l_ss_pen_j, omega_share_ss_vfi

!comunication variables
integer :: j_com, ia_com, ip_com, i_com, ir_com
real*8 :: c_com, l_com, pi_com, c_ss_com, l_ss_com

! top_ten
real*8, dimension(bigJ) :: N_ss_j_vfi, asset_pom_ss_j, top_ten_coh, cons_proc_top_ten_coh 
real*8, dimension(bigJ, 0:n_a, 0:n_aime, n_sp, omega_dim, bigT) :: gini_income_trans, gini_income_trans2
real*8 :: savings_top_ten(10), top_ten(10), savings_cohort_ten(3,bigJ), &
          consumption_top_ten(3, bigJ), top_100(100),  savings_top_100(100), &
          top_ten_trans(10,bigT), savings_top_ten_trans(10, bigT), wspl(bigT), asset_trans(bigJ,bigT) , &
          gini_weight_trans(bigJ,0:n_a, omega_dim, bigT), gini_weight_cons_trans(bigJ, 0:n_a, 0:n_aime, n_sp, omega_dim, bigT)
          
integer :: t

contains 


!*******************************************************************************************
! marginal utility of consumption
    function margu(cons, labor, tc)
        real*8, intent(in) :: cons, labor, tc !cosumption , labor and consumption tax
        real*8 :: margu, leis
        leis=1d0-labor
        margu = 1/tc*phi*(cons**phi*leis**(1d0-phi))**(1d0-theta)/cons
    end function 

!*******************************************************************************************
! value function - steady state!
 
    function valuefunc(sv_plus, aime_plus, cons, lab, j, ip)
        implicit none

        integer, intent(in) :: j, ip!, ir, id
        real*8, intent(in) :: sv_plus, aime_plus, cons, lab
        real*8 :: valuefunc, dist, dist_aime, c_help, l_help, pr
        integer :: ial, iar, aimel, aimer

        ! check whether consumption or leisure are too small
        c_help = max(cons, 1d-10)  
        l_help = min(max(lab,1d-10), 1d0-1d-10)
        ! get tomorrows utility
        call linear_int(sv_plus, ial, iar, dist, sv, n_a, a_grow)
        call linear_int(aime_plus, aimel, aimer, dist_aime, aime, n_aime, aime_grow)
        
        ! calculate tomorrow's part of the value function
        valuefunc = 0d0
        if(j < bigJ)then
            if (theta == 1) then
                !valuefunc = log(max(dist*EV_ss(j+1, ial, ip,ir, id) + &
                              !(1d0-dist)*EV_ss(j+1, iar, ip,ir, id), 1d-10))
                valuefunc =   dist*dist_aime*EV_ss(j+1, ial, aimel, ip) + &
                              dist*(1d0-dist_aime)*EV_ss(j+1, ial, aimer, ip) + &
                              (1d0-dist)*dist_aime*EV_ss(j+1, iar, aimel, ip) + &
                              (1d0-dist)*(1d0-dist_aime)*EV_ss(j+1, iar, aimer, ip)
            else
                valuefunc =     max(dist*dist_aime*EV_ss(j+1, ial, aimel, ip) + &
                              dist*(1d0-dist_aime)*EV_ss(j+1, ial, aimer, ip) + &
                              (1d0-dist)*dist_aime*EV_ss(j+1, iar, aimel, ip) + &
                        (1d0-dist)*(1d0-dist_aime)*EV_ss(j+1, iar, aimer, ip), 1d-10)**(1d0-theta)/(1d0-theta)
            endif
        endif    
        ! add todays part and discount
        if (theta == 1) then
            valuefunc = (phi*log(c_help) + (1d0-phi)*log(1d0-l_help)) +  beta*delta*delta_mult_ss_vfi*pi_com*valuefunc 
        else
            valuefunc = (c_help**phi*(1d0-l_help)**(1d0-phi))**(1d0-theta)/(1d0-theta) + beta*delta*delta_mult_ss_vfi*pi_com*valuefunc 
        endif
    end function

!*******************************************************************************************
! value function - trans !
    function valuefunc_trans(sv_plus, aime_plus, cons, lab, delta_mult_f, j, ip, it)
        implicit none

        integer, intent(in) :: j,  ip, it!, ir, id
        real*8, intent(in) :: sv_plus, aime_plus, cons, lab, delta_mult_f
        real*8 :: valuefunc_trans, dist, dist_aime, c_help, l_help, pr
        integer :: itp, ial, iar, aimel, aimer

        ! check whether consumption or leisure are too small
        c_help = max(cons, 1d-10)  
        l_help = min(max(lab,1d-10), 1-1d-10)
        ! get tomorrows year
        itp = year(it, j, j+1)
        ! get tomorrows utility
        call linear_int(sv_plus, ial, iar, dist, sv, n_a, a_grow)
        call linear_int(aime_plus, aimel, aimer, dist_aime, aime, n_aime, aime_grow)
        ! calculate tomorrow's part of the value function
        valuefunc_trans = 0d0
        if(j < bigJ)then            
            if (theta == 1) then
                valuefunc_trans = dist*dist_aime*EV_trans(j+1, ial, aimel, ip, itp) + &
                                (1d0-dist)*dist_aime*EV_trans(j+1, iar, aimel, ip, itp) + &
                                dist*(1d0-dist_aime)*EV_trans(j+1, ial, aimer, ip, itp) + &
                                (1d0-dist)*(1d0-dist_aime)*EV_trans(j+1, iar, aimer, ip, itp)
            else
                valuefunc_trans = max(dist*dist_aime*EV_trans(j+1, ial, aimel, ip, itp) + &
                                (1d0-dist)*dist_aime*EV_trans(j+1, iar, aimel, ip, itp) + &
                                dist*(1d0-dist_aime)*EV_trans(j+1, ial, aimer, ip, itp) + &
                          (1d0-dist)*(1d0-dist_aime)*EV_trans(j+1, iar, aimer, ip, itp), 1d-10)**(1d0-theta)/(1d0-theta)
            endif
        endif    
        ! add todays part and discount
        if (theta == 1) then
            valuefunc_trans = (phi*log(c_help) +(1d0-phi)*log(1d0 - l_help)) + beta*delta*delta_mult_f*pi_com*valuefunc_trans 
        else
            valuefunc_trans = (c_help**phi*(1d0-l_help)**(1d0-phi))**(1d0-theta)/(1d0-theta) + beta*delta*delta_mult_f*pi_com*valuefunc_trans 
        endif
    end function       
    
 !*******************************************************************************************   
    ! find year in which consumer will have ijj years, having in year it ij years
     function year(it, ij, ijj)

        implicit none

        integer, intent(in) :: it, ij, ijj
        integer :: year

        year = it + ijj - ij

        if(it == 1 .or. year <= 1)year = 1
        if(it == bigT .or. year >= bigT)year = bigT
    end function

 !*******************************************************************************************   
    ! find optimal labor for given consumption, wage, preference for leisure and labor tax parameters 
     function optimal_labor(c, w, w_non_tax,  phi, tau, lambda, n_sp_val)
     
        real *8 :: c, w, w_non_tax, phi, tau, lambda, n_sp_val
        real *8 :: l, f_iter, f_prim_iter
        real *8 :: optimal_labor
        
        integer :: iter
   
        !if (lambda == 0) then 
        !    l =    1d0-c*(1d0-phi)/phi*1d0/((1-tau)*w)
        !else
            ! Newthon Rapson method
            l = 0.0001
            do iter = 1, 6, 1
                f_iter = 1 - l - c*(1-phi)/phi*1/((1-tau)*(1-lambda)*w**(1-lambda)+w_non_tax)*l**lambda
                f_prim_iter = -1 - lambda*c*(1-phi)/phi*1/((1-tau)*(1-lambda)*(w*l)**(1-lambda)+w_non_tax*l**(1-lambda))
                l = l-f_iter/f_prim_iter
            enddo           
        !endif
        
        optimal_labor = min(1d0,max(l,0d0))
        if (n_sp_val == 0d0) optimal_labor = 0d0
    end function
!*******************************************************************************************

    subroutine agent_vf()


        implicit none
        

        integer :: iter
                     !print *, sum(omega_ss)
   
            call initialize_trans()
            ! solve the household problem
            call household_endo()
    
            ! calculate the distribution of households over state space
            call get_distribution_ss()
    
            ! aggregate individual decisions
            call aggregation_ss()
          
            if (switch_run_1 == 1) then 
            !    V_j_vfi(:,1) = 0d0 ! to d0
            !    
                do i = 1, 2,1            
                    c_trans(:, :, :, :, i_o, i) = c_ss !_g
                    l_trans(:, :, :, :, i_o, i) = l_ss  
                    !labor_tax_trans(:, :, :, :, i) = labor_tax
                    prob_trans(:, :, :, :, i_o, i) = prob_ss
                    svplus_trans(:, :, :, :, i_o, i) = svplus_ss
                    aime_plus_trans(:, :, :, :, i_o, i) = aime_plus_ss
                    V_trans(:, :, :,:, i_o, i) = V_ss
                    !b1_t(:, :, :, i) = b1_ss
                    !b2_t(:, :, :, i) = b2_ss
                    !c_j_vfi(:,i) = c_ss_j_vfi   
                    !l_j_vfi(:,i) = l_ss_j_vfi 
                    !l_pen_j(:, :, i) = l_ss_pen_j
                    !!labor_tax_j(:, :, i)  = labor_tax_ss_j
                    !lw_j_vfi(:,i)  = lw_ss_j_vfi
                    !lw_lambda_j_vfi(:,i)  = lw_lambda_ss_j_vfi
                    !s_pom_j_vfi(:,i) = s_pom_ss_j_vfi
                    !V_j_vfi(:,i) = V_ss_j_vfi
                    !gini_weight_trans(:,:, i) = gini_weight_sv
                    !gini_weight_cons_trans(:, :, :, :, :, i) = gini_weight_cons_trans
                    avg_ef_l_suply_trans(i) = avg_ef_l_suply
                enddo    
                
            else
                do i = 3, bigT,1            
                    c_trans(:, :, :, :, i_o, i) = c_ss !_g
                    !l_trans(:, :, :, :, i_o, i) = l_ss  
                    !labor_tax_trans(:, :, :, :, i) = labor_tax
                    prob_trans(:, :, :, :, i_o, i) = prob_ss
                    svplus_trans(:, :, :, :, i_o, i) = svplus_ss
                    aime_plus_trans(:, :, :, :, i_o, i) = aime_plus_ss
                    V_trans(:, :, :,:, i_o, i) = V_ss
                    !c_j_vfi(:,i) = c_ss_j_vfi   
                    !l_j_vfi(:,i) = l_ss_j_vfi 
                    !l_pen_j(:, :, i) = l_ss_pen_j
                    !!labor_tax_j(:, :, i)  = labor_tax_ss_j
                    !lw_j_vfi(:,i)  = lw_ss_j_vfi
                    !lw_lambda_j_vfi(:,i)  = lw_lambda_ss_j_vfi
                    !s_pom_j_vfi(:,i) = s_pom_ss_j_vfi
                    !V_j_vfi(:,i) = V_ss_j_vfi
                    !gini_weight_trans(:,:, i) = gini_weight_sv
                    !gini_weight_cons_trans(:, :, :, :, :, i) = gini_weight_cons_trans
                    avg_ef_l_suply_trans(i) = avg_ef_l_suply
                enddo
            endif
    end subroutine
!*******************************************************************************************
    
    subroutine get_transition()

        implicit none

        integer :: iter, ij, it, itmax
        logical :: check

        ! initialize remaining variables
        ! start timer

            ! solve the household problem
            do j = bigJ, 2, -1
                call household_trans_endo(j, 2)
            enddo
            
            do i = 2, bigT
                call household_trans_endo(1, i)
                !write(*,*)i
            enddo
    
            ! calculate the distribution of households over state space
            do i = 2, bigT
                call get_distribution_trans(i)
            enddo

            ! aggregate individual decisions
            do i = 2, bigT
                call aggregation_trans(i)
            enddo
    
end subroutine 
!*******************************************************************************************
  ! start policy functions iteration
subroutine agent_vf_trans() 
    implicit none
    call initialize_trans
    call get_transition()  
end subroutine
   
!*******************************************************************************************
! adjust policy function for endogenous grid to exogenous one by linear interpolation
subroutine change_grid_piecewise_lin_spline(endo_grid, egzo_grid, f, g)
    real*8 :: endo_grid(0:n_a), egzo_grid(0:n_a), f(0:n_a), g(0:n_a), coef1, coef2
    integer :: first, last, ik, it, s

if (sum(egzo_grid) == 0) then
    endo_grid = egzo_grid
else
    do ia = 0, n_a, 1 
        first=0
        last=n_a
        do while(abs(last-first)>2)
            ik=floor(fi*first+(1d0-fi)*last)
            it=floor((1d0-fi)*first+fi*last)
            if(endo_grid(ik)<endo_grid(it)) then
                if(endo_grid(it)<=egzo_grid(ia)) then
                    first=it
                else if(endo_grid(ik)<=egzo_grid(ia) .and. egzo_grid(ia)<endo_grid(it)) then
                    first=ik
                    last=it
                else if(egzo_grid(ia)<endo_grid(ik)) then
                    last=ik
                endif
            else
                 if(endo_grid(ik)<=egzo_grid(ia)) then
                    first=ik
                else if(endo_grid(it)<=egzo_grid(ia) .and. egzo_grid(ia)<endo_grid(ik)) then
                    first=it
                    last=ik
                else if(egzo_grid(ia)<endo_grid(it)) then
                    last=it
                endif
            endif
        enddo
        ik=first
        if(last-first == 2 .and. endo_grid(first+1)<egzo_grid(ia)) then
            ik=ik+1
        endif
        if(ik==0 .and. endo_grid(ik)>egzo_grid(ia)) then
            g(ia)=f(0)
        else
            coef1=abs(endo_grid(ik+1)-egzo_grid(ia))/abs(endo_grid(ik+1)-endo_grid(ik))
            coef2=abs(endo_grid(ik)-egzo_grid(ia))/abs(endo_grid(ik+1)-endo_grid(ik))
            g(ia)=coef1*f(ik)+coef2*f(ik+1)
            if(g(ia)>egzo_grid(n_a)) then 
                g(ia) = f(n_a)
            endif
        endif
    enddo    
endif
end subroutine 
!*******************************************************************************************

!******************************************************************************************
    subroutine linear_int(assets, ial, iar, dist, grid, grid_size, grid_grow)

        implicit none
        integer :: grid_size
        real*8 :: assets, grid_grow
        real*8 :: grid(0:grid_size)
        integer :: ial, iar
        real*8 :: dist, real_ia

if (grid_size == 0 .or. sum(grid) == 0) then 
        ial = 0
        iar = 0
        dist = 1d0 
else
        real_ia = grid_Val_Inv(assets, grid(0), grid(grid_size), grid_size, grid_grow)

        ial = floor(real_ia)
        ial = max(ial, 0)
        ial = min(ial, grid_size-1)
        iar = ial+1
        dist = 1d0 - (assets-grid(ial))/(grid(iar)-grid(ial))
endif
    end subroutine
!******************************************************************************************
    
   subroutine initialize_trans() 
    implicit none 
        
    real*8 :: sv_pom(0:n_a), Brackets(2)
   
    pi_trans_vfi_cond = 1d0
    pi_ss_vfi_cond = 1d0 
    
    ! here pi is conditional probability - in the rest of the code we use unconditional one 
    do i = 1 , bigT, 1    
        do j=2, bigJ        
            pi_trans_vfi_cond(j,i)=pi(j,i)/pi(j-1,max(i-1,1))  
            pi_ss_vfi_cond(j) =  pi_ss_vfi(j)/pi_ss_vfi(j-1) 
        enddo
    enddo 
    ! size of the asset grid
 
    sv_grid(0:n_a) = grid_cons(a_l, a_u, n_a, a_grow)
    sv = sv_grid
    aime(0:n_aime) = grid_cons(aime_l, aime_u, n_aime, aime_grow)
    ! based on https://fas.org/sgp/crs/misc/R43542.pdf

   ! Brackets(1) = 926d0/3921d0!
   ! Brackets(2) = 5583d0/3921d0! 
   ! 
   !do i_aime = 0, n_aime, 1
   !     if (aime(i_aime)< Brackets(1)) then 
   !         aime_replacement_rate(i_aime) = 0.9d0*aime(i_aime)  
   !     elseif (aime(i_aime)< Brackets(2)) then 
   !         aime_replacement_rate(i_aime) = 0.9d0*Brackets(1) + 0.32d0*(aime(i_aime)-Brackets(1))      
   !     else
   !         aime_replacement_rate(i_aime) = 0.9d0*Brackets(1)  + 0.32d0*(Brackets(2)-Brackets(1))  + 0.15d0*(aime(i_aime)-Brackets(2) )   
   !     endif
   ! enddo
   ! if (swich_cohort_ps == 1) then
   !     aime_replacement_rate =1d0 
   ! endif
   ! 
   ! if (switch_partial_efficiency == 1) then 
   !    aime_replacement_rate = aime
   !    aime_cap = aime(n_aime)
   ! endif 
    
    ! to do
    if (rho1 == 0) then ! Hence there's no common part of social security
        aime_replacement_rate = aime*rho2
    else
        aime_replacement_rate = 1 + aime*(rho2/rho1)
    endif
    
    if (n_aime == 0 ) then 
         aime_replacement_rate = 1d0  ! to cut (all with aime)
    endif
    
   

    
   end subroutine
   
!*******************************************************************************************
subroutine comptax(taxinc, lambda, tau, tax, mrate)
  !  
  ! Compute taxes paid and the marginal rate given
  !   (1) taxable income 
  !   (2) the tax schedule: (x,y) pairs, where x is the rate for income over y
  !
  implicit none

  real*8       :: taxinc, tau, lambda
  real*8       :: tax, mrate

 
        tax    = taxinc -(1-tau)*taxinc**(1-lambda) ! total tax collected on labor income
        mrate  = 1-(1-tau)*(1-lambda)*taxinc**(-lambda)


end subroutine comptax
!*******************************************************************************************
function foc_intratemp(av, w_tax, w_non_tax, tc, l_guess, n_sp_val)
implicit none
    real*8 :: av, w_tax, w_non_tax, tc, l_guess, n_sp_val
    real*8 ::  c, l , foc_intratemp(3)
    real*8 :: lambda , tau_prog
    real*8:: l0, del, taxinc, nontaxinc, res, lp, tax, mrate, dres
    integer :: maxit, i
    tau_prog = tL_com
    lambda = lambda_com
    !besed on Heathcote, Jonathan, Kjetil Storesletten, and Giovanni L. Violante. "Optimal tax progressivity: An analytical framework." The Quarterly Journal of Economics 132.4 (2017): 1693-1754
    ! https://www.nber.org/papers/w19899.pdf
    ! https://www.sas.upenn.edu/~dkrueger/research/LafferCurves.pdf

    ! foc_intratemp = phi/(1-phi)*(w_non_tax+lambda*(1-tau_prog)*(w_tax)**(1-tau_prog)*l**(-tau_prog)*(1-l)-c (1-phi)/phi
    ! TAKE a look at ncn emeryt\model\prog_income_tax.lyx
      maxit  = 6
      l0  = l_guess
      !if (n_sp_val == 0d0) l0 = 0d0
      del = 1d-8

      if ((switch_partial_eq == 0) .and.  (swich_fix_labor == 0d0) ) then 
          do i= 1, maxit
            taxinc = w_tax*l0
            nontaxinc = w_non_tax*l0
            call comptax(taxinc, lambda, tau_prog, tax, mrate)
            res    = (1-phi)/phi*(av+taxinc+nontaxinc-tax)-(w_tax*(1.0d0-mrate) + w_non_tax)*(1.0d0-l0)
            lp     = l0+del
            taxinc = w_tax*lp
            nontaxinc = w_non_tax*lp
            call comptax(taxinc, lambda, tau_prog, tax,mrate)
            dres   = ((1-phi)/phi*(av+taxinc+nontaxinc-tax)-(w_tax*(1.0d0-mrate) + w_non_tax)*(1.0d0-lp)-res)/del
            l0     = min(max(l0-res/dres, del), 1d0-del)
          enddo  
      endif  
      if (n_sp_val == 0d0) then
          l = 0d0
      else
          l = l0
      endif
      taxinc = w_tax*l0
      nontaxinc = w_non_tax*l0
      call comptax(taxinc, lambda, tau_prog, tax,mrate)
      c = max((av+taxinc+nontaxinc-tax)/tc, del)
      if ((c .NE. c) .or. (l .NE. l) .or. (abs(tax) > 100d0) .or. (abs(c) > 100000d0)) then
         write(*,*) 'problem 1'  
         pause
      endif
      foc_intratemp(1) = c 
      foc_intratemp(2) = min(max(l, 0d0), 1d0-del)
      foc_intratemp(3) = tax

      
endfunction 



function derivative_crra(eta,nu,lambda,zeta, kappa, c)
! derivative of function_crra which define optimal consumption
! compere with file D:\Dropbox (UW)\NCN EMERYT\__model\egm\prog_income_tax_eqg_CRRA
implicit none
    real*8 :: eta, nu, lambda, zeta, kappa, c
    real*8 :: derivative_crra


        derivative_crra = - zeta*lambda*(1-eta*c**nu)**(lambda-1)*eta*nu*c**(nu-1)&
                          -kappa*eta*(nu-1)*c**(nu-2)*((1-eta*c**nu)**(lambda)- nu/(nu-1)*lambda*(1-eta*c**nu)**(lambda-1)*eta*c**nu ) &
                          -eta*(nu-1)*c**(nu-2)

endfunction

function function_crra(eta,nu,lambda,zeta, kappa, c)
! root of function_crra define optimal consumption
! compere with file D:\Dropbox (UW)\NCN EMERYT\__model\egm\prog_income_tax_eqg_CRRA
implicit none
    real*8 :: eta, nu, lambda, zeta, kappa, c
    real*8 :: function_crra
    

        function_crra = (zeta-kappa*eta*c**(nu-1))*(max(1-eta*c**nu, 0d0))**lambda-eta*c**(nu-1)

endfunction

!*******************************************************************************************


include "pfi_household_problem.f90"

include "pfi_distribution.f90"
 
include "pfi_agregation.f90"

include "pfi_print.f90"
!************************************************************************************************

!
function optimal_consumption_and_labor_new(RHS,phi, theta, tau, lambda,w, w_NT, tc, n_sp_val)
!! return optimal labor and conumption 
!! for given taxes scheme (tc, tau, lamda)
!! utility function parameter theta, phi
!! future optimal choice given by RHS (compere with file D:\Dropbox (UW)\NCN EMERYT\__model\egm\prog_income_tax_eqg_CRRA)
real*8 :: RHS, phi, theta, tau, lambda, w, w_NT, tc, n_sp_val
real*8 :: eta, nu, zeta, kappa, c_l, c_r, c_rts, c_min, f, df, dxold, dx, l, out_of_range, decreasing_slow
real*8 :: optimal_consumption_and_labor_new(2)
integer :: iter, iter_max
    
iter_max = 50
if (phi == 1) then 
    optimal_consumption_and_labor_new(1) = max(RHS**(1d0/(-theta)),1d-10) 
    if (n_sp_val == 0d0) then 
        optimal_consumption_and_labor_new(2) =  0d0
    else
        optimal_consumption_and_labor_new(2) =  1d0
    endif
else
    
    nu = (1-phi*(1-theta))/(1-phi)/(1-theta)
    eta = (RHS)**(1/(1-phi)/(1-theta))
    zeta = (1-phi)*tc/phi/(1-tau)/(1-lambda)/(w**(1-lambda))
    kappa = w_NT/(1-tau)/(1-lambda)/(w**(1-lambda))
    
        ! in first step we use bisection to obtain coverage region for Newtor Raphson method
    
        ! initial guess: 
        ! consumption should be lower than wages - agent use to save for retirement, 
        ! you may have problem here if pension system is very generous for some agent type 
        ! thus we make shure in next lines that dunction is negative for our lower guess 
        ! and positive for higher guessed value
    if (lambda == 0) then ! 
       c_rts = ((1d0-phi)*tc/eta/phi/((1-tau)*w+w_NT))**(1d0/(nu-1))     
    else
        c_min = (1d0/eta)**(1d0/nu)
        c_l = 1.05d0*c_min
        c_r = max(w+w_NT, 2*c_l)

    
        ! orient the search such that f(c_l<0) and f(c_r)>)
        iter =1
        do while ((function_crra(eta,nu,lambda,zeta,kappa,c_r)<0) .and. (iter < iter_max))
            c_l = c_r
            c_r = 2d0*c_r
            iter = iter + 1 
            if (abs(c_l-c_r)< 1d-8) then 
                c_rts = 0.5d0*(c_l+c_r)   
                exit
            endif 
        enddo
    
        iter =1
        do while ((function_crra(eta,nu,lambda,zeta,kappa,c_l)>0) .and. (iter < iter_max))
            c_r = c_l
            c_l = 0.5d0*(c_l+c_min)
            iter = iter + 1 
            if (abs(c_l-c_r)< 1d-8) then 
                c_rts = 0.5d0*(c_l+c_r)   
                exit
            endif 
        enddo

 
        !initial guess for root
        c_rts = 0.5d0*(c_l+c_r)   
        ! the stepsize before last
        dxold = abs(c_l-c_r)
        ! the last stepsize
        dx = dxold
        f = function_crra(eta,nu,lambda,zeta,kappa,c_rts)
        df = derivative_crra(eta,nu,lambda,zeta,kappa,c_rts)

    
        do iter = 1, iter_max,1
           ! bisection if newton out of range
            out_of_range  = ((c_rts-c_r)*df-f)* ((c_rts-c_l)*df-f)
           ! or not decreasing fast enought
           decreasing_slow = abs(2d0*f)-abs(dxold*df)    
            IF ((out_of_range>0) .or. (decreasing_slow>0)) then 
                dxold = dx
                dx = 0.5*(c_r-c_l)
                c_rts = c_l+ dx
                if (abs(c_l- c_rts)<1d-8) then
                    exit! change in root is negligible
                endif
            ELSE
                ! newton step acceplatle, take it
                dxold = dx
                dx = f/df
                c_rts = c_rts -dx
            ENDIF
            IF (abs(dx)<1d-8) then
                exit
           endif
            f = function_crra(eta,nu,lambda,zeta,kappa,c_rts)
            df = derivative_crra(eta,nu,lambda,zeta,kappa,c_rts)
            IF (f<0d0) THEN
                c_l = c_rts
            ELSE
                c_r = c_rts
            ENDIF
        enddo
    endif

    if ((c_rts .NE. c_rts) .or. (l .NE. l) ) then
        write(*,*) 'problem 2'  
        pause
    endif

    continue 
    optimal_consumption_and_labor_new(1) = c_rts
    l = 1d0-(RHS/(c_rts**(phi*(1d0-theta)-1d0)))**(1d0/(1d0-phi)/(1d0-theta))
    !if (n_sp_val == 0d0) then 
    !    optimal_consumption_and_labor_new(2) =  0d0
    !else
        optimal_consumption_and_labor_new(2) = min(max(l, 0d0), 1d0-1d-8)
    !endif
    
endif
    
endfunction 
end module