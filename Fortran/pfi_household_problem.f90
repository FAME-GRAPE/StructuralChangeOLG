!*******************************************************************************************
! find futur assets for every age, assets grid point, state  
! steady state
subroutine household_endo()

implicit none
real*8 :: available, EV_prim, c_opt, w_opt, c_ss_endo(0:n_a), l_ss_endo(0:n_a), lab_income
real*8 :: av, wage, wage_non_tax, foc(3), optimal_choice(2)
real*8 :: dist, c_help, l_help
integer :: i_o2

if (iter /= 1) then ! due to this manevour next iteration starts from the same value it endened for given omega
    aime_plus_ss = aime_plus_ss_om(:,:,:,:,i_o)
endif

do ia = 0, n_a, 1 
    do i_aime=0, n_aime,1
        do ip=1, n_sp, 1
            c_ss(bigj, ia, i_aime, ip) = max(((1d0+r_ss_vfi)*sv(ia)/gam_ss_vfi + (b1_ss_vfi(bigJ, ip) + b2_ss_vfi(bigJ, ip)) + bequest_ss_j_vfi(bigJ)- upsilon_ss_vf)/tc_ss_vfi, 1d-10)  ! ip, 
            !c_ss(bigj, ia, i_aime, ip) = max(((1d0+r_ss_vfi)*sv(ia)/gam_ss_vfi + aime_replacement_rate(i_aime)*b_ss_j_vfi(bigJ) + bequest_ss_j_vfi(bigJ)- upsilon_ss_vf)/tc_ss_vfi, 1d-10)  ! ip, 
            l_ss(bigj, ia, i_aime, ip) = 0d0
            labor_tax(bigj, ia, i_aime, ip) = 0d0
            svplus_ss(bigj, ia, i_aime, ip)=0d0
            aime_plus_ss(bigJ, ia, i_aime, ip) = aime(i_aime)
            V_ss(bigj, ia, i_aime, ip) = valuefunc(0d0, aime_plus_ss(bigJ, ia, i_aime, ip), c_ss(bigj, ia, i_aime, ip),l_ss(bigJ,ia, i_aime, ip), bigJ, ip)
        enddo
    enddo
enddo
if (bigJ >= jbar_ss_vf) then ! get rid of this if for bigJ
    
do ia=0, n_a, 1 
    do i_aime=0, n_aime,1
        do ip = 1, n_sp, 1
            ip_p = ip
            EV_prim = 0d0
            EV_ss(bigj, ia, i_aime, ip)  = 0d0
            !do ip_p = 1, n_sp,1
                if(theta == 1_dp)then
                    EV_prim = EV_prim + (1d0+r_ss_vfi)/gam_ss_vfi*1/c_ss(bigj, ia, i_aime, ip_p)    
                else
                    EV_prim = EV_prim + (1d0+r_ss_vfi)/gam_ss_vfi*1*c_ss(bigj, ia, i_aime, ip_p)**(phi -theta*phi -1)
                endif 
                EV_ss(bigj, ia, i_aime, ip)  = EV_ss(bigj, ia, i_aime, ip) + 1*V_ss(bigj, ia, i_aime, ip_p)
            !enddo
            if(theta == 1)then
                RHS_ss(bigj, ia, i_aime, ip) = 1d0/(delta*delta_mult_ss_vfi*pi_ss_vfi_cond(bigJ)*EV_prim)
            else
                RHS_ss(bigj, ia, i_aime, ip) = delta*delta_mult_ss_vfi*pi_ss_vfi_cond(bigJ)*EV_prim
                EV_ss(bigj, ia, i_aime, ip)  = ((1d0-theta)*EV_ss(bigj, ia, i_aime, ip))**(1d0/(1d0-theta)) 
            endif
        enddo
    enddo
enddo
else
    do ia=0, n_a, 1 
    do i_aime=0, n_aime,1
        do ip = 1, n_sp, 1
            EV_prim = 0d0
            EV_ss(bigj, ia, i_aime, ip)  = 0d0
            do ip_p = 1, n_sp,1
                if(theta == 1_dp)then
                    EV_prim = EV_prim + (1d0+r_ss_vfi)/gam_ss_vfi*pi_ip_ss_vfi(ip, ip_p)/c_ss(bigj, ia, i_aime, ip_p)    
                else
                    EV_prim = EV_prim + (1d0+r_ss_vfi)/gam_ss_vfi*pi_ip_ss_vfi(ip, ip_p)*c_ss(bigj, ia, i_aime, ip_p)**(phi -theta*phi -1)
                endif 
                EV_ss(bigj, ia, i_aime, ip)  = EV_ss(bigj, ia, i_aime, ip) + pi_ip_ss_vfi(ip, ip_p)*V_ss(bigj, ia, i_aime, ip_p)
            enddo
            if(theta == 1)then
                RHS_ss(bigj, ia, i_aime, ip) = 1d0/(delta*delta_mult_ss_vfi*pi_ss_vfi_cond(bigJ)*EV_prim)
            else
                RHS_ss(bigj, ia, i_aime, ip) = delta*delta_mult_ss_vfi*pi_ss_vfi_cond(bigJ)*EV_prim
                EV_ss(bigj, ia, i_aime, ip)  = ((1d0-theta)*EV_ss(bigj, ia, i_aime, ip))**(1d0/(1d0-theta)) 
            endif
        enddo
    enddo
enddo

endif

!do ia=0, n_a, 1 
!    do i_aime=0, n_aime,1
!        do ip = 1, n_sp, 1
!            EV_prim = 0d0
!            EV_ss(bigj, ia, i_aime, ip)  = 0d0
!            do ip_p = 1, n_sp,1
!                if(theta == 1_dp)then
!                    EV_prim = EV_prim + (1d0+r_ss_vfi)/gam_ss_vfi*pi_ip_ss_vfi(ip, ip_p)/c_ss(bigj, ia, i_aime, ip_p)    
!                else
!                    EV_prim = EV_prim + (1d0+r_ss_vfi)/gam_ss_vfi*pi_ip_ss_vfi(ip, ip_p)*c_ss(bigj, ia, i_aime, ip_p)**(phi -theta*phi -1)
!                endif 
!                EV_ss(bigj, ia, i_aime, ip)  = EV_ss(bigj, ia, i_aime, ip) + pi_ip_ss_vfi(ip, ip_p)*V_ss(bigj, ia, i_aime, ip_p)
!            enddo
!            if(theta == 1)then
!                RHS_ss(bigj, ia, i_aime, ip) = 1d0/(delta*delta_mult_ss_vfi*pi_ss_vfi_cond(bigJ)*EV_prim)
!            else
!                RHS_ss(bigj, ia, i_aime, ip) = delta*delta_mult_ss_vfi*pi_ss_vfi_cond(bigJ)*EV_prim
!                EV_ss(bigj, ia, i_aime, ip)  = ((1d0-theta)*EV_ss(bigj, ia, i_aime, ip))**(1d0/(1d0-theta)) 
!            endif
!        enddo
!    enddo
!enddo


do j = bigJ-1, 1, -1
    !poss_ass_sum_ss(j, :) = 0d0
    !    do i= j, bigJ, 1
    !        do ip = 1, n_sp
    !            if(i < jbar_ss_vf)then
    !                poss_ass_sum_ss(j, ip) = poss_ass_sum_ss(j, ip) + ((1 - tL_ss)*(w_pom_ss_vfi(i)*n_sp_value_vfi(1))**(1-lambda) + n_sp_value_vfi(1)*w_pom_ss_implicit_vfi(i) + bequest_ss_j_vfi(i) - upsilon_ss_vf)/((1d0+r_ss_vfi)/gam_ss_vfi)**(i-j) 
    !            else
    !                poss_ass_sum_ss(j, ip) = poss_ass_sum_ss(j, ip) + (aime_replacement_rate(n_aime)*b_ss_j_vfi(ip, i)             + bequest_ss_j_vfi(i) - upsilon_ss_vf)/((1d0+r_ss_vfi)/gam_ss_vfi)**(i-j)              
    !            endif         
    !        enddo
        poss_ass_sum_ss(j) = 0d0
        do i= j, bigJ, 1
                if(i < jbar_ss_vf)then
                    poss_ass_sum_ss(j) = poss_ass_sum_ss(j) + ((1 - tL_ss)*(w_pom_ss_vfi(i)*n_sp_value_vfi(1))**(1-lambda) + n_sp_value_vfi(1)*w_pom_ss_implicit_vfi(i) + bequest_ss_j_vfi(i) - upsilon_ss_vf)/((1d0+r_ss_vfi)/gam_ss_vfi)**(i-j) 
                else
                    poss_ass_sum_ss(j) = poss_ass_sum_ss(j) + ((b1_ss_vfi(j, n_sp) + b2_ss_vfi(j, n_sp))           + bequest_ss_j_vfi(i) - upsilon_ss_vf)/((1d0+r_ss_vfi)/gam_ss_vfi)**(i-j)              
                endif       
        enddo
    do ia=0, n_a, 1
        do i_aime=0, n_aime,1
            do ip=1, n_sp,1
                if((sv(ia) + poss_ass_sum_ss(j)) < 0d0)then   ! , ip 
                    c_ss(j, ia, i_aime, ip) = 1d-10 
                    if(j < jbar_ss_vf)then
                        l_ss(j, ia, i_aime, ip) = 1d0 
                        lab_income = (1 - tL_ss)*(n_sp_value_vfi(ip)*w_pom_ss_vfi(j))**(1-lambda) + n_sp_value_vfi(ip)*w_pom_ss_implicit_vfi(j)
                    else
                        l_ss(j,ia, i_aime, ip) = 0d0
                        lab_income = 0d0
                    endif
                    sv_tempo(j, ia, i_aime, ip) = (tc_ss_vfi*c_ss(j, ia, i_aime, ip)+sv(ia)-lab_income&
                                                            - (b1_ss_vfi(j, ip) + b2_ss_vfi(j, ip)) - bequest_ss_j_vfi(j)+upsilon_ss_vf)/((1d0+r_ss_vfi)/gam_ss_vfi)   ! , ip
                else 
                    if(j>=jbar_ss_vf) then ! retireed thus labor choice is trivial 
                        l_ss(j, ia, i_aime, ip) = 0d0
                        lab_income = 0d0 
                        if(theta == 1)then ! consumption can be calculated diractly from RHS
                            c_ss(j, ia, i_aime, ip) = max(RHS_ss(j+1, ia, i_aime, ip),1d-15)
                        else
                            c_ss(j, ia, i_aime, ip) = max(RHS_ss(j+1, ia, i_aime, ip)**(1d0/(phi -theta*phi -1)),1d-15)
                        endif
                    else
                        wage            = n_sp_value_vfi(ip)*w_pom_ss_vfi(j)
                        wage_non_tax    = n_sp_value_vfi(ip)*w_pom_ss_implicit_vfi(j)    
                        if(theta == 1)then
                                c_ss(j, ia, i_aime, ip) = max(RHS_ss(j+1, ia, i_aime, ip),1d-15)
                                c_opt = tc_ss_vfi*c_ss(j, ia, i_aime, ip) 
                                l_ss(j, ia, i_aime, ip) = optimal_labor(c_opt, wage, wage_non_tax, phi, tL_ss, lambda, n_sp_value_vfi(ip))
                        else
                                optimal_choice = optimal_consumption_and_labor_new(RHS_ss(j+1, ia, i_aime, ip), phi, theta, tL_ss, lambda, wage, wage_non_tax, tc_ss_vfi, n_sp_value_vfi(ip))
                                c_ss(j, ia, i_aime, ip) = optimal_choice(1)
                                l_ss(j, ia, i_aime, ip)  = optimal_choice(2)
                        endif
                        lab_income = (1d0 - tL_ss)*(wage*l_ss(j, ia, i_aime, ip))**(1-lambda) &
                                        +  wage_non_tax*l_ss(j, ia, i_aime, ip)
                    endif   
                    sv_tempo(j, ia, i_aime, ip) = (tc_ss_vfi*c_ss(j, ia, i_aime, ip)+sv(ia)&
                                                            - lab_income - (b1_ss_vfi(j, ip) + b2_ss_vfi(j, ip)) &   ! , ip
                                                            - bequest_ss_j_vfi(j)+upsilon_ss_vf)/((1d0+r_ss_vfi)/gam_ss_vfi )                
                endif
            enddo
        enddo
    enddo
    if (j == 1) then
         l_ss_endo = 0
    endif
    do i_aime=0, n_aime, 1
        do ip=1, n_sp, 1
            call change_grid_piecewise_lin_spline(sv_tempo(j,:, i_aime, ip), sv, sv, svplus_ss(j,:, i_aime, ip))
        enddo
    enddo
     do ia=0, n_a, 1  
         do i_aime=0, n_aime,1       
            do ip=1, n_sp, 1
                if(svplus_ss(j, ia, i_aime, ip)<0d0)then
                    svplus_ss(j, ia, i_aime, ip) = 0d0
                endif
                    available = (1d0+r_ss_vfi)*sv(ia)/gam_ss_vfi + (b1_ss_vfi(j, ip) + b2_ss_vfi(j, ip)) + bequest_ss_j_vfi(j) &   !, ip
                                - upsilon_ss_vf- svplus_ss(j, ia, i_aime, ip)
                if(j>=jbar_ss_vf) then
                    c_ss(j, ia, i_aime, ip) = max((available)/tc_ss_vfi, 1e-10)
                    l_ss(j, ia, i_aime, ip)=0d0
                    labor_tax(j, ia, i_aime, ip) = 0d0
                    aime_plus_ss(j, ia, i_aime, ip) = aime(i_aime)
                else                     
                    wage =  w_pom_ss_vfi(j)*n_sp_value_vfi(ip)
                    wage_non_tax = w_pom_ss_implicit_vfi(j)*n_sp_value_vfi(ip)
                    tl_com  = tl_ss
                    lambda_com = lambda
                    foc = foc_intratemp(available, wage, wage_non_tax, tc_ss_vfi, 0.001d0, n_sp_value_vfi(ip))
                    c_ss(j, ia, i_aime, ip) = foc(1)
                    l_ss(j, ia, i_aime, ip) = foc(2)
                    labor_tax(j, ia, i_aime, ip) = foc(3) 
                    
                    !if (j == jbar_ss-1) then 
                        aime_plus_ss(j, ia, i_aime, ip) = (float(j-1)*aime(i_aime) +  min(n_sp_value_vfi(ip)*l_ss(j, ia, i_aime, ip)*omega_ss_vfi(i_o)/avg_ef_l_suply, aime_cap))/float(j)    ! ! high aime cap, unreachable
                        !aime_plus_ss(j, ia, i_aime, ip) = n_sp_value_vfi(ip)*l_ss(j, ia, i_aime, ip)/avg_ef_l_suply
                        !else
                        !aime_plus_ss(j, ia, i_aime, ip) = 0d0   
                    !endif
                endif
                pi_com = pi_ss_vfi_cond(j)
                V_ss(j, ia, i_aime, ip) = valuefunc(svplus_ss(j, ia, i_aime, ip), aime_plus_ss(j, ia, i_aime, ip), c_ss(j, ia, i_aime, ip), l_ss(j, ia, i_aime, ip), j,  ip) 
            enddo
        enddo
        do i_aime=0, n_aime,1 
            do ip=1, n_sp, 1
                    call linear_int(aime_plus_ss(max(j-1,1), ia, i_aime, ip), iaimel, iaimer, dist, aime(:), n_aime, aime_grow)
                    if (dist < 0) pause
                    EV_prim = 0d0
                    EV_ss(j, ia, i_aime, ip)  = 0d0
                    if (j >= jbar_ss_vf) then
                        ip_p = ip
                            c_help =       dist*c_ss(j, ia, iaimel, ip_p) &
                                    +(1d0-dist)*c_ss(j, ia, iaimer, ip_p)
                            l_help =       dist*l_ss(j, ia, iaimel, ip_p) &
                                    +(1d0-dist)*l_ss(j, ia, iaimer, ip_p)
                                                
                            if(theta == 1_dp)then
                                EV_prim = EV_prim + (1d0+r_ss_vfi)/gam_ss_vfi*1*1/c_help
                            else
                                if(j<jbar_ss_vf)then !base  on D:\Dropbox (UW)\NCN EMERYT\__model\egm\CRRA
                                    EV_prim =  EV_prim + (1d0+r_ss_vfi)/gam_ss_vfi*1&
                                                        *((1-l_help)/c_help)**((1d0-theta)*(1d0-phi))*c_help**(-theta)
                                else
                                    EV_prim = EV_prim + (1d0+r_ss_vfi)/gam_ss_vfi*1*c_help**(phi -theta*phi -1)
                                endif
                            endif                                  
                            EV_ss(j, ia, i_aime, ip)  = EV_ss(j, ia, i_aime, ip) + 1*V_ss(j, ia, i_aime, ip_p)
                    else
                        do ip_p=1, n_sp, 1
                                c_help =       dist*c_ss(j, ia, iaimel, ip_p) &
                                        +(1d0-dist)*c_ss(j, ia, iaimer, ip_p)
                                l_help =       dist*l_ss(j, ia, iaimel, ip_p) &
                                        +(1d0-dist)*l_ss(j, ia, iaimer, ip_p)
                                                
                                if(theta == 1_dp)then
                                    EV_prim = EV_prim + (1d0+r_ss_vfi)/gam_ss_vfi*pi_ip_ss_vfi(ip, ip_p)*1/c_help
                                else
                                    if(j<jbar_ss_vf)then !base  on D:\Dropbox (UW)\NCN EMERYT\__model\egm\CRRA
                                        EV_prim =  EV_prim + (1d0+r_ss_vfi)/gam_ss_vfi*pi_ip_ss_vfi(ip, ip_p)&
                                                            *((1-l_help)/c_help)**((1d0-theta)*(1d0-phi))*c_help**(-theta)
                                    else
                                        EV_prim = EV_prim + (1d0+r_ss_vfi)/gam_ss_vfi*pi_ip_ss_vfi(ip, ip_p)&
                                                    *c_help**(phi -theta*phi -1)
                                    endif
                                endif                                  
                                EV_ss(j, ia, i_aime, ip)  = EV_ss(j, ia, i_aime, ip) + pi_ip_ss_vfi(ip, ip_p)&
                                                                    *V_ss(j, ia, i_aime, ip_p)
                        enddo
                    endif
                    
                if(theta==1_dp)then
                    RHS_ss(j, ia, i_aime, ip)=1d0/(delta*delta_mult_ss_vfi*pi_ss_vfi_cond(j)*EV_prim)  
                else
                    RHS_ss(j, ia, i_aime, ip)= delta*delta_mult_ss_vfi*pi_ss_vfi_cond(j)*EV_prim
                endif 
                    
                if (theta == 1) then 
                    EV_ss(j, ia, i_aime, ip) = EV_ss(j, ia, i_aime, ip)
                else 
                    EV_ss(j, ia, i_aime, ip) = ((1d0-theta)*EV_ss(j, ia, i_aime, ip))**(1d0/(1d0-theta))
                endif
            enddo
         enddo 
    enddo
enddo

aime_plus_ss_om(:,:,:,:,i_o) = aime_plus_ss(:,:,:,:)
if (iter == 1) then
    do i_o2 = 2, omega_dim
        aime_plus_ss_om(:,:,:,:,i_o2) = aime_plus_ss(:,:,:,:)
    enddo
endif

end subroutine


    
!*************************************************** ****************************************
!find futur assets for every age, assets grid point, state  
!  trans
subroutine household_trans_endo(ij,ii)

implicit none
real*8 :: available, EV_prim, c_opt,  av, wage, wage_non_tax, foc(3), lab_income, optimal_choice(2), c_help, l_help, dist, temp, temp0
integer, intent(in) :: ii, ij
integer ::  j, it,i, k, ik, i_o2 
it = year(ii, ij, bigJ)

do ia = 0, n_a, 1 
    do i_aime=0, n_aime,1 
        do ip=1, n_sp, 1
            c_trans_vfi(bigj, ia, i_aime, ip, it) = max(((1d0+r_vfi(it))*sv(ia)/gam_vfi(it) +  (b1_t_vfi(bigJ, ip, it) + b2_t_vfi(bigJ, ip, it)) + bequest_j_vfi(bigJ,it)- upsilon_vfi(it))/tc_vfi(it), 1d-10)
            l_trans_vfi(bigj, ia, i_aime, ip, it) = 0d0
            labor_tax_trans(bigj, ia, i_aime, ip, it) = 0d0
            svplus_trans_vfi(bigj, ia, i_aime, ip, it) = 0d0
            aime_plus_trans_vfi(bigJ, ia, i_aime, ip, it) = aime(i_aime)
            V_trans_vfi(bigj, ia, i_aime, ip, it) = valuefunc_trans(0d0, aime(i_aime), c_trans_vfi(bigj, ia, i_aime, ip, it),l_trans_vfi(bigJ,ia, i_aime, ip,it), delta_mult_vfi(it), bigJ, ip, it)
        enddo
    enddo
enddo
!do i_o2 = 2, bigT
!c_trans_vfi(4,:, :, :, i_o2)=c_trans_vfi(4,:, :, :, 1) !!!!!
!enddo
if (j >= jbar_t_vfi(it)) then
    do ia=0, n_a, 1 
        do i_aime=0, n_aime,1 
            do ip = 1, n_sp, 1
                ip_p = ip
                EV_prim = 0d0
                EV_trans(bigj, ia, i_aime, ip, it) = 0d0
                !do ip_p=1,n_sp, 1
                    if(theta==1_dp)then
                        EV_prim = EV_prim + (1d0+r_vfi(it))/gam_vfi(it)*1/c_trans_vfi(bigj, ia, i_aime, ip_p, it)    
                    else 
                        EV_prim = EV_prim + (1d0+r_vfi(it))/gam_vfi(it)*1*c_trans_vfi(bigj, ia, i_aime, ip_p, it)**(phi - theta*phi -1)                                          
                    endif 
                    EV_trans(bigj, ia, i_aime, ip, it) = EV_trans(bigj, ia, i_aime, ip, it) + 1*V_trans_vfi(bigj, ia, i_aime, ip_p, it)
                !enddo
                if(theta==1_dp)then
                    RHS_trans(bigj, ia, i_aime, ip, it)=1d0/(tc_vfi(max(it-1,1))/tc_vfi(it)*delta*delta_mult_vfi(it)*pi_trans_vfi_cond(bigJ,it)*EV_prim)  
                else
                    RHS_trans(bigj, ia, i_aime, ip, it)= tc_vfi(max(it-1,1))/tc_vfi(it)*delta*delta_mult_vfi(it)*pi_trans_vfi_cond(bigJ,it)*EV_prim
                endif 
                
                if (theta == 1) then 
                    EV_trans(bigj, ia, i_aime, ip, it) = EV_trans(bigj, ia, i_aime, ip, it)
                else 
                    EV_trans(bigj, ia, i_aime, ip, it) = ((1d0-theta)*EV_trans(bigj, ia, i_aime, ip, it))**(1d0/(1d0-theta))
                endif
            enddo
        enddo
    enddo
else
    do ia=0, n_a, 1 
        do i_aime=0, n_aime,1 
            do ip = 1, n_sp, 1
                EV_prim = 0d0
                EV_trans(bigj, ia, i_aime, ip, it) = 0d0
                do ip_p=1,n_sp, 1
                    if(theta==1_dp)then
                        EV_prim = EV_prim + (1d0+r_vfi(it))/gam_vfi(it)*pi_ip_vfi(ip, ip_p, it)/c_trans_vfi(bigj, ia, i_aime, ip_p, it)    
                    else 
                        EV_prim = EV_prim + (1d0+r_vfi(it))/gam_vfi(it)*pi_ip_vfi(ip, ip_p, it)*c_trans_vfi(bigj, ia, i_aime, ip_p, it)**(phi - theta*phi -1)                                          
                    endif 
                    EV_trans(bigj, ia, i_aime, ip, it) = EV_trans(bigj, ia, i_aime, ip, it) + pi_ip_vfi(ip, ip_p, it)*V_trans_vfi(bigj, ia, i_aime, ip_p, it)
                enddo
                if(theta==1_dp)then
                    RHS_trans(bigj, ia, i_aime, ip, it)=1d0/(tc_vfi(max(it-1,1))/tc_vfi(it)*delta*delta_mult_vfi(it)*pi_trans_vfi_cond(bigJ,it)*EV_prim)  
                else
                    RHS_trans(bigj, ia, i_aime, ip, it)= tc_vfi(max(it-1,1))/tc_vfi(it)*delta*delta_mult_vfi(it)*pi_trans_vfi_cond(bigJ,it)*EV_prim
                endif 
                
                if (theta == 1) then 
                    EV_trans(bigj, ia, i_aime, ip, it) = EV_trans(bigj, ia, i_aime, ip, it)
                else 
                    EV_trans(bigj, ia, i_aime, ip, it) = ((1d0-theta)*EV_trans(bigj, ia, i_aime, ip, it))**(1d0/(1d0-theta))
                endif
            enddo
        enddo
    enddo      
endif
do j = bigJ-1, ij, -1
    it = year(ii,ij,j)
    i = it
    if (j <= j_young_bar) then
        sv = 0d0
    else
        sv = sv_grid
    endif
    !do ip = 1, n_sp
    !    if(j < jbar_t_vfi(it))then
    !            poss_ass_sum_ss(j, ip) = (1-tl(it))*(w_pom_trans_vfi(j,it)*n_sp_value_vfi(1))**(1-lambda_trans(it))+bequest_j_vfi(j,it)-upsilon_vfi(it) + w_pom_trans_implicit_vfi(j, it)*n_sp_value_vfi(1)
    !        else
    !            poss_ass_sum_ss(j, ip) =  aime_replacement_rate(n_aime)*b_j_vfi(j,it)+bequest_j_vfi(j,it)-upsilon_vfi(it)
    !    endif
    !    do k= j+1, bigJ, 1
    !        ik = year(ii, j, k)
    !        ! to do 
    !        if(k < jbar_t_vfi(it))then
    !            poss_ass_sum_ss(j, ip) = poss_ass_sum_ss(j, ip) + + w_pom_trans_implicit_vfi(k, ik)*n_sp_value_vfi(1) + ((1-tl(ik))*(w_pom_trans_vfi(k,ik)*n_sp_value_vfi(1))**(1-lambda_trans(ik))+bequest_j_vfi(k,ik)-upsilon_vfi(ik))/product(1d0+r_vfi(it+1:ik))*product(gam_vfi(it+1:ik)) 
    !        else
    !            poss_ass_sum_ss(j, ip) = poss_ass_sum_ss(j, ip) + ( aime_replacement_rate(n_aime)*b_j_vfi(k,ik)+bequest_j_vfi(k,ik)-upsilon_vfi(ik))/product((1d0+r_vfi(it+1:ik)))*product(gam_vfi(it+1:ik))
    !        endif
    !    enddo
    !enddo 
        if(j < jbar_t_vfi(it))then
                poss_ass_sum_ss(j) = (1-tl(it))*(w_pom_trans_vfi(j,it)*n_sp_value_vfi(1))**(1-lambda_trans(it))+bequest_j_vfi(j,it)-upsilon_vfi(it) + w_pom_trans_implicit_vfi(j, it)*n_sp_value_vfi(1)
            else
                poss_ass_sum_ss(j) =  (b1_t_vfi(j, ip, it) + b2_t_vfi(j, ip, it)) +bequest_j_vfi(j,it)-upsilon_vfi(it)
        endif
        do k= j+1, bigJ, 1
            ik = year(ii, j, k)
            ! to do 
            if(k < jbar_t_vfi(it))then
                poss_ass_sum_ss(j) = poss_ass_sum_ss(j) + w_pom_trans_implicit_vfi(k, ik)*n_sp_value_vfi(1) + ((1-tl(ik))*(w_pom_trans_vfi(k,ik)*n_sp_value_vfi(1))**(1-lambda_trans(ik))+bequest_j_vfi(k,ik)-upsilon_vfi(ik))/product(1d0+r_vfi(it+1:ik))*product(gam_vfi(it+1:ik)) 
            else
                poss_ass_sum_ss(j) = poss_ass_sum_ss(j) + ( (b1_t_vfi(k, ip, ik) + b2_t_vfi(k, ip, ik)) +bequest_j_vfi(k,ik)-upsilon_vfi(ik))/product((1d0+r_vfi(it+1:ik)))*product(gam_vfi(it+1:ik))
            endif
        enddo
    
    do ia=0, n_a, 1
        do i_aime=0, n_aime,1 
            do ip=1, n_sp,1
                if((sv(ia) + poss_ass_sum_ss(j)) < 0d0)then     ! , ip
                    c_trans_vfi(j, ia, i_aime, ip, it) = 1d-10
                    if(j < jbar_t_vfi(it))then  
                        l_trans_vfi(j, ia, i_aime, ip, it) = 1d0 
                        lab_income = (1-tL(it))*(n_sp_value_vfi(ip)*w_pom_trans_vfi(j, it)*l_trans_vfi(j, ia, i_aime, ip, it))**(1-lambda_trans(it)) + &
                                        + w_pom_trans_implicit_vfi(j, it)*n_sp_value_vfi(1)
                    else
                        l_trans_vfi(j,ia, i_aime, ip, it) = 0d0
                        lab_income = 0d0
                    endif ! Why there's no sv_tempo_trans in case of <0 here? It is in steady
                else 
                        if(theta ==1)then
                            c_trans_vfi(j, ia, i_aime, ip, it) = max(RHS_trans(j+1, ia, i_aime, ip, year(ii,ij,j+1)),1d-10) 
                            if(j>=jbar_t_vfi(it)) then
                                l_trans_vfi(j, ia, i_aime, ip, it)=0d0
                                lab_income = 0d0
                            else
                                c_opt = tc_vfi(it)*c_trans_vfi(j, ia, i_aime, ip, it) 
                                wage = n_sp_value_vfi(ip)*w_pom_trans_vfi(j, it) 
                                wage_non_tax = w_pom_trans_implicit_vfi(j, it)*n_sp_value_vfi(ip)
                                l_trans_vfi(j, ia, i_aime, ip, it) = optimal_labor(c_opt, wage, wage_non_tax, phi, tL(it), lambda_trans(it), n_sp_value_vfi(ip))
                                lab_income = (1-tL(it))*(n_sp_value_vfi(ip)*w_pom_trans_vfi(j, it)*l_trans_vfi(j, ia, i_aime, ip, it))**(1-lambda_trans(it)) + &
                                                w_pom_trans_implicit_vfi(j, it)*n_sp_value_vfi(ip)*l_trans_vfi(j, ia, i_aime, ip, it)
                                ! l_trans_vfi(j, ia, i_aime, ip, it) = max(1d0 - max((tc_vfi(it)*c_trans_vfi(j, ia, i_aime, ip, it) * (1d0 - phi)/(n_sp_value_vfi(ip)*phi*(1-tl(it))*w_pom_trans_vfi(j, it))), 0d0),0d0)
                            endif
                        else    
                            if(j>=jbar_t_vfi(it)) then
                                c_trans_vfi(j, ia, i_aime, ip, it) = max(RHS_trans(j+1, ia, i_aime, ip, year(ii,ij,j+1))**(1d0/(phi -theta*phi -1)),1d-10)
                                l_trans_vfi(j, ia, i_aime, ip, it) = 0d0
                                lab_income = 0d0
                            else
                                wage = n_sp_value_vfi(ip)*w_pom_trans_vfi(j, it) 
                                wage_non_tax = w_pom_trans_implicit_vfi(j, it)*n_sp_value_vfi(ip)
                                optimal_choice = optimal_consumption_and_labor_new(RHS_trans(j+1, ia, i_aime, ip, year(ii,ij,j+1)), phi, theta, tl(it), lambda_trans(it), wage, wage_non_tax, tc_vfi(it), n_sp_value_vfi(ip))
                                c_trans_vfi(j, ia, i_aime, ip, it)  = max(optimal_choice(1), 1d-10)
                                l_trans_vfi(j, ia, i_aime, ip, it)  = optimal_choice(2)
                                
                                lab_income = (1-tL(it))*(n_sp_value_vfi(ip)*w_pom_trans_vfi(j, it)*l_trans_vfi(j, ia, i_aime, ip, it))**(1-lambda_trans(it)) + &
                                                w_pom_trans_implicit_vfi(j, it)*n_sp_value_vfi(ip)*l_trans_vfi(j, ia, i_aime, ip, it)
                        endif
                    endif
                endif
                    sv_tempo_trans(j, ia, i_aime, ip, it) = (tc_vfi(it)*c_trans_vfi(j, ia, i_aime, ip, it)+sv(ia)&
                                                                    - lab_income- (b1_t_vfi(j, ip, it) + b2_t_vfi(j, ip, it)) &
                                                                    - bequest_j_vfi(j,it)+upsilon_vfi(it))/((1d0+r_vfi(it))/gam_vfi(it))
                    if ((it == 2) .and. (j > 1) .and. (j<jbar_t_vfi(it))) then 
                        sv_tempo_trans(j, ia, i_aime, ip, it) = sv_tempo_trans(j, ia, i_aime, ip, it) - transfer_pfi(j-1)
                    endif   
            enddo
        enddo
    enddo
    if (sv_tempo_trans(j, 1, 0, 1, it)/=sv_tempo_trans(j, 1, 0, 1, it)) then
        print *, j, it
        print *, sv_tempo_trans(j, 1, 1, 1, it)
        print *, 1
        pause
    endif
    do i_aime=0, n_aime, 1
        do ip=1, n_sp, 1
            !if (sv_tempo_trans(j, 1, i_aime, ip, it)/=sv_tempo_trans(j, 1, i_aime, ip, it)) then
            !    print *, j, i_aime, it
            !    print *, sv_tempo_trans(j, 1, i_aime, ip, it)
            !    print *, 2
            !    pause
            !endif
           call change_grid_piecewise_lin_spline(sv_tempo_trans(j, :, i_aime, ip, it), sv, sv, svplus_trans_vfi(j, :, i_aime, ip, it))
        enddo 
    enddo
     do ia=0, n_a, 1 
        do i_aime=0, n_aime,1 
            do ip=1, n_sp, 1
                if(j>=jbar_t_vfi(it)) then
                    if(svplus_trans_vfi(j, ia, i_aime, ip, it)<0d0)then
                        svplus_trans_vfi(j, ia, i_aime, ip, it) = 0d0
                    endif
                    if ((it == 2) .and. (j > 1) .and. (j<jbar_t_vfi(it))) then 
                        available = (1d0+r_vfi(it))*(sv(ia)+transfer_pfi(j-1))/gam_vfi(it) + (b1_t_vfi(j, ip, it) + b2_t_vfi(j, ip, it)) + bequest_j_vfi(j,it)- upsilon_vfi(it)
                    else    
                        available = (1d0+r_vfi(it))*sv(ia)/gam_vfi(it) + (b1_t_vfi(j, ip, it) + b2_t_vfi(j, ip, it)) + bequest_j_vfi(j,it)- upsilon_vfi(it)
                    endif
                    l_trans_vfi(j, ia, i_aime, ip, it)=0d0
                    c_trans_vfi(j, ia, i_aime, ip, it) = max( (available - svplus_trans_vfi(j, ia, i_aime, ip, it))/tc_vfi(it), 1e-10) 
                    labor_tax_trans(j, ia, i_aime, ip, it) = 0d0
                    aime_plus_trans_vfi(j, ia, i_aime, ip, it) = aime(i_aime)
                else
                        if(svplus_trans_vfi(j, ia, i_aime, ip, it)<0d0)then
                        svplus_trans_vfi(j, ia, i_aime, ip, it) = 0d0
                        endif  
                    if ((it == 2) .and. (j <= ofe_u) .and. (j > 1) .and. (switch_type_1 == 0) ) then 
                        av = (1d0+r_vfi(it))*(sv(ia)+transfer_pfi(j-1))/gam_vfi(it) - svplus_trans_vfi(j, ia, i_aime, ip, it) + bequest_j_vfi(j, it)- upsilon_vfi(it)
                    else
                        av = (1d0+r_vfi(it))*sv(ia)/gam_vfi(it) - svplus_trans_vfi(j, ia, i_aime, ip, it) + bequest_j_vfi(j, it)- upsilon_vfi(it)
                    endif
                    wage = w_pom_trans_vfi(j,it)*n_sp_value_vfi(ip)
                    wage_non_tax = w_pom_trans_implicit_vfi(j, it)*n_sp_value_vfi(ip)
                    tl_com = tl(it)
                    lambda_com = lambda
                    foc = foc_intratemp(av, wage,  wage_non_tax, tc_vfi(it), 0.001d0, n_sp_value_vfi(ip))
                    c_trans_vfi(j, ia, i_aime, ip, it)  = max(foc(1), 1d-10)  ! max( (available - w_pom_ss_vfi(j)*n_sp_value_vfi(ip)*(1d0 -  l_ss(j, ia, i_aime, ip, ir, id)) - svplus_ss(j, ia, i_aime, ip, ir, id))/tc_ss_vfi, 1e-10)
                    l_trans_vfi(j, ia, i_aime, ip, it)  = foc(2)  ! 1d0 - min( max( (1d0-phi)*(available - svplus_ss(j, ia, i_aime, ip, ir, id))/(w_pom_ss_vfi(j)*n_sp_value_vfi(ip)) , 0d0) , 1d0)
                    labor_tax_trans(j, ia, i_aime, ip, it) = foc(3)
                            
                    aime_plus_trans_vfi(j, ia, i_aime, ip, it) = (float(j-1)*aime(i_aime)+min(n_sp_value_vfi(ip)*l_trans_vfi(j, ia, i_aime, ip, it)/avg_ef_l_suply_trans(i), aime_cap))/float(j) ! later: add var avg_ef_l_suply_trans_vfi in order not to have i_o subscript here

                endif
                pi_com = pi_trans_vfi_cond(j, it)
                V_trans_vfi(j, ia, i_aime, ip, it) = valuefunc_trans(svplus_trans_vfi(j, ia, i_aime, ip, it), aime_plus_trans_vfi(j, ia, i_aime, ip, it), &
                                                                            c_trans_vfi(j, ia, i_aime, ip, it), l_trans_vfi(j,ia, i_aime, ip,it), delta_mult_vfi(it), j, ip, it)
            enddo
        enddo
        do i_aime=0, n_aime,1 
            do ip=1, n_sp, 1
                temp0 = aime_plus_trans_vfi(max(j-1,1), ia, i_aime, ip, max(it-1,1))
                call linear_int(aime_plus_trans_vfi(max(j-1,1), ia, i_aime, ip, max(it-1,1)), iaimel, iaimer, dist, aime(:), n_aime, aime_grow)
                temp = aime_plus_trans_vfi(max(j-1,1), ia, i_aime, ip, max(it-1,1))
                if ((dist > 2d0) .OR. (dist < -1d0)) then
                    !print *, aime_plus_trans_vfi(max(j-1,1), ia, i_aime, ip, max(it-1,1))
                    pause
                endif
                EV_prim = 0d0
                EV_trans(j, ia, i_aime, ip, it) =0
                if (j >= jbar_t_vfi(it)) then
                    ip_p = ip
                    c_help = dist*c_trans_vfi(j, ia, iaimel, ip_p, it)  + (1d0-dist)*c_trans_vfi(j, ia, iaimer, ip_p, it)
                    l_help = dist*l_trans_vfi(j, ia, iaimel, ip_p, it)  + (1d0-dist)*l_trans_vfi(j, ia, iaimer, ip_p, it)
                    if(theta == 1_dp)then
                        EV_prim = EV_prim + (1d0+r_vfi(it))/gam_vfi(it)*1/c_help  
                    else
                        if(j<jbar_t_vfi(it))then
                                EV_prim =  EV_prim + (1d0+r_vfi(it))/gam_vfi(it)*1 &
                                                        *((1d0-l_help)/c_help)**((1d0-theta)*(1d0-phi))&
                                                        *c_help**(-theta) 
                        else
                                EV_prim = EV_prim + (1d0+r_vfi(it))/gam_vfi(it)*1* &
                                                    c_help**(phi -theta*phi -1)
                        endif                                
                    endif 
                    
                    !if (EV_prim == 1.0/0.0)
                    !    print *, "inf"
                    !    pause
                    !endif
                    ! to do to do  pi_ir(ir,ir_r) vs pi_ir(ip,ir_r)
                    EV_trans(j, ia, i_aime, ip, it) = EV_trans(j, ia, i_aime, ip, it) + 1*V_trans_vfi(j, ia, i_aime, ip_p, it)
                else
                    do ip_p=1, n_sp, 1                
                        c_help = dist*c_trans_vfi(j, ia, iaimel, ip_p, it)  + (1d0-dist)*c_trans_vfi(j, ia, iaimer, ip_p, it)
                        l_help = dist*l_trans_vfi(j, ia, iaimel, ip_p, it)  + (1d0-dist)*l_trans_vfi(j, ia, iaimer, ip_p, it)
                        if(theta == 1_dp)then
                            EV_prim = EV_prim + (1d0+r_vfi(it))/gam_vfi(it)*pi_ip_vfi(ip, ip_p, it)/c_help  
                        else
                            if(j<jbar_t_vfi(it))then
                                    EV_prim =  EV_prim + (1d0+r_vfi(it))/gam_vfi(it)*pi_ip_vfi(ip, ip_p, it)&
                                                            *((1d0-l_help)/c_help)**((1d0-theta)*(1d0-phi))&
                                                            *c_help**(-theta) 
                            else
                                    EV_prim = EV_prim + (1d0+r_vfi(it))/gam_vfi(it)*pi_ip_vfi(ip, ip_p, it)*&
                                                        c_help**(phi -theta*phi -1)
                            endif                                
                        endif 
                    
                        !if (EV_prim == 1.0/0.0)
                        !    print *, "inf"
                        !    pause
                        !endif
                        ! to do to do  pi_ir(ir,ir_r) vs pi_ir(ip,ir_r)
                        EV_trans(j, ia, i_aime, ip, it) = EV_trans(j, ia, i_aime, ip, it) + pi_ip_vfi(ip, ip_p, it)*V_trans_vfi(j, ia, i_aime, ip_p, it)
                    enddo
                endif
                    if(theta == 1_dp)then
                    RHS_trans(j, ia, i_aime, ip, it)=1d0/(tc_vfi(max(it-1,1))/tc_vfi(it)*delta*delta_mult_vfi(it)*pi_trans_vfi_cond(j,it)*EV_prim)  
                else
                    RHS_trans(j, ia, i_aime, ip, it)= tc_vfi(max(it-1,1))/tc_vfi(it)*delta*delta_mult_vfi(it)*pi_trans_vfi_cond(j,it)*EV_prim
                    !if ((RHS_trans(j, ia, i_aime, ip, it)/=RHS_trans(j, ia, i_aime, ip, it)) .OR. ((1/RHS_trans(j, ia, i_aime, ip, it)) /= (1/RHS_trans(j, ia, i_aime, ip, it)))) then
                    !    print *, aime_plus_trans_vfi(max(j-1,1), ia, i_aime, ip, max(it-1,1))
                    !    write(*,'(F100.10)') dist
                    !    print *, j, ia, i_aime, ip, it
                    !    print *, RHS_trans(j, ia, i_aime, ip, it)
                    !    print *, l_trans_vfi(j, ia, i_aime, ip, it)
                    !    print *, 'RHS'
                    !    pause
                    !endif
                endif 
                if (theta == 1) then 
                    EV_trans(j, ia, i_aime, ip, it) = EV_trans(j, ia, i_aime, ip, it)
                else 
                    EV_trans(j, ia, i_aime, ip, it) = ((1d0-theta)*EV_trans(j, ia, i_aime, ip, it))**(1d0/(1d0-theta))
                endif
            enddo
         enddo 
     enddo
enddo
sv = sv_grid
end subroutine