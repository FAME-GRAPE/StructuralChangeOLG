! WHAT   : Closing budget -> tax rate to keep debt share unchanged in the long term
! TAKE   : unchanged in routine:   consumption_gross, capital tax [[tk]], interest rate [[r_bar]], private saving [[sum_priv_sv]], labor force growth [[nu(i)=big_l(i)/big_l(i-1)]], 
!          unchanged in routine:   change in technological progress [[gam = z_t/z_(t-1)]], wage [[w_bar]], sum of contribution to pension system in each perion [[contribution]], sum of pension benefit [[sum_b]],
!                                  (switch_init_FF == 0) .AND. (switch_init_retirement_db == 0) .AND. (switch_init_retirement_FF == 0) intial values of taxation and debt are not taken from the file 
!          changed   in routine:   debt_share, budget deficit, lump sum tax cohort unspecific [[upsilon]], consumption and labor  tax
! DO     : update tax rate during iteration on transition path
! RETURN : updated tax rate 

select case (switch_residual) 
    case(0)
    !   case 0 - upsilon is residual
        debt_share(1) = debt(1)/y(1)
        Tax(1) = tc(1)*consumption_gross_new(1) + tk(1)*r_bar(1)*sum_priv_sv(1)/(nu(1)*gam_t(1)) &
                + sum(N_t_j(1:bigJ,1)*labor_tax_j_agr(1:bigJ,1))/bigL(1) !+ tL(1)*sum_b(1)  
        do i = 2,bigT,1
            Tax(i) = tc(i)*consumption_gross_new(i) + tk(i)*r_bar(i)*sum_priv_sv(i-1)/(nu(i)*gam_t(i)) &
                    + sum(N_t_j(1:bigJ,i)*labor_tax_j_agr(1:bigJ,i))/bigL(i) !+ tL(i)*sum_b(i)  
        enddo 
        deficit(1) = debt(1) - debt(1)/(nu(1)*gam_t(1))
        upsilon(1) = (g(1) + subsidy(1) + (1 + r_bar(1))*debt(1)/(nu(1)*gam_t(1)) - Tax(1) - debt(1))*bigL(1)/N_t(1)
        do i = 2,bigT,1
            debt_share(i) = debt(i)/y(i)
            deficit(i) = debt(i) - debt(i-1)/(nu(i)*gam_t(i))
            upsilon(i) = (g(i) + subsidy(i) + (1 + r_bar(i))*debt(i-1)/(nu(i)*gam_t(i)) - Tax(i) - debt(i))*bigL(i)/N_t(i)
        enddo
        
    case(1)             
!       case 1 - tC is residual
        debt_share(1) = debt(1)/y(1)
        if (switch_ref_run_now == 0) then 
            upsilon = upsilon_r_ss_1*y*bigL/N_t
        endif
        deficit(1) = debt(1) - debt(1)/(nu(1)*gam_t(1))

        do i = 2,bigT,1
            debt_share(i) = debt(i)/y(i)
            deficit(i) = debt(i) - debt(i-1)/(nu(i)*gam_t(i))
            tc(i) =  (g(i) + subsidy(i) + (1 + r_bar(i))*debt(i-1)/(nu(i)*gam_t(i)) - debt(i) - upsilon(i)/(bigL(i)/N_t(i)) &
                     - tk(i)*r_bar(i)*sum_priv_sv(i-1)/(nu(i)*gam_t(i)) &
                     - sum(N_t_j(1:bigJ,i)*labor_tax_j_agr(1:bigJ,i))/bigL(i))/consumption_gross_new(i) 
        enddo
        
    case(2)  
!       case 2 - tL is residual
        debt_share(1) = debt(1)/y(1)
        upsilon = upsilon_r_ss_1*y*bigL/N_t
        deficit(1) = debt(1) - debt(1)/(nu(1)*gam_t(1))
        do i = 2,bigT,1
            debt_share(i) = debt(i)/y(i)
            deficit(i) = debt(i) - debt(i-1)/(nu(i)*gam_t(i))
            tl(i) = ((g(i) + subsidy(i) + (1 + r_bar(i))*debt(i-1)/(nu(i)*gam_t(i)) - debt(i) - upsilon(i)/(bigL(i)/N_t(i)) &
                            - tk(i)*r_bar(i)*sum_priv_sv(i-1)/(nu(i)*gam_t(i)) -  tc(i)*consumption_gross_new(i)&
                            - sum(N_t_j(1:bigJ,i)*(lw_j_vfi(1:bigJ,i) - lw_lambda_j_vfi(1:bigJ,i)))/bigL(i))&
                            /(sum(N_t_j(1:bigJ,i)*lw_lambda_j_vfi(1:bigJ,i))/bigL(i))) 
        enddo
        
    case(3)  
!       case 3 - tk is residual
        debt_share(1) = debt(1)/y(1)
        upsilon = upsilon_r_ss_1*y*bigL/N_t
        deficit(1) = debt(1) - debt(1)/(nu(1)*gam_t(1))
!        
        do i = 2,bigT,1
            debt_share(i) = debt(i)/y(i)
            deficit(i) = debt(i) - debt(i-1)/(nu(i)*gam_t(i))
            tk(i) =  (g(i) + subsidy(i) + (1 + r_bar(i))*debt(i-1)/(nu(i)*gam_t(i)) - debt(i) - upsilon(i)/(bigL(i)/N_t(i)) &
                     - tc(i)*consumption_gross_new(i) - sum(N_t_j(1:bigJ,i)*labor_tax_j_agr(1:bigJ,i))/bigL(i)) &
                     /(r_bar(i)*sum_priv_sv(i-1)/(nu(i)*gam_t(i)))
        enddo
        
    case(9)
!        case 9 - g is residual
!        debt_share(1) = debt(1)/y(1)
!        upsilon = upsilon_r_ss_1*y*bigL/N_t
!        deficit(1) = debt(1) - debt(1)/(nu(1)*gam_t(1))
!        
!        Tax(1) = tc(1)*consumption_gross_new(1) + tk(1)*r_bar(1)*sum_priv_sv(1)/(nu(1)*gam_t(1)) &
!        + sum(N_t_j(1:bigJ,1)*labor_tax_j_agr(1:bigJ,1))/bigL(1) !+ tL(1)*sum_b(1)  
!        do i = 2,bigT,1
!            Tax(i) = tc(i)*consumption_gross_new(i) + tk(i)*r_bar(i)*sum_priv_sv(i-1)/(nu(i)*gam_t(i)) &
!                    + sum(N_t_j(1:bigJ,i)*labor_tax_j_agr(1:bigJ,i))/bigL(i) !+ tL(i)*sum_b(i)  
!        enddo 
!!        
!        do i = 2,bigT,1
!            debt_share(i) = debt(i)/y(i)
!            deficit(i) = debt(i) - debt(i-1)/(nu(i)*gam_t(i))                
!            g_new(i) = tk(i)/((gam_t(i)*nu(i))/(r_bar(i)*sum_priv_sv(i-1))) - (subsidy(i) + (1 + r_bar(i))*debt(i-1)/(nu(i)*gam_t(i)) &
!                - debt(i) - tc(i)*consumption_gross_new(i) - sum(N_t_j(1:bigJ,i)*labor_tax_j_agr(1:bigJ,i))/bigL(i) &
!                - upsilon(i)/(bigL(i)/N_t(i)))
!            
!        !Tax_ss = tc_ss*consumption_ss_gross + tk_ss*r_bar_ss*sum_priv_sv_ss/(gam_ss*nu_ss) + sum(N_ss_j*labor_tax_ss_j_agr)/bigL_ss  !+ tL_ss*sum_b_ss
!        !deficit_ss = ((nu_ss*gam_ss - 1)/(gam_ss*nu_ss))*debt_ss ! deficit is BI from Lyx
!        !g_ss_new  = -(subsidy_ss - upsilon_ss + (1 + r_bar_ss)*debt_ss/(gam_ss*nu_ss) - debt_ss - Tax_ss)
!
!            g_new(i) = -(subsidy(i) - upsilon(i) + (1 + r_bar(i))*debt(i-1)/(nu(i)*gam_t(i)) - Tax(i) - debt(i)) !*bigL(i)/N_t(i)
!        enddo
        
        debt_share(1) = debt(1)/y(1)
        if (switch_ref_run_now == 0) then 
            upsilon = upsilon_r_ss_1*y*bigL/N_t
        endif
        deficit(1) = debt(1) - debt(1)/(nu(1)*gam_t(1))

        do i = 2,bigT,1
            debt_share(i) = debt(i)/y(i)
            deficit(i) = debt(i) - debt(i-1)/(nu(i)*gam_t(i))
             g(i) =  -(- tc(i)*consumption_gross_new(i) + subsidy(i) + (1 + r_bar(i))*debt(i-1)/(nu(i)*gam_t(i)) - debt(i) - upsilon(i)/(bigL(i)/N_t(i)) &
                     - tk(i)*r_bar(i)*sum_priv_sv(i-1)/(nu(i)*gam_t(i)) &
                     - sum(N_t_j(1:bigJ,i)*labor_tax_j_agr(1:bigJ,i))/bigL(i)) 
        enddo
        
        !if (iter == 1) g = g_new
        !where (iter /= 1 .and. g_new >= 0) g = up_t*g + (1 - up_t)*g_new
        
!    case(4)  
!    !       case 4 - t1 is residual
!        debt_share(1) = debt(1)/y(1)
!        Tax(1) = tc(1)*consumption_gross_new(1) + tk(1)*r_bar(1)*sum_priv_sv(1)/(nu(1)*gam_t(1)) + sum(N_t_j(1:bigJ,1)*labor_tax_j_vfi(1:bigJ,1))/bigL(1) !+ tL(1)*sum_b(1)  
!        do i = 2,bigT,1
!            Tax(i) = tc(i)*consumption_gross_new(i) + tk(i)*r_bar(i)*sum_priv_sv(i-1)/(nu(i)*gam_t(i)) + sum(N_t_j(1:bigJ,i)*labor_tax_j_vfi(1:bigJ,i))/bigL(i) !+ tax_weight_trans(i)/bigL(i)*sum_b(i)  
!        enddo 
!        deficit(1) = debt(1) - debt(1)/(nu(1)*gam_t(1))
!        upsilon(1) = (g(1) + subsidy(1) + (1 + r_bar(1))*debt(1)/(nu(1)*gam_t(1)) - Tax(1) - debt(1))*bigL(1)/N_t(1)
!        
!        do i = 2,bigT,1
!            if (switch_type_2 == 0) then 
!                t1(:,i) = (1 - up_tc)*t1(:,i) + up_tc*sum_b(i)/(sum(N_t_j(:,i)*omega(:,i)*w_bar(i)*l_j(:,i), dim=1)/bigL(i)) 
!            else 
!                if (i< (ofe_u + bigJ- jbar_t(i)) + 1) then ! (i< bigJ +2) then !
!                     subsidy_j(j,i) = sum_b_weight_trans(max(i + jbar_t(i) -j,1))*b_scale_factor(i)*b1_j(j,i) - t1(j,i)*omega(j,i)*w_bar(i)*l_j(j,i) 
!                     sum_b1_help(i) = 0
!                     do j = 1, bigJ, 1
!                        sum_b1_help(i) =  sum_b1_help(i) + N_t_j(j,i)*sum_b_weight_trans(max(i + jbar_t(i) -j,1))*b_scale_factor(i)*b1_j(j,i)/bigL(i)
!                     enddo
!                    t1(:,i) = (1 - up_tc)*t1(:,i) + up_tc*sum_b1_help(i)/(sum(N_t_j(:,i)*omega(:,i)*w_bar(i)*l_j(:,i), dim=1)/bigL(i))
!                endif
!            endif             
!            debt_share(i) = debt(i)/y(i)
!            deficit(i) = debt(i) - debt(i-1)/(nu(i)*gam_t(i))
!            upsilon(i) = (g(i) + subsidy(i) + (1 + r_bar(i))*debt(i-1)/(nu(i)*gam_t(i)) - Tax(i) - debt(i))*bigL(i)/N_t(i)
!        enddo
!    case(5)  
!    !   case 5 - b tax is residual
!        debt_share(1) = debt(1)/y(1)
!        Tax(1) = tc(1)*consumption_gross_new(1) + tk(1)*r_bar(1)*sum_priv_sv(1)/(nu(1)*gam_t(1)) + tL(1)*(w_bar(1) - contribution(1)) !+ tL(1)*sum_b(1)  
!        do i = 2,bigT,1
!            Tax(i) = tc(i)*consumption_gross_new(i) + tk(i)*r_bar(i)*sum_priv_sv(i-1)/(nu(i)*gam_t(i)) + sum(N_t_j(1:bigJ,i)*labor_tax_j_vfi(1:bigJ,i))/bigL(i) !+ tax_weight_trans(i)/bigL(i)*sum_b(i)  
!        enddo 
!        deficit(1) = debt(1) - debt(1)/(nu(1)*gam_t(1))
!        upsilon(1) = (g(1) + subsidy(1) + (1 + r_bar(1))*debt(1)/(nu(1)*gam_t(1)) - Tax(1) - debt(1))*bigL(1)/N_t(1)
!        
!        do i = 2,bigT,1
!            if (switch_type_2 == 0) then 
!                   b_scale_factor(i) = (1 - up_tc)*b_scale_factor(i) + up_tc*sum(N_t_j(:,i)*omega(:,i)*w_bar(i)*l_j(:,i)*t1(:,i), dim=1)/sum(N_t_j(:,i)*b1_j(:,i), dim=1)
!            else 
!                if (i< bigJ +2) then !(i< (ofe_l + bigJ- jbar_t(i)) + 1) then 
!                   b_scale_factor(i) = (1 - up_tc)*b_scale_factor(i) + up_tc*sum(N_t_j(:,i)*omega(:,i)*w_bar(i)*l_j(:,i)*t1(:,i), dim=1)/sum(N_t_j(:,i)*b1_j(:,i), dim=1)
!                endif
!            endif      
!            debt_share(i) = debt(i)/y(i)
!            deficit(i) = debt(i) - debt(i-1)/(nu(i)*gam_t(i))
!            upsilon(i) = (g(i) + subsidy(i) + (1 + r_bar(i))*debt(i-1)/(nu(i)*gam_t(i)) - Tax(i) - debt(i))*bigL(i)/N_t(i)
!        enddo 
!        
!    case(6) 
!!       case 6 - tc + debt is residual, fiscal rule with tc (to avoid rapid changes in tc)
!        debt_share(1) = debt(1)/y(1)
!        upsilon = upsilon_r_ss_1*y*bigL/N_t
!        deficit(1) = debt(1) - debt(1)/(nu(1)*gam_t(1))
!        do i = 2,n_p+1,1
!            if (i < n_debt) then
!                if ( iter> 1) then 
!                    tc(i) =  (1 - up_tc)*tc_new + up_tc*tc(i-1) + tc_growth*(forward_smoothing*sum(debt_share(i-1:i+forward-2)) - debt_constr)
!                else 
!                    tc(i) =  (1 - up_tc)*tc_new + up_tc*tc(i-1) + tc_growth*(debt_share(i-1) - debt_constr)
!                endif 
!                debt(i) = subsidy(i) + g(i) + (1 + r_bar(i))*debt(i-1)/(nu(i)*gam_t(i)) & 
!                          - (tc(i)*consumption_gross_new(i) + tk(i)*r_bar(i)*sum_priv_sv(i-1)/(nu(i)*gam_t(i)) &
!                          + sum(N_t_j(1:bigJ,i)*labor_tax_j_vfi(1:bigJ,i))/bigL(i)) - upsilon(i)/(bigL(i)/N_t(i)) 
!                            
!            else
!                debt(i) = debt_constr*y(i)                   
!                tc(i) = (g(i) + subsidy(i) + (1 + r_bar(i))*debt(i-1)/(nu(i)*gam_t(i)) - debt(i) - upsilon(i)/(bigL(i)/N_t(i)) &
!                       - tk(i)*r_bar(i)*sum_priv_sv(i-1)/(nu(i)*gam_t(i)) &
!                       - sum(N_t_j(1:bigJ,i)*labor_tax_j_vfi(1:bigJ,i))/bigL(i))/consumption_gross_new(i)                    
!            endif
!            debt_share(i) = debt(i)/y(i) 
!            deficit(i) = debt(i) - debt(i-1)/(nu(i)*gam_t(i))
!            sum_priv_sv(i) = k(i+1)*gam_t(i+1)*nu(i+1) + debt(i) - PillarII(i)
!        enddo
!        
!        sum_priv_sv(n_p+2:bigT) = k(n_p+2:bigT)*gam_t(n_p+2:bigT)*nu(n_p+2:bigT) + debt(n_p+2:bigT) - PillarII(n_p+2:bigT)
!        debt_share(n_p+2:bigT) = debt(n_p+2:bigT)/y(n_p+2:bigT)
!        do i = n_p + 2,bigT,1
!            tc(i) = tc_new 
!        enddo  
!        
!    case(7) 
!!   case 7 - tk + debt is residual, fiscal rule with tc (to avoid rapid changes in tc)
!    debt_share(1) = debt(1)/y(1)
!    upsilon = upsilon_r_ss_1*y*bigL/N_t
!    deficit(1) = debt(1) - debt(1)/(nu(1)*gam_t(1))
!    do i = 2,n_p+1,1
!        if (i < n_debt) then
!            temp_test = sum(debt_share(i-1:i+forward-2)) 
!            temp_test = forward_smoothing*sum(debt_share(i-1:i+forward-2)) 
!            temp_test = forward_smoothing*sum(debt_share(i-1:i+forward-2)) - debt_constr
!            if (iter > 1) then 
!                tk(i) =  (1 - up_tc)*tk_new + up_tc*tk(i-1) + tc_growth*2d0*(forward_smoothing*sum(debt_share(i-1:i+forward-2)) - debt_constr)
!            else
!                tk(i) =  (1 - up_tc)*tk_new + up_tc*tk(i-1) + tc_growth*(debt_share(i-1) - debt_constr)
!            endif
!            debt(i) = subsidy(i) + g(i) + (1 + r_bar(i))*debt(i-1)/(nu(i)*gam_t(i)) &
!                    - (tc(i)*consumption_gross_new(i) + tk(i)*r_bar(i)*sum_priv_sv(i-1)/(nu(i)*gam_t(i))&
!                    + sum(N_t_j(1:bigJ,i)*labor_tax_j_vfi(1:bigJ,i))/bigL(i)) & 
!                    - upsilon(i)/(bigL(i)/N_t(i)) 
!        else
!            debt(i) = debt_constr*y(i)                    
!            tk(i)  = (subsidy(i) + g(i) + (1 + r_bar(i))*debt(i-1)/(nu(i)*gam_t(i)) - debt(i) &
!                        -upsilon(i)/(bigL(i)/N_t(i))  - tc(i)*consumption_gross_new(i)&
!                        - sum(N_t_j(1:bigJ,i)*labor_tax_j_vfi(1:bigJ,i))/bigL(i))&
!                        /(r_bar(i)*sum_priv_sv(i-1)/(nu(i)*gam_t(i)))
!        endif
!        debt_share(i) = debt(i)/y(i)   
!        deficit(i) = debt(i) - debt(i-1)/(nu(i)*gam_t(i))
!        sum_priv_sv(i) = k(i+1)*gam_t(i+1)*nu(i+1) + debt(i) - PillarII(i)
!    enddo
!    sum_priv_sv(n_p+2:bigT) = k(n_p+2:bigT)*gam_t(n_p+2:bigT)*nu(n_p+2:bigT) + debt(n_p+2:bigT) - PillarII(n_p+2:bigT)
!    debt_share(n_p+2:bigT) = debt(n_p+2:bigT)/y(n_p+2:bigT)
!    do i = n_p + 2,bigT,1
!        tk(i)  = tk_new 
!    enddo
        
end select
