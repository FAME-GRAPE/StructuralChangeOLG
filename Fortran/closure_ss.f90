select case (switch_residual)
    case(0)
!       case 0 - upsilon is residual
        Tax_ss = tc_ss*consumption_ss_gross + tk_ss*r_bar_ss*sum_priv_sv_ss/(gam_ss*nu_ss) + sum(N_ss_j*labor_tax_ss_j_agr)/bigL_ss  !+ tL_ss*sum_b_ss
        deficit_ss = ((nu_ss*gam_ss - 1)/(gam_ss*nu_ss))*debt_ss ! deficit is BI from Lyx
        upsilon_ss = (subsidy_ss + g_ss + (1 + r_bar_ss)*debt_ss/(gam_ss*nu_ss) - debt_ss - Tax_ss)*bigL_ss/(sum(N_ss_j))
        upsilon_r_ss = (upsilon_ss*(sum(N_ss_j))/bigL_ss)/y_ss
        if (param_ss == 1) then
            upsilon_r_ss_1 = upsilon_ss
        endif
    case(1)             
!       case 1 - tC is residual
        if (switch_ref_run_now == 0) then
            upsilon_ss = upsilon_r_ss_1*y_ss*bigL_ss/(sum(N_ss_j))
            upsilon_r_ss_nr = upsilon_ss
        else 
            upsilon_ss = upsilon_r_ss_nr
        endif

        deficit_ss = ((nu_ss*gam_ss - 1)/(gam_ss*nu_ss))*debt_ss ! deficit is BI from Lyx
        tc_ss = (subsidy_ss + g_ss + (1 + r_bar_ss)*debt_ss/(gam_ss*nu_ss) - debt_ss &
                - tk_ss*r_bar_ss*sum_priv_sv_ss/(gam_ss*nu_ss) - sum(N_ss_j*labor_tax_ss_j_agr)/bigL_ss  &
                - upsilon_ss/(bigL_ss/(sum(N_ss_j))))/consumption_ss_gross 
        !write(*,*)bigL_ss, sum(N_ss_j)
        !read(*,*)
    case(2)             
!       case 2 - tl is residual
        upsilon_ss = upsilon_r_ss_1*y_ss*bigL_ss/(sum(N_ss_j))
        deficit_ss = ((nu_ss*gam_ss - 1)/(gam_ss*nu_ss))*debt_ss ! deficit is BI from Lyx
        tl_ss = (subsidy_ss + g_ss + (1 + r_bar_ss)*debt_ss/(gam_ss*nu_ss) - debt_ss &
                - tk_ss*r_bar_ss*sum_priv_sv_ss/(gam_ss*nu_ss) - tc_ss*consumption_ss_gross- sum(N_ss_j*sum(lw_ss_j_vfi(1:bigJ)-lw_lambda_ss_j_vfi(1:bigJ)))/bigL_ss  &
                - upsilon_ss/(bigL_ss/(sum(N_ss_j))))/(sum(N_ss_j*lw_lambda_ss_j_vfi(1:bigJ))/bigL_ss)
     
    case(3)             
!       case 3 - tk is residual
        upsilon_ss = upsilon_r_ss_1*y_ss*bigL_ss/(sum(N_ss_j))
        deficit_ss = ((nu_ss*gam_ss - 1)/(gam_ss*nu_ss))*debt_ss ! deficit is BI from Lyx
        tk_ss = (subsidy_ss + g_ss + (1 + r_bar_ss)*debt_ss/(gam_ss*nu_ss) - debt_ss &
                - tc_ss*consumption_ss_gross - sum(N_ss_j*labor_tax_ss_j_vfi)/bigL_ss &
                - upsilon_ss/(bigL_ss/(sum(N_ss_j))))/(r_bar_ss*sum_priv_sv_ss/(gam_ss*nu_ss))
    case(4)             
!       case 4 - increase contrib is residual, t1_ss keep subsidy equal 0, tc is residual
! subsidy_ss_j = b1_ss_j  - t1_ss*omega_ss*w_bar_ss*l_ss_j  
! subsidy_ss = sum(subsidy_ss_j*N_ss_j(1:bigJ))/bigL_ss
    if (switch_type == 0) then ! pension system may be inbalanced in 2nd ss 
        t1_ss = sum(N_ss_j*sum_b_weight_ss*b1_ss_j)/sum(N_ss_j*sum(omega_ss, dim = 2)*w_bar_ss*sum(l_ss_j, dim = 2)) ! sum_b_wieght should be probably from agr _om version
        Tax_ss = tc_ss*consumption_ss_gross + tk_ss*r_bar_ss*sum_priv_sv_ss/(gam_ss*nu_ss) + sum(N_ss_j*labor_tax_ss_j_vfi)/bigL_ss !+ tL_ss*sum_b_ss
        deficit_ss = ((nu_ss*gam_ss - 1)/(gam_ss*nu_ss))*debt_ss ! deficit is BI from Lyx
        upsilon_ss = (subsidy_ss + g_ss + (1 + r_bar_ss)*debt_ss/(gam_ss*nu_ss) - debt_ss - Tax_ss)*bigL_ss/(sum(N_ss_j)) 
        upsilon_r_ss = (upsilon_ss*(sum(N_ss_j))/bigL_ss)/y_ss
    else ! pension system balanced by definition 
        Tax_ss = tc_ss*consumption_ss_gross + tk_ss*r_bar_ss*sum_priv_sv_ss/(gam_ss*nu_ss) + sum(N_ss_j*labor_tax_ss_j_vfi)/bigL_ss !+ tL_ss*sum_b_ss
        deficit_ss = ((nu_ss*gam_ss - 1)/(gam_ss*nu_ss))*debt_ss ! deficit is BI from Lyx
        upsilon_ss = (subsidy_ss + g_ss + (1 + r_bar_ss)*debt_ss/(gam_ss*nu_ss) - debt_ss - Tax_ss)*bigL_ss/(sum(N_ss_j)) 
        upsilon_r_ss = (upsilon_ss*(sum(N_ss_j))/bigL_ss)/y_ss
    endif
        
    case(5)             
 !        case 5 - btax is residual
!         benefit taxation keep subsidy equal 0, tc is residual
        
        if (iter > 1) then ! otherwise b1_ss equal zero 
            b_scale_factor_ss =sum(N_ss_j*sum(omega_ss, dim=2)*w_bar_ss*sum(l_ss_j, dim=2)*t1_ss)/sum(N_ss_j*b1_ss_j) 
        endif
        Tax_ss = tc_ss*consumption_ss_gross + tk_ss*r_bar_ss*sum_priv_sv_ss/(gam_ss*nu_ss) + sum(N_ss_j*labor_tax_ss_j_vfi)/bigL_ss !+ tL_ss*sum_b_ss
        deficit_ss = ((nu_ss*gam_ss - 1)/(gam_ss*nu_ss))*debt_ss ! deficit is BI from Lyx
        upsilon_ss = (subsidy_ss + g_ss + (1 + r_bar_ss)*debt_ss/(gam_ss*nu_ss) - debt_ss - Tax_ss)*bigL_ss/(sum(N_ss_j))
        upsilon_r_ss = (upsilon_ss*(sum(N_ss_j))/bigL_ss)/y_ss
                
    case(9)
!        case 9 - g is residual
        upsilon_ss = upsilon_r_ss_1*y_ss*bigL_ss/(sum(N_ss_j))
        Tax_ss = tc_ss*consumption_ss_gross + tk_ss*r_bar_ss*sum_priv_sv_ss/(gam_ss*nu_ss) + sum(N_ss_j*labor_tax_ss_j_agr)/bigL_ss  !+ tL_ss*sum_b_ss
        deficit_ss = ((nu_ss*gam_ss - 1)/(gam_ss*nu_ss))*debt_ss ! deficit is BI from Lyx
        g_ss_new  = -(subsidy_ss - upsilon_ss + (1 + r_bar_ss)*debt_ss/(gam_ss*nu_ss) - debt_ss - Tax_ss)
        !upsilon_ss = 0d0

        if (g_ss_new >= 0) g_ss = up_ss*g_ss + (1 - up_ss)*g_ss_new
        
    end select