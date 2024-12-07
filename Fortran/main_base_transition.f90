    write (*,*) 'We are performing base transition path' 

    !include 'switch_setting.f90'
    !call clear_globals
    !call globals         
!if (com /= 3 .or. com /= 4 .or. com /= 6 .or. com /= 7) then

    if (switch_run_1 == 1) then
        call steady(switch_residual_1, switch_param_1, switch_type_1, omega_ss_1, omega_share_ss_old, alpha_ss_old, rho_1,k_ss_1, r_ss_1, r_bar_ss_1, w_bar_ss_1, l_ss_j_1, w_ss_j_1, s_ss_j_1, c_ss_j_1, b_ss_j_1, upsilon_r_ss_1, t1_ss_1, g_per_capita_ss_1, b1_ss_j_1, b2_ss_j_1, pillar1_ss_j_1, pillar2_ss_j_1)       
    endif ! run_1
    !write(*,*) V_ss_j_vfi(1)

    ! penion system chracteristic for 2nd ss
        priv_share = 0d0 !0.5d0
        t1_ss_new = t1_ss_old !(1d0-priv_share )*t1_ss_old
        t1_ss_contrib = t1_ss_new
        !t2_ss_new = priv_share*t1_ss_old
        switch_pension = abs(switch_type_1 - switch_type_2) 
        
    switch_run_1 = 0 ! to be sure that we are running 2nd ss, in pfi procedure we are fullfiling 2nd part of transition
    if (switch_run_2 == 1) then
       call steady(switch_residual_2, switch_param_2, switch_type_2, omega_ss_2, omega_share_ss_new, alpha_ss_new, rho_2,  k_ss_2, r_ss_2, r_bar_ss_2, w_bar_ss_2, l_ss_j_2, w_ss_j_2, s_ss_j_2, c_ss_j_2, b_ss_j_2, upsilon_r_ss_2, t1_ss_2, g_per_capita_ss_2, b1_ss_j_2, b2_ss_j_2, pillar1_ss_j_2, pillar2_ss_j_2)
    endif ! run_2
    
    if (switch_run_2 == 1 .AND. switch_run_t == 1) then
       call transition_path_DB(switch_residual_t, switch_param_2, l_db, c_db, tax_c_db, r_db,  V_20_years_old_db, g_per_capita_db)
    endif
    
!endif