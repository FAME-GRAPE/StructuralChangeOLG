  ! write on the screen
        write(*,*) '******* STEADY STATE PAYG *******'
        write(*,*)
        write(*,*) '*********************************'
        write(*,*) 'Calibration:'
        write(*,*)
        !if  (bigJ == 16)  then ! US
        !    write(*,'(A25,F10.7,A)') ' 100*sum_b/y =  ', 100*sum_b_ss/y_ss , '  |  Should be 5.2%'
        !else
            write(*,'(A15,F20.10,A)') ' sum_b/y = ', sum_b_ss/y_ss, '  |  Should be 5%'
        !endif
        write(*,'(A23,F10.7,A)') ' 100*subsidy/y = 0 +', 100*subsidy_ss/y_ss, '  |  Should be 0.0%'
        if (bigJ == 16)  then
            if (switch_vf > 0) then 
            write(*,'(A20,F10.7,A)') ' average hours =  33 +', 100*sum(N_ss_j(1:jbar_ss-1)*lab_ss_j_vfi(1:jbar_ss-1))/sum(N_ss_j(1:jbar_ss-1)) -33d0, '  |  Should be 33%' 
            else 
            write(*,'(A20,F10.7,A)') ' average hours = ', avg_hours_ss, '  |  Should be 33%'    
            endif
        else
            write(*,'(A20,F10.7,A)') ' average hours = ', avg_hours_ss !, '  |  Should be 56.8%' 
        endif
            if (bigJ == 4) then
                write(*,'(A20,F10.7,A)') ' 1 + r_bar_ss = ', (1 + r_bar_ss)**0.05_dp, '  |  Should be 7.8%'
                write(*,'(A20,F10.7,A)') ' r_ss = ', r_ss**0.05_dp
            elseif (bigJ == 20)  then   
                write(*,'(A20,F10.7,A)') ' 1 + r_bar_ss = ', (1 + r_bar_ss)**0.25_dp, '  |  Should be 7.8%'
                write(*,'(A20,F10.7,A)') ' r_ss = ', r_ss**0.25_dp
           elseif (bigJ == 16)  then   ! US
                write(*,'(A25,F10.7,A)') ' 1 + r_bar_ss = ', 100*((1 + r_bar_ss)**0.2_dp -1d0), '  |  Should be 5.5%'
                write(*,'(A25,F10.7,A)') ' r_ss = ', r_ss**0.2_dp
                write(*,'(A25,F10.7,A)') ' investment rate = 21 + ', 100*(y_ss - consumption_ss_gross - g_ss)/y_ss -21.0d0, ' %  |  Should be 21%'

            else
                write(*,'(A20,F10.7,A)') ' 1 + r_bar_ss = ', 1 + r_bar_ss, '  |  Should be 3.0%'
                write(*,'(A20,F10.7,A)') ' r_ss = ', r_ss
                
                write(*,'(A20,F10.7,A)') ' investment rate = ', (y_ss - consumption_ss_gross - g_ss)/y_ss !, '  |  Should be 21%'
                write(*,'(A20,F10.7,A)') ' investment rate = ', ((gam_ss+depr-1)*k_ss)/y_ss
  
        endif       

        write(*,'(A20,F10.7,A)') 'Tax_Lw/y_ss = ', 100*tL_ss*(1 - t1_ss)*(1-alpha_ss), '  |  Should be 7.3%'
        write(*,'(A20,F10.7,A)') 'Tax_K/y_ss = ',  100*tk_ss*r_bar_ss*sum_priv_sv_ss/gam_ss/y_ss , '  |  Should be 4.0%'
        write(*,'(A20,F10.7,A)') 'Tax_C/y_ss = ',  100*tc_ss*consumption_ss_gross/y_ss , '  |  Should be 10.7%'
        write(*,'(A20,F10.7,A)') 'contribution/y_ss = ',  100*contribution_ss/y_ss , '  |  Should be 12.0%'
        write(*,'(A20,F10.7,A)') 'b1_ss/w_bar = ',  100*b1_ss(jbar_ss, 1, 1)/avg_w_ss
        write(*,'(A20,F10.7,A)') 'mean b2_ss/w_bar = ',  100*sum(sum_b2_ss_omega(jbar_ss, :)*(omega_share_ss(jbar_ss, :)/sum(omega_share_ss(jbar_ss, :))))/avg_w_ss
        write(*,'(A20,F10.7,A)') 'mean b_ss/w_bar = ',  100*sum(sum_b_ss_omega(jbar_ss, :)*(omega_share_ss(jbar_ss, :)/sum(omega_share_ss(jbar_ss, :))))/avg_w_ss
        !write(*,'(A20,F10.7,A)') 'Tax_Lw/y_ss = 9.2 +', 100*sum(N_ss_j*labor_tax_ss_j_vfi)/bigL_ss/y_ss -9.2d0, '  |  Should be 9.2%' 

        write(*,*) '*********************************'
        write(*,*)
        write(*,'(A30,F10.7,A)') ' err_ss = ', err_ss
        write(*,'(A30,F10.7,A)') ' k_ss = ', k_ss
        write(*,'(A30,F10.7,A)') ' y_ss = ', y_ss
        write(*,'(A30,F10.7,A)') ' w_bar_ss = ', w_bar_ss
        write(*,'(A30,F10.7,A)') ' capital output ratio = ', k_ss/y_ss
        write(*,'(A30,F10.7,A)') ' capital labour ratio = ', k_ss/bigL_ss
        write(*,'(A30,F10.7,A)') ' labour share = ', w_bar_ss/y_ss
        write(*,'(A30,F10.7,A)') ' capital share = ', ((r_bar_ss + depr)*k_ss/y_ss)
        !write(*,'(A30,F10.7,A)') ' l_ss_pen_j(jbar-1) = ', l_ss_pen_j(jbar_ss-1) ! Powinna być suma?
        write(*,'(A30,F10.7,A)') ' l_ss_j(jbar-1) = ', l_ss_j(jbar_ss-1, 1) ! Powinna być suma?
        write(*,'(A30,F10.7,A)') ' u_ss = ', u_ss
        write(*,*)
        write(*,'(A30,F16.7,A)') ' N_ss = ', N_ss
        write(*,'(A30,F16.7,A)') ' bigL_ss = ', bigL_ss
        write(*,'(A30,F16.7,A)') ' bigK_ss = ', bigK_ss
        write(*,'(A30,F16.7,A)') ' bequest_ss = ', bequest_ss
        write(*,*)
        write(*,'(A40,F10.7,A)') ' 100*(upsilon_ss*N_ss/bigL_ss)/y_ss = ', 100*(upsilon_ss*N_ss/bigL_ss)/y_ss
        write(*,*)
        write(*,'(A30,F16.7,A)') ' g_ss = ', g_ss
        write(*,'(A30,F16.7,A)') ' upsilon_ss = ', upsilon_ss
        write(*,'(A30,F16.7,A)') ' savings_top_ten = ',  savings_top_ten(10)/sum(N_ss_j*asset_pom_ss_j)
        write(*,'(A30,F16.7,A)') ' savings_top_100 = ',  savings_top_100(100)/sum(N_ss_j*asset_pom_ss_j)
        write(*,'(A30,F16.7,A)') 'private_wealth/y_ss ratio =', sum(N_ss_j*asset_pom_ss_j)/y_ss
        write(*,'(A30,F16.7,A)') ' top_ten = ',  top_ten(10)
        write(*,'(A30,F16.7,A)') ' tc = ',  tc_ss
        write(*,'(A30,F16.7,A)') ' tk = ',  tk_ss
        write(*,'(A30,F16.7,A)') ' tl = ',  tl_ss
        write(*,'(A30,F10.7,A)') ' rho = ', rho_ss
        write(*,'(A30,F10.7,A)') ' b_scale_factor_ss = ', b_scale_factor_ss
        write(*,'(A30,F16.7,A)') ' t1 = ',  t1_ss
        write(*,'(A30,F16.7,A)') ' t1_contrib = ',  t1_ss_contrib
        write(*,'(A30,F16.7,A)') ' lambda = ',  lambda
        write(*,'(A30,F16.7,A)') ' residual = ', 100* abs((y_ss - consumption_ss_gross - g_ss)/y_ss - ((nu_ss*gam_ss+depr-1)*k_ss)/y_ss) 
        !print *, gam_ss
        write(*,*) '********************************************'

OPEN (unit=666, FILE = version//closure//"makro_db.csv")
    write(666, '(A)') "Parameters"
    write(666, '(A)') "Utility;Beta;Labor indiv.;Tax-ret link;Bequests"

    write(666, '(A)', advance='no')"C_D;"
    write(666, '(F20.10,A)', advance='no') beta, ";"

    write(666, '(A)', advance='no')"C_D;"
    
    write(666, '(A)')"cohort;"


    write(666, '(A)') ""
    write(666, '(A)') "Outcomes"
    write(666, '(A)') "y;k;c;i;bigL;u;r_bar;gam_ss;average hours;r-g"
    write(666, '(F20.10,A)', advance='no') y_ss, ";"
    write(666, '(F20.10,A)', advance='no') k_ss, ";"
    write(666, '(F20.10,A)', advance='no') consumption_ss_gross/(y_ss), ";"
    write(666, '(F20.10,A)', advance='no') ((gam_ss+depr-1)*k_ss)/y_ss, ";"
    write(666, '(F20.10,A)', advance='no') bigL_ss, ";"
    write(666, '(F20.10,A)', advance='no') u_ss, ";"
    write(666, '(F20.10,A)', advance='no') 1+r_bar_ss, ";"
    write(666, '(F20.10,A)', advance='no') gam_ss, ";"
    write(666, '(F20.10,A)', advance='no') bigL_ss/sum(N_ss_j(1:jbar_ss-1)), ";"
    write(666, '(F20.10,A)', advance='no') 1+r_bar_ss-gam_ss, ";"


    write(666, '(A)') ""
    write(666, '(A)') "Lifecycle"
    write(666, '(A)') "yr;c;l;s;V;disc"
    do j = 1, bigJ
        write(666, '(I2,A,F20.10,A,F20.10,A,F20.10,A,F20.10,A,F20.10)') j, ";", sum(c_ss_j(j, :)), ";", sum(l_ss_j(j, :)), ";", sum(s_ss_j(j, :)), ";", V_ss_j_vfi(j), ";", beta*delta**(j-1)*(pi_ss(j)/pi_ss(1)) ! Powinny byc sumy po omegach? i po delta_mult? [TODO]
    enddo
CLOSE(666)



!open(unit = 104, file= "top_ten.csv")
!write(104, '(A)') "t;sv;top_ten;savings"
!do t = 1, 10,1  
!    write(104, '(I2,A,F20.10,A,F20.10,A,F20.10)') t, ";", savings_top_ten(t)/sum(N_ss_j*asset_pom_ss_j), ";", top_ten(t),";", savings_top_ten(t)
!enddo      
!close(104)
!
!open(unit = 104, file= "top_100.csv")
!write(104, '(A)') "t;sv;top_100;savings"
!do t = 1, 100,1  
!    write(104, '(I2,A,F20.10,A,F20.10,A,F20.10)') t, ";", savings_top_100(t)/sum(N_ss_j*asset_pom_ss_j), ";", top_100(t),";", savings_top_100(t)
!enddo      
!close(104)
!
!open(unit = 104, file= "top_ten_cohort.csv")
!write(104, '(A)') "t;sv_poor; svpom;sv_rich"
!do t = 1, bigJ,1  
!    write(104, '(I2,A,F20.10,A,F20.10,A,F20.10)') t, ";", savings_cohort_ten(1,t), ";",savings_cohort_ten(3,t),";", savings_cohort_ten(2,t)
!enddo      
!close(104)
!open(unit = 104, file= "top_ten_cohort_cons.csv")
!write(104, '(A)') "t;c_poor; c;c_rich"
!do t = 1, bigJ,1  
!    write(104, '(I2,A,F20.10,A,F20.10,A,F20.10)') t, ";", consumption_top_ten(1,t), ";",consumption_top_ten(3,t),";", consumption_top_ten(2,t)
!enddo      
!close(104)
!
!OPEN (unit=1, FILE = "gini_weight_sv.txt")
!OPEN (unit=2, FILE = "gini_sv.txt")
!        
!do i_o = 1, omega_dim
!    do j = 1, bigJ, 1
!        do ia = 0, n_a, 1
!            write(1,*) gini_weight_sv(j,ia, i_o)
!            write(2,*) sv(ia)
!        enddo        
!    enddo
!enddo
!
!close(1)
!close(2)

!open(unit = 104, file= "labor.csv")
!write(104, '(A)') "l_ss; omega"
!do i_o = 1, omega_dim
!    do j = 1, bigJ, 1
!        write(104, '(F20.10,A,F20.10)')l_ss_j(j, i_o), ";", omega_ss(j, i_o)
!    enddo  
!enddo
!close(104)
!
!open(unit = 104, file= "consumption.csv")
!write(104, '(A)') "c_ss; omega; o; j"
!do i_o = 1, omega_dim
!    do j = 1, bigJ, 1
!        write(104, '(F20.10,A,F20.10,A,I1,A,I2)')c_ss_j(j, i_o), ";", omega_ss(j, i_o), ";", i_o, ";", j
!    enddo  
!enddo
!close(104)
!
!open(unit = 104, file= "assets_"//tostr(ss)//".csv")
!write(104, '(A)') "s_ss; omega; o; j"
!do i_o = 1, omega_dim
!    do j = 1, bigJ, 1
!        write(104, '(F20.10,A,F20.10,A,I1,A,I2)')s_ss_j(j, i_o), ";", omega_ss(j, i_o), ";", i_o, ";", j
!    enddo  
!enddo
!close(104)
!
!open(unit = 104, file= "gini.csv")
!write(104, '(A)') "gini_weight_sv; sv; age_sv; omega_sv; omega_share_sv"
!do i_o = 1, omega_dim
!    do j = 1, bigJ, 1
!        do ia = 0, n_a, 1 
!            if (gini_weight_sv(j, ia, i_o) /= 0) then
!                write(104, '(F20.10,A,F20.10,A,I2,A,F20.10,A,F20.10)') gini_weight_sv(j, ia, i_o), ";", sv(ia), ";", j, ";", omega_ss(j, i_o), ";", omega_share_ss(j, i_o) ! *(omega_share(j, i_o)/sum(omega_share(j, :)))
!            endif
!        enddo  
!    enddo
!enddo    
!close(104)


!open(unit = 104, file= "gini_consumption.csv")
!write(104, '(A)') "gini_weight;consumption;income;assets; age; productivity; omega; omega_share; prob; N_ss"
!do i_o = 1, omega_dim
!    do j = 1, bigJ, 1
!        do ia = 0, n_a, 1 
!            do i_aime = 0, n_aime, 1 
!                do ip = 1, n_sp, 1
!                    if (gini_weight_consumption(j, ia, i_aime, ip, i_o) /= 0) then
!                        write(104, '(F20.10,A,F20.10,A,F20.10,A,F20.10,A,I2,A,I2,A,F20.10,A,F20.10,A,F20.10,A,F20.10)') gini_weight_consumption(j, ia, i_aime, ip, i_o), ";", c_ss_g(j, ia, i_aime, ip, i_o),";", gini_income(j, ia, i_aime, ip),";" ,sv(ia), ";", j ,";", ip,";", & 
!                                                                                                        omega_ss(j, i_o), ";", omega_share_ss(j, i_o), ";",  prob_ss(j, ia, i_aime, ip), ";", N_ss_j(j)
!                    endif
!                enddo
!            enddo
!        enddo  
!    enddo
!enddo
!close(104)

!open(unit = 111, file= "prob_trans_1.csv")
!         do j = 1, bigJ,1  
!            do i = 0,n_a-1,1
!                write(111, '(F20.10)', advance='no') sum(prob_ss(j, i, :,  :))
!                write(111, '(A)', advance='no') ";"
!            enddo
!            write(111, '(F20.10)') sum(prob_ss(j, n_a, :, :))
!         enddo       
!close(111)
!!
!!
!open(unit = 104, file= "consumption_agr.csv")
!write(104, '(A)') "c_ss"
!do i_o = 1, omega_dim
!    do j = 1, bigJ, 1
!        write(104, '(F20.10,A,F20.10)')c_ss_j(j, i_o)
!    enddo  
!enddo
!close(104)
!
!open(unit = 104, file= "assets_agr.csv")
!write(104, '(A)') "s_ss"
!do i_o = 1, omega_dim
!    do j = 1, bigJ, 1
!        write(104, '(F20.10,A,F20.10)')s_ss_j(j, i_o)
!    enddo  
!enddo
!close(104)
