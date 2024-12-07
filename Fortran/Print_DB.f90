 ! 1990 on screen
        !write(*,*) '******* 1990*******'
        !write(*,*)
        !write(*,*) '*********************************'
        !write(*,*) 'Calibration:'
        !write(*,*)
        !    write(*,'(A15,F20.10,A)') ' sum_b/y = ', sum_b(11)/y(11), '  |  Should be 7%'
        !write(*,'(A23,F10.7,A)') ' 100*subsidy/y = 0 +', 100*subsidy(11)/y(11), '  |  Should be 0.0%'
        !    write(*,'(A20,F10.7,A)') ' average hours = ', bigL(11)/sum(Nn_(1:jbar_t(11)-1, 11)) !, '  |  Should be 56.8%' 
        !        write(*,'(A20,F10.7,A)') ' 1 + r_bar(11) = ', 1 + r_bar(11) !, '  |  Should be 3.0%'
        !        write(*,'(A20,F10.7,A)') ' r(11) = ', r(11)
        !        
        !        write(*,'(A20,F10.7,A)') ' investment rate = ', (y(11) - consumption_gross(11) - g(11))/y(11) !, '  |  Should be 21%'
        !        write(*,'(A20,F10.7,A)') ' investment rate = ', ((gam_t(11)+depr-1)*k(11))/y(11)
        !
        !write(*,'(A20,F10.7,A)') 'Tax_Lw/y(11) = ', 100*tL(11)*(1 - t1(11))*(1 - alpha(11)), '  |  Should be 7.3%'
        !write(*,'(A20,F10.7,A)') 'Tax_K/y(11) = ',  100*tk(11)*r_bar(11)*sum_priv_sv(11)/gam_t(11)/y(11) , '  |  Should be 4.0%'
        !write(*,'(A20,F10.7,A)') 'Tax_C/y(11) = ',  100*tc(11)*consumption_gross(11)/y(11) , '  |  Should be 10.7%'
        !write(*,'(A20,F10.7,A)') 'contribution/y(11) = ',  100*contribution(11)/y(11) , '  |  Should be 12.0%'
        !write(*,'(A20,F10.7,A)') 'b1(11)/w_bar = ',  100*b1(11)(jbar_t(11), 1, 1)/w_bar(11)
        !write(*,'(A20,F10.7,A)') 'mean b2(11)/w_bar = ',  100*sum(sum_b2(11)_omega(jbar_t(11), :)*(omega_share(11)(jbar_t(11), :)/sum(omega_share(11)(jbar_t(11), :))))/w_bar(11) 
        !write(*,'(A20,F10.7,A)') 'mean b(11)/w_bar = ',  100*sum(sum_b(11)_omega(jbar_t(11), :)*(omega_share(11)(jbar_t(11), :)/sum(omega_share(11)(jbar_t(11), :))))/w_bar(11)
        !!write(*,'(A20,F10.7,A)') 'Tax_Lw/y(11) = 9.2 +', 100*sum(Nn_(:, 11)*labor_tax(11)_j_vfi)/bigL(11)/y(11) -9.2d0, '  |  Should be 9.2%' 
        !
        !write(*,*) '*********************************'
        !write(*,*)
        !write(*,'(A30,F10.7,A)') ' err(11) = ', err(11)
        !write(*,'(A30,F10.7,A)') ' k(11) = ', k(11)
        !write(*,'(A30,F10.7,A)') ' y(11) = ', y(11)
        !write(*,'(A30,F10.7,A)') ' w_bar(11) = ', w_bar(11)
        !write(*,'(A30,F10.7,A)') ' capital output ratio = ', k(11)/y(11)
        !write(*,'(A30,F10.7,A)') ' capital labour ratio = ', k(11)/bigL(11)
        !write(*,'(A30,F10.7,A)') ' labour share = ', w_bar(11)/y(11)
        !write(*,'(A30,F10.7,A)') ' capital share = ', ((r_bar(11) + depr)*k(11)/y(11))
        !!write(*,'(A30,F10.7,A)') ' l(11)_pen_j(jbar-1) = ', l(11)_pen_j(jbar_t(11)-1) 
        !write(*,'(A30,F10.7,A)') ' l(11)_j(jbar-1) = ', l(11)_j(jbar_t(11)-1, 1) 
        !write(*,'(A30,F10.7,A)') ' u(11) = ', u(11)
        !write(*,*)
        !write(*,'(A30,F16.7,A)') ' bigN(11) = ', bigN(11)
        !write(*,'(A30,F16.7,A)') ' bigL(11) = ', bigL(11)
        !write(*,'(A30,F16.7,A)') ' bigK(11) = ', bigK(11)
        !write(*,'(A30,F16.7,A)') ' bequest(11) = ', bequest(11)
        !write(*,*)
        !write(*,'(A40,F10.7,A)') ' 100*(upsilon(11)*bigN(11)/bigL(11))/y(11) = ', 100*(upsilon(11)*bigN(11)/bigL(11))/y(11)
        !write(*,*)
        !write(*,'(A30,F16.7,A)') ' g(11) = ', g(11)
        !write(*,'(A30,F16.7,A)') ' upsilon(11) = ', upsilon(11)
        !write(*,'(A30,F16.7,A)') ' savings_top_ten = ',  savings_top_ten(10)/sum(Nn_(:, 11)*asset_pom(11)_j)
        !write(*,'(A30,F16.7,A)') ' savings_top_100 = ',  savings_top_100(100)/sum(Nn_(:, 11)*asset_pom(11)_j)
        !write(*,'(A30,F16.7,A)') 'private_wealth/y(11) ratio =', sum(Nn_(:, 11)*asset_pom(11)_j)/y(11)
        !write(*,'(A30,F16.7,A)') ' top_ten = ',  top_ten(10)
        !write(*,'(A30,F16.7,A)') ' tc = ',  tc(11)
        !write(*,'(A30,F16.7,A)') ' tk = ',  tk(11)
        !write(*,'(A30,F16.7,A)') ' tl = ',  tl(11)
        !write(*,'(A30,F10.7,A)') ' rho = ', rho(11)
        !write(*,'(A30,F10.7,A)') ' b_scale_factor(11) = ', b_scale_factor(11)
        !write(*,'(A30,F16.7,A)') ' t1 = ',  t1(11)
        !write(*,'(A30,F16.7,A)') ' t1_contrib = ',  t1(11)_contrib
        !write(*,'(A30,F16.7,A)') ' lambda = ',  lambda
        !write(*,'(A30,F16.7,A)') ' residual = ', 100* abs((y(11) - consumption_gross(11) - g(11))/y(11) - ((nu(11)*gam_t(11)+depr-1)*k(11))/y(11)) 
        !write(*,*) '********************************************'
    
    
    ! save in files

    OPEN (unit=1,  FILE = version//closure//"u_init_old_trans.txt")
    OPEN (unit=2,  FILE = version//closure//"u_all_trans.txt")
    OPEN (unit=3,  FILE = version//closure//"u20_trans.txt")
    OPEN (unit=4,  FILE = version//closure//"y_trans.txt")
    OPEN (unit=5,  FILE = version//"kapital_trans.txt")
    OPEN (unit=6,  FILE = version//closure//"r_trans.txt") 
    OPEN (unit=8,  FILE = version//closure//"subsidy_share_trans.txt")
    OPEN (unit=9,  FILE = version//closure//"benefits_trans.txt")
    OPEN (unit=10, FILE = version//closure//"bigL_trans.txt")
    OPEN (unit=11, FILE = version//closure//"replacement_trans.txt")
    OPEN (unit=12, FILE = version//closure//"upsilon_trans.txt")
    OPEN (unit=13, FILE = version//closure//"tC_trans.txt")
    OPEN (unit=14, FILE = version//closure//"tL_trans.txt")
    OPEN (unit=15, FILE = version//closure//"tK_trans.txt")
    OPEN (unit=16, FILE = version//closure//"contrib_to_gdp_trans.txt")
    OPEN (unit=17, FILE = version//closure//"debt_share_trans.txt")
    !OPEN (unit=18, FILE = version//closure//"debt_cost_share_trans.txt")
    OPEN (unit=18, FILE = version//closure//"bigN_trans.txt")
    OPEN (unit=19, FILE = version//closure//"bigK_trans.txt")
    OPEN (unit=29, FILE = version//closure//"bequest.txt")
    
    OPEN (unit=20, FILE = version//closure//"upsilon_share_trans.txt")
    OPEN (unit=21, FILE = version//closure//"replacement2_trans.txt") !rozumiane jako pierwsza emerytura do ostatniej placy
    OPEN (unit=23, FILE = version//closure//"g_trans.txt")
    OPEN (unit=21, FILE = version//closure//"C_trans.txt")
    OPEN (unit=24, FILE = version//closure//"g_share_trans.txt")
    OPEN (unit=25, FILE = version//closure//"rbar_trans.txt")
    
    OPEN (unit=26, FILE = version//closure//"tC_tax_revenue.txt")
    OPEN (unit=27, FILE = version//closure//"tL_tax_revenue.txt")
    OPEN (unit=28, FILE = version//closure//"tK_tax_revenue.txt")
    OPEN (unit=31, FILE = version//closure//"gam_cum.txt")
    OPEN (unit=32, FILE = version//closure//"savings.txt")
    OPEN (unit=33, FILE = version//closure//"_unemployment_rate.txt")
    OPEN (unit=34, FILE = version//closure//"investment_rate.txt")
    
    OPEN (unit=22, FILE = version//closure//"u_j_trans.csv")
    OPEN (unit=60, FILE = version//closure//"l_j_trans.csv")
    OPEN (unit=61, FILE = version//closure//"c_j_trans.csv")
    OPEN (unit=62, FILE = version//closure//"b_j_trans.csv")
    OPEN (unit=63, FILE = version//closure//"sv_j_trans.csv")
    OPEN (unit=64, FILE = version//closure//"mean_wage.txt")

        
    do i = 2,bigJ-1,1
        write(1, '(F20.10)')  u_init_old(i) 
    enddo

    do i = 1,bigJ+n_p,1
        write(2, '(F20.10)')  u_all(i) 
    enddo

    do i = 1,n_p+3,1 
        write(3,  '(F20.10)') u(i) 
        write(4,  '(F20.10)') y(i) 
        write(5,  '(F20.10)') k(i) 
        write(6,  '(F20.10)') r_bar(i)  
        write(8,  '(F20.10)') subsidy(i)/y(i)
        write(9,  '(F20.10)') sum_b(i)/y(i)
        write(10, '(F20.10)') bigL(i)/100 
        write(11, '(F20.10)') replacement(i)
        write(12, '(F20.10)') upsilon(i)
        write(13, '(F20.10)') tC(i)
        write(14, '(F20.10)') tL(i)
        write(15, '(F20.10)') tK(i)
        write(16, '(F20.10)') contribution(i)/y(i)
        write(17, '(F20.10)') debt_share(i)
        !write(18, '(F20.10)') ((1 + r_bar(i))*debt(max(i-1,1))/(nu(i)*gam_t(i)) - debt(i))/y(i)
        write(18, '(F20.10)') N_t(i)
        write(19, '(F20.10)') bigK(i) 
        write(20, '(F20.10)') upsilon(i)/(bigL(i)/N_t(i))/y(i) 
        write(21, '(F20.10)') replacement2(i)       
        write(22, '(F20.10)') consumption_gross(i)
        write(23, '(F20.10)') g(i)
        write(24, '(F20.10)') g(i)/y(i)
        write(25, '(F20.10)') (1d0 + r_bar(i))**(1d0/real(zbar)) - 1d0
        write(26, '(F20.10)') tc(i)*consumption_gross(i)/y(i)
        write(27, '(F20.10)') sum(N_t_j(1:bigJ,i)*labor_tax_j_vfi(1:bigJ,i))/bigL(i)/y(i)
        write(28, '(F20.10)') tk(i)*r_bar(i)*sum_priv_sv(i)/(nu(i)*gam_t(i))/y(i)
        write(29, '(F30.10)') bequest(i)
        write(30, '(F20.16)') t1(2,i)
        write(31, '(F20.10)') gam_cum(i)
        write(32, '(F20.10)') savings_pom !(i)
        write(33, '(F20.10)') unemployment_rate(i)
        write(34, '(F20.10)') (y(i) - consumption_gross(i) - g(i))/y(i)
        write(64, '(F20.10)') avg_w(i) !w_bar(i)/gam_corrector(i)
    enddo
do i_o = 1, omega_dim
    do j = 1,bigJ,1
        do i = 1,bigT-1,1
            !write(22, '(F20.10)', advance='no') u_j(j,i)
            !write(22, '(A)', advance='no')";"
            write(60, '(F20.10)', advance='no') l_j(j, i_o, i)
            write(60, '(A)', advance='no')";"
            write(61, '(F20.10)', advance='no') c_j(j, i_o, i)
            write(61, '(A)', advance='no')";"
            write(62, '(F20.10)', advance='no') sum_b_trans(j, i_o, i)
            write(62, '(A)', advance='no')";"
            write(63, '(F20.10)', advance='no') sv_j(j, i_o, i)
            write(63, '(A)', advance='no')";"
        enddo
        !write(22, '(F20.10)') u_j(j,bigT)
        write(60, '(F20.10,A,I2,A,I2)') l_j(j, i_o, bigT), ";", j, ';', i_o
        write(61, '(F20.10,A,I2,A,I2)') c_j(j, i_o, bigT), ";", j, ';', i_o
        write(62, '(F20.10,A,I2,A,I2)') sum_b_trans(j, i_o, bigT), ";", j, ';', i_o
        write(63, '(F20.10,A,I2,A,I2)') sv_j(j, i_o, bigT), ";", j, ';', i_o
    enddo
enddo
    CLOSE(1)
    CLOSE(2)
    CLOSE(3)
    CLOSE(4)
    CLOSE(5)
    CLOSE(6)
    CLOSE(7)
    CLOSE(8)
    CLOSE(9)
    CLOSE(10)
    CLOSE(11)
    CLOSE(12)
    CLOSE(13)
    CLOSE(14)
    CLOSE(15)
    CLOSE(16)
    CLOSE(17)
    CLOSE(18)
    CLOSE(19)
    CLOSE(20)
    CLOSE(21)
    CLOSE(22)
    CLOSE(23)
    CLOSE(24)
    CLOSE(25)
    CLOSE(26)
    CLOSE(27)
    CLOSE(28)
    CLOSE(29)
    CLOSE(60)
    CLOSE(61)
    CLOSE(62)
    CLOSE(63)
    CLOSE(64)
    close(32)
    close(33)
    close(34)
    
!call csv_2d_main_write("bigL_type.csv", bigL_pom)
!open(unit = 104, file= "bigL_type.csv")
!if (omega_dim == 1) then
!    write(104, '(A)') "l_omega_1"
!    do i = 1, bigT
!        write(104, '(F20.10)') bigL_pom(1, i)
!    enddo
!elseif (omega_dim == 2) then
!    write(104, '(A)') "l_omega_1; l_omega_2"
!    do i = 1, bigT
!        write(104, '(F20.10,A,F20.10)') bigL_pom(1, i), ";", bigL_pom(2, i)
!    enddo
!elseif (omega_dim == 4) then
!    write(104, '(A)') "he_se; le_se; he_ma; le_ma"
!    do i = 1, bigT
!        write(104, '(F20.10,A,F20.10,A,F20.10,A,I2)') bigL_pom(1, i), ";", bigL_pom(2, i), ";", bigL_pom(3, i), ";", bigL_pom(4, i)
!    enddo
!endif
!close(104)
!
!open(unit = 104, file= "euler_mat.csv")
!    write(104, '(A)') "euler_mat;j;i"
!    do i = 2, bigT
!        do j = 2, bigJ
!        write(104, '(F20.10,A,I2,A,I3)') (pi(j, i)/pi(j-1, i-1)*r(i)/gam_t(i)), ";", j, ";", i  !**(-theta) 
!    enddo
!        enddo
!close(104)

gini_weight_trans(:, :, :, 1) = gini_weight_sv
gini_weight_cons_trans(:, :, :, :, :, 1) = gini_weight_consumption

open(unit = 104, file= "gini_sv_trans.csv")
write(104, '(A)') "gini_weight_sv; sv; age; omega_sv; omega_share_sv; year"
open(unit = 105, file= "gini_sv2_trans.csv")
write(105, '(A)') "gini_weight_sv2; sv2; age; omega_sv; omega_share_sv; year"
do i = 10, 71
do i_o = 1, omega_dim
    do j = 1, bigJ, 1
        do ia = 0, n_a, 1 
            do i_aime = 0, n_aime, 1 
                do ip = 1, n_sp, 1
                    if (gini_weight_trans(j, ia, i_o, i) /= 0 .or. i == 1) then
                        !if (gini_weight_trans(j, ia, i_o, i) /= 0 .and. i == 1) print *, gini_weight_trans(j, ia, i_o, i)
                        write(104, '(F20.16,A,F20.10,A,I2,A,F20.10,A,F20.10,A,I3)') gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i), ";", sv(ia) + bequest_j(j, i), ";", j, ";", omega(j, i_o, i), ";", omega_share(j, i_o, i), ";", i ! *(omega_share(j, i_o)/sum(omega_share(j, :)))
                        write(105, '(F20.16,A,F20.10,A,I2,A,F20.10,A,F20.10,A,I3)') gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i), ";", svplus_trans(j, ia, i_aime, ip, i_o, i), ";", j, ";", omega(j, i_o, i), ";", omega_share(j, i_o, i), ";", i ! *(omega_share(j, i_o)/sum(omega_share(j, :)))

                            !write(108, '(F20.10,A,F20.10,A,F20.10,A,F20.10,A,F20.10,A,F20.10,A,F20.10,A,F20.10,A,I5,A,I5,A,I5,A,I5,A,I5,A,I5)') &
                                !sv(ia) + bequest_j_trans(j, ia, i_aime, ip, ir, id,i), ";", & !sav
                                !svplus_trans(j, ia, i_aime, ip, ir, id,i), ";", & !sav
                                !i, ";", & !year
                                !j , ";",  & !age
                                !ia , ";",  & !asset
                                !i_aime , ";",  & !aime
                                !ip , ";",  & !income
                    endif
                enddo
            enddo
        enddo  
    enddo
enddo    
enddo
close(104)
close(105)


open(unit = 104, file= "gini_cons_trans.csv")
!write(104, '(A)') "gini_weight_cons;cons;inc;assets; age; productivity; omega; omega_share; prob; bigN; year" ! cons = consumptiom; inc = income
write(104, '(A)') "gini_weight_cons; cons; age; year" ! cons = consumptiom
do i = 10, 71
do i_o = 1, omega_dim
    do j = 1, bigJ, 1
        do ia = 0, n_a, 1 
            do i_aime = 0, n_aime, 1 
                do ip = 1, n_sp, 1
                    if (gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i) /= 0 .or. i == 1) then
                        !if (gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i) /= 0 .and. i == 1) print *, gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i)    
                        !write(104, '(F20.10,A,F20.10,A,F20.10,A,F20.10,A,I2,A,I2,A,F20.10,A,F20.10,A,F20.10,A,F20.10,A,I3)') gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i), ";", c_trans(j, ia, i_aime, ip, i_o, i),";", gini_income_trans(j, ia, i_aime, ip, i_o, i),";" ,sv(ia), ";", j ,";", ip,";", & 
                        !                                                                                omega(j, i_o, i), ";", omega_share(j, i_o, i), ";",  prob_trans(j, ia, i_aime, ip, i_o, i), ";", N_t_j(j, i), ";", i
                        write(104, '(F20.10,A,F20.10,A,I2,A,I3)') gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i), ";", c_trans(j, ia, i_aime, ip, i_o, i),";", j ,";", i
                    endif
                enddo
            enddo
        enddo  
    enddo
enddo
enddo
close(104)

open(unit = 104, file= "gini_inc_trans.csv") ! excluding retirees
!write(104, '(A)') "gini_weight_cons;cons;inc;assets; age; productivity; omega; omega_share; prob; bigN; year" ! cons = consumptiom; inc = income
write(104, '(A)') "gini_weight_inc; inc; age; year" ! inc = income
do i = 10, 71
do i_o = 1, omega_dim
    do j = 1, bigJ, 1
        do ia = 0, n_a, 1 
            do i_aime = 0, n_aime, 1 
                do ip = 1, n_sp, 1
                    if (gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i) /= 0 .or. j < switch_fix_retirement_age) then
                        !if (gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i) /= 0 .and. i == 1) print *, gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i)    
                        !write(104, '(F20.10,A,F20.10,A,F20.10,A,F20.10,A,I2,A,I2,A,F20.10,A,F20.10,A,F20.10,A,F20.10,A,I3)') gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i), ";", c_trans(j, ia, i_aime, ip, i_o, i),";", gini_income_trans(j, ia, i_aime, ip, i_o, i),";" ,sv(ia), ";", j ,";", ip,";", & 
                        !                                                                                omega(j, i_o, i), ";", omega_share(j, i_o, i), ";",  prob_trans(j, ia, i_aime, ip, i_o, i), ";", N_t_j(j, i), ";", i
                        write(104, '(F20.10,A,F20.10,A,I2,A,I3)') gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i), ";", gini_income_trans(j, ia, i_aime, ip, i_o, i), ";", j ,";", i
                    endif
                enddo
            enddo
        enddo  
    enddo
enddo
enddo
close(104)

open(unit = 104, file= "gini_inc2_trans.csv") ! including retirees
!write(104, '(A)') "gini_weight_cons;cons;inc;assets; age; productivity; omega; omega_share; prob; bigN; year" ! cons = consumptiom; inc = income
write(104, '(A)') "gini_weight_inc2; inc2; age; year" ! inc2 = income with pensions
do i = 10, 71
do i_o = 1, omega_dim
    do j = 1, bigJ, 1
        do ia = 0, n_a, 1 
            do i_aime = 0, n_aime, 1 
                do ip = 1, n_sp, 1
                    if (gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i) /= 0) then
                        !if (gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i) /= 0 .and. i == 1) print *, gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i)    
                        !write(104, '(F20.10,A,F20.10,A,F20.10,A,F20.10,A,I2,A,I2,A,F20.10,A,F20.10,A,F20.10,A,F20.10,A,I3)') gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i), ";", c_trans(j, ia, i_aime, ip, i_o, i),";", gini_income_trans(j, ia, i_aime, ip, i_o, i),";" ,sv(ia), ";", j ,";", ip,";", & 
                        !                                                                                omega(j, i_o, i), ";", omega_share(j, i_o, i), ";",  prob_trans(j, ia, i_aime, ip, i_o, i), ";", N_t_j(j, i), ";", i
                        write(104, '(F20.10,A,F20.10,A,I2,A,I3)') gini_weight_cons_trans(j, ia, i_aime, ip, i_o, i), ";", gini_income_trans2(j, ia, i_aime, ip, i_o, i), ";", j ,";", i
                    endif
                enddo
            enddo
        enddo  
    enddo
enddo
enddo
close(104)

!! pension system colosure
!    OPEN (unit=1, FILE = version//closure//"b_scale_factor.txt")
!    OPEN (unit=2, FILE = version//closure//"t1_additional_contrib.txt")
!    OPEN (unit=3, FILE = version//closure//"upsilon.txt")
!        do i = 1,n_p+1,1
!            write(1, '(F20.10)')  b_scale_factor(i) 
!            write(2, '(F20.10)')  t1(1,i) - t1_contrib(1,i)
!            write(3, '(F20.10)')  upsilon(i) 
!    enddo
!
!    CLOSE(1)
!    CLOSE(2)
!    CLOSE(3)
!    
!
!    OPEN (unit=202,  FILE = version//closure//"savings_trans.txt")
!    OPEN (unit=203,  FILE = version//closure//"debt_trans.txt")
!    write(202,  '(F20.10)') (savings(1))/(nu(1)*gam_t(1))
!    write(203,  '(F20.10)') (debt(1))/(nu(1)*gam_t(1))
!    do i = 2,n_p+1,1 
!        write(202,  '(F20.10)') (savings(i-1))/(nu(i)*gam_t(i))
!        write(203,  '(F20.10)') (debt(i-1))/(nu(i)*gam_t(i))
!    enddo
!    write(202,  '(F20.10)') (savings(n_p+1))/(nu(n_p+1)*gam_t(n_p+1))
!    write(203,  '(F20.10)') (debt(n_p+1))/(nu(n_p+1)*gam_t(n_p+1))
!    CLOSE(202)
!    CLOSE(203)
!        
!
!    
open(unit = 104, file= version//closure//"tax_decomp.csv")
write(104, '(A)') "TL;tl_rate;TC;tc_rate;TK;tk_rate;upsilon;tcontrib;subsidy;g;debt;y"
do i = 1, n_p
    write(104, '(F20.10,A,F20.10,A,F20.10,A,F20.10,A,F20.10,A,F20.10,A,F20.10,A,F20.10,A,F20.10,A,F20.10,A,F20.10,A,F20.10)') &
                    !sum(N_t_j(1:bigJ,i)*labor_tax_j_vfi(1:bigJ,i))/bigL(i), ";", & ! tl     ! TODO przewazyc po omegach
                !sum(N_t_j(:,i)*labor_tax_j_vfi(1:bigJ,i))/bigL(i)*(omega_share(:, 1, i)/sum(omega_share(:, :, i), dim = 2)) + &
                !N_t_j(:,i)*labor_tax_j_vfi(1:bigJ,i))/bigL(i)*(omega_share(:, 2, i)/sum(omega_share(:, :, i), dim = 2)) + &
                !N_t_j(:,i)*labor_tax_j_vfi(1:bigJ,i))/bigL(i)*(omega_share(:, 3, i)/sum(omega_share(:, :, i), dim = 2)) + &
                !N_t_j(:,i)*labor_tax_j_vfi(1:bigJ,i))/bigL(i)*(omega_share(:, 4, i)/sum(omega_share(:, :, i), dim = 2)), dim=1)/bigL(i) , ";", & !tl
                sum(N_t_j(1:bigJ,i)*labor_tax_j_vfi(1:bigJ,i))/bigL(i)/bigL(i), ";", & ! tl
                tc(i)*consumption_gross_new(i), ";", & ! tc
                tc(i), ";", & ! tc
                tk(i)*r_bar(i)*sum_priv_sv(max(i-1,1))/(nu(i)*gam_t(i)) , ";",  & ! tk
                tk(i), ";", & ! tk
                upsilon(i)/(bigL(i)/N_t(i)) , ";", & !upsilon
                sum(N_t_j(:,i)*omega(:,1,i)*avg_w(i)*t1(:,i)*l_j(:,1,i)*(omega_share(:, 1, i)/sum(omega_share(:, :, i), dim = 2)) + &
                    N_t_j(:,i)*omega(:,2,i)*avg_w(i)*t1(:,i)*l_j(:,2,i)*(omega_share(:, 2, i)/sum(omega_share(:, :, i), dim = 2)) + &
                    N_t_j(:,i)*omega(:,3,i)*avg_w(i)*t1(:,i)*l_j(:,3,i)*(omega_share(:, 3, i)/sum(omega_share(:, :, i), dim = 2)) + &
                    N_t_j(:,i)*omega(:,4,i)*avg_w(i)*t1(:,i)*l_j(:,4,i)*(omega_share(:, 4, i)/sum(omega_share(:, :, i), dim = 2)), dim=1)/bigL(i) , ";", & !t1constrib
                subsidy(i) , ";", & ! subsdy
                g(i) , ";", & ! g 
                ((1 + r_bar(i))*debt(max(i-1,1))/(nu(i)*gam_t(i)) - debt(i)) , ";", & ! debt
                y(i)
enddo   
close(104) 

open(unit = 106, file = "N_out_check.csv")
write(106, '(A)') "N_o1; N_o2; N_o3; N_o4; j; i"
do i = 1, 106
    do j = 1, bigJ
        !write(106, '(F20.10,A,F20.10,A,F20.10,A,F20.10)') N_t_j(j,i)*(omega_share(j, 1, i)/sum(omega_share(j, :, i)), ";", N_t_j(j,i)*(omega_share(j, 2, i)/sum(omega_share(j, :, i)), ";", N_t_j(j,i)*(omega_share(j, 3, i)/sum(omega_share(j, :, i)) , ";", N_t_j(j,i)*(omega_share(j, 4, i)/sum(omega_share(j, :, i))
        write(106, '(F20.10,A,F20.10,A,F20.10,A,F20.10,A,I2,A,I3)') N_t_j(j,i)*omega_share(j, 1, i), ";", N_t_j(j,i)*omega_share(j, 2, i), ";", N_t_j(j,i)*omega_share(j, 3, i), ";", N_t_j(j,i)*omega_share(j, 4, i), ";", j, ";", i
    enddo
enddo
close(106)
                
!! temporary printing     
!OPEN (unit=202,  FILE = version//closure//"Value_function_20_years_old_trans.txt")
!    do j = -bigJ,bigT, 1
!        write(202,  '(F20.10)') V_20_years_old(j)
!    enddo
!CLOSE(202)
!    
!open(unit = 104, file= version//closure//"Value_function_trans.csv")
!do j = 1, bigJ,1  
!    do i = 1,bigT,1
!        write(104, '(F20.10)', advance='no') V_j_vfi(j, i)
!        write(104, '(A)', advance='no') ";"
!    enddo
!    write(104, '(F20.10)') V_j_vfi(j, bigT)
!enddo      
!close(104)
!
!OPEN (unit=202,  FILE = version//closure//"g_per_capita_trans.txt")
!do i = 1,bigT, 1
!    write(202,  '(F20.10)') g_per_capita(i)
!enddo
!CLOSE(202)
!
!
!open(unit = 104, file= "savings_top_ten.csv")
!do t = 1, 10,1  
!    do i = 2,bigT,1
!        write(104, '(F20.10)', advance='no') savings_top_ten_trans(t,i)/sum(N_t_j(:,i)*asset_trans(:,i))
!        write(104, '(A)', advance='no') ";"
!    enddo
!    write(104, '(F20.10)')savings_top_ten_trans(t,bigT)/sum(N_t_j(:,bigT)*asset_trans(:,bigT))
!enddo      
!close(104) 
!
!
!open(unit = 104, file= "savings_top_ten_1.csv")
!do t = 1, 10,1  
!    do i = 2,bigT,1
!        write(104, '(F20.10)', advance='no') savings_top_ten_trans(t,i)
!        write(104, '(A)', advance='no') ";"
!    enddo
!    write(104, '(F20.10)')savings_top_ten_trans(t,bigT)
!enddo      
!close(104) 
!
!
!open(unit = 104, file= "savings_top_ten_2.csv")
!do t = 1, 10,1  
!    do i = 2,bigT,1
!        write(104, '(F20.10)', advance='no') sum(N_t_j(:,i)*asset_trans(:,i))
!        write(104, '(A)', advance='no') ";"
!    enddo
!    write(104, '(F20.10)')sum(N_t_j(:,bigT)*asset_trans(:,bigT))
!enddo      
!close(104) 
!     
!open(unit = 104, file= "l_pen_j.csv")
!do j = 1, bigJ,1  
!    do i = 1,bigT,1
!        write(104, '(F20.10)', advance='no') l_pen_j(j, i)
!        write(104, '(A)', advance='no') ";"
!    enddo
!    write(104, '(F20.10)')l_pen_j(j, bigT)
!enddo      
!close(104)
! 
!
!OPEN (unit=1, FILE = "gini_weight_trans.txt")
!    do i = 1, bigT, 1
!        do j = 1, bigJ, 1
!            do ia = 0, n_a, 1
!                write(1,*) gini_weight_trans(j,ia,i)
!            enddo        
!        enddo
!    enddo
!close(1)
