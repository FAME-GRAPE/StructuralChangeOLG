include "globals.f90"
include "steady_wzrost.f90"
include "set_globals.f90"
include "transition.f90"
include "data.f90"
include "clock.f90"
include "main_LSRA.f90"
!include "shocks_parameters.f90"
include "defim.f90"
include "main_params.f90"
!include "create_dirs.f90"
    
program olg2
    !use defim
    use global_vars
    use steady_state
    use global_vars2
    use transition_DB
    use get_data    
    use clock 
    !use geompack3
    use LSRA 
    !use ifport
    !use new_dir
    
    implicit none
    
character(64), dimension(7) :: scenario
    
scenario = (/"0. no changes", "1. unemployment", "2. gamma", "3. Demo", "7. gamma + Structure", "5. gamma + Demo", "8. ALL"/)   ! , "6. gamma + Structure", "7. Demo + Structure"
call getcwd(cwd)
                                                   
call tic()
call date_and_time(DATE = date, TIME = time)
allocate(svplus_trans(bigJ, 0:n_a, 0:n_aime, n_sp, 1:omega_dim, bigT))
allocate(aime_plus_trans, V_trans, prob_trans, c_trans, l_trans, source = svplus_trans)
!allocate(l_cap_trans(bigJ, 0:n_a, 0:n_aime, n_sp, 1:omega_dim, bigT))
allocate(c_trans_vfi(bigJ, 0:n_a, 0:n_aime, n_sp, bigT))
allocate(labor_tax_trans, RHS_trans, l_trans_vfi, &
         sv_tempo_trans, EV_trans, ERHS_trans, &
         svplus_trans_vfi, aime_plus_trans_vfi, V_trans_vfi, prob_trans_vfi, source = c_trans_vfi)
!allocate(l_cap_trans_vfi(bigJ, 0:n_a, 0:n_aime, n_sp, bigT))

cwd_r = trim(cwd)//"\data"

name = 'alt'

switch_print = 1
    
! !dir$ ivdep
do com = 6, 1, -1  
    
    !if (com /= 3 .or. com /= 4 .or. com /= 6 .or. com /= 7) then !if (com == 1 .or. com == 2 .or. com == 5 .or. com == 8) then    !if (com /= 6 .or. com /= 7) then
        cwd_w = trim(cwd)//"\SCENARIOS\"//trim(scenario(com+1))//"\outcomes"    
        cwd_g = trim(cwd)//"\SCENARIOS\"//trim(scenario(com+1))//"\graphs"
        cwd_p = trim(cwd)//"\SCENARIOS\"//trim(scenario(com+1))
        clear_w = 'del /q "'//trim(cwd_w)//'\*.*"'  ! /s
        clear_g1 = 'del /q "'//trim(cwd_g)//'\*.png"'
        clear_g2 = 'del /q "'//trim(cwd_g)//'\*.txt"'
        call execute_command_line (trim(clear_w))
        call execute_command_line (trim(clear_g1))
        call execute_command_line (trim(clear_g2))
        call chdir(cwd_p)
        
        print *, ' '
        print *, ' '
        print *, scenario(com+1)
        print *, ' '
    
        call clear_globals
        call globals         ! globals is a subroutine in the global_vars2 module  
        
        write(theta_string,'(I1)')int(theta)
        version = 'theta'//trim(theta_string)//'_'//trim(name)//'__'
        !version2 = 'theta'//trim(theta_string)//'_'//trim(name)//'_base__'
        db_o = '"C:\Users\jlutynski\Dropbox (FAME GRAPE)\NCN EMERYT\_Paper_17_reallocation\_outcomes\theta '//trim(theta_string)//'/'//trim(name)//'/'//trim(scenario(com+1))//' - '//trim(date)//'"'
        db_g = '"C:\Users\jlutynski\Dropbox (FAME GRAPE)\NCN EMERYT\_Paper_17_reallocation\_graphs\theta '//trim(theta_string)//'/'//trim(name)//'/'//trim(scenario(com+1))//' - '//trim(date)//'"'
        cwd_db_o = "mkdir "//db_o
        cwd_db_g = "mkdir "//db_g
        !test = makedirqq(db_o)
        !test = makedirqq(db_g)
        !print *, '"F:\janek\Stata16\StataIC-64.exe" /e do "F:\janek\code trans\SCENARIOS\k-draw path.do '//trim(db_g)//'"'
!pause
        call execute_command_line (cwd_db_o)
        call execute_command_line (cwd_db_g)
!call chdir(db_o)
!OPEN (unit=29, FILE = "test.txt")
!write(29, '(F30.10)') rho(10)
!close(29)
!pause
        if (switch_reform == 0 ) then 
            include 'main_base_transition.f90'
        else
            call cons_eq(x_j_pro, x_unif_pro, sum_x_pro, x_c_j_pro, eq_unif_pro, sum_eq_pro, LS_pro)
        endif
        write (*,*) 'computations completed' 
    !endif
enddo
     
call toc()
deallocate(svplus_trans, l_trans, labor_tax_trans, c_trans, RHS_trans,  &   ! l_cap_trans, 
           sv_tempo_trans, V_trans, EV_trans, ERHS_trans, prob_trans, aime_plus_trans)

call chdir(cwd)
!stata = '"F:\STATA\StataSE-64" /e do ".\SCENARIOS\gini\gini_MAIN.do '//trim(theta_string)//'_'//trim(name)//'_'//trim(date)//'"'
call execute_command_line ('"F:\STATA\StataSE-64" /e do ".\SCENARIOS\gini\gini_MAIN.do"')
read*
pause
call output()

endprogram olg2