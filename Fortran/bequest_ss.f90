! WHAT   : BEQUEST equally devided among all kohorts (or all but first => bequest_all_switch)
! TAKE   : unchanged in routine:  size of each cohort [[N_ss_j]], interest rate [[r_ss]], amount of private [[savings_ss_j]] 
!        : unchanged in routine:  labor force growth [[nu(i)=big_l(i)/big_l(i-1)]], change in technological progress [[gam = z_t/z_(t-1)]]
!          changed   in routine:  bequest 
! DO     : called in steady_wzrost
! RETURN : updated bequest for next iteration of steady state 
    
!if (switch_run_1 == 1) then    
    
    do j = 2,bigJ,1
        bequest_left_ss_j(j-1) = (N_ss_j(j-1) - N_ss_j(j))*(r_ss*savings_ss_j_agr(j-1))/gam_ss
    enddo
    
    bequest_left_ss_j(bigJ) = (N_ss_j(bigJ))*(r_ss*savings_ss_j_agr(bigJ))/gam_ss
    bequest_ss = sum(bequest_left_ss_j(1:bigJ))
        
    bequest_ss_j_old = bequest_ss_j
    bequest_ss_j(1) = 0d0
      
    if (bequest_all_switch == 0) then
        do j = 1,bigJ,1     !do j = 2,bigJ,1
            ! bequest_ss_j(j) = up_ss*bequest_ss_j_old(j) + (1 - up_ss)*bequest_left_ss_j(j-1)/N_ss_j(j)   
            bequest_ss_j(j) = up_ss*bequest_ss_j_old(j) + (1 - up_ss)*bequest_ss/N_ss 
        enddo
    elseif (bequest_all_switch == 1) then
        do j = 2, bigJ, 1     !do j = 2,bigJ,1
            ! bequest_ss_j(j) = up_ss*bequest_ss_j_old(j) + (1 - up_ss)*bequest_left_ss_j(j-1)/N_ss_j(j)   
            bequest_ss_j(j) = up_ss*bequest_ss_j_old(j) + (1 - up_ss)*sum(bequest_left_ss_j(1:bigJ))/sum(N_ss_j(2:bigJ))
        enddo
    else
        print *, "invalid value of bequest_all_switch"
        pause
    endif
    
!elseif (switch_run_1 == 0 .and. switch_run_2 == 1) then
!        
    !do j = 2,bigJ,1
    !    bequest_left_ss_j(j-1) = (pi_ss(j-1) - pi_ss(j))*(r_ss*savings_ss_j_agr(j-1))/gam_ss
    !enddo
    !
    !bequest_left_ss_j(bigJ) = (pi_ss(bigJ))*(r_ss*savings_ss_j_agr(bigJ))/gam_ss
    !bequest_ss = sum(bequest_left_ss_j(1:bigJ))
    !    
    !bequest_ss_j_old = bequest_ss_j
    !bequest_ss_j(1) = 0d0
    !
    !do j = 2,bigJ,1
    !    bequest_ss_j(j) = up_ss*bequest_ss_j_old(j) + (1 - up_ss)*bequest_left_ss_j(j-1)/pi_ss(j)   
    !enddo  
!    
!else
!        
!    print *, "bequest_ss error"
!    pause
!    
!endif