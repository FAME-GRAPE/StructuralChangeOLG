! WHAT   : BEQUEST equally devided among all kohorts (or all but first => bequest_all_switch)
! TAKE   : unchanged in routine:  size of each cohort [[N_t_j]], interest rate [[r]], amount of private [[sv]]
!        : unchanged in routine:  labor force growth [[nu(i)=big_l(i)/big_l(i-1)]], change in technological progress [[gam = z_t/z_(t-1)]]
!          changed   in routine:  bequest 
! DO     : called in transition and transition_iteration
! RETURN : updated bequest for next iteration on transition path 

do i = 1, bigT, 1
    do j = 2,jbar_t(i),1
         bequest_left_j(j-1,max(i-1,1)) = (N_t_j(j-1,max(i-1,1)) - N_t_j(j,i))*r(i)*savings_j(j-1,max(i-1,1))/(gam_t(i))   
    enddo
    do j = jbar_t(i)+1,bigJ,1
        bequest_left_j(j-1,max(i-1,1)) = (N_t_j(j-1,max(i-1,1)) - N_t_j(j,i))*(r(i)*savings_j(j-1,max(i-1,1)))/(gam_t(i))
    enddo
    bequest_left_j(bigJ,max(i-1,1)) = (N_t_j(bigJ,max(i-1,1)))*(r(i)*savings_j(bigJ,max(i-1,1)))/(gam_t(i))    
enddo
                 
bequest = up_t*bequest + (1 - up_t)*sum(bequest_left_j(1:bigJ,:), dim=1)
    
if (bequest_all_switch == 0) then
    do i = 1, bigT, 1  
        do j = 1,bigJ,1 !do j = 2,bigJ,1
            !bequest_j(j,i) = bequest_left_j(j-1,max(i-1,1))/N_t_j(j,i) 
            bequest_j(j,i) = up_t*bequest_j(j,i) + (1 - up_t)*sum(bequest_left_j(:, max(i-1,1)), dim = 1)/sum(N_t_j(:,i))
        enddo              
    enddo
elseif (bequest_all_switch == 1) then
    do i = 1, bigT, 1  
        do j = 2, bigJ, 1 !do j = 2,bigJ,1
            !bequest_j(j,i) = bequest_left_j(j-1,max(i-1,1))/N_t_j(j,i) 
            bequest_j(j, i) = sum(bequest_left_j(:, max(i-1,1)), dim = 1)/sum(N_t_j(2:bigJ, i))
        enddo              
    enddo
else
    print *, "invalid value of bequest_all_switch"
    pause
endif