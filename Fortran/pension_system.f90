!    rI(1) = gam_t(1)*nu(1) - 1 
!    rI(2) = gam_t(1)*nu(1) - 1
!    do i = 3,n_p+1,1
!        rI(i) = gam_t(i-1)*(w_bar(i-1)*bigL(i-1))/(w_bar(i-2)*bigL(i-2))-1 !*(N_t(i-1)/N_t(i-2))-1 !(gam_t(i-1)*w_bar(i-1)*bigL(i-1)*N_t(i-1))/(w_bar(i-2)*bigL(i-2)*N_t(i-2))-1
!    enddo
!    rI(n_p+2:bigT) = nu(n_p+1)*gam_t(n_p+2) - 1    
    
    !!!!!!!!!!!!!!!!! DB pension system !!!!!!!!!!!!!!!!!
!    if (switch_type_2 == 0) then 
!    ! indivdual pension benefits 
! 
do i_o = 1, omega_dim
    do i = 2,bigT,1
        !b1_j(1:jbar_t(i)-1, i) = 0
        !!b2_j(1:jbar_t(i)-1,i) = 0
        !    do j = jbar_t(i),bigJ,1
        !        if (j == jbar_t(i)) then
        !            if (jbar_t(i) == jbar_t(i-1)+1) then
        !                !b1_j(j, i) = valor_mult(i)*b1_j(j-1, i-1)
        !                !b2_j(j,i) = 0
        !            else
        !                if(i < bigJ) then 
        !                    if (rho1 == 0) then ! Hence there's no common part of social security
        !                        b1_j(j, i) = ((bigJ-i)*rho_1 + i*rho_2)/float(bigJ)*avg_ef_l_suply_trans(i)
        !                    else
        !                        b1_j(j, i) = rho1*((bigJ-i)*rho_1 + i*rho_2)/float(bigJ)*avg_ef_l_suply_trans(i)
        !                    endif
        !                    !write(*,*)'a ', b1_j(j, i)
        !                else 
        !                    if (rho1 == 0) then ! Hence there's no common part of social security
        !                        b1_j(j, i) = rho_2*avg_ef_l_suply_trans(i)
        !                    else
        !                        b1_j(j, i) = rho1*rho_2*avg_ef_l_suply_trans(i) 
        !                    endif
        !                    !write(*,*)'b ', b1_j(j, i)
        !                endif
        !                !b2_j(j,i) = 0
        !            endif
        !        else if (b1_j(j-1, i-1) == 0) then
        !            if(i < bigJ) then 
        !                if (rho1 == 0) then ! Hence there's no common part of social security
        !                    b1_j(j, i) = ((bigJ-i)*rho_1 + i*rho_2)/float(bigJ)*avg_ef_l_suply_trans(i) 
        !                else
        !                    b1_j(j, i) = rho1*((bigJ-i)*rho_1 + i*rho_2)/float(bigJ)*avg_ef_l_suply_trans(i)
        !                endif
        !                !write(*,*)'c ', b1_j(j, i)
        !            else 
        !                if (rho1 == 0) then ! Hence there's no common part of social security
        !                    b1_j(j, i) = rho_2*avg_ef_l_suply_trans(i) 
        !                else
        !                    b1_j(j, i) = rho1*rho_2*avg_ef_l_suply_trans(i)
        !                endif
        !                !write(*,*)'d ', b1_j(j, i)
        !            endif
        !            !b2_j(j,i) = 0                                                
        !        else
        !            b1_j(j, i) = valor_mult(i)*b1_j(j-1, i-1)
        !            !write(*,*)'e ', b1_j(j, i)
        !            !b2_j(j,i) = 0
        !        endif        
        !    enddo 
        !b_j(:, i_o, i) = b_scale_factor(i)*b1_j(:, i)    
        do j = 1, bigJ, 1
            !subsidy_j(j, i_o, i) = sum_b_weight_trans_om(i_o, min(max(i + jbar_t(i) -j,1), bigT))*b_j(j, i_o, i) - t1(j,i)*w_bar(i)*l_j(j, i_o, i) ! *omega(j, i_o, i)
            subsidy_j(j, i_o, i) = sum_b_trans(j, i_o, i) - t1(j, i)*w_bar(i)*l_j(j, i_o, i)
        enddo
        contribution_j(:, i_o, i) = t1(:,i)*w_bar(i)*l_j(:, i_o, i) ! *omega(:,i)
    enddo 
    
    !subsidy_j(:, i_o, 1) = sum_b_weight_trans_om(i_o, 1)*b_j(:, i_o, 1) - t1(:,1)*w_bar(1)*l_j(:, i_o, 1) ! *omega(:,1) 
    subsidy_j(:, i_o, 1) = sum_b_trans(:, i_o, 1) - t1(:, 1)*w_bar(1)*l_j(:, i_o, 1)
    contribution_j(:, i_o, 1) = t1(:,1)*w_bar(1)*l_j(:, i_o, 1) ! *omega(:,1)
 
     ! macro agg    
    do i = 1, bigT
        subsidy_pom(i) = subsidy_pom(i) + sum(N_t_j(:, i)*subsidy_j(:, i_o, i)*(omega_share(:, i_o, i)/sum(omega_share(:, :, i), dim = 2)), dim=1)
        sum_b_pom(i) =   sum_b_pom(i) + sum(N_t_j(:, i)*sum_b_trans(:, i_o, i)*(omega_share(:, i_o, i)/sum(omega_share(:, :, i), dim = 2)), dim=1)
        contribution_pom(i) = contribution_pom(i)  + sum(N_t_j(:, i)*contribution_j(:, i_o, i)*(omega_share(:, i_o, i)/sum(omega_share(:, :, i), dim = 2)), dim=1)
    enddo
enddo
    !do i = 1, bigT, 1   
    !sum_b(i) = 0d0
    !    do j = jbar_t(i), bigJ, 1
    !        sum_b(i) = sum_b(i) + sum_b_weight_trans_om(i_o, max(i + jbar_t(i)-j, 1))*b_j(j, i_o, i)*N_t_j(j,i)*(omega_share(j, i_o, i)/sum(omega_share(j, :, i))) !/bigL(i)
    !    enddo
    !enddo  
    !subsidy_ss_pom = subsidy_ss_pom + (sum(N_t_j(:, i)*sum_b_trans(:, i_o, i)*(omega_share(:, i_o, i)/sum(omega_share(:, :, i), dim = 2)), dim=1)) &
    !                        - sum(N_t_j(:, i)*t1(:, 1)*w_bar*l_j(:, i_o, i)*(omega_share(:, i_o, i)/sum(omega_share(:, :, i), dim = 2)), dim=1)))!/bigL_ss
    
!else ! (switch_type_2 == 1) then 
!!!!!!!!!!!!!!!!!! DC pension system !!!!!!!!!!!!!!!!!
!    do j = 1,bigJ,1
!        contributionI_j(j,:) = t1_contrib(j,:)*omega(j,:)*w_bar(:)*l_j(j,:)
!        contributionII_j(j,:) = t2(j,:)*omega(j,:)*w_bar(:)*l_j(j,:)
!    enddo
!    pillarI_j(1,:) = contributionI_j(1,:)
!    pillarII_j(1,:) = contributionII_j(1,:)
!    if (switch_type_1 == 0) then
!        ! instead of init cap from data...
!        !pillarI_j(:,1) = (t1_ss_old/0.1953_dp)*init_cap*average_w(1)
!        !... we use theoretical value 
!        !pillarI_j(1,1) = (t1(1,1)*omega(1,1)*w_bar(1)*l_j(1,1))
!        pillarI_j(1,1) = (t1_contrib(1,1)*omega(1,1)*w_bar(1)*l_j(1,1))
!        do j = 2,min(jbar_t(1)-1, ofe_u-1), 1 
!                pillarI_j(j,1) =  (1+rI(1))/gam_t(1)*pillarI_j(j-1,1)*N_t_j(j-1,1)/N_t_j(j,1) &
!                                + t1_contrib(j,1)*omega(j,1)*w_bar(1)*l_j(j,1)
!        enddo    
!        ! t1_a and t2 are equal zero in 1st period of transition path  
!        ! hence we add only contribution driven by t1 
!        pillarII_j(:,1) = 0
!    endif
!        do i = 2,bigT,1 
!            do j = 2,bigj,1
!                if (j < jbar_t(i)) then
!                    pillarI_j(j,i) = (1 + rI(i))*pillarI_j(j-1,i-1)/gam_t(i)*pi(j-1, i-1)/pi(j,i)  + contributionI_j(j,i) 
!                    pillarII_j(j,i) = (1 + r_bar(i))*pillarII_j(j-1,i-1)/gam_t(i)*pi(j-1, i-1)/pi(j,i)  + contributionII_j(j,i)
!                elseif (((j == jbar_t(i)) .and. ((j-1) .ne. jbar_t(i-1))) .or. (j > jbar_t(i) .and. b1_j(j-1,i-1) == 0)) then ! pension is not individual but cohort-specific
!                ! elseif (j == jbar_t(i)) then       ! pension is not individual but cohort-specific
!                    accountI = (1 + rI(i))*pillarI_j(j-1,i-1)/gam_t(i)*pi(j-1, i-1)/pi(j,i) 
!                    accountII = (1 + r_bar(i))*pillarII_j(j-1,i-1)/gam_t(i)*pi(j-1, i-1)/pi(j,i) 
!                    b1_j(j,i) = accountI/life_exp(j,i) ! beginning of the year
!                    b2_j(j,i) = accountII/life_exp(j,i) ! beginning of the year
!                    accountII = 0.0_dp
!                    pillarI_j(j,i) = (1 + rI(i))*pillarI_j(j-1,i-1)/gam_t(i)*pi(j-1, i-1)/pi(j,i)  - b1_j(j,i) ! end of the year
!                    pillarII_j(j,i) = (1 + r_bar(i))*pillarII_j(j-1,i-1)/gam_t(i)*pi(j-1, i-1)/pi(j,i)  - b2_j(j,i)! end of the year
!                else
!                    b1_j(j,i) = valor_mult(i)*b1_j(j-1,i-1)
!					b2_j(j,i) = b2_j(j-1,i-1)*(1 + r_bar(i))/gam_t(i) ! beginning of the year
!                    pillarI_j(j,i) = (1 + rI(i))*pillarI_j(j-1,i-1)/gam_t(i)*pi(j-1, i-1)/pi(j,i)  - b1_j(j,i) ! end of the year
!                    pillarII_j(j,i) = (1 + r_bar(i))*pillarII_j(j-1,i-1)/gam_t(i)*pi(j-1, i-1)/pi(j,i)  - b2_j(j,i) ! end of the year
!                endif  
!            enddo
!            b_j(:,i) = (b_scale_factor(i)*b1_j(:,i) + b2_j(:,i))!*(1 - tL(i))
!        enddo
!        
!    if (switch_pension == 1) then  ! 0 = all are in new pension scheme in transitionFF; 1 = old cohorts remain in the old system in transitionFF
!        do i = 2,bigT,1
!            do j = 2,bigJ,1
!                if (j >= jbar_t(1)+i-1) then 
!                    b1_j(j,i) = valor_mult(i)*b1_j(j-1,i-1)
!                    b2_j(j,i) = 0.0_dp
!                else
!                    if ((j-i+2 > ofe_u) .and. (j >= jbar_t(i))) then
!                        if (j == jbar_t(i)) then
!                            if (jbar_t(i) == jbar_t(i-1)+1) then
!                                b1_j(j,i) = valor_mult(i)*b1_j(j-1,i-1)
!                                b2_j(j,i) = 0.0_dp
!                            else
!                                b1_j(j,i) = rho_1*avg_wl(i) !! rho_1*average_w_10(i)
!                                b2_j(j,i) = 0.0_dp
!                            endif
!                        elseif (b1_j(j-1,i-1) == 0) then ! had pensions already in the old ss
!                            b1_j(j,i) = rho_1*avg_wl(i) 
!                            b2_j(j,i) = 0.0_dp                                               
!                        else
!                            b1_j(j,i) = valor_mult(i)*b1_j(j-1,i-1)
!                            b2_j(j,i) = 0.0_dp
!                        endif
!                    endif
!                endif 
!            enddo
!            b_j(:,i) = (b_scale_factor(i)*b1_j(:,i) + b2_j(:,i))
!        enddo
!    endif
!    ! macro agg    
!    PillarII = sum(N_t_j*pillarII_j, dim=1)/bigL
!    ContributionI = sum(N_t_j*contributionI_j, dim=1)/bigL
!    ContributionII = sum(N_t_j*contributionII_j, dim=1)/bigL
!    PillarI = sum(N_t_j*pillarI_j, dim=1)/bigL
!    
!    do i = 1,bigT,1
!        do j = 1, bigJ, 1 
!            if (j-(i-1) >ofe_u-1) then ! benefits from old pension system 
!                subsidy_j(j,i) = sum_b_weight_trans(max(i + jbar_t(i) -j,1))*b_scale_factor(i)*b1_j(j,i) - t1(j,i)*omega(j,i)*w_bar(i)*l_j(j,i) 
!            else 
!                subsidy_j(j,i) = b_scale_factor(i)*b1_j(j,i)  - t1(j,i)*omega(j,i)*w_bar(i)*l_j(j,i) 
!            endif
!            contribution_j(:,i) = (t1(:,i) +  t2(:,i))*omega(:,i)*w_bar(i)*l_j(:,i)
!        enddo 
!        sum_b(i) = sum((b_scale_factor(i)*b1_j(:,i) + b2_j(:,i))*N_t_j(:,i),dim=1)/bigL(i)
!
!    enddo 
!            
!    subsidy = sum(N_t_j*subsidy_j, dim=1)/bigL
!    contribution = sum(N_t_j*contribution_j, dim=1)/bigL
!endif
