! WHAT   : AGGREGATION of variables
! TAKE   : unchanged in routine:  size of each cohort [[N_t_j]], shares of each sector [[omega_share]]
!        : unchanged in routine:  kohort specyfic: savings [[sv_j]], consumption [[c_j]]
!          changed   in routine:  bequest 
! DO     : called in transition and tansition_iteration
! RETURN : aggregates on transition path 
    
do i_o = 1, omega_dim
    consumption_gross_j = c_j(:, i_o, :)     
    do i = 1, bigT
        consumption_gross_pom(i) = consumption_gross_pom(i) + sum(N_t_j(:, i)*consumption_gross_j(:, i)*(omega_share(:, i_o, i)/sum(omega_share(:, :, i), dim = 2)), dim=1)
        savings_j(:, i) = savings_j(:, i) + sv_j(:, i_o, i)*(omega_share(:, i_o, i)/sum(omega_share(:, :, i), dim = 2)) !loop??
        savings_pom(i) = savings_pom(i) + sum(N_t_j(:, i)*sv_j(:, i_o, i)*(omega_share(:, i_o, i)/sum(omega_share(:, :, i), dim = 2)))
    enddo
enddo
savings = savings_pom/bigL
!consumption = sum(N_t_j*c_j, dim=1)/bigL    
!consumption_gross = consumption!/(1+tc)
consumption_gross = consumption_gross_pom/bigL
if (sum(consumption_gross_new) == 0) then
   consumption_gross_new = consumption_gross
else
   consumption_gross_new = up_t*consumption_gross_new + (1.0_dp-up_t)*consumption_gross    
endif