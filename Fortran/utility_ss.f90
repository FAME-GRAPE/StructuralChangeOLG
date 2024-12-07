! WHAT   : Consumer problem -> value of utility function in steady state for both utility function (GHH & CD)
! TAKE   : unchanged in routine:   change in technological progress [[gam = z_t/z_(t-1)]], consumption [[c_ss]], consumption tax [[tc]], [the inverse of Frisch elasticity of labor [[ksi]],  the disutility of labor [[psi]]: use in GHH utility function]
!          unchanged in routine:   labor supply [[l_ss]], preference for leisure [[phi]],time-consistent (exponential) preferences [[beta]], the standard discount factor [[delta]], parameters for CES utility function [[theta]]
!          changed   in routine:   utility [[u_ss]] 
! DO     : called during iterations to calculate utility with given unchanged variable (look up)
! RETURN : updated utility for next iteration of steady state routine

do i_o = 1, omega_dim
    do j = 1,bigJ,1 
        u_ss_j(j, i_o) = log((((gam_ss**(j-1))*(c_ss_j(j, i_o)/(1 + tc_ss)))**phi)*((1-l_ss_j(j, i_o))**(1-phi)))
    enddo
enddo
u_ss = 0
do i_o = 1, omega_dim
    do j = 1,bigJ,1
        if (j==1) then
            u_ss = u_ss + u_ss_j(1, i_o)
        else
            u_ss = u_ss + beta*(delta*delta_mult_ss(i_o))**(j-1)*(pi_ss(j)/pi_ss(1))*u_ss_j(j, i_o)    
        endif
    enddo
enddo
   