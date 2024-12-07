!***************************************************************************************

! get distribution for every gridpoint and state for every age
! Steady state
    subroutine get_distribution_ss()

            implicit none

            integer :: j, ial, iar, iaimel, iaimer
            real*8 :: dist, dist_aime

            ! set distribution to zero
            prob_ss = 0d0

            ! get initial distribution in age 1
            call linear_int(0d0, ial, iar, dist, sv, n_a, a_grow)
            ial = min(ial, n_a)
            iar = min(iar, n_a)
            dist = min(dist, 1d0)
            
            prob_ss(1, ial, 0, n_sp_initial_vfi) = dist ! y-ss rename f_dens_ss ! poczatkowy rozk³ad
            prob_ss(1, iar, 0, n_sp_initial_vfi) = 1d0 - dist
            
            ! successively compute distribution over ages
            do j = 2, bigJ  
            ! iterate over yesterdays gridpoints
                do ia = 0, n_a, 1
                    do i_aime = 0, n_aime, 1
                        do ip = 1 , n_sp, 1
                            ! interpolate yesterday's savings decision
                            call linear_int(svplus_ss(j-1, ia, i_aime, ip), ial, iar, dist, sv, n_a, a_grow)
                            call linear_int(aime_plus_ss(j-1, ia, i_aime, ip), iaimel, iaimer, dist_aime, aime(:), n_aime, aime_grow)
                            ! restrict values to grid just in case               
                            dist = min(abs(dist), 1d0)

                            ! redistribute households
                            if (j >= jbar_ss_vf) then
                                ip_p = ip
                                prob_ss(j, ial, iaimel, ip_p) = prob_ss(j, ial, iaimel, ip_p) &
                                                                                + 1*dist       *dist_aime       *prob_ss(j-1, ia, i_aime, ip)
                                prob_ss(j, iar, iaimel, ip_p) = prob_ss(j, iar, iaimel, ip_p) &
                                                                                + 1*(1d0-dist) *dist_aime       *prob_ss(j-1, ia, i_aime, ip) 
                                prob_ss(j, ial, iaimer, ip_p) = prob_ss(j, ial, iaimer, ip_p) &
                                                                                + 1*dist       *(1d0-dist_aime) *prob_ss(j-1, ia, i_aime, ip)
                                prob_ss(j, iar, iaimer, ip_p) = prob_ss(j, iar, iaimer, ip_p) &
                                                                                + 1*(1d0-dist) *(1d0-dist_aime) *prob_ss(j-1, ia, i_aime, ip) 
                            else
                                do ip_p = 1, n_sp,1
                                    prob_ss(j, ial, iaimel, ip_p) = prob_ss(j, ial, iaimel, ip_p) &
                                                                                + pi_ip_ss_vfi(ip, ip_p)*dist       *dist_aime       *prob_ss(j-1, ia, i_aime, ip)
                                    prob_ss(j, iar, iaimel, ip_p) = prob_ss(j, iar, iaimel, ip_p) &
                                                                                + pi_ip_ss_vfi(ip, ip_p)*(1d0-dist) *dist_aime       *prob_ss(j-1, ia, i_aime, ip) 
                                    prob_ss(j, ial, iaimer, ip_p) = prob_ss(j, ial, iaimer, ip_p) &
                                                                                + pi_ip_ss_vfi(ip, ip_p)*dist       *(1d0-dist_aime) *prob_ss(j-1, ia, i_aime, ip)
                                    prob_ss(j, iar, iaimer, ip_p) = prob_ss(j, iar, iaimer, ip_p) &
                                                                                + pi_ip_ss_vfi(ip, ip_p)*(1d0-dist) *(1d0-dist_aime) *prob_ss(j-1, ia, i_aime, ip) 
                                enddo
                            endif    
                        enddo
                    enddo
                enddo
            enddo 
        end subroutine
!***************************************************************************************

! get distribution for every gridpoint and state for every age
! for transition

    subroutine get_distribution_trans(i)

            implicit none
            integer, intent(in) :: i
            integer :: j, ial, iar, iaimel, iaimer, itm
            real*8 :: dist, dist_aime

            ! get yesterdays year
            itm = year(i, 2, 1) 
        
            ! set distribution to zero
            prob_trans_vfi(:,:,:,:,i) = 0d0 
            ! get initial distribution in age 1
             call linear_int(0d0, ial, iar, dist, sv, n_a, a_grow)
             ial = min(ial, n_a)
             iar = min(iar, n_a)
             dist = min(dist, 1d0)

            ! prob_trans_vfi(1, ial, 0, n_sp_initial_vfi, i) = dist
            ! prob_trans_vfi(1, iar, 0, n_sp_initial_vfi, i) = 1d0 - dist
            !
            !! successively compute distribution over ages
            !do j = 2, bigJ
            !    ! iterate over yesterdays gridpoints
            !    do ia = 0, n_a, 1
            !        do ip = 1 , n_sp, 1
    !                    ! interpolate_trans yesterday's savings decision
    !                    call linear_int(svplus_trans_vfi(j-1, ia, 0, ip, itm), ial, iar, dist, sv, n_a, a_grow)
    !                    ! restrict values to grid just in case
    !                    ial = min(ial, n_a)
    !                    iar = min(iar, n_a)
    !                    dist = min(abs(dist), 1d0)
    !                    ! redistribute household_transs
    !                    do ip_p = 1, n_sp,1
    !                        do ir_r=1, n_sr, 1
    !                            do id_d =1, n_sd, 1
    !                                prob_trans_vfi(j, ial, 0, ip_p, i) = prob_trans_vfi(j, ial, 0, ip_p, i) + pi_ip_vfi(ip, ip_p)*dist*prob_trans_vfi(j-1, ia, 0, ip, itm)
    !                                prob_trans_vfi(j, iar, 0, ip_p, i) = prob_trans_vfi(j, iar, 0, ip_p, i) + pi_ip_vfi(ip, ip_p)*(1d0-dist)*prob_trans_vfi(j-1, ia, 0, ip, itm)
    !                            enddo
    !                        enddo
    !                    enddo
            !        enddo
            !    enddo
            !enddo 
            
            prob_trans_vfi(1, ial, 0, n_sp_initial_vfi, i) = dist
            prob_trans_vfi(1, iar, 0, n_sp_initial_vfi, i) = 1d0 - dist         
            if (dist < 0d0 .or. dist > 1d0) pause
            ! successively compute distribution over ages
            do j = 2, bigJ ! prob sum up to 1 for each (j, i)
                ! iterate over yesterdays gridpoints
                do ia = 0, n_a, 1
                    do i_aime = 0, n_aime, 1
                        do ip = 1 , n_sp, 1
                            ! interpolate_trans yesterday's savings decision
                            call linear_int(   svplus_trans_vfi(j-1, ia, i_aime, ip, itm), ial, iar, dist, sv, n_a, a_grow)
                            call linear_int(aime_plus_trans_vfi(j-1, ia, i_aime, ip, itm), iaimel, iaimer, dist_aime, aime(:), n_aime, aime_grow)
                            ! restrict values to grid just in case
                            ial = min(ial, n_a)
                            iar = min(iar, n_a)
                            dist = min(abs(dist), 1d0)
                            if (dist < 0d0 .or. dist > 1d0) pause
                            ! redistribute household_transs
                            if (j >= jbar_t_vfi(i)) then
                                ip_p = ip
                                prob_trans_vfi(j, ial, iaimel, ip_p, i) = prob_trans_vfi(j, ial, iaimel, ip_p, i) + 1*dist*dist_aime            *prob_trans_vfi(j-1, ia, i_aime, ip, itm)
                                prob_trans_vfi(j, iar, iaimel, ip_p, i) = prob_trans_vfi(j, iar, iaimel, ip_p, i) + 1*(1d0-dist)*dist_aime      *prob_trans_vfi(j-1, ia, i_aime, ip, itm)
                                prob_trans_vfi(j, ial, iaimer, ip_p, i) = prob_trans_vfi(j, ial, iaimer, ip_p, i) + 1*dist*(1d0-dist_aime)      *prob_trans_vfi(j-1, ia, i_aime, ip, itm)
                                prob_trans_vfi(j, iar, iaimer, ip_p, i) = prob_trans_vfi(j, iar, iaimer, ip_p, i) + 1*(1d0-dist)*(1d0-dist_aime)*prob_trans_vfi(j-1, ia, i_aime, ip, itm)
                            else 
                                do ip_p = 1, n_sp,1
                                    prob_trans_vfi(j, ial, iaimel, ip_p, i) = prob_trans_vfi(j, ial, iaimel, ip_p, i) + pi_ip_vfi(ip, ip_p, i)*dist*dist_aime            *prob_trans_vfi(j-1, ia, i_aime, ip, itm)
                                    prob_trans_vfi(j, iar, iaimel, ip_p, i) = prob_trans_vfi(j, iar, iaimel, ip_p, i) + pi_ip_vfi(ip, ip_p, i)*(1d0-dist)*dist_aime      *prob_trans_vfi(j-1, ia, i_aime, ip, itm)
                                    prob_trans_vfi(j, ial, iaimer, ip_p, i) = prob_trans_vfi(j, ial, iaimer, ip_p, i) + pi_ip_vfi(ip, ip_p, i)*dist*(1d0-dist_aime)      *prob_trans_vfi(j-1, ia, i_aime, ip, itm)
                                    prob_trans_vfi(j, iar, iaimer, ip_p, i) = prob_trans_vfi(j, iar, iaimer, ip_p, i) + pi_ip_vfi(ip, ip_p, i)*(1d0-dist)*(1d0-dist_aime)*prob_trans_vfi(j-1, ia, i_aime, ip, itm)
                                enddo                                
                            endif
                        enddo
                    enddo
                enddo
            enddo 
            !if (iter == 1) print *, sum(prob_trans_vfi(5, :, :, :, i))
            ! check if sum prob == 1
    end subroutine
    
!***************************************************************************************

! get distribution for every gridpoint and state for every age
! before iteration start in order to correct unemployment inputs

    !subroutine get_distribution_pre_trans(i)
    !
    !        implicit none
    !        integer, intent(in) :: i
    !        integer :: j, ial, iar, iaimel, iaimer, itm
    !        real*8 :: dist, dist_aime
    !
    !        ! get yesterdays year
    !        itm = year(i, 2, 1) 
    !    
    !        ! set distribution to zero
    !        prob_trans_vfi_u(:,:,i) = 0d0 
    !        ! get initial distribution in age 1
    !         call linear_int(0d0, ial, iar, dist, sv, n_a, a_grow)
    !         ial = min(ial, n_a)
    !         iar = min(iar, n_a)
    !         dist = min(dist, 1d0)
    !        
    !        prob_trans_vfi_u(1, n_sp_initial_vfi, i) = dist
    !        prob_trans_vfi_u(1, n_sp_initial_vfi, i) = 1d0 - dist         
    !        if (dist < 0d0 .or. dist > 1d0) pause
    !        ! successively compute distribution over ages
    !        do j = 1, bigJ ! before 2
    !            ! iterate over yesterdays gridpoints
    !            !do ia = 0, n_a, 1
    !                !do i_aime = 0, n_aime, 1
    !                    do ip = 1 , n_sp, 1
    !                        !! interpolate_trans yesterday's savings decision
    !                        !call linear_int(   svplus_trans_vfi(j-1, ia, i_aime, ip, itm), ial, iar, dist, sv, n_a, a_grow)
    !                        !call linear_int(aime_plus_trans_vfi(j-1, ia, i_aime, ip, itm), iaimel, iaimer, dist_aime, aime(:), n_aime, aime_grow)
    !                        !! restrict values to grid just in case
    !                        !ial = min(ial, n_a)
    !                        !iar = min(iar, n_a)
    !                        !dist = min(abs(dist), 1d0)
    !                        !if (dist < 0d0 .or. dist > 1d0) pause
    !                        ! redistribute household_transs
    !                        do ip_p = 1, n_sp,1
    !                            prob_trans_vfi_u(j, ip_p, i) = prob_trans_vfi_u(j, ip_p, i) + pi_ip_vfi(ip, ip_p, i)*prob_trans_vfi_u(j-1, ip, itm)
    !                            !prob_trans_vfi(j, iar, iaimel, ip_p, i) = prob_trans_vfi(j, iar, iaimel, ip_p, i) + pi_ip_vfi(ip, ip_p, i)*(1d0-dist)*dist_aime      *prob_trans_vfi(j-1, ip, itm)
    !                            !prob_trans_vfi(j, ial, iaimer, ip_p, i) = prob_trans_vfi(j, ial, iaimer, ip_p, i) + pi_ip_vfi(ip, ip_p, i)*dist*(1d0-dist_aime)      *prob_trans_vfi(j-1, ip, itm)
    !                            !prob_trans_vfi(j, iar, iaimer, ip_p, i) = prob_trans_vfi(j, iar, iaimer, ip_p, i) + pi_ip_vfi(ip, ip_p, i)*(1d0-dist)*(1d0-dist_aime)*prob_trans_vfi(j-1, ip, itm)
    !                        enddo
    !                    enddo
    !                !enddo
    !            !enddo
    !        enddo 
    !        ! check if sum prob == 1
    !end subroutine
    
    !    subroutine get_distribution_pre_trans(i)
    !
    !        implicit none
    !        integer, intent(in) :: i
    !        integer :: j, ial, iar, iaimel, iaimer, itm
    !        real*8 :: dist, dist_aime
    !
    !        ! get yesterdays year
    !        itm = year(i, 2, 1) 
    !    
    !        ! set distribution to zero
    !        prob_trans_vfi(:,:,:,:,i) = 0d0 
    !        ! get initial distribution in age 1
    !         call linear_int(0d0, ial, iar, dist, sv, n_a, a_grow)
    !         ial = min(ial, n_a)
    !         iar = min(iar, n_a)
    !         dist = min(dist, 1d0)
    !        
    !        prob_trans_vfi(1, ial, 0, n_sp_initial_vfi, i) = dist
    !        prob_trans_vfi(1, iar, 0, n_sp_initial_vfi, i) = 1d0 - dist         
    !        if (dist < 0d0 .or. dist > 1d0) pause
    !        ! successively compute distribution over ages
    !        do j = 2, bigJ
    !            ! iterate over yesterdays gridpoints
    !            !do ia = 0, n_a, 1
    !                !do i_aime = 0, n_aime, 1
    !                    do ip = 1 , n_sp, 1
    !                        ! interpolate_trans yesterday's savings decision
    !                        call linear_int(   svplus_trans_vfi(j-1, ia, i_aime, ip, itm), ial, iar, dist, sv, n_a, a_grow)
    !                        call linear_int(aime_plus_trans_vfi(j-1, ia, i_aime, ip, itm), iaimel, iaimer, dist_aime, aime(:), n_aime, aime_grow)
    !                        ! restrict values to grid just in case
    !                        ial = min(ial, n_a)
    !                        iar = min(iar, n_a)
    !                        dist = min(abs(dist), 1d0)
    !                        if (dist < 0d0 .or. dist > 1d0) pause
    !                        ! redistribute household_transs
    !                        do ip_p = 1, n_sp,1
    !                            prob_trans_vfi(j, ial, iaimel, ip_p, i) = prob_trans_vfi(j, ial, iaimel, ip_p, i) + pi_ip_vfi(ip, ip_p, i)*dist*dist_aime            *prob_trans_vfi(j-1, ia, i_aime, ip, itm)
    !                            prob_trans_vfi(j, iar, iaimel, ip_p, i) = prob_trans_vfi(j, iar, iaimel, ip_p, i) + pi_ip_vfi(ip, ip_p, i)*(1d0-dist)*dist_aime      *prob_trans_vfi(j-1, ia, i_aime, ip, itm)
    !                            prob_trans_vfi(j, ial, iaimer, ip_p, i) = prob_trans_vfi(j, ial, iaimer, ip_p, i) + pi_ip_vfi(ip, ip_p, i)*dist*(1d0-dist_aime)      *prob_trans_vfi(j-1, ia, i_aime, ip, itm)
    !                            prob_trans_vfi(j, iar, iaimer, ip_p, i) = prob_trans_vfi(j, iar, iaimer, ip_p, i) + pi_ip_vfi(ip, ip_p, i)*(1d0-dist)*(1d0-dist_aime)*prob_trans_vfi(j-1, ia, i_aime, ip, itm)
    !                        enddo
    !                    enddo
    !                !enddo
    !            !enddo
    !        enddo 
    !        ! check if sum prob == 1
    !end subroutine