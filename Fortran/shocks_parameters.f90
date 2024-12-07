!read(1,*) ! Shocks params
    
        aime_u    = 50d0 !9165d0/3921d0 ! to capture Old-Age, Survivors, and Disability Insurance (OASDI) tax cap
                            ! based on https://fas.org/sgp/crs/misc/R43542.pdf
                            ! and https://www.thebalancecareers.com/average-salary-information-for-us-workers-2060808
        
        aime_cap = 50d0 !9165d0/3921d0 ! to capture Old-Age, Survivors, and Disability Insurance (OASDI) tax cap
                    ! based on https://fas.org/sgp/crs/misc/R43542.pdf
                    ! and https://www.thebalancecareers.com/average-salary-information-for-us-workers-2060808

  
! here sigma is variance !!!
        
    ! shocks    
if (bigJ <= 4) then
        ! Germany
        !zeta_p  =  0.7826d0 
        !sigma_nu_p =  0.0737d0 
        !          
        !zeta_p = zeta_p**zbar
        if (omega_dim == 1) then
            zeta_p  =  0.938648d0
            sigma_nu_p = 0.0206832d0
        elseif (omega_dim == 2) then
            !zeta_p  =  (/0.938648d0, &     !   High & West
            !             0.762217d0/)      !   Low & West
            !sigma_nu_p = (/0.0206832d0, &     !   High & West
            !               0.012829d0/)
        elseif (omega_dim == 4) then
            !zeta_p  =  (/0.938648d0, &     !   High & West
            !             0.762217d0, &     !   Low & West
            !             0.8985868d0, &     !   High & East
            !             0.8258406d0/)      !   Low & East
            !!zeta_p = zeta_p*0.9
            !sigma_nu_p = (/0.0206832d0, &     !   High & West
            !               0.012829d0, &     !   Low & West
            !               0.031149d0, &     !   High & East
            !               0.0228816d0/) ! 0.038d0*(1-zeta_p**zbar)/(1-zeta_p) !(/0.038d0*(1-zeta_p**zbar)/(1-zeta_p), /)
        endif
        !zeta_p  =  0.95d0
        !sigma_nu_p =  0.0375d0*(1-zeta_p**zbar)/(1-zeta_p)
        zeta_p = zeta_p**zbar   
        
        a_l    = 0d0   ! dla bigJ = 80, a_l = -2d0, inaczej -8d0
        a_u    = 120d0   !dla bigJ = 80, a_u = 10d0, inaczej 30d0
        a_grow = 0.00d0  !dla bigJ = 80, a_grow = 0.05d0, inaczej 0.04d0  

        aime_l    = 0d0
    
elseif ((bigJ == 16) .or. (bigJ == 20) )then 
        
        if (omega_dim == 1) then
            zeta_p  =  0.938648d0
            sigma_nu_p = 0.0206832d0
        !elseif (omega_dim == 2) then
        !    zeta_p  =  (/0.938648d0, &     !   High & West
        !             0.762217d0/) !, &     !   Low & West
        !    sigma_nu_p = (/0.0206832d0, &     !   High & West
        !                   0.012829d0/)       !   Low & West
        elseif (omega_dim == 4) then
            !zeta_p  =  (/0.95828d0, &     !   High & West
            !             0.95666d0, &     !   Low & West
            !             0.95828d0, &     !   High & East
            !             0.95666d0/)      !   Low & East
            !!zeta_p = zeta_p*0.9
            !sigma_nu_p = (/0.0206832d0, &     !   High & West
            !               0.012829d0, &     !   Low & West
            !               0.031149d0, &     !   High & East
            !               0.0228816d0/) ! 0.038d0*(1-zeta_p**zbar)/(1-zeta_p) !(/0.038d0*(1-zeta_p**zbar)/(1-zeta_p), /)
        endif
        !sigma_nu_p = sigma_nu_p*2
        zeta_p = zeta_p**zbar   ! Why multiply by zbar ?

        
        a_l    = 0d0   !dla bigJ = 80, a_l = -2d0, inaczej -8d0
        a_u    = 200d0   !dla bigJ = 80, a_u = 10d0, inaczej 30d0
        a_grow = 0.04d0 !dla bigJ = 80, a_grow = 0.05d0, inaczej 0.04d0        
        aime_l    = 0d0
    

elseif (bigJ == 80) then
        !Germany
        sigma_nu_p =  0.034641777664665d0 
        zeta_p  =  0.901600770945489d0 
        
            zeta_p  =  (/0.954764109566016d0, &     !   High & West
                         0.901575250193900d0, &     !   Low & West
                         0.954764109566016d0, &     !   High & East
                         0.901575250193900d0/)      !   Low & East
            !zeta_p  =  (/0.966599469324064d0, &     !   High & West
            !             0.901600770945489d0, &     !   Low & West
            !             0.966599469324064d0, &     !   High & East
            !             0.901600770945489d0/)      !   Low & East
            !zeta_p = zeta_p*0.9
            sigma_nu_p = (/	0.00984463940054701d0, &     !   High & West
                           0.0346525116013985d0, &     !   Low & West
                           	0.00984463940054701d0, &     !   High & East
                           0.03465251160139855d0/) ! 0.038d0*(1-zeta_p**zbar)/(1-zeta_p) !(/0.038d0*(1-zeta_p**zbar)/(1-zeta_p), /)
            !sigma_nu_p = (/	0.007277334536640d0, &     !   High & West
            !               0.034641777664665d0, &     !   Low & West
            !               	0.007277334536640d0, &     !   High & East
            !               0.034641777664665d0/) ! 0.038d0*(1-zeta_p**zbar)/(1-zeta_p) !(/0.038d0*(1-zeta_p**zbar)/(1-zeta_p), /)
        
            !sigma_nu_p = 5*sigma_nu_p
            
            !employment_mat(2, 1, :) = 0.269136639d0   ! (/job finding rate, separation rate/)
            !employment_mat(2, 2, :) = 0.269136639d0
            !employment_mat(2, 3, :) = 0.06728416d0
            !employment_mat(2, 4, :) = 0.06728416d0
            !employment_mat(2, :, 1) = 0d0
            !do i = 1, bigT
            !    employment_mat(:, 1:2, i) = employment_mat(:, 1:2, i)*sum(omega_share(1, 1:2, i))/sum(omega_share(1, 3:4, i))
            !    employment_mat(:, 3:4, i) = employment_mat(:, 3:4, i)*sum(omega_share(1, 3:4, i))/sum(omega_share(1, 1:2, i))
            !enddo
            !employment_mat = 0d0
            
        a_l    = 0d0   !dla bigJ = 80, a_l = -2d0, inaczej -8d0
        a_u    = 140d0   !dla bigJ = 80, a_u = 10d0, inaczej 30d0
        a_grow = 0.04d0 !dla bigJ = 80, a_grow = 0.05d0, inaczej 0.04d0   
        aime_l    = 0d0
        aime_u    = 2d0 !2.8d0*0.33d0 ! to capture Old-Age, Survivors, and Disability Insurance (OASDI) tax cap
        a_grow = 0.04d0
    endif
    
    
pi_ip = 0d0
pi_ip_disc = 0d0
n_sp_value = 0d0
    
    
do i_o = 1, omega_dim
    
    n_sp_initial = int(n_sp_disc/2)+1   

    if (n_sp_disc > 5) then 

    elseif (n_sp_disc > 1)  then
        call discretize_AR(zeta_p(i_o), 0d0, sigma_nu_p(i_o), n_sp_value(1:n_sp_disc, i_o), pi_ip_disc(1:n_sp_disc, 1:n_sp_disc, i_o))
        n_sp_value(:, i_o) = exp(n_sp_value(:, i_o)) 
        
        n_sp_value(n_sp, :) = 0.25d0  !0d0
        do i = 1, bigT
            pi_ip(1:n_sp_disc, 1:n_sp_disc, i_o, i) = pi_ip_disc(1:n_sp_disc, 1:n_sp_disc, i_o)
        
            do ip = 1, n_sp_disc
                 pi_ip(ip, 1:n_sp_disc, i_o, i) = pi_ip(ip, 1:n_sp_disc, i_o, i)*(1 - employment_mat(2, i_o, i))
            enddo
            pi_ip(1:n_sp_disc, n_sp, i_o, i) = employment_mat(2, i_o, i)
            pi_ip(n_sp, 1:n_sp_disc, i_o, i) = employment_mat(1, i_o, i)/n_sp_disc
            pi_ip(n_sp, n_sp, i_o, i) = (1 - employment_mat(1, i_o, i))
        enddo
        
    elseif (n_sp_disc == 1 .and. n_sp > 1)  then    
        print *, "shock_params"
        pause
    else      
        pi_ip = 1d0
        n_sp_value = 1d0      
    endif
enddo
    
if (com == 0) then
    do i = 1, bigT
        pi_ip(:, :, :, i) = pi_ip(:, :, :, 1)
    enddo
endif
    
call chdir(cwd_w)
OPEN (unit=533,  FILE = "pi_ip.csv")
write(533, '(A)') "ip1; ip2; ip3; ip4; ip5; ip6; omega; i"
do i = 1, 30
    do ip = 1, n_sp
        !write(533,  '(F20.10, A, F20.10, A, F20.10, A, F20.10, A, F20.10, A, F20.10, A, I1, A, I2)') pi_ip(ip, 1, 1, i), ";",  pi_ip(ip, 2, 1, i), ";", pi_ip(ip, 3, 1, i), ";", pi_ip(ip, 4, 1, i), ";", pi_ip(ip, 5, 1, i), ";", pi_ip(ip, 6, 1, i), ";", i_o, ";", i
    enddo
enddo
close(533)
call chdir(cwd_r)
!pause
!call discretize_AR(zeta_r, 1d0, sigma_nu_r, n_sr_value, pi_ir)
!if (n_sd > 1) then 
!    call discretize_AR(zeta_d, 1d0, sigma_nu_d, n_sd_value, pi_id)
!endif
!    
!if (n_sr >1) then 
!    call normal_discrete_1(n_sr_value, prob_norm, 1d0, sigma_nu_r)
!    do  s = 1, n_sr, 1
!        pi_ir(s,:) = prob_norm     
!    enddo
!endif   

!! todo model is deterministic (evry state is the same) but we use vfi to solve it 
!do i_n_sp, n_sp
!    n_sp_value(i_n_sp) = n_sp_value(i_n_sp)*()
!enddo
    
!n_sp_value = 1d0
!n_sp_value = (/.7d0, 1d0, 1.75d0/)    


!write(*,*)n_sp_value

!write(f_name,'(I2)')int(theta*10)
!write(f_name3,'(I2)')int(n_sp_value(2)*10)
!write(f_name4,'(I2)')int(n_sp_value(3)*10)
!    
!if (n_sp_value(1) < 1) then
!    write(f_name2,'(I1)')int(n_sp_value(1)*10)
!    c_name = 'c_t_' // trim(f_name) // '_sp_0' // trim(f_name2) // '_' // trim(f_name3) // '_' // trim(f_name4) // '.csv'
!    s_name = 's_t_' // trim(f_name) // '_sp_0' // trim(f_name2) // '_' // trim(f_name3) // '_' // trim(f_name4) // '.csv'
!else
!    write(f_name2,'(I2)')int(n_sp_value(1)*10)  
!    c_name = 'c_t_' // trim(f_name) // '_sp_' // trim(f_name2) // '_' // trim(f_name3) // '_' // trim(f_name4) // '.csv'
!    s_name = 's_t_' // trim(f_name) // '_sp_' // trim(f_name2) // '_' // trim(f_name3) // '_' // trim(f_name4) // '.csv'
!endif


!write(*,*)f_name
!write(*,*)int(theta*10) 
!write(*,*)c_name
