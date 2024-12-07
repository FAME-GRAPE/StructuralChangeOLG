!module params
!    
!    implicit none 
!    integer, save :: split(8)
!    !integer :: split(8)
!    !save
!    integer, public :: n_iter_ss
!    integer, public :: n_iter_t
!    integer, public :: bigJ
!    integer, public :: omega_dim
!    integer, public :: n_sp
!
!    !integer, parameter :: n_iter_ss = 200
!    !integer, parameter :: n_iter_t = 30
!    !integer, parameter :: bigJ = 4d0
!    !integer, parameter :: omega_dim = 2d0
!    !integer, parameter :: n_sp = 1
!    !call read_main_params(split(8)) 
!    
!contains 
!    subroutine read_main_params()    
!        implicit none
!        character*32 :: params_vector
!        
!        integer :: n, nn
!    
!        call execute_command_line ('dir "f:\janek\code trans\parm" /b > "f:\janek\code trans\_params.txt"') !_ *.txt
!        open (unit=900, FILE = "_params.txt")
!            read(900,*) params_vector
!        close(900)
!        !params_vector = "2,4,63,3"
!        !nn = len(params_vector)
!        n = count(transfer(params_vector, 'a', len(params_vector)) == ",")
!        read(params_vector, *) split(1:n+1) ! N+1 because one more int than comma   ! problem is here
!        call defi(split(8))
!        !print *, 'nvalues=', n+1
!        print '(i3)', split(1:n+1)
!        ! add command that deletes _params.txt
!    end subroutine
!    !subroutine defi(split)
!    !    implicit none 
!    !    integer :: split
!    !    integer, parameter :: n_iter_ss = split(1)
!    !    integer, parameter :: n_iter_t = split(2)
!    !    integer, parameter :: bigJ = split(3)
!    !    integer, parameter :: omega_dim = split(4)
!    !    integer, parameter :: n_sp = split(5)
!    !end subroutine
!
!end module 