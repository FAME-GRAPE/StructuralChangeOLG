!module dynamic_wrt
!    
!    
!    
!implicit none
! 
!integer,dimension(:),allocatable :: x
!integer :: i,n
! 
!integer,parameter :: chunk_size = 100
! 
!n = 0
!do i=0,100000
!    call add_to(x,i,n,chunk_size,finished=i==100000)
!end do
! 
!contains
!     
!    function wrt(to_file, dims)    
!    
!        real*8, dimension(dims) :: temp_mat
!        integer, allocatable :: dims
!    
!        do w = 1, size(dims)
!            do ww = 1, dims(w)
!                write(*,*)to_file()
!            enddo
!        enddo
!
!    end function
!
!end module