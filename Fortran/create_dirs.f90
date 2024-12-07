module new_dir
    
    use ifport
    
contains

    subroutine ndir(dir)
        
        character(256), intent(in) :: dir
        logical(4) :: test
        
        test = makedirqq(dir)
        
        if (test == FALSE) print *, 'Directory already exists'
            
    end subroutine
    
end module