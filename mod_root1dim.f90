module mod_root1dim
    
    type opt_root1dim
        real(8)::xtol=1d-6  ! x tolerance
        integer::maxit=1000 ! maximum number of itration
    end type
        
    contains
    
    subroutine sub_bisect(x,f,a,b,opt)    
        implicit none
        real(8),intent(inout)::x
        interface
            function f(x) result(y)
                implicit none
                real(8),intent(in)::x
                real(8)::y                
            end function
        end interface
        real(8),intent(in)::a,b
        integer,intent(in),optional::opt
        
        ! local       
        real(8)::fm,m,fa,fb,tol
        real(8),parameter::twoeps=2d0*epsilon(0d0)
        integer::iter,iter_max
        real(8)::p,q
        type(opt_root1dim)::crt
        
        if (present(opt)) then
            iter_max=crt%maxit              ! Maximum number of iteration when opt is present
            tol=crt%xtol                    ! tolerance level when opt is present
        else
            iter_max=2000                   ! Default value of the maximum number of iteration
            tol=1d-10                       ! Default value of tolerance level
        end if
               
        p=a                                 ! Storage variable for a
        q=b                                 ! Storage variable for b
        
        do iter=1,iter_max
        
        fa=f(p)
        fb=f(q)
        m=.5d0*(p+q)
        
        if (fa*fb>0d0) then                 ! Check the sign of f(a) and f(b). If sign(f(a))==sign(f(b)), then it will stop
            stop 'root is not bracketed!'   ! Not bracketed case
        else
            if (abs(p-q)<abs(m)*twoeps+tol) then
                x=merge(p,q,abs(fa)<abs(fb))
                return
            else
                fm=f(m)
                if (f(m)==0d0) then
                    x=m
                    return
                end if
                
                if (f(m)*f(p)>0d0) then
                    p=m
                else
                    q=m
                end if
            end if
        end if
        
        end do
        
    end subroutine sub_bisect
    
end module mod_root1dim