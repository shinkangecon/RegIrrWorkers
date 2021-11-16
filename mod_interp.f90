module mod_interp
    
    
    contains
    
    function bsearch(xhat,x) result(ilo)
    
    implicit none
    real(8),dimension(:),intent(in)::x
    real(8),intent(in)::xhat
    integer::ilo,n,a,b,iter,iter_max
    real(8)::p
    
    
    ! Declare needed variables
    n=size(x)
    a=1
    b=n
    
    ! Maximum of the number of iteration
    p=n
    iter_max=floor(log10(p)/log10(2d0))+1
    
    
    ! Binary search
    if (xhat<=x(1)) then
        ilo=1
    elseif (xhat>=x(n)) then
        ilo=n-1
    else
        do iter=1,iter_max            
            ilo=floor((b+a)*.5d0)                
            
            if ((b-a)==1) then               
                exit
            end if
                       
            if (xhat<x(ilo)) then
                b=ilo
            else
                a=ilo
            end if
            
        end do        
        ilo=a
    end if 

    end function bsearch
    
    ! This function constructs a linear interpolation of the data assuming x is sorted and evaluates it
    
    function interp1q(x,y,xi) result(yi)
    
    implicit none
    
    real(8),intent(in)::x(:),y(:),xi
    real(8)::yi
    
    ! local
    integer::iter,n
    real(8)::m    
    n=size(x)
    
    if (xi<x(1)) then
        m=(y(2)-y(1))/(x(2)-x(1))
        yi=y(1)+(xi-x(1))*m
    elseif (xi>x(n)) then
        m=(y(n)-y(n-1))/(x(n)-x(n-1))
        yi=y(n-1)+(xi-x(n-1))*m
    else
        iter=bsearch(xi,x)
        m=(y(iter+1)-y(iter))/(x(iter+1)-x(iter))
        yi=y(iter)+(xi-x(iter))*m
    end if
    
    end function interp1q
    
    
end module mod_interp