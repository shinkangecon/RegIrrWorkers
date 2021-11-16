module mod_matlab
    
    implicit none
    
    real(8),parameter::pi=3.14159265358979323846
    
    contains
    !function: linspace. Reference from Matlab code in toolbox(linspace.m)
    !This linsapce code is restrictive. n should be integer
    function linspace(a,b,n) result(x)
    
    ! input variables
    real(8),intent(in)::a,b
    integer,intent(in)::n
    
        
    real(8)::x(n),step
    integer::iter
    
    step=(b-a)/(n-1)
    x(1)=a
        
    do iter=2,n
        x(iter)=x(iter-1)+step
    end do
    end function linspace
    
    !function: normcdf. Reference from Matlab function in toolbox(normcdf.m) and Wikepedia
    function normcdf(x,mean,std_dev) result(F)
    real(8),intent(in)::x,std_dev,mean
    real(8)::F,z
    
    if (std_dev<=0) then 
        stop 'variance cannot be negative neither zero!'
    end if
    
    ! Standardize
    z=(x-mean)/std_dev
    
    F=.5d0*erfc(-z/(2**(.5d0)))
    
    end function normcdf
    
    
    
end module mod_matlab