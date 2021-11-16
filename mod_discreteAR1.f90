module mod_discreteAR1
    use mod_matlab
    
    implicit none
    
    contains
    subroutine sub_tauchen(logy,Fyy,ar1,cdnal_stdev,cover,n)
    
    ! Note: I treat logy as zgrid
       
    integer,intent(in)::n
    real(8),intent(in)::ar1,cdnal_stdev,cover
    real(8),intent(out)::logy(n),Fyy(n,n)
    
    real(8)::step,m(n-1),cond_mean,sig_z
    integer::iter,iy,iyp
    
    ! Special case: If n=1. i.e, deterministic case
    if (n==1) then
        logy=0d0
        Fyy=1d0
        print*,'This economy does not contain stochastic component'
    end if
    
    
    ! [Step1] Construct ygrid
    ! cover: the counterpart of kappa
    
    logy(1)=-cover
    logy(n)=cover
    step=(logy(n)-logy(1))/(n-1)   

    
    do iter=2,n-1
        logy(iter)=logy(iter-1)+step
    end do
    
    iter=1   
    
    ! [Step2] Construct transition probability matrix
    ! 1. Construct mid_point
     do iter=1,n-1
        m(iter)=(logy(iter)+logy(iter+1))/2d0
     end do
    
     do iy=1,n
        cond_mean=ar1*logy(iy)
        Fyy(iy,1)=normcdf(m(1),cond_mean,cdnal_stdev)
        do iyp=2,n-1
            Fyy(iy,iyp)=normcdf(m(iyp),cond_mean,cdnal_stdev)-normcdf(m(iyp-1),cond_mean,cdnal_stdev)
        end do
        Fyy(iy,n)=1-normcdf(m(n-1),cond_mean,cdnal_stdev)
     end do
     
    end subroutine sub_tauchen
    
    
end module mod_discreteAR1