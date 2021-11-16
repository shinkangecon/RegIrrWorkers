module mod_ergodic
    
    implicit none
    
    contains
    
    subroutine sub_stationary_distribution(phi1,phi,api,Fzz,nK,nZ)
    
    ! Variables
    integer,intent(in):: nK,nZ
    integer,intent(in)::api(nK,nZ)
    real(8),intent(out)::phi1(nK,nZ)
    real(8),dimension(nK,nZ),intent(in)::phi(nK,nZ)
    real(8),intent(in)::Fzz(nZ,nZ)
    
    
    ! Declare parameter for iteration
    integer::ik,iz,izz
    
    
    
    do iz=1,nZ
        do izz=1,nZ
            do ik=1,nK
                phi1(api(ik,iz),izz)=phi1(api(ik,iz),izz)+Fzz(iz,izz)*phi(ik,iz)
            end do
        end do
    end do
    
    
    end subroutine sub_stationary_distribution 
    
    

    
end module mod_ergodic        