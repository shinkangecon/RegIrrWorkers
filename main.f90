program main 
    
    ! OLG model
    
    use mod_parameter
    use mod_matlab
    use mod_discreteAR1
    use mod_bellman
    use mod_ergodic
    use mod_interp
    
    implicit none    
    
    ! Value and Policy functions
    real(8), dimension(num_reg_grid,nx):: pi_val,pi_val0,Epi_val
    real(8), dimension(num_reg_grid,nx):: gn_reg,gn_irr    
    real(8), dimension(num_reg_grid,nx):: wage_reg,wage_reg_guess,wage_irr,wage_irr_guess
    integer, dimension(num_reg_grid,nx):: idx_reg,idx_irr
    
    ! Stationary distribution
    real(8), dimension(num_reg_grid,nx):: phi0, phi
    
    ! Grid and Tauchen
    real(8):: reg_grid(num_reg_grid), irr_grid(num_irr_grid), logx(nx), xgrid(nx), Fxx(nx,nx)
    
    ! Temp
    real(8):: tmp_pi_exp(nx),firing,hiring,delta_np,delta_nm,errV,errW,cover, qn, t1,t2, tmp_num_ntgrid, tmp_mpn_reg, tmp_mpn_irr, tmp_irr, tmp_reg,exp_wage_reg,xt
        
    ! For iteration
    integer:: iter, i_n,i_nt,i_np,i_ntp,ix,ixp,iter_v
    
    ! Simulation
    !real(8),dimension(simT,simN):: sim_emp,sim_emp_nosubsidy,sim_emp_recession,sim_emp_recession_nosubsidy,sim_emp_recession_covidsubsidy,sim_leave,sim_leave_nosubsidy,sim_leave_recession,sim_leave_recession_nosubsidy,sim_leave_recession_covidsubsidy
    !integer:: sim_time,sim_firm,coin
    
    
    ! Tauchen
    cover=3d0*sig_x/sqrt(1d0-rho_x**2d0)   
    call sub_tauchen(logx,Fxx,rho_x,sig_x,cover,nx)
    xgrid=exp(logx)  
    
    ! Construct grids: Employment n_{t-1} \& On-leave employment n^t_{t-1}
    reg_grid(1:99)=linspace(1d0,99d0,99)                                            ! Employment n_{t-1}: 1~99명까진 1명 단위 grid                               
    reg_grid(100:119)=linspace(100d0,290d0,20)                                      ! 100~299명까진 10명 단위 grid
    reg_grid(120:num_reg_grid)=linspace(300d0,nr_max,num_reg_grid-120+1)            ! 300~n_{max}까지 20명 단위 grid
    
    irr_grid(1:100)=linspace(0d0,99d0,100)
    irr_grid(101:120)=linspace(100d0,290d0,20)
    irr_grid(121:num_irr_grid)=linspace(300d0,nir_max,num_irr_grid-121+1)
    
    open(201,file='reg_grid.txt',status='replace')
    write(201,*) reg_grid
    close(201)
    
    open(202,file='irr_grid.txt',status='replace')
    write(202,*) irr_grid
    close(202)
    
    open(203,file='xgrid.txt',status='replace')
    write(203,*) xgrid
    close(203)
    
        
    ! Guess on wages
    do ix=1,nx
        xt=xgrid(ix)
        do i_n=1,num_reg_grid
            tmp_irr=max(1d0,.3d0*reg_grid(i_n))
            tmp_mpn_reg=(1d0-tau_corp)*p*xt*alpha*reg_grid(i_n)**(rho-1d0)*(alpha*reg_grid(i_n)**rho+(1d0-alpha)*tmp_irr**rho)**(1d0/rho-1d0)
            tmp_mpn_irr=(1d0-tau_corp)*p*xt*(1d0-alpha)*tmp_irr**(rho-1d0)*(alpha*reg_grid(i_n)**rho+(1d0-alpha)*tmp_irr**rho)**(1d0/rho-1d0)
            
            wage_reg_guess(i_n,ix)=xgrid(ix)*tmp_mpn_reg
            wage_irr_guess(i_n,ix)=xgrid(ix)*tmp_mpn_irr
        end do
    end do    
        
    ! Benchmark case    
    ! Initialize error value
    errV=1d0
    errW=1d0
    iter=1
    !pi_val0=0d0    
    
    write(*,*) 'Solving Bellman equation'
    
    call CPU_TIME(t1)
    do while(errW>tol)          ! outer-loop for wage
        errV=1d0
        iter_v=1
        do while(errV>tol)
            ! Computing the expected value of VF
            Epi_val=matmul(pi_val0,transpose(Fxx))
            
            ! Value function iteration: Grid search
            !    sub_bellman_discrete(pi_val,Epi_val,gn_reg,gn_irr,wage_reg_guess,wage_irr_guess,xgrid,idx_reg,idx_irr,cm,cp_large,cp_small,beta,nx,num_reg_grid,num_irr_grid,reg_grid,irr_grid,alpha,rho,p,sec_reg_firm, sec_irr_firm, health_reg_firm, health_irr_firm, emp_ins, ind_ins_firm,tau_corp)
            call sub_bellman_discrete(pi_val,Epi_val,gn_reg,gn_irr,wage_reg_guess,wage_irr_guess,xgrid,idx_reg,idx_irr,cm,cp_large,cp_small,beta,nx,num_reg_grid,num_irr_grid,reg_grid,irr_grid,alpha,rho,p,sec_reg_firm, sec_irr_firm, health_reg_firm, health_irr_firm, emp_ins, ind_ins_firm,tau_corp)
            errV=maxval(abs(pi_val-pi_val0)/abs(1d0+pi_val0))
            
            ! Updating
            pi_val0=adj*pi_val+(1d0-adj)*pi_val0
            iter_v=iter_v+1
            
            !if (mod(iter_v,10)==0) then
            !    write(*,*) iter_v,errV
            !end if
        end do       

        
        ! Wage updating
        do ix=1,nx
            xt=xgrid(ix)
            do i_n=1,num_reg_grid
                tmp_reg=gn_reg(i_n,ix)
                tmp_irr=gn_irr(i_n,ix)
                !tmp_mpn_reg=p*xt*(1d0-tau_corp)*alpha*tmp_reg**(rho-1d0)*(alpha*tmp_reg**rho+(1d0-alpha)*tmp_irr**rho)**(1d0/rho-1d0)
                !tmp_mpn_irr=p*xt*(1d0-tau_corp)*(1d0-alpha)*tmp_irr**(rho-1d0)*(alpha*tmp_reg**rho+(1d0-alpha)*tmp_irr**rho)**(1d0/rho-1d0)
                tmp_mpn_reg=(1d0-tau_corp)*(p*xt*alpha*(tmp_reg**(rho-1d0))*(alpha*tmp_reg**rho+(1d0-alpha)*tmp_irr**rho)**(1d0/rho-1d0))
                tmp_mpn_irr=(1d0-tau_corp)*(p*xt*(1d0-alpha)*(tmp_irr**(rho-1d0))*(alpha*tmp_reg**rho+(1d0-alpha)*tmp_irr**rho)**(1d0/rho-1d0))
                !tmp_mpn_reg=(1d0-tau_corp)*alpha*eta_reg*(p*xt*(alpha*tmp_reg**rho+(1d0-alpha)*tmp_irr**rho)**(1d0/rho))/(tmp_reg+tmp_irr) ! <- 여기서 범핑되는게 다른듯
                !tmp_mpn_irr=(1d0-tau_corp)*(1d0-alpha)*eta_irr*(p*xt*(alpha*tmp_reg**rho+(1d0-alpha)*tmp_irr**rho)**(1d0/rho))/(tmp_reg+tmp_irr)
                if (tmp_reg==0d0) then
                    wage_reg(i_n,ix)=0d0
                else
                    ! 보험이 들어가는 부분을 수정해보자
                    !wage_reg(i_n,ix)=eta_reg*tmp_mpn_reg/(eta_reg*(1d0+sec_reg_firm+health_reg_firm+emp_ins+ind_ins_firm)+(1d0-eta_reg)*(1d0-sec_reg_work-health_reg_work-emp_ins)-(1d0-eta_reg)*rep_ratio) 
                    wage_reg(i_n,ix)=eta_reg*tmp_mpn_reg+(1d0-eta_reg)*5d0!+(1d0-eta_reg)*rep_ratio*wage_reg_guess(i_n,ix)
                    !exp_wage_reg=sum(wage_reg_guess(idx_reg(i_n,ix),:)*Fxx(ix,:))
                    !exp_wage_reg=0d0
                    !if (tmp_reg+tmp_irr<300) then                         
                    !    wage_reg(i_n,ix)=eta_reg*(tmp_mpn_reg+beta*exp_wage_reg*(cp_small-cm))/(eta_reg*(1d0+sec_reg_firm+health_reg_firm+emp_ins+ind_ins_firm)+(1d0-eta_reg)*(1d0-sec_reg_work-health_reg_work)-(1d0-eta_reg)*rep_ratio)
                    !else
                    !    wage_reg(i_n,ix)=eta_reg*(tmp_mpn_reg+beta*exp_wage_reg*(cp_large-cm))/(eta_reg*(1d0+sec_reg_firm+health_reg_firm+emp_ins+ind_ins_firm)+(1d0-eta_reg)*(1d0-sec_reg_work-health_reg_work)-(1d0-eta_reg)*rep_ratio)
                    !end if
                end if
                
                if (tmp_irr==0d0) then
                    wage_irr(i_n,ix)=0d0
                else              
                    !wage_irr(i_n,ix)=eta_irr*tmp_mpn_irr/(eta_irr*(1d0+sec_irr_firm+health_irr_firm+emp_ins+ind_ins_firm)+(1d0-eta_reg)*(1d0-sec_irr_work-health_irr_work)-(1d0-eta_irr)*rep_ratio)
                    wage_irr(i_n,ix)=eta_irr*tmp_mpn_irr+(1d0-eta_reg)*5d0!+(1d0-eta_irr)*rep_ratio*wage_irr_guess(i_n,ix)
                end if
            end do
        end do
    
        errW=max(maxval(abs(wage_reg-wage_reg_guess)),maxval(abs(wage_irr-wage_irr_guess)))     
        
        if (mod(iter,10)==0) then
            write(*,*) iter, errW
            open(300,file='gn_reg.txt',status='replace')
            write(300,*) gn_reg
            close(300)

            open(301,file='gn_irr.txt',status='replace')
            write(301,*) gn_irr
            close(301)

            open(302,file='wage_reg.txt',status='replace')
            write(302,*) wage_reg
            close(302)

            open(303,file='wage_irr.txt',status='replace')
            write(303,*) wage_irr
            close(303)
 
            open(303,file='pi_val.txt',status='replace')
            write(303,*) pi_val
            close(303)
        end if
        
        ! update
        !wage_reg_guess=adj*wage_reg+(1d0-adj)*wage_reg_guess
        !wage_irr_guess=adj*wage_irr+(1d0-adj)*wage_irr_guess
        wage_reg_guess=wage_reg
        wage_irr_guess=wage_irr
        iter=iter+1        
        
        if (iter>2000) then
            errW=0d0
        end if        
    end do
    
    ! Stationary distribution
    !phi0=1d0/(nx*num_reg_grid*num_ntgrid)
    !phi0(100,1,5)=1d0
    !phi=0d0
    !errV=1d0
    !write(*,*) 'Computing stationary distribution'
    !do while (errV<1d-10)
    !    do ix=1,nx
    !        do i_n=1,num_reg_grid
    !            !tmp_num_ntgrid=floor(.5d0*i_n)+2
    !            do i_nt=1,num_ntgrid
    !                if (reg_grid(i_n)>ntgrid(i_nt) .AND. idx_emp(i_n,i_nt,ix)>0 .AND. idx_leave(i_n,i_nt,ix)>0) then
    !                    do ixp=1,nx
    !                        phi(idx_emp(i_n,i_nt,ix),idx_leave(i_n,i_nt,ix),ixp)=phi(idx_emp(i_n,i_nt,ix),idx_leave(i_n,i_nt,ix),ixp)+Fxx(ix,ixp)*phi0(i_n,i_nt,ix)
    !                    end do
    !                end if                    
    !            end do
    !        end do
    !    end do
    !    
    !    errV=maxval(phi-phi0)        
    !    !updating
    !    phi0=phi
    !end do
    
    call CPU_TIME(t2)
        
    ! Print out results
   
    
end program main