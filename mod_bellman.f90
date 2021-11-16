module mod_bellman
    implicit none
    
    contains
    
    subroutine sub_bellman_discrete(pi_val,Epi_val,gn_reg,gn_irr,wage_reg,wage_irr,xgrid,idx_reg,idx_irr,cm,cp_large,cp_small,beta,nx,num_reg_grid,num_irr_grid,reg_grid,irr_grid,alpha,rho,p,sec_reg_firm, sec_irr_firm, health_reg_firm, health_irr_firm, emp_ins, ind_ins_firm,tau_corp)
        ! Varialbes
        integer, intent(in):: nx,num_reg_grid,num_irr_grid
        real(8), intent(in):: reg_grid(num_reg_grid), irr_grid(num_irr_grid), xgrid(nx),beta,alpha,rho,p
        real(8), dimension(num_reg_grid,nx), intent(in) :: Epi_val,wage_reg,wage_irr
        real(8), dimension(num_reg_grid,nx), intent(out):: pi_val,gn_reg,gn_irr
        integer, dimension(num_reg_grid,nx), intent(out):: idx_reg,idx_irr
        real(8), intent(in):: cm,cp_large,cp_small
        real(8), intent(in):: sec_reg_firm, sec_irr_firm, health_reg_firm, health_irr_firm, emp_ins, ind_ins_firm, tau_corp
    
        ! temporal index
        integer:: ix, i_ntm, i_nt, i_n_irr, tmp_irr_grid
        real(8):: ntm, xt, tmp_reg, tmp_irr, tmp_pi_val
        
        gn_reg=0d0
        gn_irr=0d0
    
        do ix=1,nx
            xt=xgrid(ix)
            do i_ntm=1,num_reg_grid
                ntm=reg_grid(i_ntm)
                pi_val(i_ntm,ix)=0d0
                do i_nt=1,num_reg_grid
                    tmp_reg=reg_grid(i_nt)
                    do i_n_irr=1,num_irr_grid
                        tmp_irr=irr_grid(i_n_irr)
                        ! evaluating the value of firm by size
                        if (tmp_reg+tmp_irr<300) then
                            tmp_pi_val=p*xt*(alpha*tmp_reg**rho+(1d0-alpha)*tmp_irr**rho)**(1d0/rho)-wage_reg(i_ntm,ix)*(1d0+sec_reg_firm+health_reg_firm+emp_ins+ind_ins_firm)*tmp_reg-wage_irr(i_ntm,ix)*(1d0+sec_irr_firm+health_irr_firm+emp_ins+ind_ins_firm)*tmp_irr-wage_reg(i_ntm,ix)*(cm*max(0d0,ntm-tmp_reg)+cp_small*max(0d0,tmp_reg-ntm))
                            tmp_pi_val=(1d0-tau_corp)*tmp_pi_val+beta*Epi_val(i_nt,ix)
                        else
                            tmp_pi_val=p*xt*(alpha*tmp_reg**rho+(1d0-alpha)*tmp_irr**rho)**(1d0/rho)-wage_reg(i_ntm,ix)*(1d0+sec_reg_firm+health_reg_firm+emp_ins+ind_ins_firm)*tmp_reg-wage_irr(i_ntm,ix)*(1d0+sec_irr_firm+health_irr_firm+emp_ins+ind_ins_firm)*tmp_irr-wage_reg(i_ntm,ix)*(cm*max(0d0,ntm-tmp_reg)+cp_large*max(0d0,tmp_reg-ntm))
                            tmp_pi_val=(1d0-tau_corp)*tmp_pi_val+beta*Epi_val(i_nt,ix)
                        end if
    
                        if (tmp_pi_val<0d0) then
                            tmp_pi_val=-1d100
                        else
                            if (tmp_pi_val>pi_val(i_ntm,ix)) then
                                pi_val(i_ntm,ix)=tmp_pi_val
                                gn_reg(i_ntm,ix)=tmp_reg
                                gn_irr(i_ntm,ix)=tmp_irr
                                idx_reg(i_ntm,ix)=i_nt
                                idx_irr(i_ntm,ix)=i_n_irr
                            end if
                        end if
                    end do
                end do
            end do
        end do
    
    end subroutine sub_bellman_discrete  
    
end module mod_bellman        