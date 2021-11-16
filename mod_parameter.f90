module mod_parameter
    implicit none   
    
    !! Production function and policy instruments
    ! Y=x(alpha*nr^rho+(1-alpha)*nir^rho)^(1/rho): Estimation result from WPS 2015~2017
    real(8), parameter:: beta=(1d0/1.038d0)**(1d0/12d0)                         ! monthly time discount factor
    real(8), parameter:: alpha=.838d0                                           ! Share of Regular workers
    real(8), parameter:: rho=.528d0                                             ! Elasticity of Substitution between Regular and Irregualr workers
    real(8), parameter:: p=1d0                                                  ! x'=A+rho_x+eps_x, eps_x~N(0,sig_x)
    
    ! Firing & Hiring cost
    real(8), parameter:: cm=4.3d0*7d0/31d0!cm=4.3d0*7d0/31d0               ! �ذ��� ���:����ӱ� 4.3��ġ. �ذ��� ����� 27.4��ġ. �ڷ�: https://www.thescoop.co.kr/news/articleView.html?idxno=34705
    ! Hiring cost: ����Ʈ �������� 2009�� �ڷ� ����. ������ ���� �����ϴٰ� ���� 
    real(8), parameter:: cp_large=1885d0/(3886d0*12d0)                 ! 2009�� ����Ʈ ��������: ä�뱤��, ��.�����˻� ��. ���� ä����: 188��5õ��(https://biz.chosun.com/site/data/html_dir/2010/01/05/2010010500234.html), 500�� �̻� ����ӱ�: 3886õ�� (https://news.mt.co.kr/mtview.php?no=2021031415120751577, ��μ� '��-�߼ұ�� �� �뵿���� ���� ��ȭ �м�')
    real(8), parameter:: cp_small=889d0/(2489d0*12d0)                  ! 2009�� ����Ʈ ��������: ä�뱤��, ��.�����˻� ��. �߼ұ�� ä����: 89��9õ��(https://biz.chosun.com/site/data/html_dir/2010/01/05/2010010500234.html), 5~499�� �̻� ����ӱ�: 2489õ�� (https://news.mt.co.kr/mtview.php?no=2021031415120751577, ��μ� '��-�߼ұ�� �� �뵿���� ���� ��ȭ �м�')
    
    ! Policy intstrument: ���ο��� - ����� vs. �ӽ�/�Ͽ���
    real(8), parameter:: sec_reg_firm=.045d0, sec_reg_work=.045d0, sec_irr_firm=0d0, sec_irr_work=.09d0
    
    ! Policy intstrument: �ǰ����� - ����� vs. �ӽ�/�Ͽ���
    real(8), parameter:: health_reg_firm=.0323d0, health_reg_work=.0323d0, health_irr_firm=0d0, health_irr_work=.0648d0
    
    ! Policy instrument: ��뺸��, �ٷ���/����� 50%
    real(8), parameter:: emp_ins=.008d0
    
    ! Policy instrument: ���纸��. 2021�⵵ ��������� ���纸�����
    real(8), parameter:: ind_ins_firm=(185d0+57d0+16d0+11d0+20d0+10d0+13d0+7d0+13d0+10d0+6d0+24d0+8d0+36d0+8d0+18d0+9d0+58d0+28d0+20d0+8d0+9d0+6d0+8d0+7d0+9d0+6d0)/(1000d0*28d0), ind_ins_worker=0d0    
    real(8), parameter:: tau_corp=.1665d0           ! ���μ� 2015 & 2017�� ��� ��ȿ����
    
    !! Idiosyncratic shock process: Estimation using KED & EIDB
    real(8), parameter:: rho_x=.72869d0**(1d0/12d0), sig_x=.2375d0*sqrt(1d0-.72869d0**(1d0/6d0))/sqrt(1d0-.72869d0**2d0)                           
    
    !! Wage equation
    ! Bargarining power: Estimated Elasticity of Matching function from ������(2020) and applying Hosios condition
    real(8), parameter:: eta_reg=.7d0, eta_irr=.4d0!hosios=.859d0, eta_reg=(1d0-(hosios-0.08d0)), eta_irr=(1d0-(hosios+0.08d0))
    real(8), parameter:: rep_ratio=.615d0                                        ! Replacement ratio of unemployment insurance benefit: 61.5%. �̴�â(2016) "�ѱ��� �����޿� ���޷� �������� �м�", �뵿�������� 39(1) 1-31
    
    !! Grid
    real(8), parameter:: nr_min=1d0, nr_max=3000d0, nir_min=0d0, nir_max=.3d0*nr_max
    integer, parameter:: num_reg_grid=120+floor(.05d0*(nr_max-300d0)), num_irr_grid=121+floor(.05d0*(nir_max-300d0))
    integer, parameter:: nx=15
    
    !! Computation criteria
    real(8), parameter:: tol=1d-6, adj=.8d0
    
    !! Calibration target moment
    real(8), parameter:: En_target=2271.5d0/417.5d0
    
    ! Simulation
    integer,parameter:: simT=1000, simN=5000
    
end module mod_parameter