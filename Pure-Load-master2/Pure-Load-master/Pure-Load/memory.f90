    module memory
    implicit none
    contains
    !-------------------------------------------------------------------------------------
    ! �����ڴ�
    !-------------------------------------------------------------------------------------
    subroutine alloc_memory_stru()
    use electr
    use outputdata
    use xml_data_helicopter_parameters,only:NE,NDE,NGS,NE,NB,NP,NRAIR,NRMOM
    use elemindata, airfoil_ele => airfoil
    implicit none
    !һά�����洢��Ҫ�õ�������
    allocate(NBND(6,NDE))
    allocate(NADR(NDE))
    allocate(NNOD(5,NE))
    allocate(POS_DOF(6,NDE))
    !����Ҷ�������Բ���
    allocate(D(NGS,NE))
    allocate(G(NGS,NE))
    allocate(AL(NE))
    allocate(A(NGS,NE))
    allocate(EY(NGS,NE))
    allocate(Ez(NGS,NE))
    allocate(jY(NGS,NE))
    allocate(jz(NGS,NE))
    allocate(sj(6,NGS,NE))
    allocate(AR(NGS,NE))
    allocate(bR(NGS,NE))
    allocate(XA(NGS,NE))
    allocate(CD0(NGS,NE))
    allocate(airfoil_ele(NGS,NE))
    allocate(THIT0(NE))
    allocate(j(NGS,NE))
    allocate(THIT(NGS,NE))
    allocate(YC14(NGS,NE))
    allocate(ZC14(NGS,NE))
    !��Ԫ����λ��
    allocate(DIS(NB,NE,(NP+1)))
    allocate(VEL(NB,NE,(NP+1)))
    allocate(ACC(NB,NE,(NP+1)))
    !����Ԫ��˹�㴦�ٶȳ�
    allocate(VR0(3,NGS,NE,(NP+1)))
    allocate(ART(3,(NP+1)))
    allocate(OMGH(3,3,(NP+1)))
    allocate(OMGS(3,3,(NP+1)))
    !�������������
    allocate(PITCH(NP+1))
    allocate(VITCH(NP+1))
    allocate(AITCH(NP+1))
    !������뽰Ҷ������
    allocate(QEHP(6,NE,(NP+1)))
    allocate(QELCP(6,NE,(NP+1)))
    allocate(QEGHP(6,NGS,NE,(NP+1)))
    allocate(QEGLCP(6,NGS,NE,(NP+1)))
    !�����������Ϣ
    allocate(AIRL(10,NGS,NE,(NP+1)))
    allocate(ALEN(NGS,NE))
    !�յ�����ֵ
    allocate(FLOWX(NGS*NE,NP+1))
    allocate(FLOWY(NGS*NE,NP+1))
    allocate(FLOWZ(NGS*NE,NP+1))
    allocate(TIAL(21,NGS,NE))
    allocate(FMOM(6,NGS,NE,(NP+1)))
    end subroutine alloc_memory_stru
    subroutine alloc_memory_aero()
    use xml_data_helicopter_parameters
    use xml_data_solver_parameters
    use FRIGHT
    use parameters
    use BLADEFEM
    use FREE_WAKE
    use FREE_WAKE_ITERATION_MODULE
    use aerodynamic_force
    use FLOWGRID
    use DYNAMICS_STALL
    use AZIMUTH
    use RESULTV
    use GRID
    implicit none

    allocate(XFLAP(3,NC,N+1))                                      !��Ҷ����Ԫ
    allocate(XROTOR(3,N+1))
    allocate(XCONTROL_BLAde(3,NC,N))
    allocate(xtrailedge_blade(3,n+1))
    allocate(XNOde(3,NC,N+1,rotors(1)%number_of_blades))
    allocate(XCONTROL(3,NC,N,rotors(1)%number_of_blades))
    allocate(Xtrail_edge(3,N+1,rotors(1)%number_of_blades) )
    allocate(xgrid_node(3,Nc+1,N+1))

    allocate(xflap0(3,N+1))
    allocate(xcontrol0(3,N,rotors(1)%number_of_blades))
    allocate(xnode0(3,N+1,rotors(1)%number_of_blades))

    allocate(C(N))
    allocate(C_NOde(N+1))
    allocate(BETA(3,rotors(1)%number_of_blades,LMAX))
    allocate(VB(N,rotors(1)%number_of_blades,LMAX))
    allocate(RC_NEAR(N+1))

    allocate(THETA(rotors(1)%number_of_blades,LMAX))
    allocate(SITAIn(N+1,rotors(1)%number_of_blades,LMAX))
    allocate(SITATIn(N+1,rotors(1)%number_of_blades,LMAX))
    allocate(SITA_CENTERIn(N,rotors(1)%number_of_blades,LMAX))
    allocate(SITA(N+1,rotors(1)%number_of_blades,LMAX))
    allocate(SITAT(N+1,rotors(1)%number_of_blades,LMAX))
    allocate(SITA_CENTER(N,rotors(1)%number_of_blades,LMAX))

    allocate(R0(3,KMAX,rotors(1)%number_of_blades,LMAX))                                   !β��ģ��
    allocate(V0(3,KMAX,rotors(1)%number_of_blades,LMAX))
    allocate(FTIP0(KMAX,rotors(1)%number_of_blades,LMAX))
    allocate(RC0(KMAX,rotors(1)%number_of_blades,LMAX))
    allocate(FB0(N,rotors(1)%number_of_blades,LMAX))
    allocate(fb_bound0(NC,N,rotors(1)%number_of_blades,LMAX))
    allocate(FNEAR0(NNEAR,N+1,rotors(1)%number_of_blades,LMAX))
    allocate(ROLD(3,KMAX,rotors(1)%number_of_blades,LMAX))

    ! * * * * * * * * * * * * * * * * * *
    !      Aerodynamic module           *
    ! * * * * * * * * * * * * * * * * * *
    allocate(AFLA(N,rotors(1)%number_of_blades,LMAX))
    allocate(CM_B(N,rotors(1)%number_of_blades,LMAX))

    allocate(CL_B(N,rotors(1)%number_of_blades,LMAX))
    allocate(CD_B(N,rotors(1)%number_of_blades,LMAX))
    allocate(CLM_B(N,rotors(1)%number_of_blades,LMAX))

    allocate(Force_cell(3,N,rotors(1)%number_of_blades,LMAX))
    allocate(moment_cell(3,N,rotors(1)%number_of_blades,LMAX))

    allocate(Force_section(3,N+1,rotors(1)%number_of_blades,LMAX))
    allocate(moment_section(3,N+1,rotors(1)%number_of_blades,LMAX))

    allocate(Force_hub(3,LMAX))
    allocate(Moment_hub(3,LMAX))

    allocate(Force_fuselage(3,Lmax))
    allocate(Moment_fuselage(3,Lmax))

    allocate(CT_ROTOR(LMAX))
    allocate(CQ_ROTOR(LMAX))

    allocate(CL_Rotor(Lmax))
    allocate(CD_Rotor(Lmax))

    allocate(force_flap_hinge(3,rotors(1)%number_of_blades,LMAX))
    allocate(moment_flap_hinge(3,rotors(1)%number_of_blades,LMAX))
    allocate(MFLAP(rotors(1)%number_of_blades,LMAX))

    ALLOCATE(V_IND(3,N,rotors(1)%number_of_blades,LMAX))


    allocate(R(3,KMAX,rotors(1)%number_of_blades))                                         !β������ģ��
    allocate(RNEW(3,KMAX,rotors(1)%number_of_blades))
    allocate(GEOMETRY_FAR_NEAR_BOUND(3*KMAX*rotors(1)%number_of_blades,(KMAX-1+(NNEAR-1)*(N+1)+NC*N)*rotors(1)%number_of_blades))
    allocate(Circulation_FAR_NEAR_BOUND((KMAX-1+(NNEAR-1)*(N+1)+NC*N)*rotors(1)%number_of_blades))
    allocate(NBC0(N*rotors(1)%number_of_blades,N*rotors(1)%number_of_blades))
    allocate(NBC(NC*N*rotors(1)%number_of_blades,NC*N*rotors(1)%number_of_blades))
    allocate(IBC(NC*N*rotors(1)%number_of_blades,NC*N*rotors(1)%number_of_blades,LMAX))
    allocate(ipiv(NC*N*rotors(1)%number_of_blades,LMAX))
    allocate(FTIP(KMAX,rotors(1)%number_of_blades))
    allocate(RC(KMAX,rotors(1)%number_of_blades))
    allocate(FB(N,rotors(1)%number_of_blades))
    allocate(fb_bound(NC,N,rotors(1)%number_of_blades))
    allocate(RNEAR(3,NNEAR,N+1,rotors(1)%number_of_blades))
    allocate(FNEAR(NNEAR,N+1,rotors(1)%number_of_blades))
    allocate(V(3,KMAX,rotors(1)%number_of_blades))
    allocate(VPRE1(3,KMAX,rotors(1)%number_of_blades))
    allocate(VPRE2(3,KMAX,rotors(1)%number_of_blades))
    allocate(VCOR1(3,KMAX,rotors(1)%number_of_blades))
    allocate(VCOR2(3,KMAX,rotors(1)%number_of_blades))




    allocate(XEVAL(3,NTEST))                                       !���Ե�ģ��
    allocate(VTEST(3,NTEST,rotors(1)%number_of_blades,LMAX))
    allocate(RABUDATEST(NTEST,rotors(1)%number_of_blades,LMAX))
    allocate(MIUTEST(NTEST,rotors(1)%number_of_blades,LMAX))
    allocate(TIAL(21,N,rotors(1)%number_of_blades))

    IALLOCATE = 0
    NCELLR=N*LMAX                                                   ! ��������
    NNOdeR=(N+1)*LMAX
    NCELLW=(KMAX-1)*rotors(1)%number_of_blades
    NNOdeW=KMAX*rotors(1)%number_of_blades
    NOdeBLAde=rotors(1)%number_of_blades*(N+1)*(NC+1)
    NCELLBLAde=rotors(1)%number_of_blades*N*NC
    NOdeNEAR=NNEAR*(N+1)*rotors(1)%number_of_blades
    NCELLNEAR=(NNEAR-1)*(N+1)*rotors(1)%number_of_blades
    allocate(ICELL(4,NCELLR))
    allocate(MCELL(2,KMAX-1,rotors(1)%number_of_blades))
    allocate(COOR(2,NNOdeR))
    allocate(ICELLBLAde(4,NCELLBLAde))
    allocate(ICELLNEAR(2,NCELLNEAR))
    allocate(COORBLAde(3,NOdeBLAde,LMAX))
    allocate(COORNEAR(3,NOdeNEAR))

    R = 0.0
    RNEW = 0.0
    GEOMETRY_FAR_NEAR_BOUND = 0.0
    CIRCULATION_FAR_NEAR_BOUND = 0.0
    FTIP = 0.0
    RC = 0.0
    FB = 0.0
    fb_bound = 0.0
    RNEAR = 0.0
    FNEAR = 0.0
    V = 0.0
    VPRE1 = 0.0
    VPRE2 = 0.0
    VCOR1 = 0.0
    VCOR2 = 0.0
    ROLD = 0.0

    ! * * * * * * * * * * * * * * * * * *
    !      Aerodynamic module           *
    ! * * * * * * * * * * * * * * * * * *
    AFLA = 0.0
    CM_B = 0.0

    CL_B = 0.0
    CD_B = 0.0
    CLM_B = 0.0

    Force_cell = 0.0
    moment_cell = 0.0

    Force_section = 0.0
    moment_section = 0.0

    Force_hub = 0.0
    Moment_hub = 0.0

    Force_fuselage = 0.0
    Moment_fuselage = 0.0

    CT_ROTOR = 0.0
    CQ_ROTOR = 0.0

    CL_Rotor = 0.0
    CD_Rotor = 0.0

    force_flap_hinge = 0.0
    moment_flap_hinge = 0.0
    MFLAP = 0.0

    end


    !-------------------------------------------------------------------------------------
    ! �ͷ��ڴ�
    !-------------------------------------------------------------------------------------
    subroutine dealloc_memory_aero()
    use xml_data_helicopter_parameters
    use FRIGHT
    use parameters
    use BLADEFEM
    use FREE_WAKE
    use FREE_WAKE_ITERATION_MODULE
    use aerodynamic_force
    use AZIMUTH
    use RESULTV
    use DYNAMICS_STALL
    use GRID
    use FLOWGRID
    implicit none

    deallocate(XFLAP)                                              !��Ҷ����Ԫ
    deallocate(XROTOR)
    deallocate(XCONTROL_BLAde)
    deallocate(xtrailedge_blade)
    deallocate(XNOde)
    deallocate(XCONTROL)
    deallocate(Xtrail_edge)
    deallocate(xgrid_node)

    deallocate(C)
    deallocate(BETA)
    deallocate(THETA)
    deallocate(VB)
    deallocate(SITA)
    deallocate(SITAT)
    deallocate(SITA_CENTER)

    deallocate(xflap0)
    deallocate(xcontrol0)
    deallocate(xnode0)

    deallocate(R0)                                                 !β��ģ��
    deallocate(V0)
    deallocate(FTIP0)
    deallocate(RC0)
    deallocate(FB0)
    deallocate(fb_bound0)
    deallocate(FNEAR0)
    deallocate(ROLD)

    ! * * * * * * * * * * * * * * * * * *
    !      Aerodynamic module           *
    ! * * * * * * * * * * * * * * * * * *
    deallocate(AFLA)
    deallocate(CM_B)

    deallocate(CL_B)
    deallocate(CD_B)
    deallocate(CLM_B)

    deallocate(Force_cell)
    deallocate(moment_cell)

    deallocate(Force_section)
    deallocate(moment_section)

    deallocate(Force_hub)
    deallocate(Moment_hub)

    deallocate(CT_ROTOR)
    deallocate(CQ_ROTOR)

    deallocate(force_flap_hinge)
    deallocate(moment_flap_hinge)
    deallocate(MFLAP)

    deallocate(R)                                                  !β������ģ��
    deallocate(RNEW)
    deallocate(GEOMETRY_FAR_NEAR_BOUND)
    deallocate(CIRCULATION_FAR_NEAR_BOUND)
    deallocate(NBC0)
    deallocate(NBC)
    deallocate(IPIV)
    deallocate(IBC)
    deallocate(FTIP)
    deallocate(RC)
    deallocate(FB)
    deallocate(fb_bound)
    deallocate(RNEAR)
    deallocate(FNEAR)
    deallocate(V)
    deallocate(VPRE1)
    deallocate(VPRE2)
    deallocate(VCOR1)
    deallocate(VCOR2)


    deallocate(XEVAL)                                              !���Ե�ģ��
    deallocate(VTEST)
    deallocate(RABUDATEST)
    deallocate(MIUTEST)

    deallocate(TIAL)

    deallocate(ICELL)                                              !����
    deallocate(MCELL)
    deallocate(COOR)
    deallocate(ICELLBLAde)
    deallocate(ICELLNEAR)
    deallocate(COORBLAde)
    deallocate(COORNEAR)

    if(Iallocate==1) then
        deallocate(FLOWCELL)
        deallocate(FLOWCOOR)
        deallocate(FLOWVELOCITY)
    end if

    end

    end module memory