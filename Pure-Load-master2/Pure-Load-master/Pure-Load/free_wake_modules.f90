

    module FRIGHT                                                  !飞行条件模块
    implicit none
    real VFREE(3)
    real fpos(3)
    real fvel(3)
    real facc(3)
    real faccyaw
    real faccpitch
    real facclist
    real PITCH_ROLL(3)
    real PITCH_ROLL_T(3)
    end module

    module BLADEFEM                                                !桨叶有限元模块
    implicit none
    real,allocatable    :: XFLAP(:,:,:)
    real,allocatable    :: XROTOR(:,:)
    real,allocatable    :: XCONTROL_BLAde(:,:,:)
    real,allocatable    :: xtrailedge_blade(:,:)
    real,allocatable    :: XNOde(:,:,:,:,:)
    real,allocatable    :: XCONTROL(:,:,:,:,:)
    real,allocatable    :: Xtrail_edge(:,:,:,:)
    real,allocatable    :: xgrid_node(:,:,:)
    real,allocatable    :: xflap0(:,:)
    real,allocatable    :: xcontrol0(:,:,:,:)
    real,allocatable    :: xnode0(:,:,:,:)
    real,allocatable    :: C(:)
    real,allocatable    :: C_NOde(:)
    real,allocatable    :: THETA(:,:,:)
    real,allocatable    :: SITAIn(:,:,:,:)
    real,allocatable    :: SITATIn(:,:,:,:)
    real,allocatable    :: SITA_CENTERIn(:,:,:,:)
    real,allocatable    :: SITA(:,:,:,:)
    real,allocatable    :: SITAT(:,:,:,:)
    real,allocatable    :: SITA_CENTER(:,:,:,:)
    real,allocatable    :: BETA(:,:,:,:)
    real,allocatable    :: VB(:,:,:,:)
    real,allocatable    :: RC_NEAR(:)
    integer,allocatable :: Nmuber_part(:)
    integer,allocatable :: IP_airoil_r(:,:)
    real,allocatable    :: sweep(:)
    real,allocatable    :: anhedral(:)
    end module

    module FREE_WAKE                                               !尾迹模块
    implicit none
    real,allocatable :: R0(:,:,:,:,:)
    real,allocatable :: V0(:,:,:,:,:)
    real,allocatable :: FTIP0(:,:,:,:)
    real,allocatable :: RC0(:,:,:,:)
    real,allocatable :: FB0(:,:,:,:)
    real,allocatable :: FNEAR0(:,:,:,:,:)
    real,allocatable :: fb_bound0(:,:,:,:,:)
    real,allocatable :: ROLD(:,:,:,:,:)
    end module

    module aerodynamic_force                    ! aerodynamic force
    implicit none
    real,allocatable :: AFLA(:,:,:,:)
    real,allocatable :: CM_B(:,:,:,:)

    real,allocatable :: CL_B(:,:,:,:)
    real,allocatable :: CD_B(:,:,:,:)
    real,allocatable :: CLM_B(:,:,:,:)

    real,allocatable :: Force_cell(:,:,:,:,:)
    real,allocatable :: moment_cell(:,:,:,:,:)

    real,allocatable :: Force_section(:,:,:,:,:)
    real,allocatable :: moment_section(:,:,:,:,:)

    real,allocatable :: Force_hub(:,:,:)
    real,allocatable :: Moment_hub(:,:,:)

    real,allocatable :: Force_fuselage(:,:,:)
    real,allocatable :: Moment_fuselage(:,:,:)

    real,allocatable :: CT_ROTOR(:,:)
    real,allocatable :: CQ_ROTOR(:,:)

    real,allocatable :: CL_Rotor(:,:)
    real,allocatable :: CD_Rotor(:,:)

    real,allocatable :: force_flap_hinge(:,:,:,:)
    real,allocatable :: moment_flap_hinge(:,:,:,:)
    real,allocatable :: MFLAP(:,:,:)

    REAL,ALLOCATABLE ::V_IND(:,:,:,:,:)
    end module

    module FREE_WAKE_ITERATION_MODULE                              !尾迹迭代模块
    implicit none
    real,allocatable :: R(:,:,:,:)
    real,allocatable :: RNEW(:,:,:,:)
    real,allocatable :: GEOMETRY_FAR_NEAR_BOUND(:,:)
    real,allocatable :: Circulation_FAR_NEAR_BOUND(:)
    real,allocatable :: NBC0(:,:)
    real,allocatable :: NBC(:,:)
    real,allocatable :: IBC(:,:,:)
    integer,allocatable :: ipiv(:,:)
    real,allocatable :: FTIP(:,:,:)
    real,allocatable :: RC(:,:,:)
    real,allocatable :: FB(:,:,:)
    real,allocatable :: fb_bound(:,:,:,:)
    real,allocatable :: RNEAR(:,:,:,:,:)
    real,allocatable :: FNEAR(:,:,:,:)
    real,allocatable :: V(:,:,:,:)
    real,allocatable :: VPRE1(:,:,:,:)
    real,allocatable :: VPRE2(:,:,:,:)
    real,allocatable :: VCOR1(:,:,:,:)
    real,allocatable :: VCOR2(:,:,:,:)
    end module

    module AZIMUTH                                                 !方位角模块
    implicit none
    real deTPESI
    real deTSIGMA
    real deTPESIBLAde
    real DWA
    real det_time
    end module

    module RESULTV                                                 !测试点模块
    implicit none
    real,allocatable :: XEVAL(:,:)
    real,allocatable :: VTEST(:,:,:,:,:)
    real,allocatable :: RABUDATEST(:,:,:,:)
    real,allocatable :: MIUTEST(:,:,:,:)
    end module

    module DYNAMICS_STALL                                          !动态失速模块
    real,allocatable :: TIAL(:,:,:,:)
    end module

    module C81                                                     !翼型特性表
    implicit none
    integer,parameter :: N_interp = 20
    type :: airfoil_c81
        integer :: N_afla_cl          !升力系数
        integer :: N_M_cl
        real,allocatable :: afa_cl(:)
        real,allocatable :: cm_cl(:)
        real,allocatable :: cl(:,:)

        integer :: N_afla_cd          !阻力系数
        integer :: N_M_cd
        real,allocatable :: afa_cd(:)
        real,allocatable :: cm_cd(:)
        real,allocatable :: cd(:,:)

        integer :: N_afla_cm          !力矩系数表
        integer :: N_M_cm
        real,allocatable :: afa_cm(:)
        real,allocatable :: cm_cm(:)
        real,allocatable :: cm(:,:)

        integer :: N_point            !翼型表面坐标点
        real,allocatable :: x_up_surface(:,:)
        real,allocatable :: x_down_surface(:,:)
        real,allocatable :: x_center_line(:,:)
        real,allocatable :: x_up_surface_new(:,:)
        real :: x_hinge_pitch(2)
        real :: x_interp(N_interp)
        real :: up_interp(N_interp)
        real :: down_interp(N_interp)
        integer ::nx_cl,ny_cl,nx_cd,ny_cd,nx_cm,ny_cm
        real,allocatable,dimension(:) :: tx_cl,ty_cl,tx_cd,ty_cd,tx_cm,ty_cm
        real,allocatable,dimension(:) :: c_cl,c_cd,c_cm
    end type
    type(airfoil_c81), allocatable :: airfoils_C81(:)
    
    
    real CONST(7,21)
    integer,parameter :: kx = 3,ky = 3
    contains
    subroutine build_C81_spline()
    use xml_data_helicopter_parameters,only:number_of_airfoils
    use parameters,only:deg2rad
    implicit none
    integer i,j,mx,my,nx,ny
    integer,parameter :: iopt = 0
    real,parameter :: s = 0.0
    integer id
    do id = 1,number_of_airfoils
        mx = airfoils_C81(id)%N_afla_cl
        my = airfoils_C81(id)%N_M_cl 
        allocate(airfoils_C81(id)%tx_cl(mx+kx+1))
        allocate(airfoils_C81(id)%ty_cl(my+ky+1))
        allocate(airfoils_C81(id)%c_cl(mx*my))
        call spline_generatror(mx,airfoils_C81(id)%afa_cl*deg2rad,my,airfoils_C81(id)%cm_cl,airfoils_C81(id)%cl,&
        airfoils_C81(id)%nx_cl,airfoils_C81(id)%tx_cl,airfoils_C81(id)%ny_cl,airfoils_C81(id)%ty_cl,airfoils_C81(id)%c_cl)
        mx = airfoils_C81(id)%N_afla_cd
        my = airfoils_C81(id)%N_M_cd 
        allocate(airfoils_C81(id)%tx_cd(mx+kx+1))
        allocate(airfoils_C81(id)%ty_cd(my+ky+1))
        allocate(airfoils_C81(id)%c_cd(mx*my))
        call spline_generatror(mx,airfoils_C81(id)%afa_cd*deg2rad,my,airfoils_C81(id)%cm_cd,airfoils_C81(id)%cd,&
        airfoils_C81(id)%nx_cd,airfoils_C81(id)%tx_cd,airfoils_C81(id)%ny_cd,airfoils_C81(id)%ty_cd,airfoils_C81(id)%c_cd)
        mx = airfoils_C81(id)%N_afla_cm
        my = airfoils_C81(id)%N_M_cm 
        allocate(airfoils_C81(id)%tx_cm(mx+kx+1))
        allocate(airfoils_C81(id)%ty_cm(my+ky+1))
        allocate(airfoils_C81(id)%c_cm(mx*my))
        call spline_generatror(mx,airfoils_C81(id)%afa_cm*deg2rad,my,airfoils_C81(id)%cm_cm,airfoils_C81(id)%cm,&
        airfoils_C81(id)%nx_cm,airfoils_C81(id)%tx_cm,airfoils_C81(id)%ny_cm,airfoils_C81(id)%ty_cm,airfoils_C81(id)%c_cm)
    end do
    contains
    subroutine spline_generatror(mx,x,my,y,z2d,nx,tx,ny,ty,c)
    real :: wrk(10000)
    integer :: iwrk(1000)    
    integer,intent(in) :: mx,my
    real,intent(in) :: x(mx),y(my),z2d(mx,my)
    integer,intent(out) :: nx,ny
    real,intent(out) :: tx(mx+kx+1),ty(my+ky+1),c(mx*my)
    real :: xb,xe,yb,ye,fp,z(mx*my)
    integer :: nxest,nyest,lwrk,kwrk
    integer :: i,j,ier
    xb = minval(x)
    xe = maxval(x)
    yb = minval(y)
    ye = maxval(y)
    nxest=mx+kx+1
    nyest=my+ky+1
    lwrk = 4+nxest*(my+2*kx+5)+nyest*(2*ky+5)+mx*(kx+1)+my*(ky+1) + merge(my,nxest,my>nxest)
    kwrk = 3+mx+my+nxest+nyest
    do i = 1,mx
        do j = 1,my
            z(my*(i-1)+j) = z2d(i,j)
        end do
    end do
    call regrid(iopt,mx,x,my,y,z,xb,xe,yb,ye,kx,ky,s,nxest,nyest,nx,tx,ny,ty,c,fp,wrk,lwrk,iwrk,kwrk,ier)
    end subroutine spline_generatror
    end subroutine
    subroutine query_clcdcm(id,alpha,Ma,cl,cd,cm)
    implicit none
    integer,parameter :: lwrk = 8,kwrk=2
    real :: wrk(lwrk)
    integer :: iwrk(kwrk),ier
    real,dimension(1) :: alphas,Mas,z
    integer,intent(in) :: id
    real,intent(in) :: alpha,Ma
    real,intent(out) :: cl,cd,cm
    alphas(1) = alpha
    Mas(1) = Ma
    call bispev(airfoils_C81(id)%tx_cl,airfoils_C81(id)%nx_cl,&
                airfoils_C81(id)%ty_cl,airfoils_C81(id)%ny_cl,&
                airfoils_C81(id)%c_cl,kx,ky,alphas,1,Mas,1,z,wrk,lwrk,iwrk,kwrk,ier)
    cl = z(1)
    call bispev(airfoils_C81(id)%tx_cd,airfoils_C81(id)%nx_cd,&
                airfoils_C81(id)%ty_cd,airfoils_C81(id)%ny_cd,&
                airfoils_C81(id)%c_cd,kx,ky,alphas,1,Mas,1,z,wrk,lwrk,iwrk,kwrk,ier)
    cd = z(1)
    call bispev(airfoils_C81(id)%tx_cm,airfoils_C81(id)%nx_cm,&
                airfoils_C81(id)%ty_cm,airfoils_C81(id)%ny_cm,&
                airfoils_C81(id)%c_cm,kx,ky,alphas,1,Mas,1,z,wrk,lwrk,iwrk,kwrk,ier)
    cm = z(1)
    end subroutine
    end module

    module GRID                                                    !网格
    implicit none
    integer NCELLR
    integer NNOdeR
    integer NCELLW
    integer NNOdeW
    integer NOdeBLAde
    integer NCELLBLAde
    integer NOdeNEAR
    integer NCELLNEAR
    integer,allocatable :: ICELL(:,:)
    integer,allocatable :: MCELL(:,:,:,:)
    integer,allocatable :: ICELLBLAde(:,:)
    integer,allocatable :: ICELLNEAR(:,:)
    real,allocatable :: COOR(:,:)
    real,allocatable :: COORBLAde(:,:,:)
    real,allocatable :: COORNEAR(:,:)
    end module

    module fuselage_grid_parameters
    real :: fuselage_center(3)
    integer :: Nnode_fuselage
    integer :: Ncell_fuselage
    integer :: icell_fuselage(4,20000)
    real :: xnode_fuselage(3,20000)
    real :: xfuselage(3,20000)
    end module

    module FLOWGRID
    integer :: NFLOWCELL
    integer :: NFLOWNOde
    integer :: NFLOWIP
    integer :: Iallocate
    integer,allocatable :: FLOWCELL(:,:)
    real,allocatable :: FLOWCOOR(:,:,:)
    real,allocatable :: FLOWVELOCITY(:,:,:)
    real :: XBOUNDR,XBOUNDL,ZBOUNDD,ZBOUNDU
    integer :: NXI,NZJ,IMAX,JMAX
    integer,parameter :: NMAX=1000
    end module


