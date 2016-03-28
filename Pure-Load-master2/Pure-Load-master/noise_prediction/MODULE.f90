    
    MODULE xml_data_compution_control
    IMPLICIT NONE
    CHARACTER(80) :: OUTPUT_FOLDERNAME
    END MODULE
    
    
    MODULE PARAMETEERS
    IMPLICIT NONE
    REAL(8)::PI=3.1415926
    REAL(8)::G=9.8
    REAL(8)::RHO=1.25
    REAL(8)::SOUNDSPEED=341.0
    END MODULE
    
    
    MODULE GLOBAL_PARAMETERS
    IMPLICIT NONE		
    INTEGER :: N_R,N_C !桨叶展向和弦向有限元段数
    INTEGER :: NTIME   !时间步数
    REAL(8) :: T_VISIT,T_PSI,PSI_MAX_DEG ! 观察时间
    REAL(8) :: X_VISIT(3) ! 观察位置
    REAL(8) :: V_INF(3)
    
    REAL(8) :: DET_PSI_DEG,DET_PSI
    INTEGER :: obTIME! TIME 对应不同观测时间
    REAL(8):: TIME_PEROID
    INTEGER :: N    !FOR OUTPUT
    INTEGER :: ID_LOADING   !载荷分布id，用以标示不同的分布模型
    
    INTEGER :: INIT_LOADING = 1
    
    INTEGER :: N_WINDOW=16
    integer :: N_SPEC
    
    END MODULE
    
    
    MODULE ROTOR
    IMPLICIT NONE
    
    INTEGER :: N_BLADE
    !桨毂中心在XYZ1坐标系的空间坐标，描述和观察者位移的相对位置
    REAL(8) :: XROTORCENTER(3)=0 
    !桨叶几何参数
    REAL(8) :: R_BLADE_END,R_BLADE_BEGIN ! 半径
    !REAL(8) :: C_BLADE     !弦长
    REAL(8),ALLOCATABLE :: C_BLADE_R(:)
    REAL(8) :: AL_ROTOR=0 ! 旋翼轴前倾角
    REAL(8) :: OMEGA !转速
    
    REAL(8),ALLOCATABLE :: THETA_BLADE(:,:,:),BETA_BLADE(:,:,:),DBETA_BLADE(:,:,:),D2BETA_BLADE(:,:,:)
    !扭转角和挥舞角以及挥舞角的一阶导数，第一个参数表示径向坐标，第二个参数表示时间
    REAL(8),ALLOCATABLE :: PT_TIME(:,:),PT_R(:) !对应beta和theta数据的节点站位
    INTEGER :: NPT_R,NPT_TIME
    REAL(8),ALLOCATABLE::FORCE_CELL(:,:,:,:)
    !INTEGER :: NFORCE_R,NFORCE_TIME
    REAL(8),ALLOCATABLE::V_INDUCE(:,:,:,:),DV_INDUCE(:,:,:)
    
    
    REAL(8),ALLOCATABLE:: LOADING_MATRIX(:,:,:,:) ,PSI_ARRAY(:,:)!对应 EXP1_data_blade 中的数据
    
    
    !桨叶面元几何参数
    REAL(8),ALLOCATABLE :: XPANEL_UP(:,:,:),SPANEL_UP(:,:),NPANEL_UP(:,:,:)
    REAL(8),ALLOCATABLE :: XPANEL_DOWN(:,:,:),SPANEL_DOWN(:,:),NPANEL_DOWN(:,:,:)
    !位置，面积，法向向量
    
    !在随叶坐标系中，坐标系转换，慎重！
    !REAL(8),ALLOCATABLE :: VPANEL_UP(:,:,:),APANEL_UP(:,:,:)
    !REAL(8),ALLOCATABLE :: VPANEL_DOWN(:,:,:),APANEL_DOWN(:,:,:)
    !速度，加速度
    
    REAL(8),ALLOCATABLE :: BLADE_SPACING(:)
      
    END 
    
    MODULE AIRFOIL
    IMPLICIT NONE
    INTEGER :: NAIRFOIL
    REAL(8),ALLOCATABLE::AIRFOIL_POINT_UP(:,:),AIRFOIL_POINT_DOWN(:,:)
    REAL(8),ALLOCATABLE::Y2A(:),Y2B(:)
    END 
    
    
    
    MODULE ACP_HIS
    IMPLICIT NONE
    REAL(8),ALLOCATABLE::TIME_HIS(:)
    REAL(8),ALLOCATABLE::PL_HIS(:)
    REAL(8),ALLOCATABLE::PT_HIS(:)
    REAL(8),ALLOCATABLE::P_TOTAL_HIS(:)
    END MODULE
    
    MODULE SPEC
    IMPLICIT NONE
    REAL(8)::FREQ_SAMPLING
    
    REAL(8),ALLOCATABLE::PT_HIS_EXPEND(:)
    REAL(8),ALLOCATABLE::PL_HIS_EXPEND(:)
    REAL(8),ALLOCATABLE::P_TOTAL_HIS_EXPEND(:)
    
    REAL(8),ALLOCATABLE::PT_SPEC(:)
    REAL(8),ALLOCATABLE::PL_SPEC(:)
    REAL(8),ALLOCATABLE::P_TOTAL_SPEC(:)
    
    REAL(8),ALLOCATABLE::SPLT_SPEC(:)
    REAL(8),ALLOCATABLE::SPLL_SPEC(:)
    REAL(8),ALLOCATABLE::SPL_TOTAL_SPEC(:)
    
    REAL(8),ALLOCATABLE::PT_SPEC_A(:)
    REAL(8),ALLOCATABLE::PL_SPEC_A(:)
    REAL(8),ALLOCATABLE::P_TOTAL_SPEC_A(:)
    
    REAL(8),ALLOCATABLE::SPLT_SPEC_A(:)
    REAL(8),ALLOCATABLE::SPLL_SPEC_A(:)
    REAL(8),ALLOCATABLE::SPL_TOTAL_SPEC_A(:)
    
    
    
    REAL(8)::P_REF=2E-5
    REAL(8)::SPLT,SPLT_A
    REAL(8)::SPLL,SPLL_A
    REAL(8)::SPL,SPL_A
    
    END MODULE
    
    
    
    MODULE COOR_TRAN
    IMPLICIT NONE
    REAL(8) :: A21(3,3),A32(3,3),A43(3,3),A54(3,3),A51(3,3),A15(3,3),A12(3,3),A13(3,3),A35(3,3),A34(3,3),A45(3,3),A53(3,3),A31(3,3)
    END MODULE
    
    
