    
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
    INTEGER :: N_R,N_C !��Ҷչ�����������Ԫ����
    INTEGER :: NTIME   !ʱ�䲽��
    REAL(8) :: T_VISIT,T_PSI,PSI_MAX_DEG ! �۲�ʱ��
    REAL(8) :: X_VISIT(3) ! �۲�λ��
    REAL(8) :: V_INF(3)
    
    REAL(8) :: DET_PSI_DEG,DET_PSI
    INTEGER :: obTIME! TIME ��Ӧ��ͬ�۲�ʱ��
    REAL(8):: TIME_PEROID
    INTEGER :: N    !FOR OUTPUT
    INTEGER :: ID_LOADING   !�غɷֲ�id�����Ա�ʾ��ͬ�ķֲ�ģ��
    
    INTEGER :: INIT_LOADING = 1
    
    INTEGER :: N_WINDOW=16
    integer :: N_SPEC
    
    END MODULE
    
    
    MODULE ROTOR
    IMPLICIT NONE
    
    INTEGER :: N_BLADE
    !���������XYZ1����ϵ�Ŀռ����꣬�����͹۲���λ�Ƶ����λ��
    REAL(8) :: XROTORCENTER(3)=0 
    !��Ҷ���β���
    REAL(8) :: R_BLADE_END,R_BLADE_BEGIN ! �뾶
    !REAL(8) :: C_BLADE     !�ҳ�
    REAL(8),ALLOCATABLE :: C_BLADE_R(:)
    REAL(8) :: AL_ROTOR=0 ! ������ǰ���
    REAL(8) :: OMEGA !ת��
    
    REAL(8),ALLOCATABLE :: THETA_BLADE(:,:,:),BETA_BLADE(:,:,:),DBETA_BLADE(:,:,:),D2BETA_BLADE(:,:,:)
    !Ťת�Ǻͻ�����Լ�����ǵ�һ�׵�������һ��������ʾ�������꣬�ڶ���������ʾʱ��
    REAL(8),ALLOCATABLE :: PT_TIME(:,:),PT_R(:) !��Ӧbeta��theta���ݵĽڵ�վλ
    INTEGER :: NPT_R,NPT_TIME
    REAL(8),ALLOCATABLE::FORCE_CELL(:,:,:,:)
    !INTEGER :: NFORCE_R,NFORCE_TIME
    REAL(8),ALLOCATABLE::V_INDUCE(:,:,:,:),DV_INDUCE(:,:,:)
    
    
    REAL(8),ALLOCATABLE:: LOADING_MATRIX(:,:,:,:) ,PSI_ARRAY(:,:)!��Ӧ EXP1_data_blade �е�����
    
    
    !��Ҷ��Ԫ���β���
    REAL(8),ALLOCATABLE :: XPANEL_UP(:,:,:),SPANEL_UP(:,:),NPANEL_UP(:,:,:)
    REAL(8),ALLOCATABLE :: XPANEL_DOWN(:,:,:),SPANEL_DOWN(:,:),NPANEL_DOWN(:,:,:)
    !λ�ã��������������
    
    !����Ҷ����ϵ�У�����ϵת�������أ�
    !REAL(8),ALLOCATABLE :: VPANEL_UP(:,:,:),APANEL_UP(:,:,:)
    !REAL(8),ALLOCATABLE :: VPANEL_DOWN(:,:,:),APANEL_DOWN(:,:,:)
    !�ٶȣ����ٶ�
    
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
    
    
