
    SUBROUTINE TRAN_A21(AL,A21)
    IMPLICIT NONE
    REAL(8),INTENT(IN) :: AL
    REAL(8),INTENT(OUT)::A21(3,3)
    !������ϵX1Y1Z1 -> ���ϵX2Y2Z2
    ! X1+����:��ʻԱǰ����Y1+����:��ʻԱ�ҷ���Z1+����:��ʻԱ�·���
    ! X2+����:������ǰ����Y2+����:�������ҷ���Z2+����:�������·���
    ! AL��������ǰ���(RAD),��ǰΪ��
    ! (X2Y2Z2)=A21*(X1Y1Z1)
        A21(1,:)=(/COS(AL),0,SIN(AL)/)
        A21(2,:)=(/0,1,0/)
        A21(3,:)=(/-SIN(AL),0,COS(AL)/)
    END SUBROUTINE
    

    SUBROUTINE TRAN_A32(PSI,A32)
    IMPLICIT NONE
    REAL(8),INTENT(IN) :: PSI
    REAL(8),INTENT(OUT)::A32(3,3)
    !���ϵX2Y2Z2 -> ��תϵX3Y3Z3
    !X2+����:������ǰ����Y2+����:�������ҷ���Z2+����:�������·���
    !X3+����:��Ҷչ�����⣻Y3+����:��Ҷ�������Z3+����:�������·��� 
    !PSI����Ҷ��ת��(RAD)������Ϊ�㣬��ʱ��Ϊ��
    ! (X3Y3Z3)=A32*(X2Y2Z2)
        A32(1,:)=(/-COS(PSI),SIN(PSI),0/)
        A32(2,:)=(/-SIN(PSI),-COS(PSI),0/)
        A32(3,:)=(/0,0,1/)
    END SUBROUTINE
    
    SUBROUTINE TRAN_A43(BETA,A43)
    IMPLICIT NONE
    REAL(8),INTENT(IN) :: BETA
    REAL(8),INTENT(OUT)::A43(3,3)
    !��תϵX3Y3Z3 -> ����ϵX4Y4Z4
    !X3+����:��Ҷչ�����⣻Y3+����:��Ҷ�������Z3+����:�������·��� 
    !X4+����:��Ҷչ�����⣻Y4+����:��Ҷ�������Z4+����:�������·���(�����)
    !BETA����Ҷ�����(RAD)���ϻ�Ϊ��
    ! (X4Y4Z4)=A43*(X3Y3Z3)
        A43(1,:)=(/COS(BETA),0,-SIN(BETA)/)
        A43(2,:)=(/0,1,0/)
        A43(3,:)=(/SIN(BETA),0,COS(BETA)/)
    END SUBROUTINE
    
    SUBROUTINE TRAN_A54(THETA,A54)
    IMPLICIT NONE
    REAL(8),INTENT(IN) :: THETA
    REAL(8),INTENT(OUT):: A54(3,3)
    !����ϵX4Y4Z4 -> ��ҶϵX5Y5Z5
    !X4+����:��Ҷչ�����⣻Y4+����:��Ҷ�������Z4+����:�������·���(�����)
    !X5+����:��Ҷչ�����⣻Y5+����:��Ҷ�������Z5+����:�������·���(Ťת��)
    !THETA����Ҷ�ܾ��(RAD)��̧ͷΪ��
    ! (X5Y5Z5)=A54*(X4Y4Z4)
        A54(1,:)=(/1,0,0/)
        A54(2,:)=(/DBLE(0),COS(THETA),SIN(THETA)/)
        A54(3,:)=(/DBLE(0),-SIN(THETA),COS(THETA)/)
    END SUBROUTINE
    
    
    SUBROUTINE TRAN_A15(AL_ROTOR,TAO_PSI,BETA_RES,THETA_RES,A15)
    IMPLICIT NONE
    REAL(8),INTENT(IN) :: AL_ROTOR,TAO_PSI,BETA_RES,THETA_RES
    REAL(8),INTENT(OUT):: A15(3,3)
    
    REAL(8)::A51(3,3),A21(3,3),A32(3,3),A43(3,3),A54(3,3)
    
    !����ϵX4Y4Z4 -> ��ҶϵX5Y5Z5
    !X4+����:��Ҷչ�����⣻Y4+����:��Ҷ�������Z4+����:�������·���(�����)
    !X5+����:��Ҷչ�����⣻Y5+����:��Ҷ�������Z5+����:�������·���(Ťת��)
    !THETA����Ҷ�ܾ��(RAD)��̧ͷΪ��
    ! (X5Y5Z5)=A54*(X4Y4Z4)
     
    CALL TRAN_A21(AL_ROTOR,A21)
    CALL TRAN_A32(TAO_PSI,A32)
    CALL TRAN_A43(BETA_RES,A43)
    CALL TRAN_A54(THETA_RES,A54)
    A51=MATMUL(A54,MATMUL(A43,matmul(A32,A21)))
    A15=TRANSPOSE(A51)
        
    END SUBROUTINE
    
    
    
    SUBROUTINE TRAN_A35(AL_ROTOR,TAO_PSI,BETA_RES,THETA_RES,A35)
    IMPLICIT NONE
    REAL(8),INTENT(IN) :: AL_ROTOR,TAO_PSI,BETA_RES,THETA_RES
    REAL(8),INTENT(OUT):: A35(3,3)
    REAL(8)::A53(3,3),A43(3,3),A54(3,3)
    CALL TRAN_A43(BETA_RES,A43)
    CALL TRAN_A54(THETA_RES,A54)
    A53=MATMUL(A54,A43)
    A35=TRANSPOSE(A53)  
    END SUBROUTINE
    
    
    SUBROUTINE TRAN_A13(AL_ROTOR,TAO_PSI,BETA_RES,THETA_RES,A13)
    IMPLICIT NONE
    REAL(8),INTENT(IN) :: AL_ROTOR,TAO_PSI,BETA_RES,THETA_RES
    REAL(8),INTENT(OUT):: A13(3,3)
    REAL(8)::A31(3,3),A21(3,3),A32(3,3)
    CALL TRAN_A21(AL_ROTOR,A21)
    CALL TRAN_A32(TAO_PSI,A32)
    A31=MATMUL(A32,A21)
    A13=TRANSPOSE(A31)
    END SUBROUTINE
    
    SUBROUTINE TRAN_A31(AL_ROTOR,TAO_PSI,A31)
    IMPLICIT NONE
    REAL(8),INTENT(IN) :: AL_ROTOR,TAO_PSI
    REAL(8),INTENT(OUT):: A31(3,3)
    REAL(8)::A21(3,3),A32(3,3)
    CALL TRAN_A21(AL_ROTOR,A21)
    CALL TRAN_A32(TAO_PSI,A32)
    A31=MATMUL(A32,A21)
    END SUBROUTINE