    
    SUBROUTINE FWHsolver()
    USE GLOBAL_PARAMETERS
    USE ROTOR,ONLY:N_BLADE
    USE ACP_HIS
    USE xml_data_compution_control
    
    IMPLICIT NONE
    !CALL RESTIME()
    !CALL F1A()
    INTEGER:: I,J,K,IB
    REAL(8):: RES_TIME  !每个面元的延迟时间
    REAL(8):: PL,PT,P,P_TOL,SPH,PT_TOL,PL_TOL,SPH_PT,SPH_PL
    REAL(8):: PT_NEAR,PT_FAR,PL_NEAR,PL_FAR,PT_NEAR_TOL,PT_FAR_TOL,PL_NEAR_TOL,PL_FAR_TOL
    
    
 
            
    P_TOL=0
    PT_TOL=0
    PL_TOL=0
    PT_NEAR_TOL=0
    PT_FAR_TOL=0
    PL_NEAR_TOL=0
    PL_FAR_TOL=0
  
    !WRITE(19,*) '   i      j      theta       beta        psi'
   ! I_R,J_C,XPANEL,theta_res*180/pi,beta_res*180/pi,psi*180/pi  
    DO IB=1,n_blade
    !DO IB=3,3
        !write(*,*)ib
        DO K=1,2    !标记上下表面
            !DO I=1,N_R
            !    
                do i=1,n_R
                    !DO J=80,90
                    do j=1,n_c
                        
                    CALL RESTIME(I,J,RES_TIME,K,IB,PT,PL,PT_NEAR,PT_FAR,PL_NEAR,PL_FAR)
                    !CALL READ_LOAD(I,J,TAO)
                    !CALL SDPRESS(I,J,DETP_PANEL)
                    !P_VISIT = P_VISIT + DETP_PANEL
                    !CALL F1A_PRE(I,J,RES_TIME,P_TOL,K)
                    !CALL P2DB(P,SPH)
                    P_TOL=P_TOL+PT+PL
                    PT_TOL=PT_TOL+PT
                    PL_TOL=PL_TOL+PL
                    PT_NEAR_TOL=PT_NEAR_TOL+PT_NEAR
                    PT_FAR_TOL=PT_FAR_TOL+PT_FAR
                    PL_NEAR_TOL=PL_NEAR_TOL+PL_NEAR
                    PL_FAR_TOL=PL_FAR_TOL+PL_FAR
                    !CALL P2DB(P,SPH)
                    !WRITE(*,*)ib,k,i,j,pt,pl
                END DO
            END DO
         
        END DO
    END DO
    
   !    
    PL_HIS(OBTIME)=PL_TOL
    PT_HIS(OBTIME)=PT_TOL
    P_TOTAL_HIS(OBTIME)=P_TOL
    
    !CALL P2DB(P_TOL,SPH)
    !CALL P2DB(PT_TOL,SPH_PT)
    !CALL P2DB(PL_TOL,SPH_PL)
    
    
    !WRITE(*,*)SPH_PT,SPH_PL
    
    WRITE(18,*)'TOTAL:',P_TOL,PT_TOL,PL_TOL
   ! WRITE(18,*)'SPH',SPH,SPH_PT,SPH_PL
    
    WRITE(18,1001)T_VISIT/TIME_PEROID,PT_FAR_TOL,PT_NEAR_TOL,PT_TOL
    WRITE(19,1001)T_VISIT/TIME_PEROID,PL_FAR_TOL,PL_NEAR_TOL,PL_TOL
    !WRITE(18,*)'-------END----------'
    

1001 FORMAT(8(F12.6,3X) )     
    
   ! WRITE(20,'(4F12.6)')T_VISIT/TIME_PEROID,SPH_PT,SPH_PL,SPH
    WRITE(20,'(4F12.6)')T_VISIT/TIME_PEROID,PT_TOL,PL_TOL,P_TOL
    WRITE(21,'(4F12.6)')T_VISIT/TIME_PEROID,SPH_PT,SPH_PL,SPH
    

    
    
    END SUBROUTINE   
    
    
    
    SUBROUTINE P2DB(P,SPH)
    IMPLICIT NONE
    REAL(8),INTENT(IN) :: P
    REAL(8),INTENT(OUT):: SPH
    SPH=10*LOG10((P/2E-5)*(P/2E-5))
    END SUBROUTINE P2DB
    

  