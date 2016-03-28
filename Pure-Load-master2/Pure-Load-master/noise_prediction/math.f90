    
    SUBROUTINE VECTOR_CROSS3 (Z,X,Y)
    !叉乘
    IMPLICIT NONE
    REAL(8),INTENT(IN) :: X(3),Y(3)
    REAL(8),INTENT(OUT):: Z(3)
    Z(1)=X(2)*Y(3)-X(3)*Y(2)
    Z(2)=-(X(1)*Y(3)-X(3)*Y(1))
    Z(3)=X(1)*Y(2)-X(2)*Y(1)
    END   
    
    SUBROUTINE UNIT3(X)
    !求单位向量
    IMPLICIT NONE
    REAL(8),INTENT(INOUT) :: X(3)
    INTEGER :: I
    REAL(8) :: S
    S=0.0
    DO I=1,3
        S=S+X(I)*X(I)
    END DO
    S=SQRT(S)
    X=X/S
    END SUBROUTINE
    
    !FUNCTION NORM3(X)
    !!获得x的模
    !IMPLICIT NONE
    !REAL(8),INTENT(IN) :: X(3)
    !INTEGER :: I
    !REAL(8) :: S
    !S=0.0
    !DO I=1,3
    !    S=S+X(I)*X(I)
    !END DO
    !S=SQRT(S)
    !RETURN S
    !END FUNCTION

    
    
    
    !---------------------------------------------------------------------
    !   三次样条曲线差值
    !---------------------------------------------------------------------
    

      SUBROUTINE spline(x,y,n,yp1,ypn,y2)
	  IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NMAX=500)
      INTEGER i,k
      REAL(8) x(n),y(n),y2(n),u(NMAX)
      
      if(yp1.gt..99e30) then
         y2(1)=0.
         u(1)=0.
      else
         y2(1)=-0.5
         u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      endif
      DO i=2,n-1
         sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
         p=sig*y2(i-1)+2.
         y2(i)=(sig-1.)/p
         u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/ &
         (x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p   
      END DO
      if (ypn.gt..99e30) then
          qn=0.
          un=0.
      else
          qn=0.5
          un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      DO k=n-1,1,-1
         y2(k)=y2(k)*y2(k+1)+u(k)
      END DO
      
    END

    
    SUBROUTINE splint(xa,ya,y2a,n,x,y)
	IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      REAL(8) xa(n),y2a(n),ya(n)
      
      klo=1
      khi=n
1     IF(khi-klo.gt.1) then
         k=(khi+klo)/2
         IF(xa(k).gt.x)then
            khi=k
         ELSE
            klo=k
         END IF
         goto 1
      END IF
      h=xa(khi)-xa(klo)
      IF(ABS(h).LT.1.0E-6) pause 'bad xa input in splint'
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
     
    END

    SUBROUTINE LINEAR1D(M,M_VEC,N_VEC,X,Z)
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: M
    REAL(8),INTENT(IN)::M_VEC(M),N_VEC(M),X
    REAL(8),INTENT(INOUT)::Z
    INTEGER::I
    REAL(8)::Z1,Z2
    REAL(8)::EPS=1E-5
    INTEGER::IP=0
    EPS=0
    DO I=1,M-1
            IF( ((X-M_VEC(I))*(X-M_VEC(I+1))).LE.EPS ) THEN
                Z =(X-M_VEC(I))/(M_VEC(I+1)-M_VEC(I))*(N_VEC(I+1)-N_VEC(I))+N_VEC(I)
            END IF
            IP=1
    END DO
    IF(IP.EQ.0) WRITE(*,*)'ERROR IN LINEAR2D'
    END SUBROUTINE
    
    
    
    SUBROUTINE LINEAR2D(M,N,M_VEC,N_VEC,MN_MATRIX,X,Y,Z)
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: M,N
    REAL(8),INTENT(IN)::M_VEC(M),N_VEC(N),MN_MATRIX(M,N),X,Y
    REAL(8),INTENT(INOUT)::Z
    INTEGER::I,J
    REAL(8)::Z1,Z2
    REAL(8)::EPS=1E-5
    
    INTEGER:: IP=0
    EPS=0
    DO I=1,M-1
        DO J=1,N-1
            IF(((X-M_VEC(I))*(X-M_VEC(I+1)).LE.EPS) .AND. ((Y-N_VEC(J))*(Y-N_VEC(J+1)).LE.EPS))THEN
                Z1=(X-M_VEC(I))/(M_VEC(I+1)-M_VEC(I))*(MN_MATRIX(I+1,J)-MN_MATRIX(I,J))+MN_MATRIX(I,J)
                Z2=(X-M_VEC(I))/(M_VEC(I+1)-M_VEC(I))*(MN_MATRIX(I+1,J+1)-MN_MATRIX(I,J+1))+MN_MATRIX(I,J+1)
                Z =(Y-N_VEC(J))/(N_VEC(J+1)-N_VEC(J))*(Z2-Z1)+Z1
                IP=1
            END IF
        END DO
    END DO
    IF(IP.EQ.0) WRITE(*,*)'warning IN LINEAR2D'
    
    END SUBROUTINE
    
    
    
    
    !!-------------------------------------------------------------------------
    !!-------------------------------------------------------------------------
      SUBROUTINE four1(dat,nn,isign)
      ! fourier transform
      
      ! nn: 采样点数，要求为2的幂次
      ! dat: 输入：含有2*NN的实数组，分别存放每个数据的实部和虚部 输出：变换后的复数值
      ! isgn: 1 为正变换 ，-1为逆变换（输出结果要除以NN）
      
      implicit none
      INTEGER,intent(in):: isign,nn
      REAL(8),intent(inout):: dat(2*nn)
      
      INTEGER i,istep,j,m,mmax,n
      REAL(8) tempi,tempr
      real(8) theta,wi,wpi,wpr,wr,wtemp
      n=2*nn
      j=1
      do 11 i=1,n,2
        if(j.gt.i)then
          tempr=dat(j)
          tempi=dat(j+1)
          dat(j)=dat(i)
          dat(j+1)=dat(i+1)
          dat(i)=tempr
          dat(i+1)=tempi
        endif
        m=n/2
1       if ((m.ge.2).and.(j.gt.m)) then
          j=j-m
          m=m/2
        goto 1
        endif
        j=j+m
11    continue
      mmax=2

2     if (n.gt.mmax) then
        istep=2*mmax
        theta=6.28318530717959d0/(isign*mmax)
        wpr=-2.d0*sin(0.5d0*theta)**2
        wpi=sin(theta)
        wr=1.d0
        wi=0.d0
        do 13 m=1,mmax,2
          do 12 i=m,n,istep
            j=i+mmax
            tempr=sngl(wr)*dat(j)-sngl(wi)*dat(j+1)
            tempi=sngl(wr)*dat(j+1)+sngl(wi)*dat(j)
            dat(j)=dat(i)-tempr
            dat(j+1)=dat(i+1)-tempi
            dat(i)=dat(i)+tempr
            dat(i+1)=dat(i+1)+tempi
12        continue
          wtemp=wr
          wr=wr*wpr-wi*wpi+wr
          wi=wi*wpr+wtemp*wpi+wi
13      continue
        mmax=istep

      goto 2
      endif
      return
    END
    
    SUBROUTINE DFT(TM_SIGNAL,FREQ_SIGNAL,N,TIME,FREQ_SAMPLING)
    ! 频谱分析
    
    IMPLICIT NONE
    INTEGER,INTENT(IN)::N
    REAL(8),INTENT(IN)::TM_SIGNAL(N),TIME
    REAL(8),INTENT(OUT)::FREQ_SIGNAL(N/2+1),FREQ_SAMPLING
    
    INTEGER ::I,MM
    REAL(8) ::NN
    REAL(8):: DT
    REAL(8):: SPEC(N*2)
    
    DT=TIME/N
    FREQ_SAMPLING=1/DT
    
    MM=FLOOR(ALOG(N*1.0)/ALOG(2.0))
    IF ((2**MM).LT.NN-0.5)THEN
        WRITE(*,*)'n is not power of 2'
    END IF
    
    DO I=1,N
        SPEC(I*2-1)=TM_SIGNAL(I)
        SPEC(I*2)=0
    END DO
    !
    !
    CALL FOUR1(SPEC,N,1)!DFT
    SPEC=SPEC/n
    
    
    DO I=1,N/2+1
        FREQ_SIGNAL(I)=SQRT(SPEC(2*i-1)*SPEC(2*i-1)+SPEC(2*i)*SPEC(2*i))
    END DO
    
    END SUBROUTINE
    