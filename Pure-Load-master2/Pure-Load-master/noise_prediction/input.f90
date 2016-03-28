    !module IO
    !use xml_data_noise_parameters
    !!use parameters
    !!use math
    !implicit none
    !real(kind=single),allocatable :: r(:,:),psi(:,:),lambda_i(:,:),phi(:,:)
    !real(kind=single),allocatable :: x(:),w(:)
    !contains
    !subroutine input()
    !! read helicopter model data and initialize helicopter module
    !
    !call read_xml_file_noise_parameters( 'NASA_2MRTS_ROTOR.xml' )
    !
    !
    !call compute_derived_arguments() 
    !call write_xml_file_NOISE_parameters( 'output_NASA_2MRTS_ROTOR.xml' )
    !!!write(*,*) rotors(1)%solidity
    !!call blade_gauss_point_coordinate()
    !return
    !end subroutine input
    !
    !subroutine compute_derived_arguments()
    !!rotors(1)%area               = pi_4 * rotors(1)%rotor_radius**2
    !!rotors(1)%solidity           = real(NBLAD,single) * rotors(1)%chord / pi_4 / rotors(1)%rotor_radius
    !!rotors(1)%tip_velocity       = rotors(1)%rotor_radius*rotors(1)%rotational_speed
    !!rotors(1)%advance_ratio      = flight%velocity/rotors(1)%tip_velocity
    !!flight%rou                    = flight%pressuare/R_air/(flight%temperature+T0)
    !!rotors(1)%thrust             = flight%mass*g0
    !!rotors(1)%thrust_coefficient = rotors(1)%thrust/flight%rou/rotors(1)%area/rotors(1)%tip_velocity**2
    !!rotors(1)%alpha_s            = -atan(rotors(1)%shaft_direction(1)/rotors(1)%shaft_direction(3))
    !!rotors(1)%alpha              = rotors(1)%alpha_s
    !end subroutine compute_derived_arguments
    !
    !subroutine blade_gauss_point_coordinate()
    !!integer::ngs,np,ne,i,j
    !!ngs = rotors(1)%rotor_blade%number_of_gauss_point
    !!np = rotors(1)%number_of_azimuth_coordinate
    !!ne = rotors(1)%rotor_blade%number_of_elements
    !!!ngs = 5
    !!!np = 90
    !!!ne = 9
    !!allocate(r(ne*ngs,1),psi(1,np),lambda_i(ne*ngs,np),x(ngs),w(ngs))
    !!call LegZo_S(ngs,x,w)
    !!r(1,1) = rotors(1)%rotor_blade%element_start_position
    !!do i = 1,ne-1
    !!    r(i*ngs+1,1) =  r((i-1)*ngs+1,1)+rotors(1)%rotor_blade%elements(i)%length
    !!end do
    !!do i = 1,ne   
    !!    r((i-1)*ngs+1:i*ngs,1) = r((i-1)*ngs+1,1) + rotors(1)%rotor_blade%elements(i)%length*(-x+1.0)/2.0
    !!end do 
    !!r = r/rotors(1)%rotor_radius
    !!do j = 1,np
    !!    psi(1,j) = real(j-1)/np*2*pi_8
    !!end do    
    !end subroutine
    !
    !!> read induced flow field from INDU.DAT
    !subroutine read_induced_flow_field(num_radial_station,num_angular_station,r,psi,lambda_i)
    !integer,intent(out) :: num_radial_station,num_angular_station
    !real(kind = single),intent(out),allocatable :: r(:,:)
    !real(kind = single),intent(out),allocatable :: psi(:,:)
    !real(kind = single),intent(out),allocatable :: lambda_i(:,:)
    !integer::i,j
    !!输出诱导入流比到'indu.dat'文件中
    !open(6,file='INDU.DAT')
    !read(6,*)   num_angular_station,num_radial_station
    !allocate(r(num_radial_station,1),psi(1,num_angular_station),lambda_i(num_radial_station,num_angular_station))
    !read(6,*) (r(i,1),i=1,num_radial_station)
    !do i = 1,num_angular_station
    !    read(6,*) psi(1,i),(lambda_i(j,i),j = 1,4)
    !    read(6,*) (lambda_i(j,i),j = 5,num_radial_station)
    !end do
    !close(6)
    !end subroutine
    !!subroutine files()
    !!! set i/o and open files   
    !!
    !!open(iin,file='sa349.inp',status='old',form='formatted')
    !!open(iout,file='sa349.rpt',status='unknown',form='formatted')
    !!open(iout1,file='sfrl.rpt',status='unknown',form='formatted')
    !!open(iout2,file='sfrh.rpt',status='unknown',form='formatted')
    !!open(iout3,file='blade.dat',status='unknown',form='formatted')
    !!open(iout4,file='kgm.dat',status='unknown',form='unformatted')
    !!open(iout5,file='body.dat',status='unknown',form='unformatted')
    !!open(iout6,file='rotor.dat',status='unknown',form='unformatted')
    !!return
    !!end subroutine files
    !end module IO