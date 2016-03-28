module xml_data_helicopter_parameters
   use READ_XML_PRIMITIVES
   use WRITE_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_

type airfoil_section
   integer                                         :: id
   real                                            :: airfoil_start_position
   real                                            :: airfoil_end_position
   real                                            :: airfoil_start_chord
   real                                            :: airfoil_end_chord
   integer                                         :: airfoil_number
   real                                            :: swept_back
   real                                            :: dihedral
   integer                                         :: parabolic
end type airfoil_section

type airfoil
   integer                                         :: id
   character(len=80)                                :: airfoil_name
   character(len=80)                                :: C81_file_name
   character(len=80)                                :: XY_file_name
end type airfoil

type rotor
   integer                                         :: number_of_blades
   integer                                         :: hub_type
   real, dimension(:), pointer                     :: hub_position => null()
   real, dimension(:), pointer                     :: shaft_direction => null()
   real                                            :: rotor_radius
   real                                            :: flap_hinge_position
   real                                            :: lag_hinge_position
   real                                            :: pitch_bearing_position
   integer                                         :: twist_type
   real                                            :: twist_angle
   character(len=80)                                :: twist_file_name
   integer                                         :: twist_sections
   real, dimension(:), pointer                     :: r_tw => null()
   real, dimension(:), pointer                     :: theta_tw => null()
   integer                                         :: number_of_airfoil_section
   type(airfoil_section), dimension(:), pointer    :: airfoil_sections => null()
   character(len=1)                                :: section_file_name
   real                                            :: chord
   real                                            :: lock_number
   real, dimension(:), pointer                     :: pitch_input => null()
   real, dimension(:), pointer                     :: flap_angle => null()
   real                                            :: alpha_s
   real                                            :: gamma_s
   real                                            :: hub_height
   real                                            :: thrust_coefficient
   real                                            :: inflow_ratio
   real                                            :: advance_ratio
   real                                            :: rotational_speed
   real                                            :: flap_frequency
   real                                            :: flap_hinge_stiffness
   real                                            :: flap_inertia
   real                                            :: precone_angle
   real                                            :: blade_first_moment_of_inertia
   real                                            :: pitch_flap_coupling
   real                                            :: solidity
   real                                            :: tip_velocity
   real                                            :: area
end type rotor

type fuselage
   real                                            :: roll_inertia
   real                                            :: pitch_inertia
   real                                            :: yaw_inertia
   real                                            :: roll_angle
   real                                            :: pitch_angle
   real                                            :: yaw_angle
   real                                            :: roll_rate
   real                                            :: pitch_rate
   real                                            :: yaw_rate
   character(len=80)                                :: aeroforce_file
end type fuselage

type engine
   character(len=80)                                :: engine_name
end type engine

type landing_gear
   real                                            :: stiffness
   real                                            :: damping_ratio
end type landing_gear

type control_system
   real                                            :: K_theta
end type control_system
   type(airfoil), dimension(:), pointer            :: airfoils => null()
   integer                                         :: number_of_airfoils
   type(rotor), dimension(:), pointer              :: rotors => null()
   integer                                         :: number_of_rotors
   type(fuselage)                                  :: airframe
   integer                                         :: NE
   integer                                         :: NGS
   integer                                         :: NB
   integer                                         :: NP
   integer                                         :: NU
   integer                                         :: NZ
   integer                                         :: NRAIR
   integer                                         :: NRMOM
   integer                                         :: NDE
   integer, dimension(:), pointer                  :: NRTR => null()
   character(len=80)                                :: section_property_file_name
   real(kind=kind(1.0d0))                          :: DD
   real(kind=kind(1.0d0))                          :: AA
   real(kind=kind(1.0d0))                          :: alfaz
   integer                                         :: nezh
   real(kind=kind(1.0d0))                          :: BETA0
   real(kind=kind(1.0d0))                          :: K_flap
   real(kind=kind(1.0d0))                          :: c_flap
   real(kind=kind(1.0d0))                          :: k_lag
   real(kind=kind(1.0d0))                          :: c_lag
   real(kind=kind(1.0d0))                          :: k_pitch
   real(kind=kind(1.0d0))                          :: c_pitch
   real(kind=kind(1.0d0))                          :: k_matcher_lag
   real(kind=kind(1.0d0))                          :: k_matcher_flap
   real(kind=kind(1.0d0))                          :: c_matcher_lag
   real(kind=kind(1.0d0))                          :: c_matcher_flap
   real(kind=kind(1.0d0))                          :: flap_offset
   real(kind=kind(1.0d0))                          :: pitch_horn_length
   integer                                         :: I_FH
   integer                                         :: I_LH
   integer                                         :: I_PB
   integer                                         :: IM
   real(kind=kind(1.0d0))                          :: ROA
   real(kind=kind(1.0d0))                          :: OMG
   real(kind=kind(1.0d0))                          :: VF
   real(kind=kind(1.0d0))                          :: CT
   real(kind=kind(1.0d0))                          :: alpha_tpp
   real(kind=kind(1.0d0))                          :: alpha_rotor
   real(kind=kind(1.0d0))                          :: psi_rotor
   real(kind=kind(1.0d0))                          :: PCH0
   real(kind=kind(1.0d0))                          :: CC
   real(kind=kind(1.0d0))                          :: SS
contains
subroutine read_xml_type_airfoil_section_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(airfoil_section), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(airfoil_section), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_airfoil_section( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_airfoil_section_array

subroutine read_xml_type_airfoil_section( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(airfoil_section), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=len(starttag))                 :: tag
   logical                                         :: has_id
   logical                                         :: has_airfoil_start_position
   logical                                         :: has_airfoil_end_position
   logical                                         :: has_airfoil_start_chord
   logical                                         :: has_airfoil_end_chord
   logical                                         :: has_airfoil_number
   logical                                         :: has_swept_back
   logical                                         :: has_dihedral
   logical                                         :: has_parabolic
   has_id                               = .false.
   has_airfoil_start_position           = .false.
   has_airfoil_end_position             = .false.
   has_airfoil_start_chord              = .false.
   has_airfoil_end_chord                = .false.
   has_airfoil_number                   = .false.
   has_swept_back                       = .false.
   has_dihedral                         = .false.
   has_parabolic                        = .false.
   call init_xml_type_airfoil_section(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('id')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%id, has_id )
      case('airfoil_start_position')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%airfoil_start_position, has_airfoil_start_position )
      case('airfoil_end_position')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%airfoil_end_position, has_airfoil_end_position )
      case('airfoil_start_chord')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%airfoil_start_chord, has_airfoil_start_chord )
      case('airfoil_end_chord')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%airfoil_end_chord, has_airfoil_end_chord )
      case('airfoil_number')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%airfoil_number, has_airfoil_number )
      case('swept_back')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%swept_back, has_swept_back )
      case('dihedral')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%dihedral, has_dihedral )
      case('parabolic')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%parabolic, has_parabolic )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // TRIM(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_id ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on id')
   endif
   if ( .not. has_airfoil_start_position ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on airfoil_start_position')
   endif
   if ( .not. has_airfoil_end_position ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on airfoil_end_position')
   endif
   if ( .not. has_airfoil_start_chord ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on airfoil_start_chord')
   endif
   if ( .not. has_airfoil_end_chord ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on airfoil_end_chord')
   endif
   if ( .not. has_airfoil_number ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on airfoil_number')
   endif
   if ( .not. has_swept_back ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on swept_back')
   endif
   if ( .not. has_dihedral ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on dihedral')
   endif
   if ( .not. has_parabolic ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on parabolic')
   endif
end subroutine read_xml_type_airfoil_section
subroutine init_xml_type_airfoil_section_array( dvar )
   type(airfoil_section), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_airfoil_section_array
subroutine init_xml_type_airfoil_section(dvar)
   type(airfoil_section) :: dvar
end subroutine init_xml_type_airfoil_section
subroutine write_xml_type_airfoil_section_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(airfoil_section), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_airfoil_section( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_airfoil_section_array

subroutine write_xml_type_airfoil_section( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(airfoil_section)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',TRIM(tag), '>'
   call write_to_xml_integer( info, 'id', indent+3, dvar%id)
   call write_to_xml_real( info, 'airfoil_start_position', indent+3, dvar%airfoil_start_position)
   call write_to_xml_real( info, 'airfoil_end_position', indent+3, dvar%airfoil_end_position)
   call write_to_xml_real( info, 'airfoil_start_chord', indent+3, dvar%airfoil_start_chord)
   call write_to_xml_real( info, 'airfoil_end_chord', indent+3, dvar%airfoil_end_chord)
   call write_to_xml_integer( info, 'airfoil_number', indent+3, dvar%airfoil_number)
   call write_to_xml_real( info, 'swept_back', indent+3, dvar%swept_back)
   call write_to_xml_real( info, 'dihedral', indent+3, dvar%dihedral)
   call write_to_xml_integer( info, 'parabolic', indent+3, dvar%parabolic)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //TRIM(tag) // '>'
end subroutine write_xml_type_airfoil_section

subroutine read_xml_type_airfoil_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(airfoil), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(airfoil), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_airfoil( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_airfoil_array

subroutine read_xml_type_airfoil( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(airfoil), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=len(starttag))                 :: tag
   logical                                         :: has_id
   logical                                         :: has_airfoil_name
   logical                                         :: has_C81_file_name
   logical                                         :: has_XY_file_name
   has_id                               = .false.
   has_airfoil_name                     = .false.
   has_C81_file_name                    = .false.
   has_XY_file_name                     = .false.
   call init_xml_type_airfoil(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('id')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%id, has_id )
      case('airfoil_name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%airfoil_name, has_airfoil_name )
      case('C81_file_name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%C81_file_name, has_C81_file_name )
      case('XY_file_name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%XY_file_name, has_XY_file_name )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // TRIM(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_id ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on id')
   endif
   if ( .not. has_airfoil_name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on airfoil_name')
   endif
   if ( .not. has_C81_file_name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on C81_file_name')
   endif
   if ( .not. has_XY_file_name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on XY_file_name')
   endif
end subroutine read_xml_type_airfoil
subroutine init_xml_type_airfoil_array( dvar )
   type(airfoil), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_airfoil_array
subroutine init_xml_type_airfoil(dvar)
   type(airfoil) :: dvar
end subroutine init_xml_type_airfoil
subroutine write_xml_type_airfoil_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(airfoil), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_airfoil( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_airfoil_array

subroutine write_xml_type_airfoil( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(airfoil)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',TRIM(tag), '>'
   call write_to_xml_integer( info, 'id', indent+3, dvar%id)
   call write_to_xml_word( info, 'airfoil_name', indent+3, dvar%airfoil_name)
   call write_to_xml_word( info, 'C81_file_name', indent+3, dvar%C81_file_name)
   call write_to_xml_word( info, 'XY_file_name', indent+3, dvar%XY_file_name)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //TRIM(tag) // '>'
end subroutine write_xml_type_airfoil

subroutine read_xml_type_rotor_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(rotor), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(rotor), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_rotor( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_rotor_array

subroutine read_xml_type_rotor( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(rotor), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=len(starttag))                 :: tag
   logical                                         :: has_number_of_blades
   logical                                         :: has_hub_type
   logical                                         :: has_hub_position
   logical                                         :: has_shaft_direction
   logical                                         :: has_rotor_radius
   logical                                         :: has_flap_hinge_position
   logical                                         :: has_lag_hinge_position
   logical                                         :: has_pitch_bearing_position
   logical                                         :: has_twist_type
   logical                                         :: has_twist_angle
   logical                                         :: has_twist_file_name
   logical                                         :: has_twist_sections
   logical                                         :: has_r_tw
   logical                                         :: has_theta_tw
   logical                                         :: has_number_of_airfoil_section
   logical                                         :: has_airfoil_sections
   logical                                         :: has_section_file_name
   logical                                         :: has_chord
   logical                                         :: has_lock_number
   logical                                         :: has_pitch_input
   logical                                         :: has_flap_angle
   logical                                         :: has_alpha_s
   logical                                         :: has_gamma_s
   logical                                         :: has_hub_height
   logical                                         :: has_thrust_coefficient
   logical                                         :: has_inflow_ratio
   logical                                         :: has_advance_ratio
   logical                                         :: has_rotational_speed
   logical                                         :: has_flap_frequency
   logical                                         :: has_flap_hinge_stiffness
   logical                                         :: has_flap_inertia
   logical                                         :: has_precone_angle
   logical                                         :: has_blade_first_moment_of_inertia
   logical                                         :: has_pitch_flap_coupling
   logical                                         :: has_solidity
   logical                                         :: has_tip_velocity
   logical                                         :: has_area
   has_number_of_blades                 = .false.
   has_hub_type                         = .false.
   has_hub_position                     = .false.
   has_shaft_direction                  = .false.
   has_rotor_radius                     = .false.
   has_flap_hinge_position              = .false.
   has_lag_hinge_position               = .false.
   has_pitch_bearing_position           = .false.
   has_twist_type                       = .false.
   has_twist_angle                      = .false.
   has_twist_file_name                  = .false.
   has_twist_sections                   = .false.
   has_r_tw                             = .false.
   has_theta_tw                         = .false.
   has_number_of_airfoil_section        = .false.
   has_airfoil_sections                 = .false.
   allocate(dvar%airfoil_sections(0))
   has_section_file_name                = .false.
   has_chord                            = .false.
   has_lock_number                      = .false.
   has_pitch_input                      = .false.
   has_flap_angle                       = .false.
   has_alpha_s                          = .false.
   has_gamma_s                          = .false.
   has_hub_height                       = .false.
   has_thrust_coefficient               = .false.
   has_inflow_ratio                     = .false.
   has_advance_ratio                    = .false.
   has_rotational_speed                 = .false.
   has_flap_frequency                   = .false.
   has_flap_hinge_stiffness             = .false.
   has_flap_inertia                     = .false.
   has_precone_angle                    = .false.
   has_blade_first_moment_of_inertia    = .false.
   has_pitch_flap_coupling              = .false.
   has_solidity                         = .false.
   has_tip_velocity                     = .false.
   has_area                             = .false.
   call init_xml_type_rotor(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('number_of_blades')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%number_of_blades, has_number_of_blades )
      case('hub_type')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%hub_type, has_hub_type )
      case('hub_position')
         call read_xml_real_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%hub_position, has_hub_position )
      case('shaft_direction')
         call read_xml_real_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%shaft_direction, has_shaft_direction )
      case('rotor_radius')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%rotor_radius, has_rotor_radius )
      case('flap_hinge_position')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%flap_hinge_position, has_flap_hinge_position )
      case('lag_hinge_position')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%lag_hinge_position, has_lag_hinge_position )
      case('pitch_bearing_position')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%pitch_bearing_position, has_pitch_bearing_position )
      case('twist_type')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%twist_type, has_twist_type )
      case('twist_angle')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%twist_angle, has_twist_angle )
      case('twist_file_name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%twist_file_name, has_twist_file_name )
      case('twist_sections')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%twist_sections, has_twist_sections )
      case('r_tw')
         call read_xml_real_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%r_tw, has_r_tw )
      case('theta_tw')
         call read_xml_real_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%theta_tw, has_theta_tw )
      case('number_of_airfoil_section')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%number_of_airfoil_section, has_number_of_airfoil_section )
      case('airfoil_sections')
         call read_xml_type_airfoil_section_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%airfoil_sections, has_airfoil_sections )
      case('section_file_name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%section_file_name, has_section_file_name )
      case('chord')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%chord, has_chord )
      case('lock_number')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%lock_number, has_lock_number )
      case('pitch_input')
         call read_xml_real_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%pitch_input, has_pitch_input )
      case('flap_angle')
         call read_xml_real_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%flap_angle, has_flap_angle )
      case('alpha_s')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%alpha_s, has_alpha_s )
      case('gamma_s')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%gamma_s, has_gamma_s )
      case('hub_height')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%hub_height, has_hub_height )
      case('thrust_coefficient')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%thrust_coefficient, has_thrust_coefficient )
      case('inflow_ratio')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%inflow_ratio, has_inflow_ratio )
      case('advance_ratio')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%advance_ratio, has_advance_ratio )
      case('rotational_speed')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%rotational_speed, has_rotational_speed )
      case('flap_frequency')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%flap_frequency, has_flap_frequency )
      case('flap_hinge_stiffness')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%flap_hinge_stiffness, has_flap_hinge_stiffness )
      case('flap_inertia')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%flap_inertia, has_flap_inertia )
      case('precone_angle')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%precone_angle, has_precone_angle )
      case('blade_first_moment_of_inertia')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%blade_first_moment_of_inertia, has_blade_first_moment_of_inertia )
      case('pitch_flap_coupling')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%pitch_flap_coupling, has_pitch_flap_coupling )
      case('solidity')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%solidity, has_solidity )
      case('tip_velocity')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%tip_velocity, has_tip_velocity )
      case('area')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%area, has_area )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // TRIM(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_number_of_blades ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on number_of_blades')
   endif
   if ( .not. has_hub_type ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on hub_type')
   endif
   if ( .not. has_hub_position ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on hub_position')
   endif
   if ( .not. has_shaft_direction ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on shaft_direction')
   endif
   if ( .not. has_rotor_radius ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on rotor_radius')
   endif
   if ( .not. has_flap_hinge_position ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on flap_hinge_position')
   endif
   if ( .not. has_lag_hinge_position ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on lag_hinge_position')
   endif
   if ( .not. has_pitch_bearing_position ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on pitch_bearing_position')
   endif
   if ( .not. has_twist_type ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on twist_type')
   endif
   if ( .not. has_twist_angle ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on twist_angle')
   endif
   if ( .not. has_twist_file_name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on twist_file_name')
   endif
   if ( .not. has_twist_sections ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on twist_sections')
   endif
   if ( .not. has_r_tw ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on r_tw')
   endif
   if ( .not. has_theta_tw ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on theta_tw')
   endif
   if ( .not. has_number_of_airfoil_section ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on number_of_airfoil_section')
   endif
   if ( .not. has_airfoil_sections ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on airfoil_sections')
   endif
   if ( .not. has_section_file_name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on section_file_name')
   endif
   if ( .not. has_chord ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on chord')
   endif
   if ( .not. has_lock_number ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on lock_number')
   endif
   if ( .not. has_pitch_input ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on pitch_input')
   endif
   if ( .not. has_flap_angle ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on flap_angle')
   endif
   if ( .not. has_alpha_s ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on alpha_s')
   endif
   if ( .not. has_gamma_s ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on gamma_s')
   endif
   if ( .not. has_hub_height ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on hub_height')
   endif
   if ( .not. has_thrust_coefficient ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on thrust_coefficient')
   endif
   if ( .not. has_inflow_ratio ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on inflow_ratio')
   endif
   if ( .not. has_advance_ratio ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on advance_ratio')
   endif
   if ( .not. has_rotational_speed ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on rotational_speed')
   endif
   if ( .not. has_flap_frequency ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on flap_frequency')
   endif
   if ( .not. has_flap_hinge_stiffness ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on flap_hinge_stiffness')
   endif
   if ( .not. has_flap_inertia ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on flap_inertia')
   endif
   if ( .not. has_precone_angle ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on precone_angle')
   endif
   if ( .not. has_blade_first_moment_of_inertia ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on blade_first_moment_of_inertia')
   endif
   if ( .not. has_pitch_flap_coupling ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on pitch_flap_coupling')
   endif
   if ( .not. has_solidity ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on solidity')
   endif
   if ( .not. has_tip_velocity ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on tip_velocity')
   endif
   if ( .not. has_area ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on area')
   endif
end subroutine read_xml_type_rotor
subroutine init_xml_type_rotor_array( dvar )
   type(rotor), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_rotor_array
subroutine init_xml_type_rotor(dvar)
   type(rotor) :: dvar
end subroutine init_xml_type_rotor
subroutine write_xml_type_rotor_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(rotor), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_rotor( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_rotor_array

subroutine write_xml_type_rotor( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(rotor)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',TRIM(tag), '>'
   call write_to_xml_integer( info, 'number_of_blades', indent+3, dvar%number_of_blades)
   call write_to_xml_integer( info, 'hub_type', indent+3, dvar%hub_type)
   call write_to_xml_real_array( info, 'hub_position', indent+3, dvar%hub_position)
   call write_to_xml_real_array( info, 'shaft_direction', indent+3, dvar%shaft_direction)
   call write_to_xml_real( info, 'rotor_radius', indent+3, dvar%rotor_radius)
   call write_to_xml_real( info, 'flap_hinge_position', indent+3, dvar%flap_hinge_position)
   call write_to_xml_real( info, 'lag_hinge_position', indent+3, dvar%lag_hinge_position)
   call write_to_xml_real( info, 'pitch_bearing_position', indent+3, dvar%pitch_bearing_position)
   call write_to_xml_integer( info, 'twist_type', indent+3, dvar%twist_type)
   call write_to_xml_real( info, 'twist_angle', indent+3, dvar%twist_angle)
   call write_to_xml_word( info, 'twist_file_name', indent+3, dvar%twist_file_name)
   call write_to_xml_integer( info, 'twist_sections', indent+3, dvar%twist_sections)
   call write_to_xml_real_array( info, 'r_tw', indent+3, dvar%r_tw)
   call write_to_xml_real_array( info, 'theta_tw', indent+3, dvar%theta_tw)
   call write_to_xml_integer( info, 'number_of_airfoil_section', indent+3, dvar%number_of_airfoil_section)
   call write_xml_type_airfoil_section_array( info, 'airfoil_sections', indent+3, dvar%airfoil_sections)
   call write_to_xml_word( info, 'section_file_name', indent+3, dvar%section_file_name)
   call write_to_xml_real( info, 'chord', indent+3, dvar%chord)
   call write_to_xml_real( info, 'lock_number', indent+3, dvar%lock_number)
   call write_to_xml_real_array( info, 'pitch_input', indent+3, dvar%pitch_input)
   call write_to_xml_real_array( info, 'flap_angle', indent+3, dvar%flap_angle)
   call write_to_xml_real( info, 'alpha_s', indent+3, dvar%alpha_s)
   call write_to_xml_real( info, 'gamma_s', indent+3, dvar%gamma_s)
   call write_to_xml_real( info, 'hub_height', indent+3, dvar%hub_height)
   call write_to_xml_real( info, 'thrust_coefficient', indent+3, dvar%thrust_coefficient)
   call write_to_xml_real( info, 'inflow_ratio', indent+3, dvar%inflow_ratio)
   call write_to_xml_real( info, 'advance_ratio', indent+3, dvar%advance_ratio)
   call write_to_xml_real( info, 'rotational_speed', indent+3, dvar%rotational_speed)
   call write_to_xml_real( info, 'flap_frequency', indent+3, dvar%flap_frequency)
   call write_to_xml_real( info, 'flap_hinge_stiffness', indent+3, dvar%flap_hinge_stiffness)
   call write_to_xml_real( info, 'flap_inertia', indent+3, dvar%flap_inertia)
   call write_to_xml_real( info, 'precone_angle', indent+3, dvar%precone_angle)
   call write_to_xml_real( info, 'blade_first_moment_of_inertia', indent+3, dvar%blade_first_moment_of_inertia)
   call write_to_xml_real( info, 'pitch_flap_coupling', indent+3, dvar%pitch_flap_coupling)
   call write_to_xml_real( info, 'solidity', indent+3, dvar%solidity)
   call write_to_xml_real( info, 'tip_velocity', indent+3, dvar%tip_velocity)
   call write_to_xml_real( info, 'area', indent+3, dvar%area)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //TRIM(tag) // '>'
end subroutine write_xml_type_rotor

subroutine read_xml_type_fuselage_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(fuselage), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(fuselage), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_fuselage( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_fuselage_array

subroutine read_xml_type_fuselage( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(fuselage), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=len(starttag))                 :: tag
   logical                                         :: has_roll_inertia
   logical                                         :: has_pitch_inertia
   logical                                         :: has_yaw_inertia
   logical                                         :: has_roll_angle
   logical                                         :: has_pitch_angle
   logical                                         :: has_yaw_angle
   logical                                         :: has_roll_rate
   logical                                         :: has_pitch_rate
   logical                                         :: has_yaw_rate
   logical                                         :: has_aeroforce_file
   has_roll_inertia                     = .false.
   has_pitch_inertia                    = .false.
   has_yaw_inertia                      = .false.
   has_roll_angle                       = .false.
   has_pitch_angle                      = .false.
   has_yaw_angle                        = .false.
   has_roll_rate                        = .false.
   has_pitch_rate                       = .false.
   has_yaw_rate                         = .false.
   has_aeroforce_file                   = .false.
   call init_xml_type_fuselage(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('roll_inertia')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%roll_inertia, has_roll_inertia )
      case('pitch_inertia')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%pitch_inertia, has_pitch_inertia )
      case('yaw_inertia')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%yaw_inertia, has_yaw_inertia )
      case('roll_angle')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%roll_angle, has_roll_angle )
      case('pitch_angle')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%pitch_angle, has_pitch_angle )
      case('yaw_angle')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%yaw_angle, has_yaw_angle )
      case('roll_rate')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%roll_rate, has_roll_rate )
      case('pitch_rate')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%pitch_rate, has_pitch_rate )
      case('yaw_rate')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%yaw_rate, has_yaw_rate )
      case('aeroforce_file')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%aeroforce_file, has_aeroforce_file )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // TRIM(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_roll_inertia ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on roll_inertia')
   endif
   if ( .not. has_pitch_inertia ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on pitch_inertia')
   endif
   if ( .not. has_yaw_inertia ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on yaw_inertia')
   endif
   if ( .not. has_roll_angle ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on roll_angle')
   endif
   if ( .not. has_pitch_angle ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on pitch_angle')
   endif
   if ( .not. has_yaw_angle ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on yaw_angle')
   endif
   if ( .not. has_roll_rate ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on roll_rate')
   endif
   if ( .not. has_pitch_rate ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on pitch_rate')
   endif
   if ( .not. has_yaw_rate ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on yaw_rate')
   endif
   if ( .not. has_aeroforce_file ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on aeroforce_file')
   endif
end subroutine read_xml_type_fuselage
subroutine init_xml_type_fuselage_array( dvar )
   type(fuselage), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_fuselage_array
subroutine init_xml_type_fuselage(dvar)
   type(fuselage) :: dvar
end subroutine init_xml_type_fuselage
subroutine write_xml_type_fuselage_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(fuselage), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_fuselage( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_fuselage_array

subroutine write_xml_type_fuselage( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(fuselage)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',TRIM(tag), '>'
   call write_to_xml_real( info, 'roll_inertia', indent+3, dvar%roll_inertia)
   call write_to_xml_real( info, 'pitch_inertia', indent+3, dvar%pitch_inertia)
   call write_to_xml_real( info, 'yaw_inertia', indent+3, dvar%yaw_inertia)
   call write_to_xml_real( info, 'roll_angle', indent+3, dvar%roll_angle)
   call write_to_xml_real( info, 'pitch_angle', indent+3, dvar%pitch_angle)
   call write_to_xml_real( info, 'yaw_angle', indent+3, dvar%yaw_angle)
   call write_to_xml_real( info, 'roll_rate', indent+3, dvar%roll_rate)
   call write_to_xml_real( info, 'pitch_rate', indent+3, dvar%pitch_rate)
   call write_to_xml_real( info, 'yaw_rate', indent+3, dvar%yaw_rate)
   call write_to_xml_word( info, 'aeroforce_file', indent+3, dvar%aeroforce_file)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //TRIM(tag) // '>'
end subroutine write_xml_type_fuselage

subroutine read_xml_type_engine_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(engine), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(engine), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_engine( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_engine_array

subroutine read_xml_type_engine( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(engine), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=len(starttag))                 :: tag
   logical                                         :: has_engine_name
   has_engine_name                      = .false.
   call init_xml_type_engine(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('engine_name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%engine_name, has_engine_name )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // TRIM(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_engine_name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on engine_name')
   endif
end subroutine read_xml_type_engine
subroutine init_xml_type_engine_array( dvar )
   type(engine), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_engine_array
subroutine init_xml_type_engine(dvar)
   type(engine) :: dvar
end subroutine init_xml_type_engine
subroutine write_xml_type_engine_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(engine), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_engine( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_engine_array

subroutine write_xml_type_engine( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(engine)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',TRIM(tag), '>'
   call write_to_xml_word( info, 'engine_name', indent+3, dvar%engine_name)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //TRIM(tag) // '>'
end subroutine write_xml_type_engine

subroutine read_xml_type_landing_gear_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(landing_gear), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(landing_gear), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_landing_gear( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_landing_gear_array

subroutine read_xml_type_landing_gear( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(landing_gear), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=len(starttag))                 :: tag
   logical                                         :: has_stiffness
   logical                                         :: has_damping_ratio
   has_stiffness                        = .false.
   has_damping_ratio                    = .false.
   call init_xml_type_landing_gear(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('stiffness')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%stiffness, has_stiffness )
      case('damping_ratio')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%damping_ratio, has_damping_ratio )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // TRIM(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_stiffness ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on stiffness')
   endif
   if ( .not. has_damping_ratio ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on damping_ratio')
   endif
end subroutine read_xml_type_landing_gear
subroutine init_xml_type_landing_gear_array( dvar )
   type(landing_gear), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_landing_gear_array
subroutine init_xml_type_landing_gear(dvar)
   type(landing_gear) :: dvar
end subroutine init_xml_type_landing_gear
subroutine write_xml_type_landing_gear_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(landing_gear), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_landing_gear( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_landing_gear_array

subroutine write_xml_type_landing_gear( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(landing_gear)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',TRIM(tag), '>'
   call write_to_xml_real( info, 'stiffness', indent+3, dvar%stiffness)
   call write_to_xml_real( info, 'damping_ratio', indent+3, dvar%damping_ratio)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //TRIM(tag) // '>'
end subroutine write_xml_type_landing_gear

subroutine read_xml_type_control_system_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(control_system), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(control_system), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_control_system( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_control_system_array

subroutine read_xml_type_control_system( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(control_system), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=len(starttag))                 :: tag
   logical                                         :: has_K_theta
   has_K_theta                          = .false.
   call init_xml_type_control_system(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('K_theta')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%K_theta, has_K_theta )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // TRIM(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_K_theta ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on K_theta')
   endif
end subroutine read_xml_type_control_system
subroutine init_xml_type_control_system_array( dvar )
   type(control_system), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_control_system_array
subroutine init_xml_type_control_system(dvar)
   type(control_system) :: dvar
end subroutine init_xml_type_control_system
subroutine write_xml_type_control_system_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(control_system), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_control_system( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_control_system_array

subroutine write_xml_type_control_system( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(control_system)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',TRIM(tag), '>'
   call write_to_xml_real( info, 'K_theta', indent+3, dvar%K_theta)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //TRIM(tag) // '>'
end subroutine write_xml_type_control_system

subroutine read_xml_file_helicopter_parameters(fname, lurep, errout)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep
   logical, intent(out), optional         :: errout

   type(XML_PARSE)                        :: info
   logical                                :: error
   character(len=80)                      :: tag
   character(len=80)                      :: starttag
   logical                                :: endtag
   character(len=80), dimension(1:2,1:20) :: attribs
   integer                                :: noattribs
   character(len=200), dimension(1:100)   :: data
   integer                                :: nodata
   logical                                         :: has_airfoils
   logical                                         :: has_number_of_airfoils
   logical                                         :: has_rotors
   logical                                         :: has_number_of_rotors
   logical                                         :: has_airframe
   logical                                         :: has_NE
   logical                                         :: has_NGS
   logical                                         :: has_NB
   logical                                         :: has_NP
   logical                                         :: has_NU
   logical                                         :: has_NZ
   logical                                         :: has_NRAIR
   logical                                         :: has_NRMOM
   logical                                         :: has_NDE
   logical                                         :: has_NRTR
   logical                                         :: has_section_property_file_name
   logical                                         :: has_DD
   logical                                         :: has_AA
   logical                                         :: has_alfaz
   logical                                         :: has_nezh
   logical                                         :: has_BETA0
   logical                                         :: has_K_flap
   logical                                         :: has_c_flap
   logical                                         :: has_k_lag
   logical                                         :: has_c_lag
   logical                                         :: has_k_pitch
   logical                                         :: has_c_pitch
   logical                                         :: has_k_matcher_lag
   logical                                         :: has_k_matcher_flap
   logical                                         :: has_c_matcher_lag
   logical                                         :: has_c_matcher_flap
   logical                                         :: has_flap_offset
   logical                                         :: has_pitch_horn_length
   logical                                         :: has_I_FH
   logical                                         :: has_I_LH
   logical                                         :: has_I_PB
   logical                                         :: has_IM
   logical                                         :: has_ROA
   logical                                         :: has_OMG
   logical                                         :: has_VF
   logical                                         :: has_CT
   logical                                         :: has_alpha_tpp
   logical                                         :: has_alpha_rotor
   logical                                         :: has_psi_rotor
   logical                                         :: has_PCH0
   logical                                         :: has_CC
   logical                                         :: has_SS
   has_airfoils                         = .false.
   allocate(airfoils(0))
   has_number_of_airfoils               = .false.
   has_rotors                           = .false.
   allocate(rotors(0))
   has_number_of_rotors                 = .false.
   has_airframe                         = .false.
   has_NE                               = .false.
   has_NGS                              = .false.
   has_NB                               = .false.
   has_NP                               = .false.
   has_NU                               = .false.
   has_NZ                               = .false.
   has_NRAIR                            = .false.
   has_NRMOM                            = .false.
   has_NDE                              = .false.
   has_NRTR                             = .false.
   has_section_property_file_name       = .false.
   has_DD                               = .false.
   has_AA                               = .false.
   has_alfaz                            = .false.
   has_nezh                             = .false.
   has_BETA0                            = .false.
   has_K_flap                           = .false.
   has_c_flap                           = .false.
   has_k_lag                            = .false.
   has_c_lag                            = .false.
   has_k_pitch                          = .false.
   has_c_pitch                          = .false.
   has_k_matcher_lag                    = .false.
   has_k_matcher_flap                   = .false.
   has_c_matcher_lag                    = .false.
   has_c_matcher_flap                   = .false.
   has_flap_offset                      = .false.
   has_pitch_horn_length                = .false.
   has_I_FH                             = .false.
   has_I_LH                             = .false.
   has_I_PB                             = .false.
   has_IM                               = .false.
   has_ROA                              = .false.
   has_OMG                              = .false.
   has_VF                               = .false.
   has_CT                               = .false.
   has_alpha_tpp                        = .false.
   has_alpha_rotor                      = .false.
   has_psi_rotor                        = .false.
   has_PCH0                             = .false.
   has_CC                               = .false.
   has_SS                               = .false.

   call init_xml_file_helicopter_parameters
   call xml_open( info, fname, .true. )
   call xml_options( info, report_errors=.true., ignore_whitespace=.true.)
   lurep_ = 0
   if ( present(lurep) ) then
      lurep_ = lurep
      call xml_options( info, report_lun=lurep )
   endif
   do
      call xml_get( info, starttag, endtag, attribs, noattribs, &
         data, nodata)
      if ( starttag .ne. '!--' ) exit
   enddo
   if ( starttag .ne. "helicopter" ) then
      call xml_report_errors( info, &
         'XML-file should have root element "helicopter"')
      error = .true.
      call xml_close(info)
      return
   endif
   strict_ = .true.
   error = .false.
   do
      call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
      if ( xml_error(info) ) then
         write(lurep_,*) 'Error reading input file!'
         error = .true.
         return
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('airfoils')
         call read_xml_type_airfoil_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            airfoils, has_airfoils )
      case('number_of_airfoils')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            number_of_airfoils, has_number_of_airfoils )
      case('rotors')
         call read_xml_type_rotor_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            rotors, has_rotors )
      case('number_of_rotors')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            number_of_rotors, has_number_of_rotors )
      case('airframe')
         call read_xml_type_fuselage( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            airframe, has_airframe )
      case('NE')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NE, has_NE )
      case('NGS')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NGS, has_NGS )
      case('NB')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NB, has_NB )
      case('NP')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NP, has_NP )
      case('NU')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NU, has_NU )
      case('NZ')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NZ, has_NZ )
      case('NRAIR')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NRAIR, has_NRAIR )
      case('NRMOM')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NRMOM, has_NRMOM )
      case('NDE')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NDE, has_NDE )
      case('NRTR')
         call read_xml_integer_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NRTR, has_NRTR )
      case('section_property_file_name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            section_property_file_name, has_section_property_file_name )
      case('DD')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            DD, has_DD )
      case('AA')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            AA, has_AA )
      case('alfaz')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            alfaz, has_alfaz )
      case('nezh')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            nezh, has_nezh )
      case('BETA0')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            BETA0, has_BETA0 )
      case('K_flap')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            K_flap, has_K_flap )
      case('c_flap')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            c_flap, has_c_flap )
      case('k_lag')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            k_lag, has_k_lag )
      case('c_lag')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            c_lag, has_c_lag )
      case('k_pitch')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            k_pitch, has_k_pitch )
      case('c_pitch')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            c_pitch, has_c_pitch )
      case('k_matcher_lag')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            k_matcher_lag, has_k_matcher_lag )
      case('k_matcher_flap')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            k_matcher_flap, has_k_matcher_flap )
      case('c_matcher_lag')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            c_matcher_lag, has_c_matcher_lag )
      case('c_matcher_flap')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            c_matcher_flap, has_c_matcher_flap )
      case('flap_offset')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            flap_offset, has_flap_offset )
      case('pitch_horn_length')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            pitch_horn_length, has_pitch_horn_length )
      case('I_FH')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            I_FH, has_I_FH )
      case('I_LH')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            I_LH, has_I_LH )
      case('I_PB')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            I_PB, has_I_PB )
      case('IM')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            IM, has_IM )
      case('ROA')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            ROA, has_ROA )
      case('OMG')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            OMG, has_OMG )
      case('VF')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            VF, has_VF )
      case('CT')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            CT, has_CT )
      case('alpha_tpp')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            alpha_tpp, has_alpha_tpp )
      case('alpha_rotor')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            alpha_rotor, has_alpha_rotor )
      case('psi_rotor')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            psi_rotor, has_psi_rotor )
      case('PCH0')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            PCH0, has_PCH0 )
      case('CC')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            CC, has_CC )
      case('SS')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            SS, has_SS )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // TRIM(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_airfoils ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on airfoils')
   endif
   if ( .not. has_number_of_airfoils ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on number_of_airfoils')
   endif
   if ( .not. has_rotors ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on rotors')
   endif
   if ( .not. has_number_of_rotors ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on number_of_rotors')
   endif
   if ( .not. has_airframe ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on airframe')
   endif
   if ( .not. has_NE ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NE')
   endif
   if ( .not. has_NGS ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NGS')
   endif
   if ( .not. has_NB ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NB')
   endif
   if ( .not. has_NP ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NP')
   endif
   if ( .not. has_NU ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NU')
   endif
   if ( .not. has_NZ ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NZ')
   endif
   if ( .not. has_NRAIR ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NRAIR')
   endif
   if ( .not. has_NRMOM ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NRMOM')
   endif
   if ( .not. has_NRTR ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NRTR')
   endif
   if ( .not. has_section_property_file_name ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on section_property_file_name')
   endif
   if ( .not. has_DD ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on DD')
   endif
   if ( .not. has_AA ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on AA')
   endif
   if ( .not. has_alfaz ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on alfaz')
   endif
   if ( .not. has_nezh ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on nezh')
   endif
   if ( .not. has_BETA0 ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on BETA0')
   endif
   if ( .not. has_K_flap ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on K_flap')
   endif
   if ( .not. has_c_flap ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on c_flap')
   endif
   if ( .not. has_k_lag ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on k_lag')
   endif
   if ( .not. has_c_lag ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on c_lag')
   endif
   if ( .not. has_k_pitch ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on k_pitch')
   endif
   if ( .not. has_c_pitch ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on c_pitch')
   endif
   if ( .not. has_k_matcher_lag ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on k_matcher_lag')
   endif
   if ( .not. has_k_matcher_flap ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on k_matcher_flap')
   endif
   if ( .not. has_c_matcher_lag ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on c_matcher_lag')
   endif
   if ( .not. has_c_matcher_flap ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on c_matcher_flap')
   endif
   if ( .not. has_flap_offset ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on flap_offset')
   endif
   if ( .not. has_pitch_horn_length ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on pitch_horn_length')
   endif
   if ( .not. has_I_FH ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on I_FH')
   endif
   if ( .not. has_I_LH ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on I_LH')
   endif
   if ( .not. has_I_PB ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on I_PB')
   endif
   if ( .not. has_IM ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on IM')
   endif
   if ( .not. has_ROA ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on ROA')
   endif
   if ( .not. has_OMG ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on OMG')
   endif
   if ( .not. has_VF ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on VF')
   endif
   if ( .not. has_CT ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on CT')
   endif
   if ( .not. has_alpha_tpp ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on alpha_tpp')
   endif
   if ( .not. has_alpha_rotor ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on alpha_rotor')
   endif
   if ( .not. has_psi_rotor ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on psi_rotor')
   endif
   if ( .not. has_PCH0 ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on PCH0')
   endif
   if ( .not. has_CC ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on CC')
   endif
   if ( .not. has_SS ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on SS')
   endif
   if ( present(errout) ) errout = error
end subroutine

subroutine write_xml_file_helicopter_parameters(fname, lurep)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep

   type(XML_PARSE)                        :: info
   integer                                :: indent = 0

   call xml_open( info, fname, .false. )
   call xml_options( info, report_errors=.true.)
   if ( present(lurep) ) then
       call xml_options( info, report_errors=.true.)
   endif
   write(info%lun,'(a)') &
      '<helicopter>'
   call write_xml_type_airfoil_array( info, 'airfoils', indent+3, airfoils)
   call write_to_xml_integer( info, 'number_of_airfoils', indent+3, number_of_airfoils)
   call write_xml_type_rotor_array( info, 'rotors', indent+3, rotors)
   call write_to_xml_integer( info, 'number_of_rotors', indent+3, number_of_rotors)
   call write_xml_type_fuselage( info, 'airframe', indent+3, airframe)
   call write_to_xml_integer( info, 'NE', indent+3, NE)
   call write_to_xml_integer( info, 'NGS', indent+3, NGS)
   call write_to_xml_integer( info, 'NB', indent+3, NB)
   call write_to_xml_integer( info, 'NP', indent+3, NP)
   call write_to_xml_integer( info, 'NU', indent+3, NU)
   call write_to_xml_integer( info, 'NZ', indent+3, NZ)
   call write_to_xml_integer( info, 'NRAIR', indent+3, NRAIR)
   call write_to_xml_integer( info, 'NRMOM', indent+3, NRMOM)
   call write_to_xml_integer( info, 'NDE', indent+3, NDE)
   call write_to_xml_integer_array( info, 'NRTR', indent+3, NRTR)
   call write_to_xml_word( info, 'section_property_file_name', indent+3, section_property_file_name)
   call write_to_xml_double( info, 'DD', indent+3, DD)
   call write_to_xml_double( info, 'AA', indent+3, AA)
   call write_to_xml_double( info, 'alfaz', indent+3, alfaz)
   call write_to_xml_integer( info, 'nezh', indent+3, nezh)
   call write_to_xml_double( info, 'BETA0', indent+3, BETA0)
   call write_to_xml_double( info, 'K_flap', indent+3, K_flap)
   call write_to_xml_double( info, 'c_flap', indent+3, c_flap)
   call write_to_xml_double( info, 'k_lag', indent+3, k_lag)
   call write_to_xml_double( info, 'c_lag', indent+3, c_lag)
   call write_to_xml_double( info, 'k_pitch', indent+3, k_pitch)
   call write_to_xml_double( info, 'c_pitch', indent+3, c_pitch)
   call write_to_xml_double( info, 'k_matcher_lag', indent+3, k_matcher_lag)
   call write_to_xml_double( info, 'k_matcher_flap', indent+3, k_matcher_flap)
   call write_to_xml_double( info, 'c_matcher_lag', indent+3, c_matcher_lag)
   call write_to_xml_double( info, 'c_matcher_flap', indent+3, c_matcher_flap)
   call write_to_xml_double( info, 'flap_offset', indent+3, flap_offset)
   call write_to_xml_double( info, 'pitch_horn_length', indent+3, pitch_horn_length)
   call write_to_xml_integer( info, 'I_FH', indent+3, I_FH)
   call write_to_xml_integer( info, 'I_LH', indent+3, I_LH)
   call write_to_xml_integer( info, 'I_PB', indent+3, I_PB)
   call write_to_xml_integer( info, 'IM', indent+3, IM)
   call write_to_xml_double( info, 'ROA', indent+3, ROA)
   call write_to_xml_double( info, 'OMG', indent+3, OMG)
   call write_to_xml_double( info, 'VF', indent+3, VF)
   call write_to_xml_double( info, 'CT', indent+3, CT)
   call write_to_xml_double( info, 'alpha_tpp', indent+3, alpha_tpp)
   call write_to_xml_double( info, 'alpha_rotor', indent+3, alpha_rotor)
   call write_to_xml_double( info, 'psi_rotor', indent+3, psi_rotor)
   call write_to_xml_double( info, 'PCH0', indent+3, PCH0)
   call write_to_xml_double( info, 'CC', indent+3, CC)
   call write_to_xml_double( info, 'SS', indent+3, SS)
   write(info%lun,'(a)') '</helicopter>'
   call xml_close(info)
end subroutine

subroutine init_xml_file_helicopter_parameters
   NDE = 0

end subroutine

end module
