module xml_data_flight_parameters
   use READ_XML_PRIMITIVES
   use WRITE_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_
   
   

type flight_info
   integer                                         :: flight_number
   integer                                         :: flight_description
   real                                            :: altitude
   real                                            :: velocity
   real                                            :: mass
   real                                            :: temperature
   real                                            :: pressuare
   real                                            :: rou
   real                                            :: load_factor
   real                                            :: power
end type flight_info
   type(flight_info)                               :: flight
contains
subroutine read_xml_type_flight_info_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(flight_info), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(flight_info), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_flight_info( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_flight_info_array

subroutine read_xml_type_flight_info( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(flight_info), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=len(starttag))                 :: tag
   logical                                         :: has_flight_number
   logical                                         :: has_flight_description
   logical                                         :: has_altitude
   logical                                         :: has_velocity
   logical                                         :: has_mass
   logical                                         :: has_temperature
   logical                                         :: has_pressuare
   logical                                         :: has_rou
   logical                                         :: has_load_factor
   logical                                         :: has_power
   has_flight_number                    = .false.
   has_flight_description               = .false.
   has_altitude                         = .false.
   has_velocity                         = .false.
   has_mass                             = .false.
   has_temperature                      = .false.
   has_pressuare                        = .false.
   has_rou                              = .false.
   has_load_factor                      = .false.
   has_power                            = .false.
   call init_xml_type_flight_info(dvar)
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
      case('flight_number')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%flight_number, has_flight_number )
      case('flight_description')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%flight_description, has_flight_description )
      case('altitude')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%altitude, has_altitude )
      case('velocity')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%velocity, has_velocity )
      case('mass')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%mass, has_mass )
      case('temperature')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%temperature, has_temperature )
      case('pressuare')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%pressuare, has_pressuare )
      case('rou')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%rou, has_rou )
      case('load_factor')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%load_factor, has_load_factor )
      case('power')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%power, has_power )
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
   if ( .not. has_flight_number ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on flight_number')
   endif
   if ( .not. has_altitude ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on altitude')
   endif
   if ( .not. has_velocity ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on velocity')
   endif
   if ( .not. has_mass ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on mass')
   endif
   if ( .not. has_temperature ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on temperature')
   endif
   if ( .not. has_pressuare ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on pressuare')
   endif
   if ( .not. has_rou ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on rou')
   endif
   if ( .not. has_load_factor ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on load_factor')
   endif
   if ( .not. has_power ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on power')
   endif
end subroutine read_xml_type_flight_info
subroutine init_xml_type_flight_info_array( dvar )
   type(flight_info), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_flight_info_array
subroutine init_xml_type_flight_info(dvar)
   type(flight_info) :: dvar
   dvar%flight_description = 1
end subroutine init_xml_type_flight_info
subroutine write_xml_type_flight_info_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(flight_info), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_flight_info( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_flight_info_array

subroutine write_xml_type_flight_info( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(flight_info)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',TRIM(tag), '>'
   call write_to_xml_integer( info, 'flight_number', indent+3, dvar%flight_number)
   call write_to_xml_integer( info, 'flight_description', indent+3, dvar%flight_description)
   call write_to_xml_real( info, 'altitude', indent+3, dvar%altitude)
   call write_to_xml_real( info, 'velocity', indent+3, dvar%velocity)
   call write_to_xml_real( info, 'mass', indent+3, dvar%mass)
   call write_to_xml_real( info, 'temperature', indent+3, dvar%temperature)
   call write_to_xml_real( info, 'pressuare', indent+3, dvar%pressuare)
   call write_to_xml_real( info, 'rou', indent+3, dvar%rou)
   call write_to_xml_real( info, 'load_factor', indent+3, dvar%load_factor)
   call write_to_xml_real( info, 'power', indent+3, dvar%power)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //TRIM(tag) // '>'
end subroutine write_xml_type_flight_info

subroutine read_xml_file_flight_parameters(fname, lurep, errout)
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
   logical                                         :: has_flight
   has_flight                           = .false.

   call init_xml_file_flight_parameters
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
   if ( starttag .ne. "flight_infomation" ) then
      call xml_report_errors( info, &
         'XML-file should have root element "flight_infomation"')
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
      case('flight')
         call read_xml_type_flight_info( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            flight, has_flight )
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
   if ( .not. has_flight ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on flight')
   endif
   if ( present(errout) ) errout = error
end subroutine

subroutine write_xml_file_flight_parameters(fname, lurep)
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
      '<flight_infomation>'
   call write_xml_type_flight_info( info, 'flight', indent+3, flight)
   write(info%lun,'(a)') '</flight_infomation>'
   call xml_close(info)
end subroutine

subroutine init_xml_file_flight_parameters

end subroutine

end module
