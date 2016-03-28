module xml_data_compution_control
   use READ_XML_PRIMITIVES
   use WRITE_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_
   integer                                         :: compution_type
   character(len=80)                                :: solver_file_name
   character(len=80)                                :: helicopter_file_name
   character(len=80)                                :: flight_file_name
   character(len=80)                                :: output_folder
contains
subroutine read_xml_file_compution_control(fname, lurep, errout)
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
   logical                                         :: has_compution_type
   logical                                         :: has_solver_file_name
   logical                                         :: has_helicopter_file_name
   logical                                         :: has_flight_file_name
   logical                                         :: has_output_folder
   has_compution_type                   = .false.
   has_solver_file_name                 = .false.
   has_helicopter_file_name             = .false.
   has_flight_file_name                 = .false.
   has_output_folder                    = .false.

   call init_xml_file_compution_control
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
   if ( starttag .ne. "compution_control" ) then
      call xml_report_errors( info, &
         'XML-file should have root element "compution_control"')
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
      case('compution_type')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            compution_type, has_compution_type )
      case('solver_file_name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            solver_file_name, has_solver_file_name )
      case('helicopter_file_name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            helicopter_file_name, has_helicopter_file_name )
      case('flight_file_name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            flight_file_name, has_flight_file_name )
      case('output_folder')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            output_folder, has_output_folder )
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
   if ( .not. has_compution_type ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on compution_type')
   endif
   if ( .not. has_solver_file_name ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on solver_file_name')
   endif
   if ( .not. has_helicopter_file_name ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on helicopter_file_name')
   endif
   if ( .not. has_flight_file_name ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on flight_file_name')
   endif
   if ( .not. has_output_folder ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on output_folder')
   endif
   if ( present(errout) ) errout = error
end subroutine

subroutine write_xml_file_compution_control(fname, lurep)
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
      '<compution_control>'
   call write_to_xml_integer( info, 'compution_type', indent+3, compution_type)
   call write_to_xml_word( info, 'solver_file_name', indent+3, solver_file_name)
   call write_to_xml_word( info, 'helicopter_file_name', indent+3, helicopter_file_name)
   call write_to_xml_word( info, 'flight_file_name', indent+3, flight_file_name)
   call write_to_xml_word( info, 'output_folder', indent+3, output_folder)
   write(info%lun,'(a)') '</compution_control>'
   call xml_close(info)
end subroutine

subroutine init_xml_file_compution_control

end subroutine

end module
