module program_constants

  ! Preamble:
  ! =========
  ! Programming constants used throughout the OO-GRASP program.
  ! Everything here is public.

  implicit none

  public
  
  ! Define compiler-specific KIND numbers for integers,
  ! single and double-precision reals to help ensure consistency of
  ! performance across platforms:
  integer, parameter :: &
       ip = selected_int_kind(9), &
       sp = selected_real_kind(6,37), &
       dp = selected_real_kind(15,307)
  
  ! Define UNIT numbers for Fortran I/O:
  integer, parameter :: &
       ctrl_file_handle = 11, &
       data_file_handle = 12, &
       names_file_handle = 13, &
       out_file_handle = 13
  
  ! Define maximum lengths for various types of character strings:
  integer, parameter :: &
       file_name_length = 256, &
       var_name_length = 8, &
       case_id_length = 8
  
  ! Define the maximum line widths for various types of files:
  integer, parameter :: &
       ctrl_line_width = 80, &
       data_line_width = 1024, &
       names_line_width = 80, &
       out_line_width = 70
  
  ! Common integer values returned by all functions to indicate
  ! success or failure:
  integer(kind=ip), parameter :: &
       RETURN_SUCCESS = 0, &
       RETURN_FAIL = -1
  
  ! Character strings describing this program:
  character(len=*), parameter :: &
       program_name = "OO-GRASP", &
       program_description = &
       "An Object-Oriented version of the General Atomic Structure Package", &
       program_version = "Version 1.0", &
       program_version_and_date = "Version 1.0 - Sep, 2019", &
       program_author = "Written by C. Froese-Fischer and A. Senchuk", &
       program_institution_1 = &
       "Department of Computer Science", &
       program_institution_2 = "University of British Columbia"

end module program_constants
