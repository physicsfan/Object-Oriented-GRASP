!#####################################################################
module dynamic_allocation
   ! Routines for allocating and deallocating pointers to arrays
   use error_handler
   use program_constants
   implicit none
   private ! by default
   public :: dyn_alloc, dyn_dealloc
   ! For allocating pointers to arrays
   interface dyn_alloc
      module procedure int1_alloc
      module procedure int2_alloc
      module procedure int3_alloc
      module procedure int4_alloc
      module procedure int5_alloc
      module procedure int6_alloc
      module procedure dbl1_alloc
      module procedure dbl2_alloc
      module procedure dbl3_alloc
      module procedure dbl4_alloc
      module procedure dbl5_alloc
      module procedure dbl6_alloc
      module procedure str1_alloc
      module procedure str2_alloc
      module procedure str3_alloc
      module procedure str4_alloc
      module procedure str5_alloc
      module procedure str6_alloc
   end interface
   ! For deallocating pointers to arrays
   interface dyn_dealloc
      module procedure int1_dealloc
      module procedure int2_dealloc
      module procedure int3_dealloc
      module procedure int4_dealloc
      module procedure int5_dealloc
      module procedure int6_dealloc
      module procedure dbl1_dealloc
      module procedure dbl2_dealloc
      module procedure dbl3_dealloc
      module procedure dbl4_dealloc
      module procedure dbl5_dealloc
      module procedure dbl6_dealloc
      module procedure str1_dealloc
      module procedure str2_dealloc
      module procedure str3_dealloc
      module procedure str4_dealloc
      module procedure str5_dealloc
      module procedure str6_dealloc
   end interface
   ! parameters private to this module
   character(len=*), parameter :: modname = "dynalloc"
   !##################################################################
contains
   !##################################################################
   integer(our_int) function int1_alloc(intArray, dim1, err, &
        lbound1) result(answer)
      ! Allocates an integer array of rank 1
      implicit none
      ! declare required arguments
      integer(kind=our_int), pointer :: intArray(:)
      integer, intent(in) :: dim1
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1
      ! declare local variables and parameters
      integer :: lb1, status
      character(len=*), parameter :: subname = "int1_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(intArray) ) deallocate(intArray, stat=status)
      if( status /= 0) goto 800
      lb1 = 1
      if( present(lbound1) ) lb1 = lbound1
      allocate( intArray(lb1:dim1), stat=status )
      if( status /= 0) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function int1_alloc
   !##################################################################
   integer(our_int) function int2_alloc(intArray, dim1, dim2, err, &
        lbound1, lbound2) result(answer)
      ! Allocates an integer array of rank 2
      implicit none
      ! declare required arguments
      integer(kind=our_int), pointer :: intArray(:,:)
      integer, intent(in) :: dim1, dim2
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1, lbound2
      ! declare local variables and parameters
      integer :: status, lb1, lb2
      character(len=*), parameter :: subname = "int2_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(intArray) ) deallocate(intArray, stat=status)
      if( status /= 0 ) goto 800
      lb1 = 1
      lb2 = 1
      if( present(lbound1) ) lb1 = lbound1
      if( present(lbound2) ) lb2 = lbound2
      if(status == 0) allocate( intArray(lb1:dim1, lb2:dim2), &
           stat=status ) 
      if(status /= 0 ) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function int2_alloc
   !##################################################################
   integer(our_int) function int3_alloc(intArray, dim1, dim2, dim3, &
        err, lbound1, lbound2, lbound3) result(answer)
      ! Allocates an integer array of rank 3
      implicit none
      ! declare required arguments
      integer(kind=our_int), pointer :: intArray(:,:,:)
      integer, intent(in) :: dim1, dim2, dim3
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1, lbound2, lbound3
      ! declare local variables and parameters
      integer :: status, lb1, lb2, lb3
      character(len=*), parameter :: subname = "int3_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(intArray) ) deallocate(intArray, stat=status)
      if( status /= 0 ) goto 800
      lb1 = 1
      lb2 = 1
      lb3 = 1
      if( present(lbound1) ) lb1 = lbound1
      if( present(lbound2) ) lb2 = lbound2
      if( present(lbound3) ) lb3 = lbound3
      if(status == 0) allocate( intArray(lb1:dim1, lb2:dim2, &
           lb3:dim3), stat=status ) 
      if(status /= 0 ) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function int3_alloc
   !##################################################################
   integer(our_int) function int4_alloc(intArray, dim1, dim2, dim3, &
        dim4, err, lbound1, lbound2, lbound3, lbound4) result(answer)
      ! Allocates an integer array of rank 3
      implicit none
      ! declare required arguments
      integer(kind=our_int), pointer :: intArray(:,:,:,:)
      integer, intent(in) :: dim1, dim2, dim3, dim4
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1, lbound2, lbound3, &
         lbound4
      ! declare local variables and parameters
      integer :: status, lb1, lb2, lb3, lb4
      character(len=*), parameter :: subname = "int4_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(intArray) ) deallocate(intArray, stat=status)
      if( status /= 0 ) goto 800
      lb1 = 1
      lb2 = 1
      lb3 = 1
      lb4 = 1
      if( present(lbound1) ) lb1 = lbound1
      if( present(lbound2) ) lb2 = lbound2
      if( present(lbound3) ) lb3 = lbound3
      if( present(lbound4) ) lb4 = lbound4
      if(status == 0) allocate( intArray(lb1:dim1, lb2:dim2, &
           lb3:dim3, lb4:dim4), stat=status ) 
      if(status /= 0 ) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function int4_alloc
   !##################################################################
   integer(our_int) function int5_alloc(intArray, dim1, dim2, dim3, &
        dim4, dim5, err, lbound1, lbound2, lbound3, lbound4, &
        lbound5) result(answer)
      ! Allocates an integer array of rank 3
      implicit none
      ! declare required arguments
      integer(kind=our_int), pointer :: intArray(:,:,:,:,:)
      integer, intent(in) :: dim1, dim2, dim3, dim4, dim5
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1, lbound2, lbound3, &
         lbound4, lbound5
      ! declare local variables and parameters
      integer :: status, lb1, lb2, lb3, lb4, lb5
      character(len=*), parameter :: subname = "int5_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(intArray) ) deallocate(intArray, stat=status)
      if( status /= 0 ) goto 800
      lb1 = 1
      lb2 = 1
      lb3 = 1
      lb4 = 1
      lb5 = 1
      if( present(lbound1) ) lb1 = lbound1
      if( present(lbound2) ) lb2 = lbound2
      if( present(lbound3) ) lb3 = lbound3
      if( present(lbound4) ) lb4 = lbound4
      if( present(lbound5) ) lb5 = lbound5
      if(status == 0) allocate( intArray(lb1:dim1, lb2:dim2, &
           lb3:dim3, lb4:dim4, lb5:dim5), stat=status ) 
      if(status /= 0 ) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function int5_alloc
   !##################################################################
   integer(our_int) function int6_alloc(intArray, dim1, dim2, dim3, &
        dim4, dim5, dim6, err, lbound1, lbound2, lbound3, lbound4, &
        lbound5, lbound6) result(answer)
      ! Allocates an integer array of rank 3
      implicit none
      ! declare required arguments
      integer(kind=our_int), pointer :: intArray(:,:,:,:,:,:)
      integer, intent(in) :: dim1, dim2, dim3, dim4, dim5, dim6
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1, lbound2, lbound3, &
         lbound4, lbound5, lbound6
      ! declare local variables and parameters
      integer :: status, lb1, lb2, lb3, lb4, lb5, lb6
      character(len=*), parameter :: subname = "int6_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(intArray) ) deallocate(intArray, stat=status)
      if( status /= 0 ) goto 800
      lb1 = 1
      lb2 = 1
      lb3 = 1
      lb4 = 1
      lb5 = 1
      lb6 = 1
      if( present(lbound1) ) lb1 = lbound1
      if( present(lbound2) ) lb2 = lbound2
      if( present(lbound3) ) lb3 = lbound3
      if( present(lbound4) ) lb4 = lbound4
      if( present(lbound5) ) lb5 = lbound5
      if( present(lbound6) ) lb6 = lbound6
      if(status == 0) allocate( intArray(lb1:dim1, lb2:dim2, &
           lb3:dim3, lb4:dim4, lb5:dim5, lb6:dim6), stat=status ) 
      if(status /= 0 ) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function int6_alloc
   !##################################################################
   integer(our_int) function dbl1_alloc(dblArray, dim1, err, &
        lbound1) result(answer)
      ! Allocates a double-precision real array of rank 1
      implicit none
      ! declare required arguments
      real(kind=our_dble), pointer :: dblArray(:)
      integer, intent(in) :: dim1
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1
      ! declare local variables and parameters
      integer :: lb1, status
      character(len=*), parameter :: subname = "dbl1_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(dblArray) ) deallocate(dblArray, stat=status)
      if( status /= 0) goto 800
      lb1 = 1
      if( present(lbound1) ) lb1 = lbound1
      allocate( dblArray(lb1:dim1), stat=status )
      if( status /= 0) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function dbl1_alloc
   !##################################################################
   integer(our_int) function dbl2_alloc(dblArray, dim1, dim2, err, &
        lbound1, lbound2) result(answer)
      ! Allocates a double-precision real array of rank 2
      implicit none
      ! declare required arguments
      real(kind=our_dble), pointer :: dblArray(:,:)
      integer, intent(in) :: dim1, dim2
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1, lbound2
      ! declare local variables and parameters
      integer :: status, lb1, lb2
      character(len=*), parameter :: subname = "dbl2_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(dblArray) ) deallocate(dblArray, stat=status)
      if( status /= 0 ) goto 800
      lb1 = 1
      lb2 = 1
      if( present(lbound1) ) lb1 = lbound1
      if( present(lbound2) ) lb2 = lbound2
      if(status == 0) allocate( dblArray(lb1:dim1, lb2:dim2), &
           stat=status ) 
      if(status /= 0 ) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function dbl2_alloc
   !##################################################################
   integer(our_int) function dbl3_alloc(dblArray, dim1, dim2, dim3, &
        err, lbound1, lbound2, lbound3) result(answer)
      ! Allocates a double-precision real array of rank 3
      implicit none
      ! declare required arguments
      real(kind=our_dble), pointer :: dblArray(:,:,:)
      integer, intent(in) :: dim1, dim2, dim3
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1, lbound2, lbound3
      ! declare local variables and parameters
      integer :: status, lb1, lb2, lb3
      character(len=*), parameter :: subname = "dbl3_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(dblArray) ) deallocate(dblArray, stat=status)
      if( status /= 0 ) goto 800
      lb1 = 1
      lb2 = 1
      lb3 = 1
      if( present(lbound1) ) lb1 = lbound1
      if( present(lbound2) ) lb2 = lbound2
      if( present(lbound3) ) lb3 = lbound3
      if(status == 0) allocate( dblArray(lb1:dim1, lb2:dim2, &
           lb3:dim3), stat=status ) 
      if(status /= 0 ) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function dbl3_alloc
   !##################################################################
   integer(our_int) function dbl4_alloc(dblArray, dim1, dim2, dim3, &
        dim4, err, lbound1, lbound2, lbound3, lbound4) result(answer)
      ! Allocates a double-precision real array of rank 4
      implicit none
      ! declare required arguments
      real(kind=our_dble), pointer :: dblArray(:,:,:,:)
      integer, intent(in) :: dim1, dim2, dim3, dim4
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1, lbound2, lbound3, &
         lbound4
      ! declare local variables and parameters
      integer :: status, lb1, lb2, lb3, lb4
      character(len=*), parameter :: subname = "dbl4_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(dblArray) ) deallocate(dblArray, stat=status)
      if( status /= 0 ) goto 800
      lb1 = 1
      lb2 = 1
      lb3 = 1
      lb4 = 1
      if( present(lbound1) ) lb1 = lbound1
      if( present(lbound2) ) lb2 = lbound2
      if( present(lbound3) ) lb3 = lbound3
      if( present(lbound4) ) lb4 = lbound4
      if(status == 0) allocate( dblArray(lb1:dim1, lb2:dim2, &
           lb3:dim3, lb4:dim4), stat=status ) 
      if(status /= 0 ) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function dbl4_alloc
   !##################################################################
   integer(our_int) function dbl5_alloc(dblArray, dim1, dim2, dim3, &
        dim4, dim5, err, lbound1, lbound2, lbound3, lbound4, &
        lbound5) result(answer)
      ! Allocates a double-precision real array of rank 5
      implicit none
      ! declare required arguments
      real(kind=our_dble), pointer :: dblArray(:,:,:,:,:)
      integer, intent(in) :: dim1, dim2, dim3, dim4, dim5
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1, lbound2, lbound3, &
         lbound4, lbound5
      ! declare local variables and parameters
      integer :: status, lb1, lb2, lb3, lb4, lb5
      character(len=*), parameter :: subname = "dbl5_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(dblArray) ) deallocate(dblArray, stat=status)
      if( status /= 0 ) goto 800
      lb1 = 1
      lb2 = 1
      lb3 = 1
      lb4 = 1
      lb5 = 1
      if( present(lbound1) ) lb1 = lbound1
      if( present(lbound2) ) lb2 = lbound2
      if( present(lbound3) ) lb3 = lbound3
      if( present(lbound4) ) lb4 = lbound4
      if( present(lbound5) ) lb5 = lbound5
      if(status == 0) allocate( dblArray(lb1:dim1, lb2:dim2, &
           lb3:dim3, lb4:dim4, lb5:dim5), stat=status ) 
      if(status /= 0 ) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function dbl5_alloc
   !##################################################################
   integer(our_int) function dbl6_alloc(dblArray, dim1, dim2, dim3, &
        dim4, dim5, dim6, err, lbound1, lbound2, lbound3, lbound4, &
        lbound5, lbound6) result(answer)
      ! Allocates a double-precision real array of rank 5
      implicit none
      ! declare required arguments
      real(kind=our_dble), pointer :: dblArray(:,:,:,:,:,:)
      integer, intent(in) :: dim1, dim2, dim3, dim4, dim5, dim6
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1, lbound2, lbound3, &
         lbound4, lbound5, lbound6
      ! declare local variables and parameters
      integer :: status, lb1, lb2, lb3, lb4, lb5, lb6
      character(len=*), parameter :: subname = "dbl6_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(dblArray) ) deallocate(dblArray, stat=status)
      if( status /= 0 ) goto 800
      lb1 = 1
      lb2 = 1
      lb3 = 1
      lb4 = 1
      lb5 = 1
      lb6 = 1
      if( present(lbound1) ) lb1 = lbound1
      if( present(lbound2) ) lb2 = lbound2
      if( present(lbound3) ) lb3 = lbound3
      if( present(lbound4) ) lb4 = lbound4
      if( present(lbound5) ) lb5 = lbound5
      if( present(lbound6) ) lb6 = lbound6
      if(status == 0) allocate( dblArray(lb1:dim1, lb2:dim2, &
           lb3:dim3, lb4:dim4, lb5:dim5, lb6:dim6), stat=status ) 
      if(status /= 0 ) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function dbl6_alloc
   !##################################################################
   integer(our_int) function str1_alloc(strArray, dim1, err, &
        lbound1) result(answer)
      ! Allocates a character-string array of rank 1
      implicit none
      ! declare required arguments
      character(len=*), pointer :: strArray(:)
      integer, intent(in) :: dim1
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1
      ! declare local variables and parameters
      integer :: lb1, status
      character(len=*), parameter :: subname = "str1_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(strArray) ) deallocate(strArray, stat=status)
      if( status /= 0) goto 800
      lb1 = 1
      if( present(lbound1) ) lb1 = lbound1
      allocate( strArray(lb1:dim1), stat=status )
      if( status /= 0) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function str1_alloc
   !##################################################################
   integer(our_int) function str2_alloc(strArray, dim1, dim2, err, &
        lbound1, lbound2) result(answer)
      ! Allocates a character-string array of rank 2
      implicit none
      ! declare required arguments
      character(len=*), pointer :: strArray(:,:)
      integer, intent(in) :: dim1, dim2
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1, lbound2
      ! declare local variables and parameters
      integer :: status, lb1, lb2
      character(len=*), parameter :: subname = "str2_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(strArray) ) deallocate(strArray, stat=status)
      if( status /= 0 ) goto 800
      lb1 = 1
      lb2 = 1
      if( present(lbound1) ) lb1 = lbound1
      if( present(lbound2) ) lb2 = lbound2
      if(status == 0) allocate( strArray(lb1:dim1, lb2:dim2), &
           stat=status ) 
      if(status /= 0 ) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function str2_alloc
   !##################################################################
   integer(our_int) function str3_alloc(strArray, dim1, dim2, dim3, &
        err, lbound1, lbound2, lbound3) result(answer)
      ! Allocates a character-string array of rank 3
      implicit none
      ! declare required arguments
      character(len=*), pointer :: strArray(:,:,:)
      integer, intent(in) :: dim1, dim2, dim3
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1, lbound2, lbound3
      ! declare local variables and parameters
      integer :: status, lb1, lb2, lb3
      character(len=*), parameter :: subname = "str3_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(strArray) ) deallocate(strArray, stat=status)
      if( status /= 0 ) goto 800
      lb1 = 1
      lb2 = 1
      lb3 = 1
      if( present(lbound1) ) lb1 = lbound1
      if( present(lbound2) ) lb2 = lbound2
      if( present(lbound3) ) lb3 = lbound3
      if(status == 0) allocate( strArray(lb1:dim1, lb2:dim2, &
           lb3:dim3), stat=status ) 
      if(status /= 0 ) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function str3_alloc
   !##################################################################
   integer(our_int) function str4_alloc(strArray, dim1, dim2, dim3, &
        dim4, err, lbound1, lbound2, lbound3, lbound4) result(answer)
      ! Allocates a character-string array of rank 4
      implicit none
      ! declare required arguments
      character(len=*), pointer :: strArray(:,:,:,:)
      integer, intent(in) :: dim1, dim2, dim3, dim4
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1, lbound2, lbound3, &
         lbound4
      ! declare local variables and parameters
      integer :: status, lb1, lb2, lb3, lb4
      character(len=*), parameter :: subname = "str4_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(strArray) ) deallocate(strArray, stat=status)
      if( status /= 0 ) goto 800
      lb1 = 1
      lb2 = 1
      lb3 = 1
      lb4 = 1
      if( present(lbound1) ) lb1 = lbound1
      if( present(lbound2) ) lb2 = lbound2
      if( present(lbound3) ) lb3 = lbound3
      if( present(lbound4) ) lb4 = lbound4
      if(status == 0) allocate( strArray(lb1:dim1, lb2:dim2, &
           lb3:dim3, lb4:dim4), stat=status ) 
      if(status /= 0 ) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function str4_alloc
   !##################################################################
   integer(our_int) function str5_alloc(strArray, dim1, dim2, dim3, &
        dim4, dim5, err, lbound1, lbound2, lbound3, lbound4, lbound5 &
        ) result(answer)
      ! Allocates a character-string array of rank 5
      implicit none
      ! declare required arguments
      character(len=*), pointer :: strArray(:,:,:,:,:)
      integer, intent(in) :: dim1, dim2, dim3, dim4, dim5
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1, lbound2, lbound3, &
         lbound4, lbound5
      ! declare local variables and parameters
      integer :: status, lb1, lb2, lb3, lb4, lb5
      character(len=*), parameter :: subname = "str5_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(strArray) ) deallocate(strArray, stat=status)
      if( status /= 0 ) goto 800
      lb1 = 1
      lb2 = 1
      lb3 = 1
      lb4 = 1
      lb5 = 1
      if( present(lbound1) ) lb1 = lbound1
      if( present(lbound2) ) lb2 = lbound2
      if( present(lbound3) ) lb3 = lbound3
      if( present(lbound4) ) lb4 = lbound4
      if( present(lbound5) ) lb5 = lbound5
      if(status == 0) allocate( strArray(lb1:dim1, lb2:dim2, &
           lb3:dim3, lb4:dim4, lb5:dim5), stat=status ) 
      if(status /= 0 ) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function str5_alloc
   !##################################################################
   integer(our_int) function str6_alloc(strArray, dim1, dim2, dim3, &
        dim4, dim5, dim6, err, lbound1, lbound2, lbound3, lbound4, &
        lbound5, lbound6) result(answer)
      ! Allocates a character-string array of rank 6
      implicit none
      ! declare required arguments
      character(len=*), pointer :: strArray(:,:,:,:,:,:)
      integer, intent(in) :: dim1, dim2, dim3, dim4, dim5, dim6
      type(error_type), intent(inout) :: err
      ! declare optional arguments
      integer, intent(in), optional :: lbound1, lbound2, lbound3, &
         lbound4, lbound5, lbound6
      ! declare local variables and parameters
      integer :: status, lb1, lb2, lb3, lb4, lb5, lb6
      character(len=*), parameter :: subname = "str6_alloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(strArray) ) deallocate(strArray, stat=status)
      if( status /= 0 ) goto 800
      lb1 = 1
      lb2 = 1
      lb3 = 1
      lb4 = 1
      lb5 = 1
      lb6 = 1
      if( present(lbound1) ) lb1 = lbound1
      if( present(lbound2) ) lb2 = lbound2
      if( present(lbound3) ) lb3 = lbound3
      if( present(lbound4) ) lb4 = lbound4
      if( present(lbound5) ) lb5 = lbound5
      if( present(lbound6) ) lb6 = lbound6
      if(status == 0) allocate( strArray(lb1:dim1, lb2:dim2, &
           lb3:dim3, lb4:dim4, lb5:dim5, lb6:dim6), stat=status ) 
      if(status /= 0 ) goto 810
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
810   call err_handle(err, 200, &
              called_from = subname//" in MOD "//modname)
      return
   end function str6_alloc
   !##################################################################
   integer(kind=our_int) function int1_dealloc(intArray, err) &
        result(answer)
      ! Deallocates an integer array of rank 1
      implicit none
      ! declare arguments
      integer(kind=our_int), pointer :: intArray(:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "int1_dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(intArray) ) deallocate(intArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function int1_dealloc
   !##################################################################
   integer(kind=our_int) function int2_dealloc(intArray, err) &
        result(answer)
      ! Deallocates an integer array of rank 2
      implicit none
      ! declare arguments
      integer(kind=our_int), pointer :: intArray(:,:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "int2_dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(intArray) ) deallocate(intArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function int2_dealloc
   !##################################################################
   integer(kind=our_int) function int3_dealloc(intArray, err) &
        result(answer)
      ! Deallocates an integer array of rank 3
      implicit none
      ! declare arguments
      integer(kind=our_int), pointer :: intArray(:,:,:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "int3_dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(intArray) ) deallocate(intArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function int3_dealloc
   !##################################################################
   integer(kind=our_int) function int4_dealloc(intArray, err) &
        result(answer)
      ! Deallocates an integer array of rank 4
      implicit none
      ! declare arguments
      integer(kind=our_int), pointer :: intArray(:,:,:,:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "int4_dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(intArray) ) deallocate(intArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function int4_dealloc
   !##################################################################
   integer(kind=our_int) function int5_dealloc(intArray, err) &
        result(answer)
      ! Deallocates an integer array of rank 5
      implicit none
      ! declare arguments
      integer(kind=our_int), pointer :: intArray(:,:,:,:,:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "int5_dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(intArray) ) deallocate(intArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function int5_dealloc
   !##################################################################
   integer(kind=our_int) function int6_dealloc(intArray, err) &
        result(answer)
      ! Deallocates an integer array of rank 6
      implicit none
      ! declare arguments
      integer(kind=our_int), pointer :: intArray(:,:,:,:,:,:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "int6_dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(intArray) ) deallocate(intArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function int6_dealloc
   !##################################################################
   integer(kind=our_int) function dbl1_dealloc(dblArray, err) &
        result(answer)
      ! Deallocates a double-precision real array of rank 1
      implicit none
      ! declare arguments
      real(kind=our_dble), pointer :: dblArray(:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "dbl1_dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(dblArray) ) deallocate(dblArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function dbl1_dealloc
   !##################################################################
   integer(kind=our_int) function dbl2_dealloc(dblArray, err) &
        result(answer)
      ! Deallocates a double-precision real array of rank 2
      implicit none
      ! declare arguments
      real(kind=our_dble), pointer :: dblArray(:,:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "dbl2_dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(dblArray) ) deallocate(dblArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function dbl2_dealloc
   !##################################################################
   integer(kind=our_int) function dbl3_dealloc(dblArray, err) &
        result(answer)
      ! Deallocates a double-precision real array of rank 3
      implicit none
      ! declare arguments
      real(kind=our_dble), pointer :: dblArray(:,:,:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "dbl3dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(dblArray) ) deallocate(dblArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function dbl3_dealloc
   !##################################################################
   integer(kind=our_int) function dbl4_dealloc(dblArray, err) &
        result(answer)
      ! Deallocates a double-precision real array of rank 4
      implicit none
      ! declare arguments
      real(kind=our_dble), pointer :: dblArray(:,:,:,:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "dbl4dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(dblArray) ) deallocate(dblArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function dbl4_dealloc
   !##################################################################
   integer(kind=our_int) function dbl5_dealloc(dblArray, err) &
        result(answer)
      ! Deallocates a double-precision real array of rank 5
      implicit none
      ! declare arguments
      real(kind=our_dble), pointer :: dblArray(:,:,:,:,:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "dbl5dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(dblArray) ) deallocate(dblArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function dbl5_dealloc
   !##################################################################
   integer(kind=our_int) function dbl6_dealloc(dblArray, err) &
        result(answer)
      ! Deallocates a double-precision real array of rank 5
      implicit none
      ! declare arguments
      real(kind=our_dble), pointer :: dblArray(:,:,:,:,:,:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "dbl6dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(dblArray) ) deallocate(dblArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function dbl6_dealloc
   !##################################################################
   integer(kind=our_int) function str1_dealloc(strArray, err) &
        result(answer)
      ! Deallocates a character-string array of rank 1
      implicit none
      ! declare arguments
      character(len=*), pointer :: strArray(:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "str1_dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(strArray) ) deallocate(strArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function str1_dealloc
   !##################################################################
   integer(kind=our_int) function str2_dealloc(strArray, err) &
        result(answer)
      ! Deallocates a character-string array of rank 2
      implicit none
      ! declare arguments
      character(len=*), pointer :: strArray(:,:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "str2_dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(strArray) ) deallocate(strArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function str2_dealloc
   !##################################################################
   integer(kind=our_int) function str3_dealloc(strArray, err) &
        result(answer)
      ! Deallocates a character-string array of rank 3
      implicit none
      ! declare arguments
      character(len=*), pointer :: strArray(:,:,:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "str3_dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(strArray) ) deallocate(strArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function str3_dealloc
   !##################################################################
   integer(kind=our_int) function str4_dealloc(strArray, err) &
        result(answer)
      ! Deallocates a character-string array of rank 4
      implicit none
      ! declare arguments
      character(len=*), pointer :: strArray(:,:,:,:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "str4_dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(strArray) ) deallocate(strArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function str4_dealloc
   !##################################################################
   integer(kind=our_int) function str5_dealloc(strArray, err) &
        result(answer)
      ! Deallocates a character-string array of rank 5
      implicit none
      ! declare arguments
      character(len=*), pointer :: strArray(:,:,:,:,:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "str5_dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(strArray) ) deallocate(strArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function str5_dealloc
   !##################################################################
   integer(kind=our_int) function str6_dealloc(strArray, err) &
        result(answer)
      ! Deallocates a character-string array of rank 6
      implicit none
      ! declare arguments
      character(len=*), pointer :: strArray(:,:,:,:,:,:)
      type(error_type), intent(inout) :: err
      ! declare local variables and parameters
      integer :: status
      character(len=*), parameter :: subname = "str6_dealloc"
      ! begin
      answer = RETURN_FAIL
	  status = 0
      if( associated(strArray) ) deallocate(strArray, stat=status)
      if( status /= 0 ) goto 800
      ! normal exit
      answer = RETURN_SUCCESS
      return
      ! error traps
800   call err_handle(err, 201, &
              called_from = subname//" in MOD "//modname)
      return
   end function str6_dealloc
   !##################################################################
end module dynamic_allocation
!#####################################################################
