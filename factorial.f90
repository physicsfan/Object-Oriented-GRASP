module show_me_the_facs

  private

  public:: factorial

  interface factorial
     module procedure factorial_int
     module procedure factorial_real
  end interface factorial


contains
  
  recursive integer function factorial_int(x) result(answer)
    implicit none
    integer, intent(in) :: x
    
    if(x == 0) then
       answer = 1
    else
       answer = x * factorial_int(x-1)
    end if
  end function factorial_int

  recursive real function factorial_real(x) result(answer)
    implicit none
    real, intent(in) :: x
    real :: pi = 4.0*atan(1.0)
    
    if(x == 0.5) then
       answer = sqrt(pi)
    else
       answer = x * factorial_real(x-1)
    end if
  end function factorial_real
  
end module show_me_the_facs

program just_the_facs
  use show_me_the_facs
  ! program to test the recursive factorial function 
  implicit none
  
  integer :: n
  real :: p
  
  print '(A)', "Please enter an integer:"
  read(*,*) n
  
  if(n > 9) then
     print '(A)', "n is TOO BIG!"      ! test overflow of integers
  else  
     print '(/,i2,A,i6)', n, "! is: ", factorial(n)
  endif

  print '(A)', "Please enter a multiple of 1/2 (n/2):"
  read(*,*) p
  
  if(n > 9) then
     print '(A)', "n is TOO BIG!"      ! test overflow of integers
  else  
     print '(/,i2,A,i6)', n, "! is: ", factorial(n)
  endif
  
end program just_the_facs
