! A Fortran Module that contains another module and a file of declarations
!
! set parameters
! next step will be doing some work
! and writing some output to a file.
!
! Jessica A Kelly

module my_sim_model
use my_declarations
use json_module
implicit none
contains

! use json read
! these variables declared in my_declarations
subroutine set_parameters
  write(*,*) "doing nothing, just in set parameters"
end subroutine set_parameters

! do some actual work these are bracket rules
subroutine calc_bracket

end subroutine


! run the thing
subroutine my_sim_main
  call calc_bracket
  call writeit ! in my_write
end subroutine my_sim_main



end module my_sim_model
