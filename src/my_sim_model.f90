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


type(json_file) :: json
logical :: found
integer :: i,j,k

contains



! use json read
! these variables declared in my_declarations
subroutine set_parameters
  write(*,*) "here we go, setting parameters"

  ! initialize the json class
  call json%initialize()
  ! read the file
  ! calling from the parent directory, so need to go into the sub directory
  ! if the file isn't found, nothing happens - no error, so need to add something
  ! to take care of that.
  call json%load_file(filename = 'src/params.json')
  ! print the file to the console
  call json%print_file()
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
