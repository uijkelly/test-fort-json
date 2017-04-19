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
integer :: i,j,k ! might not need these, placeholders for reading in data
REAL*8, DIMENSION (:), ALLOCATABLE :: TEMP_ARR

contains



! use json read
! these variables declared in my_declarations
subroutine set_parameters

  write(*,*) "Here we go, setting parameters"

  ! initialize the json class
  call json%initialize()
  ! read the file
  ! calling from the parent directory, so need to go into the sub directory
  ! if the file isn't found, nothing happens - no error, so need to add something
  ! to take care of that.
  call json%load_file(filename = 'src/params.json')
  ! print the file to the console
  !call json%print_file()

  ! start to get the data when we know the name of it and what type it is
  ! here totsim is an integer and saved into the TOTSIM variable
  call json%get('TOTSIM', TOTSIM, found)
  if (.not. found) then
    write(*,*) "Could not find TOTSIM" !TODO, this should exit because it's a real problem
  else
    write(*,*) "Found TOTSIM ", TOTSIM
  end if

  call json%get('MAXBRACK', MAXBRACK, found)
  if (.not. found) then
    write(*,*) "Could not found MAXBRACK" !TODO, this should exit because it's a real problem
  else
    write(*,*) "Found MAXBRACK", MAXBRACK
  end if

  ! Once we have the size from the JSON, then allocate the space.
  ALLOCATE ( SBRACK				(MAXBRACK, TOTSIM) )
  ALLOCATE ( MBRACK				(MAXBRACK, TOTSIM) )
  ALLOCATE ( HBRACK				(MAXBRACK, TOTSIM) )

  write(*,*) "SBRACK before assigning", SBRACK

  !throwing an error
  !known bug https://github.com/jacobwilliams/json-fortran/issues/245
  !so probably will want to just set these file paths in a separate file.
  ! but should try out any new solution, too
  !call json%get('FILE7', FILE7, found)
  !write(*,*)"FILE7 = ", FILE7

  call json%get('NBRACK', NBRACK, found)
  if (.not. found) then
    write(*,*) "Could not find NBRACK"
  else
    write(*,*) "Found NBRACK ", NBRACK
  end if

  write(*,*) "nbrack1 =", NBRACK(1)

  ! SBRACK1 - SBRACK7 ( that 7 is set by MAXBRACK), are saved to SBRACK
  ! Similarly, HBRACK and MBRACK are HBRACK1 - HBRACK7 and MBRACK1 - MBRACK7
  ! These aren't actual parameters that we are going to let vary at this point
  ! in time, but they are the same type.
  ! Though they are stored in the model as reals, not keeping decimals? needs investigating.

! MAKE INTO A FUNCTION
  ! read sbrack1 into temp_arr
  call json%get('SBRACK1', TEMP_ARR, found)
  !call json%get('SBRACK1', SBRACK(1), found) ! would like to do something like this
  if (.not. found) then
    write(*,*) "Could not find SBRACK1"
  else
    write(*,*) "Found SBRACK1", TEMP_ARR
    SBRACK(1,:) = TEMP_ARR(1:) ! copy all of temp_array into the first col of SBRACK
  end if
! END MAKE INTO A FUNCTION

  call json%get('SBRACK2', TEMP_ARR, found)
  if (.not. found) then
    write(*,*) "Could not find SBRACK2"
  else
    write(*,*) "Found SBRACK2", TEMP_ARR
    SBRACK(2,:) = TEMP_ARR(1:) ! copy all of temp_array into the first col of SBRACK
  end if
  call json%get('SBRACK3', TEMP_ARR, found)
  if (.not. found) then
    write(*,*) "Could not find SBRACK3"
  else
    write(*,*) "Found SBRACK3", TEMP_ARR
    SBRACK(3,:) = TEMP_ARR(1:) ! copy all of temp_array into the first col of SBRACK
  end if
  call json%get('SBRACK4', TEMP_ARR, found)
  if (.not. found) then
    write(*,*) "Could not find SBRACK4"
  else
    write(*,*) "Found SBRACK4", TEMP_ARR
    SBRACK(4,:) = TEMP_ARR(1:) ! copy all of temp_array into the first col of SBRACK
  end if
  call json%get('SBRACK5', TEMP_ARR, found)
  if (.not. found) then
    write(*,*) "Could not find SBRACK5"
  else
    write(*,*) "Found SBRACK5", TEMP_ARR
    SBRACK(5,:) = TEMP_ARR(1:) ! copy all of temp_array into the first col of SBRACK
  end if
  call json%get('SBRACK6', TEMP_ARR, found)
  if (.not. found) then
    write(*,*) "Could not find SBRACK6"
  else
    write(*,*) "Found SBRACK6", TEMP_ARR
    SBRACK(6,:) = TEMP_ARR(1:) ! copy all of temp_array into the first col of SBRACK
  end if
  call json%get('SBRACK7', TEMP_ARR, found)
  if (.not. found) then
    write(*,*) "Could not find SBRACK7"
  else
    write(*,*) "Found SBRACK7", TEMP_ARR
    SBRACK(7,:) = TEMP_ARR(1:) ! copy all of temp_array into the first col of SBRACK
  end if

  !write(*,*) "SBRACK = ", SBRACK
end subroutine set_parameters

! do some actual work these are bracket rules
subroutine calc_bracket

end subroutine


! run the thing
subroutine my_sim_main
  call calc_bracket
  !call writeit ! in my_write
end subroutine my_sim_main



end module my_sim_model
