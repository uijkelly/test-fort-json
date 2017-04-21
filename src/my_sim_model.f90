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
REAL*8, DIMENSION (:), ALLOCATABLE :: TEMP_ARR !placeholder for reading data.
character*7 :: label7 ! the 7 means this is 7 char long
character*6 :: label6 ! the 6 means this is 6 char long
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

  ALLOCATE ( RATES				(MAXBRACK, TOTSIM) )

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
  ! write loop? something?
  label7 = "SBRACK1"
  call read_one_dim_into_two(label7, TEMP_ARR, SBRACK, 1)
  label7 = "SBRACK2"
  call read_one_dim_into_two(label7, TEMP_ARR, SBRACK, 2)
  label7 = "SBRACK3"
  call read_one_dim_into_two(label7, TEMP_ARR, SBRACK, 3)
  label7 = "SBRACK4"
  call read_one_dim_into_two(label7, TEMP_ARR, SBRACK, 4)
  label7 = "SBRACK5"
  call read_one_dim_into_two(label7, TEMP_ARR, SBRACK, 5)
  label7 = "SBRACK6"
  call read_one_dim_into_two(label7, TEMP_ARR, SBRACK, 6)
  label7 = "SBRACK7"
  call read_one_dim_into_two(label7, TEMP_ARR, SBRACK, 7)

  write(*,*) "SBRACK = ", SBRACK


  label6 = "RATES1"
  call read_one_dim_into_two(label6, TEMP_ARR, RATES, 1)
  label6 = "RATES2"
  call read_one_dim_into_two(label6, TEMP_ARR, RATES, 2)
  label6 = "RATES3"
  call read_one_dim_into_two(label6, TEMP_ARR, RATES, 3)
  label6 = "RATES4"
  call read_one_dim_into_two(label6, TEMP_ARR, RATES, 4)
  label6 = "RATES5"
  call read_one_dim_into_two(label6, TEMP_ARR, RATES, 5)
  label6 = "RATES6"
  call read_one_dim_into_two(label6, TEMP_ARR, RATES, 6)
  label6 = "RATES7"
  call read_one_dim_into_two(label6, TEMP_ARR, RATES, 7)
  write(*,*) "RATES = ", RATES

end subroutine set_parameters

! Description: Subroutine to set a part of a two dimensional array with a one dimensional array
!  so for example, will read all of SBRACK1 and put into first col of SBRACK
! Arguments:
!  FIELDNAME - the one dimensional array to read from JSON, eg: SBRACK1, RATES1 etc
!  TEMP_ARR  - a temporary array to read this data into. intend to copy over and over. but
!              this routine will not clear it first, if that's an issue, do before or after this
!              is called.
!  FINAL_ARR - the two dimensional array that we are copying into. eg: SBRACK, RATES etc
!  col       - the column in the two dimensional array that we are copying to.
subroutine read_one_dim_into_two(FIELDNAME, TEMP_ARR, FINAL_ARR, col)
  type(json_file) :: json
  logical :: found
  REAL*8, DIMENSION (:), ALLOCATABLE :: TEMP_ARR !placeholder for reading data.
  REAL*8, DIMENSION (:,:), ALLOCATABLE :: FINAL_ARR
  character(len=*) :: FIELDNAME
  INTEGER :: col
  !write(*,*) "Fieldname = ", FIELDNAME
  !write(*,*) "Trimmed fieldname = ", TRIM(FIELDNAME)
  call json%load_file(filename = 'src/params.json') ! have to have the file open here or else won't find.
                                                    ! inefficient?
  call json%get(TRIM(FIELDNAME), TEMP_ARR, found)
  if (found) then
     FINAL_ARR(col,:) = TEMP_ARR(1:)
  else
    write(*,*) "DID not find FIELDNAME = ", TRIM(FIELDNAME)
  end if
end subroutine





! do some actual work these are bracket rules
subroutine calc_bracket

end subroutine


! run the thing
subroutine my_sim_main
  call calc_bracket
  !call writeit ! in my_write
end subroutine my_sim_main



end module my_sim_model
