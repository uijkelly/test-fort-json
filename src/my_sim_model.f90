! A Fortran Module that contains another module and a file of declarations
!
! set parameters
! next step will be doing some work
! and writing some output to a file.
!
! TODO: try initializing label to 128 and all blank. then try trimming or a substring or something
!
! Jessica A Kelly

module my_sim_model
use my_declarations
use json_module
use json_file_module
use json_value_module !for parse
use, intrinsic :: iso_fortran_env , only: error_unit, output_unit
implicit none


type(json_file) :: json
logical :: found
integer :: i,j,k ! might not need these, placeholders for reading in data
REAL*8, DIMENSION (:), ALLOCATABLE :: TEMP_ARR !placeholder for reading data.
character*7 :: label7 ! the 7 means this is 7 char long
character*6 :: label6 ! the 6 means this is 6 char long
contains



! Description: use json read for file src/params.json
! these variables declared in my_declarations
!
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

! Description: Use json file form_params_system.json
!  System-level parameters of interest for us (in this test) are TOTSIM
!  and MAXBRACK. Both defined inside this json file (but not the only thing)
! What will make things easier is to know the number of elements that are
! being passed. Should be able to update the JSON stream with that, and also
! separate out by data type if we need to. But don't have to if passing that element.
!
! Third prototype includes count now, and the data table is called "data" instead
! of being blank.
subroutine set_parameters_system
  !type(json_file) :: json
  !type(json_value),pointer :: p
  ! here are the elements that we expect to find
  ! ID, FORMAT, CONTROL, EDIT_INTERNALLY, EDIT_EXTERNALLY, HTMLTYPE, HELP_ID,
  ! LABEL, DESCRIPTION, GROUPNAME, ROWNUM, PARAM, VAL
  integer:: count !used locally to get the count.
  integer:: i !for loop
  character(len=:), allocatable :: element ! for generating the data string
  character(len=5) :: charI
  integer, target:: ID, CONTROL, EDIT_INTERNALLY, EDIT_EXTERNALLY, HTMLTYPE, HELP_ID, ROWNUM
  character(len=:), allocatable :: FORMAT, LABEL, DESCRIPTION, GROUPNAME, PARAM, VAL, FORTRAN_TYPE
  logical :: found
  type(json_core) :: json
  type(json_value),pointer :: p,p1,p2
  write(*,*) "Here we go, setting parameters from form_params_system"
  ! Here, we will need to read in the first full element from { to } and pick
  ! it apart. We do know the structure of it before-hand, and will assume all
  ! values will be TEXT and that we will have to convert some of them to integers
  ! should the Fortran require integers (and it will for TOTSIM and MAXBRACK).
  ! This fortran type should be added to the database and sent as another key-value pair
  ! For now, just going to make some assumptions.

  call json%parse(file='src/form_params_system.json', p=p) ! put it all into p
  !call json%print(p,output_unit)
  ! get the number of things in data
  call json%get(p,"count", count)
  write(*,*) "the count of elements is = ", count
  ! to get the whole thing that is just the first object
  !TODO: ADD A LOOP so we do "data(i)"
  do i=1,count
    write(charI,"(I1)") i ! here the length is going to need to be adjusted. fine.
    element = "data("//trim(charI)//")"
    call json%get(p,element,p1) ! put it all into p1
    call json%get(p1,"PARAM",PARAM)
    call json%get(p1,"VAL", VAL)
    call json%get(p1,"CONTROL",CONTROL)
    write(*,*) "from first object only PARAM = ",PARAM
    write(*,*) "from first object only VAL = ", VAL
  enddo

end subroutine

! Description: Array rules
!  Let's see what kind of fun craziness we get into here. Hopefully any issues can be
!  Addressed when we get to the query and create the JSON piece
!  Thinking specifically to make sure we get the baseline in one set and the proposal in
!  another set? it might not matter though.
! But it probably would be easier. And now that I know how to do that (sort of, anyway)
! I will probably want to explore that further. But for now, just be simple.
subroutine set_parameters_yearly
  integer, target:: ID, CONTROL, EDIT_INTERNALLY, EDIT_EXTERNALLY, HTMLTYPE, HELP_ID, ROWNUM
  character(len=:), allocatable :: FORMAT, LABEL, DESCRIPTION, GROUPNAME, PARAM, VAL
  ! though but maybe this won't always be integer? so it could be weird. and it's been changed from CL2012 to B2012
  integer:: CL2011, P2011, CL2012, P2012 ! and would be longer as it stands, and the number of things will vary
  ! what do they put data into?

  logical :: found
  type(json_core) :: json
  type(json_value),pointer :: p,p1,p2
  write(*,*) "Here we go, setting parameters from form_params_years"

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
