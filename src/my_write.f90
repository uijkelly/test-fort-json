! writes to file defined in my_delcarations

subroutine writeit
  use my_declarations
  implicit none


    OPEN (unit=outputfile, FILE=file7,status='replace',action='write')
    write(7,*) "Inside Fortran, writing to the file"
    write(7,*) "totsim, nbrack and sbrack"
    write(7,*) nbrack
    write(7,*) sbrack
end subroutine writeit
