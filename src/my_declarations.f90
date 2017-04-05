! Declaring some global variables
module my_declarations
  SAVE
  
  CHARACTER*128 FILE7		!outputfile
  INTEGER :: TOTSIM		! total number of simulations to perform
  INTEGER :: MAXBRACK	! maximum number of brackets
  INTEGER, DIMENSION (:), ALLOCATABLE :: NBRACK		! number of brackets for each simulation
  REAL*8, DIMENSION (:,:), ALLOCATABLE :: SBRACK ! single brackets
  REAL*8, DIMENSION (:,:), ALLOCATABLE :: MBRACK ! MFJ brackets
  REAL*8, DIMENSION (:,:), ALLOCATABLE :: HBRACK ! HOH brackets
  INTEGER, PARAMETER :: outputfile = 7 ! ouput file

END MODULE my_declarations
