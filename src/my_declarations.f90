! Declaring some global variables

! Remarks:
!   SBRACK should be:
! 0.0000000000000000        7000.0000000000000        22100.000000000000        53500.000000000000        111586.60000000001        242579.54999999999        243568.47000000000
! 0.0000000000000000        7000.0000000000000        22100.000000000000        53500.000000000000        111586.60000000001        242579.54999999999        243568.47000000000

! RESULTS
!SBRACK =
!0.0000000000000000        7000.0000000000000        22100.000000000000        53500.000000000000        111586.60000000001        242579.54999999999        243568.47000000000
!0.0000000000000000        7000.0000000000000        22100.000000000000        53500.000000000000        111586.60000000001        242579.54999999999        243568.47000000000

module my_declarations
  SAVE

  CHARACTER*128 FILE7		!outputfile -- traditionally to TPC model comes in as a command line arg
  INTEGER :: TOTSIM		! total number of simulations to perform - could be 2-48
  INTEGER :: MAXBRACK	! maximum number of brackets - 6 or 7 this is pretty stable.
  INTEGER, DIMENSION (:), ALLOCATABLE :: NBRACK		! number of brackets for each simulation 6 or 7, maximum bracket here is TOTSIM
  REAL*8, DIMENSION (:,:), ALLOCATABLE :: SBRACK ! single brackets
  REAL*8, DIMENSION (:,:), ALLOCATABLE :: MBRACK ! MFJ brackets
  REAL*8, DIMENSION (:,:), ALLOCATABLE :: HBRACK ! HOH brackets
  REAL*8, DIMENSION (:,:), ALLOCATABLE :: RATES ! tax rates by bracket
  INTEGER, PARAMETER :: outputfile = 7 ! ouput file
  LOGICAL AGEDATA
  CHARACTER*6 WEIGHT

END MODULE my_declarations
