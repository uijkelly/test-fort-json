!
! This is the main entry point of the code
!
! Jessica A Kelly
!
! run my_sim_model

program test_fort_json
  use my_sim_model

  implicit none

  write(*,*) "Using Fortran with input as JSON"
  write(*,*) "reading the parameters from the json file"
  write(*,*) ""
  !CALL set_parameters
  CALL set_parameters_system
  !CALL my_sim_main

  write(*,*) ""

end program test_fort_json
