! run my_sim_model
program test_fort_json
  use my_sim_model
  implicit none

  CALL set_parameters

  write(*,*) "not calling anything just structure set up"

end program test_fort_json
