#simple make for helloworld
#slashes go / for linux compile
#change dockerfile too to copy from subfolder
FC=gfortran

$(ENAME): my_declarations.o my_write.o my_sim_model.o test_fort_json.o
	gfortran -o $(ENAME) -g -ffree-line-length-none my_declarations.o my_write.o my_sim_model.o test_fort_json.o

my_declarations.o: src/my_declarations.f90
	gfortran -c -g -ffree-line-length-none src/my_declarations.f90
my_write.o: src/my_write.f90
	gfortran -c -g -ffree-line-length-none src/my_write.f90
my_sim_model.o: src/my_sim_model.f90
	gfortran -c -g -ffree-line-length-none src/my_sim_model.f90
test_fort_json.o: src/test_fort_json.f90
	gfortran -c -g -ffree-line-length-none src/test_fort_json.f90
clean:
	rm -f test_fort_json.o test_fort_json.exe
#end of makefile
