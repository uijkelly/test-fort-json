# Notes on what I did

## Resources
- https://github.com/jacobwilliams/json-fortran For the library
- https://github.com/jacobwilliams/json-fortran/wiki/Example-Usage  For some simple example usage
- http://jacobwilliams.github.io/json-fortran/ for full documentation

## Details

Homebrew to install didn't quite seem to do the trick, so went with manual download of git repo, but that also means a <code>pip install FoBiS.py</code> is required also. Will have to do more digging [there](https://github.com/szaghi/FoBiS) because it seems like it might be helpful in the future for builds.

Then once FoBiS was installed, ran <code>sh build.sh</code> in the json-fortran directory to build the module, and runs some unit tests.

Now the module files are in <code>json-fortran/lib</code>, and all I have to do is figure out how to include them in this here project! For now, not being fancy, and will just copy them over to the main folder and go from there.

### gfortran with a static library

In the makefile, where the main executable is being built, specify the location of the library and the name. gfortran will look for a naming convention here of <code>lib[whatyouspecified].a</code> so in my makefile here I have:

```
$(ENAME): my_declarations.o my_write.o my_sim_model.o test_fort_json.o
	gfortran my_declarations.o my_write.o my_sim_model.o test_fort_json.o -o $(ENAME) -g -ffree-line-length-none -I/Users/jkelly/projects/fortran/json-fortran/lib -L/Users/jkelly/projects/fortran/json-fortran/lib -ljsonfortran
```

the <code>-L</code> gives the full path to the location of the library and the <code>-l</code> gives the name to look for. It will find the file <code>/Users/jkelly/projects/fortran/json-fortran/libjsonfortran.a</code> and include that in the linking step.


### using the json library

More in that makefile, the <code>-I</code> flag gives the definition to the additional include libraries. It is not only in the line that defines the executable, but also the compiling of the file that uses things

```
my_sim_model.o: src/my_sim_model.f90
	gfortran -c -g -ffree-line-length-none -I/Users/jkelly/projects/fortran/json-fortran/lib src/my_sim_model.f90
```

What are all these % signs in the code? It's the equivalent of the . in C++. Makes complete sense.

### What is left to figure out?

It seems easy to read the JSON file if we know the name of the item, and the type that it is supposed to be. Also seems to make sense that Fortran would really need to know before reading what type we are reading in. Could we just look for every single item that it **could** be and override a default? Is there a better way? This is important because we want to vary what is input, and not break anything, and also need to know how to look up things. Plus, only want to write the code to read the file once. But it would break if we added a new item and didn't edit the code to read that new item. Order though will no longer matter. 

And non-trivially, need to sort out an array read and make sure that it comes in the right way.
