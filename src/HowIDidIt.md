# Notes on what I did

## Resources
- https://github.com/jacobwilliams/json-fortran For the library
- https://github.com/jacobwilliams/json-fortran/wiki/Example-Usage  For some simple example usage
- http://jacobwilliams.github.io/json-fortran/ for full documentation

## Details

Homebrew to install didn't quite seem to do the trick, so went with manual download of git repo, but that also means a <code>pip install FoBiS.py</code> is required also. Will have to do more digging [there](https://github.com/szaghi/FoBiS) because it seems like it might be helpful in the future for builds.

Then once FoBiS was installed, ran <code>sh build.sh</code> in the json-fortran directory to build the module, and runs some unit tests.

Though the latest update I ran on 21 Apr 2017 throws some segmentation errors, so will have to sort this out.

Now the module files are in <code>json-fortran/lib</code>, and all I have to do is figure out how to include them in this here project! To start, not being fancy, and will just copy them over to the main folder and go from there. But did eventually figure out makefile and gfortran details to leave in the folder where they are which is better. Details of that below.

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

**First Prototype**

The json file was like

```json
{
  "FILE7" : "test-fort-json-output.txt",
  "TOTSIM": 2,
  "MAXBRACK": 7
}
```

And then the code to open, and read in TOTSIM was

```fortran
type(json_file) :: json
call json%initialize()
call json%load_file(filename = 'src/params.json')
call json%get('TOTSIM', TOTSIM, found)
if (.not. found) then
	write(*,*) "Could not find TOTSIM" !TODO, this should exit because it's a real problem
else
	write(*,*) "Found TOTSIM ", TOTSIM
end if

```

which resulted in the number 2 being read from the JSON file and saved to TOTSIM

**Second Prototype**

In practice, the JSON created by the Django API looks like the following

```json
[
	{
			"ID": 2,
			"FORMAT": "TOTSIM",
			"CONTROL": 0,
			"EDIT_EXTERNALLY": 0,
			"EDIT_INTERNALLY": 0,
			"HTMLTYPE": 0,
			"HELP_ID": null,
			"LABEL": "TOTSIM",
			"DESCRIPTION": "TOTSIM",
			"GROUPNAME": "Base Parameters",
			"ROWNUM": 1,
			"PARAM": "TOTSIM",
			"VAL": "50"
	},
	{
			"ID": 3,
			"FORMAT": "MAXBRACK",
			"CONTROL": 0,
			"EDIT_EXTERNALLY": 0,
			"EDIT_INTERNALLY": 0,
			"HTMLTYPE": 0,
			"HELP_ID": null,
			"LABEL": "MAXBRACK",
			"DESCRIPTION": "MAXBRACK",
			"GROUPNAME": "Base Parameters",
			"ROWNUM": 2,
			"PARAM": "MAXBRACK",
			"VAL": "7"
	}
]
```

This meant there was a little more to do in order to pull out the data from each JSON object in our array. Also note that the array of objects is nameless. Fortran code to read became

```Fortran
type(json_core) :: json
type(json_value),pointer :: p,p1,p2

call json%parse(file='src/form_params_system.json', p=p)
call json%get(p,"(1).ID",p1)    ! since there is no name, just use the index to get at the element.
call json%print(p1,output_unit) ! this should return 2, the value of ID for the first record.

```

If the array of objects had a name, say <code>system</code> then the line that gets the element would be <code>call json%get(p,"system(1).ID",p1)</code>

This does not do everything yet, I still have to write code to look-up the value of <code>PARAM</code> for each object, then save the <code>VAL</code> into the correct variable.

**Third Prototype**

This includes a variable called <code>count</code> in the JSON to indicate how many elements are in the main field, an array now called <code>data</code> that was without a name previously.

Lastly, all the <code>VAL</code> elements are now string, and will need to be converted to the proper type as given in the field <code>FORTRAN_TYPE</code>

Array rules -- the "yearly" parameters -- will be split out into Baseline and Proposal.

---
 What is left to figure out?
---
**First Prototype Remaining Thoughts**

It seems easy to read the JSON file if we know the name of the item, and the type that it is supposed to be. Also seems to make sense that Fortran would really need to know before reading what type we are reading in. Could we just look for every single item that it **could** be and override a default? Is there a better way? This is important because we want to vary what is input, and not break anything, and also need to know how to look up things. Plus, only want to write the code to read the file once. But it would break if we added a new item and didn't edit the code to read that new item. Order though will no longer matter.

And non-trivially, need to sort out an array read and make sure that it comes in the right way.

**Second Prototype Remaining Thoughts**

Still not sure the best way to read the object -- particularly the array rules. The number of columns and the variables that we are going to look for is going to vary. I don't want to know what the names are going to be. Am I going to need that separately? Or will I just want to read everything? And how to know what everything is?
