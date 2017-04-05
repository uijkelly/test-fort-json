# Notes on what I did

## Resources
- https://github.com/jacobwilliams/json-fortran For the library
- https://github.com/jacobwilliams/json-fortran/wiki/Example-Usage  For some simple example usage
- http://jacobwilliams.github.io/json-fortran/ for full documentation

## Details

Homebrew to install didn't quite seem to do the trick, so went with manual download of git repo, but that also means a <code>pip install FoBiS.py</code> is required also. Will have to do more digging [there](https://github.com/szaghi/FoBiS) because it seems like it might be helpful in the future for builds.

Then once FoBiS was installed, ran <code>sh build.sh</code> in the json-fortran directory to build the module, and runs some unit tests.

Now the module files are in <code>json-fortran/lib</code>, and all I have to do is figure out how to include them in this here project! For now, not being fancy, and will just copy them over to the main folder and go from there. 
