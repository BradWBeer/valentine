
# Valentine: A Common Lisp wrapper for the Bullet Physics Library

## Notes

* You can build the bindings. You will need SWIG and g++. 

* The CLOS interface doesn't work yet. There are problems with package locks and methods with different numbers of arguments.

## To build: 

* find a work directory

* type: git clone https://github.com/bulletphysics/bullet3.git

* make and enter a build directory

* type: cmake <path to bullet repo> -G "Unix Makefiles" -DINSTALL_LIBS=ON -DCMAKE_INSTALL_PREFIX=/usr -DBUILD_SHARED_LIBS=ON -DBUILD_EXTRAS=OFF
  (If you are not building for Unix...I don't know what to do for you.)

* type: make -j <number of cores you have>

* type: sudo make install 
  (Enter your password) 

* cd to ~/quicklisp/local-projects

* type: git clone git@github.com:BradWBeer/valentine.git

* enter the valentine directory and type: make clean all

* start up your local lisp 

* in the repl type: (ql:quicklisp :valentine)

* use valentine...oh, all the symbols are private right now so be sure to use valentine::<symbol>

* report any bugs...I bet there's a lot of them

* have fun!


