
CC=g++
AR=ar
SED=sed
CCFLAGS=  -g -Wall -O3 -fPIC
SWIG=swig
SFLAGS= -cffi -c++ -generate-typedef -fastdispatch -DSWIG_TYPE_TABLE=swigbullet 
INCLUDES= -I/usr/include/bullet

BULLET_DIR=../tools/bullet3/bin/
BULLET_BUILD=_gmake_x64_release.a
BULLET_LIBS= $(BULLET_DIR)libBulletSoftBody$(BULLET_BUILD) $(BULLET_DIR)libBulletDynamics$(BULLET_BUILD) $(BULLET_DIR)libBulletCollision$(BULLET_BUILD) $(BULLET_DIR)libLinearMath$(BULLET_BUILD)

LIBS= -lBulletSoftBody -lBulletDynamics -lBulletCollision -lLinearMath
LDFLAGS= -shared 

target= libbullet_wrap.so 
lisp= swigbullet.lisp swigbullet-clos.lisp
objects= bullet_wrap.o 

.PRECIOUS: %_wrap.cxx

all: $(target)

$(target): $(objects) 
	$(CC) $(LDFLAGS) $(objects) -o $@ $(LIBS) $(LIBS)

%_wrap.cxx %.lisp %-clos.lisp: %.i
	$(SWIG) $(SFLAGS) $(INCLUDES) -o $@ $<

%.o: %.cxx
	$(CC) $(CCFLAGS) $(INCLUDES) -c $< -o $@

clean:
	rm -f swigbullet*.lisp
	rm -f *.cxx
	rm -f $(objects)
	rm -f $(target)

install: all
	echo "Install not implemented yet!"
