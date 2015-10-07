
CC=g++
AR=ar
SED=sed
CCFLAGS=  -g -Wall -O3 -fPIC
SWIG=swig
SFLAGS= -cffi -c++ -fastdispatch -DSWIG_TYPE_TABLE=swigbullet 
INCLUDES= -I/usr/include/bullet

BULLET_DIR=../tools/bullet3/bin/
BULLET_BUILD=_gmake_x64_release.a
BULLET_LIBS= $(BULLET_DIR)libBulletSoftBody$(BULLET_BUILD) $(BULLET_DIR)libBulletDynamics$(BULLET_BUILD) $(BULLET_DIR)libBulletCollision$(BULLET_BUILD) $(BULLET_DIR)libLinearMath$(BULLET_BUILD)

LIBS= -lBulletSoftBody -lBulletDynamics -lBulletCollision -lLinearMath
LDFLAGS= -shared 

target= libbullet_wrap.so
objects= bullet_wrap.o 

.PRECIOUS: %_wrap.cxx

all: $(target)

$(target): $(objects) 
	$(CC) $(LDFLAGS) $(objects) -o $@ $(LIBS) $(LIBS)

%_wrap.cxx: %.i
	$(SWIG) $(SFLAGS) $(INCLUDES) -o $@ $< 
	$(SED)  -i 's/#\.BT_ENABLE_GYROSCOPIC_FORCE_IMPLICIT_BODY/#.8/' swigbullet.lisp 
	$(SED)  -i 's/( :pointer)/(_ :pointer)/p' swigbullet.lisp 
	$(SED)  -i 's/("_wrap_length" length)/("_wrap_length" length1)/' swigbullet.lisp 

%.o: %.cxx
	$(CC) $(CCFLAGS) $(INCLUDES) -c $< -o $@

clean:
	rm -f swigbullet*.lisp
	rm -f *.cxx
	rm -f $(objects)
	rm -f $(target)

install: all
	echo "Install not implemented yet!"
