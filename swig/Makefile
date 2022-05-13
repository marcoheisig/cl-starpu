INCLUDES := \
 -I/usr/include/i386-linux-gnu \
 -I/usr/include/x86_64-linux-gnu \
 -I/usr/lib/gcc/x86_64-linux-gnu/9/include \
 -I/usr/lib/gcc/i386-linux-gnu/9/include \
 -I/usr/include \
 -I/usr/local/include

swig-interface.lisp: starpu.swig cl-starpu-tweaks.h prologue.lisp
	swig3.0 -cffi $(INCLUDES) -noswig-lisp -module starpu $<
#       We handle structs, enums, unions and constants with CFFI's groveller
	cat prologue.lisp > swig-interface.lisp
	sed '/^(\(cffi:defcstruct\|cffi:defcenum\|cffi:defcunion\|cl:defconstant\)/,/^$$/d' starpu.lisp >> swig-interface.lisp

all: starpu.lisp
