glf.so: glf.i
	swig -chicken -nounit glf.i 
	csc -I/usr/include/ -I/usr/include/GL -L/usr/X11R6/lib -lglut -lGLU -lGL -lXmu -lXt -lX11 -lXi -lm -sv *.scm *.c -o glf.so

clean: *.scm *.c *.so
	rm *.scm *.c *.so
