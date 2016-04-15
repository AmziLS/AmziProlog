.SUFFIXES: .plm .pro

OBJECTS = rubdata.plm rubdisp.plm rubedit.plm rubhelp.plm rubhist.plm rubik.plm rubmov.plm

.pro.plm:
	acmp $<

rubik.xpl: $(OBJECTS)
	alnk rubik $(OBJECTS)
