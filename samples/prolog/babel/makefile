.SUFFIXES: .plm .pro

OBJECTS = babel.plm english.plm german.plm japanese.plm latin.plm spanish.plm

.pro.plm:
	acmp $<

babel.xpl: $(OBJECTS)
	alnk babel $(OBJECTS)
