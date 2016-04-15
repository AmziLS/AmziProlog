.SUFFIXES: .plm .pro

OBJECTS = dw_main.plm dw_rules.plm dw_data.plm

.pro.plm:
	acmp $<

dw.xpl: $(OBJECTS)
	alnk dw $(OBJECTS)
