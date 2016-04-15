retefoop.xpl: retefoop.plm retepred.plm
	cpl retefoop retefoop retepred
   cppkg retefoop

retefoop.plm: retefoop.pro
	cpc retefoop

retepred.plm: retepred.pro
	cpc retepred

