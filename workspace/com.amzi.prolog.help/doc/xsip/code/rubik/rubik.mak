# Makefile for building rubik's cube solver using Cogent Prolog

rubik.xpl : rubik.plm &
    rubdata.plm rubdisp.plm rubedit.plm rubhelp.plm rubhist.plm rubmov.plm
  cpl rubik rubik rubdata rubdisp rubedit rubhelp rubhist rubmov

rubik.plm : rubik.pro
  cpc rubik

rubdata.plm : rubdata.pro
  cpc rubdata

rubdisp.plm : rubdisp.pro
  cpc rubdisp

rubedit.plm : rubedit.pro
  cpc rubedit

rubhelp.plm : rubhelp.pro
  cpc rubhelp

rubhist.plm : rubhist.pro
  cpc rubhist

rubmov.plm : rubmov.pro
  cpc rubmov


  
