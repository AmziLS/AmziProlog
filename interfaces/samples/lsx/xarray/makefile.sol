#DEBUG
#C_FLAGS = -c -g

C_FLAGS = -c

CC = CC

all: xarray.lsx xarray.xpl

xarray.lsx: xarray.c
	$(CC) -I$(AMZI_DIR)/include $(C_FLAGS) -o xarray.o xarray.c
	$(CC) -o xarray.lsx -G xarray.o

xarray.xpl: xarray.pro
	acmp xarray
	alnk xarray xarray
