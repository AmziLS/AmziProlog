#ifdef DEBUG
C_FLAGS = -c -g -fpic
#else
C_FLAGS = -c -fpic
#endif

xarray.lsx: xarray.c
	$(CC) -I$(AMZI_DEV_DIR)/include $(C_FLAGS) -o xarray.o xarray.c
	$(CC) -shared -o xarray.lsx xarray.o
