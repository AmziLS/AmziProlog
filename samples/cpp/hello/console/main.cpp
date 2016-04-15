// MAIN.CPP - A main program that calls the hello classs

#include "chello.h"

int main()
{
	CHello hello;

	if (hello.init())
		hello.run();

	return 0;
}
