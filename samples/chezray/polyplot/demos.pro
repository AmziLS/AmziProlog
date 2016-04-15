%-*-Prolog-*-

    /**************************************************************\
    *                                                              *
    * demos.pro - demos of polynomial algebra, using poly library  *
    *                                                              *
    \**************************************************************/

demo1:- reliefpq(1, pq(1, [0, 0, 1]), "inverse square: w = 1/zz").

demo2:-	reliefpq(1, pq([1,1], [-1,1]), "bilinear:   w = (z+1)/(z-1) ").

demo3:-
	B1 is 2*cos(pi/8),
	B2 is 2*cos(3*pi/8),
	eval([1, B1, 1]*[1, B2, 1], Q),
	Legend = "maxiflat: w = 1/(zz + 2cos(3pi/8)z + 1)(zz + 2cos(pi/8)z + 1)",
	reliefpq(1, pq(1, Q), Legend).

demo4:- newton([-1,0,0,0,0,0,0,1], S, N),
	reliefpq(S, N, "Newton transform: w = newton(z^7 - 1)").

demo5:- halley([-1,0,0,0,0,0,0,1], S, H),
	reliefpq(S, H, "Halley transform: w = halley(z^7 - 1)").

demo6:- halley([-1,0,0,0,0,0,0,1], S1, H1), halley(H1, S2, H2),
	reliefpq(S2, H2, "Halley transform: w = halley(halley(z^7 - 1))").

demo7:- eval([-1,0,0,0,0,0,0,1]*[0,1],X),
	halley(X, S, H),
	reliefpq(S, H, "Halley transform: w = halley(z(z^7 - 1))").


demo11:- cartesianpq(1, pq(1, [0, 0, 1]), "inverse square: w = 1/zz").
demo12:- cartesianpq(1, pq([1,1], [-1,1]), "bilinear:   w = (z+1)/(z-1) ").
demo13:-
	B1 is 2*cos(pi/8),
	B2 is 2*cos(3*pi/8),
	eval([1, B1, 1]*[1, B2, 1], Q),
	cartesianpq(1, pq(1, Q),
	"maxiflat: w = 1/(zz + 2cos(3pi/8)z + 1)(zz + 2cos(pi/8)z + 1)").
/*
 * root finding transformers
 * plot distance to root
 */
demo20:-
	write(`Plotting ZN = z^n -1. \nChoose n (2 - 10): `),
	read(N), 
	eval0([1.0] >> N, Z), 
	eval0(Z - 1.0, ZN),
	halley(ZN, S, H), 
	distancepq(S, H, "Distance to root: Halley(z^n - 1)").

demo21:-
	write(`PlottingZZN = z(z^n -1). \nChoose n (2 - 10): `),
	read(N),
	eval0([1.0] >> N, Z),
	eval0(Z - 1.0, Z1),
	eval0(Z1*z, ZZN),
	halley(ZZN, S, H), 
	distancepq(S, H, "Distance to root: Halley(z(z^n - 1))").

demo30:-
	mandelbrotpq(1, pq([0, 0, 1], 1), "Mandelbrot: z^2 + Zin").

/*************************************************************************/
reliefpq(Scale, PQ, Legend):- 
	portray(pq, Scale, PQ),
	initpq(Scale, PQ, Legend),!,         % send function to shared lib
	comm('plotserver relief').           % start the plotter
/*************************************************************************/
cartesianpq(Scale, PQ, Legend):-
	initpq(Scale, PQ, Legend),!,         % send function to shared lib
	comm('plotserver cartesian').        % start the plotter
/*************************************************************************/
distancepq(Scale, PQ, Legend):-
	initpq(Scale, PQ, Legend),!,         % send function to shared lib
	comm('plotserver distance').         % start the plotter
/*************************************************************************/
mandelbrotpq(Scale, PQ, Legend):-
	initpq(Scale, PQ, Legend),!,         % send function to shared lib
	comm('plotserver mandelbrot').       % start the plotter
/*************************************************************************/




