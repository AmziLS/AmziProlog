% RUBMOV - copyright (C) 1994, Amzi! inc.

% this file contains the definitions of all of the 
% moves and rotations primitive to Rubik's Cube.

% Both moves and rotations are done using Prologs unification.
% The first argument is the name of the move or rotation, and the
% second and third arguments define transformations of the structure
% which represents the cube.

% By convention the moves are named by a single character which stands
% for the position of the side being turned.  Rotations are used to
% reposition the entire cube (leaving the pieces in the same relative
% positions).  They are named by the side which defines the axis
% of rotation, preceded by the letter r.

% (Why the funny variable names?  This program was originally written
%  in micro-Prolog (one of my favorites) with its parenthetical list
%  notation.  I then acquired Arity Prolog and wrote a translation
%  program converted the micro-Prolog syntax to Edinburgh syntax.
%  It did the dumb thing with variable names, and I've never bothered
%  to fix many of them, such as these.)

% The sides are: u up, d down, l left, r right, f front, b back.

:- export move/3,rot/3.

move(u, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, V10, V11, V12, 
         V13, V14, V15, V16, V17, V18, V19, V20, V21, V22, V23, 
         V24, V25, V26, V27, V28, V29, 
         V30, V31, V32, V33, V34, 
         V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, V45, 
         V46, V47, V48, V49, V50, V51, V52, V53, V54), 
       cube(X1, X2, X3, X4, X5, X6, V20, V19, V21, V10, V11, 
         V12, V8, V7, V9, V16, V17, V18, V26, V25, V27, V22, 
         V23, V24, V14, V13, V15, V28, V29, V30, V43, V44, 
         V33, V34, V39, V40, V37, V38, V31, V32, V41, V42, 
         V35, V36, V45, V46, V47, V48, V49, V50, V51, V52, 
         V53, V54)).
move(d, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, 
  V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, 
  V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42, 
  V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54), 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9,
  V17, V16, V18, V13, V14, V15, V29, V28, 
  V30, V19, V20, V21, V11, 
  V10, V12, V25, V26, V27, V23, V22, V24, V31, V32, 
  V41, V42, V35, V36, V45, V46, V39, V40, V37, V38, V43, V44, V33, 
  V34, V47, V48, V49, V50, V51, V52, V53, V54)).
move(r, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, V18, 
  V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, V30, V31, 
  V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, 
  V45, V46, V47, V48, V49, V50, V51, V52, V53, V54), 
       cube(X1, X2, X3, X4, X5, X6, V12, V11, V10, V24,
  V23, V22, V13, V14, V15, V16, V17, V18, V9, 
  V8, V7, V21, V20, V19, V25, V26, V27, V28, V29, V30, V48, V47, V52, 
  V51, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, V45, V46, 
  V34, V33, V49, V50, V32, V31, V53, V54)).
move(l, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,
  V21, V22, V23, V24, V25, V26, V27, V28, V29, 
  V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41,
  V42, V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54), 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V27, V26, V25, V15, V14, V13, V19, V20, V21, V22, V23, V24, 
  V30, V29, V28, V18, V17, V16, V31, V32, V33, V34, V54, V53,
  V50, V49, V39, V40, V41, V42, V43, V44, V45, V46, V47, V48,
  V36, V35, V51, V52, V38, V37)).
move(f, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,
  V21, V22, V23, V24, V25, V26, V27, V28, V29, 
  V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41,
  V42, V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54), 
       cube(X1, X2, X3, X4, X5, X6, V13, V15, V14, V7, V9, V8, V16, V18, V17, 
  V10, V12, V11, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, 
  V30, V31, V32, V33, V34, V35, V36, V37, V38, V49, V50, V47, V48,
  V43, V44, V45, V46, V39, V40, V41, V42, V51, V52, V53, V54)).
move(b, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21,
  V22, V23, V24, V25, V26, V27, V28, V29, 
  V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41,
  V42, V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54), 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, V18, V22, V24, V23, V28, 
  V30, V29, V19, V21, V20, V25, V27, V26, V31, V32, V33, V34, V35,
  V36, V37, V38, V39, V40, V41, V42, V51, V52, V53, V54, V47,
  V48, V49, V50, V45, V46, V43, V44)).
rot(ru, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21,
  V22, V23, V24, V25, V26, V27, V28, V29, 
  V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42,
  V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54), 
       cube(X2, X4, X3, X5, X1, X6, V20, V19, V21, V23, V22,
  V24, V8, V7, V9, V11, 
  V10, V12, V26, V25, V27, V29, V28, 
  V30, V14, V13, V15, V17, V16, V18, V43, V44, V45, V46, V39,
  V40, V41, V42, V31, V32, V33, V34, V35, V36, V37, V38, V52,
  V51, V48, V47, V54, V53, V50, V49)).
rot(rr, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,
  V21, V22, V23, V24, V25, V26, V27, V28, V29, 
  V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41,
  V42, V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54), 
       cube(X6, X2, X1, X3, X5, X4, V12, V11, 
  V10, V24, V23, V22, V18, V17, V16, 
  V30, V29, V28, V9, V8, V7, V21, V20, V19, V15, V14, V13,
  V27, V26, V25, V48, V47, V52, V51, V50, V49, V54, V53, V42,
  V41, V46, V45, V40, V39, V44, V43, V34, V33, V38, V37,
  V32, V31, V36, V35)).
rot(rf, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, V18,
  V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, 
  V30, V31,
  V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44,
  V45, V46, V47, V48, V49, V50, V51, V52, V53, V54),
       cube(X1, X3, X5, X4, X6, X2, V13, V15, V14, V7, V9, V8, V16, V18, V17, 
  V10, V12, V11,
  V25, V27, V26, V19, V21, V20, V28, 
  V30, V29, V22, V24, V23, V36,
  V35, V32, V31, V38, V37, V34, V33, V49, V50, V47, V48, V53, V54,
  V51, V52, V39, V40, V41, V42, V43, V44, V45, V46)).
