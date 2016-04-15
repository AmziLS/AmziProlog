#include <windows.h>    // includes basic windows functionality
#include <stdio.h>      // includes standard file i/o functionality
#include <stdlib.h>
#include <string.h>     // includes string functions
#include <cderr.h>      // includes the common dialog error codes
#include <commctrl.h>   // includes common controls
#include <mmsystem.h>   // sound
#include "complex.h"
#include "pq.h"
#include "quad.h"
#include "resource.h"

extern POLAR reliefFunction(COMPLEX);
extern COMPLEX cartesianFunction(COMPLEX);
extern void pqInit(int, int);
extern void pqHeader(char **);
extern void passQ(QUAD *);
extern int  colorFunction(COMPLEX);
extern POINT *getOrbit();

HINSTANCE    hInst;
HWND         hWnd;                                  // main window handle
HWND         hStatusWnd;                            // status window handle
WNDCLASSEX   wc; 
int          cmdshow;
int          view;
char MTitle[60];
char scaleStr[40];
OPENFILENAME OpenFileName;
CHAR         szDirName[256];
CHAR         szFile[256];
CHAR         szFileTitle[256];

void InitStatus(HWND);

QUAD    Q;  
      
HDC           hDC;
PRINTDLG      pd;
HPEN          pen;
COLORREF      penColor;
PALOMINE      pal;
HPALETTE      logPal;

HDC           memDC;
HBITMAP       hbit;
HBRUSH        hbrush;
int           maxX, maxY;

LRESULT CALLBACK WindowFunc(HWND, UINT, WPARAM, LPARAM);
char szWinName[] = "Win" ;                       // name of window class
void newWindow(char *, char *);
BOOL APIENTRY About(HWND, UINT, UINT, LONG);

//functions
BOOL InitApplication(HANDLE);
BOOL InitInstance(HANDLE, int);
BOOL CALLBACK dlgProcCaller(HWND, UINT, WPARAM, LPARAM );
/****************************************************************************
*
*    FUNCTION: WinMain(HANDLE, HANDLE, LPSTR, int)
*
*    PURPOSE: calls initialization function, processes message loop
*
*    COMMENTS:
*
*
****************************************************************************/

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
		     LPSTR lpCmdLine, int nCmdShow)
{   /* Acquire and dispatch messages until a WM_QUIT message is received. */

  char caller[] = "polyplot";
  MSG msg;                                   
  DWORD status;
  hInst = hInstance;
  cmdshow = nCmdShow;
  if(strcmp(lpCmdLine, "relief") == 0)
    view = RELIEF;
  else
    if(strcmp(lpCmdLine, "cartesian") == 0)
      view = CARTESIAN;
    else
      if(strcmp(lpCmdLine, "distance") == 0)
	{
	  view = DISTANCE;
	  passQ(&Q);
	}
      else
	if(strcmp(lpCmdLine, "mandelbrot") == 0)
	  {
	    view = MANDELBROT;
	    passQ(&Q);
	  }
	else
	  view = 0;

                                           // Define a window class. 
  wc.cbSize = sizeof(WNDCLASSEX);
  wc.hInstance = hInstance;                // Application that owns the class
  wc.lpszClassName = szWinName;            // Name used in CreateWindow 
  wc.lpfnWndProc = WindowFunc;             // Function to retrieve messages 
  wc.style = 0;                            // Class style(s).        
  wc.hIcon = LoadIcon(NULL, IDI_APPLICATION); // standard icon 
  wc.hIconSm = LoadIcon(NULL, IDI_WINLOGO); // small icon 
  wc.hCursor = LoadCursor(NULL, IDC_ARROW);
  wc.hbrBackground = (struct HBRUSH__ *)GetStockObject(WHITE_BRUSH); 
  wc.lpszMenuName =  "";                   // Name of menu rsrc in .RC file. 
  wc.cbClsExtra = 0;                       // No per-class extra data.    
  wc.cbWndExtra = 0;                       // No per-window extra data.  
  
  if(!RegisterClassEx(&wc))                // Register the window class 

    return FALSE;                          // return failure code. 

  hWnd = CreateWindow(/* Create main window for this appln instance */
			szWinName,           // See RegisterClass() call.  
			"Quad Rendering of Root Finding Methods",
			WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN,
			CW_USEDEFAULT,       // Default horizontal position. 
			CW_USEDEFAULT,       // Default vertical position.   
			WIDTH,               // Default width.             
			HEIGHT,              // Default height. 
			NULL,                // no parent
			NULL,                // Use the window class menu.   
			hInst,               // This inst owns this window.
			NULL                 // Pointer not needed.
			);
  if (!hWnd)                               
    return FALSE;                            // Window could not be created
  
  ShowWindow(hWnd, cmdshow);                 // Show the window     
  UpdateWindow(hWnd);                        // Sends WM_PAINT message  
  hDC= GetDC(hWnd);

  InitStatus(hWnd);                          // make status bar
  Q.initLut();
  pqInit(15, view);                          // default angle 15

  status = DialogBox(hInst, 
		     MAKEINTRESOURCE(IDD_DIALOG1), 
		     hWnd, (DLGPROC)(dlgProcCaller));
  if(status == 0)
    {
      PostQuitMessage(0);
      return FALSE;
    }
  
  sprintf(scaleStr, "%d pixels/unit.", Q.getScale());
 
  SetWindowText(hWnd, MTitle); 
  while (GetMessage(&msg, NULL, 0, 0))                    
    {
      TranslateMessage(&msg);                // Translates virtual key codes  
      DispatchMessage(&msg);                 // dispatches message to window  
    }

  return msg.wParam;
}

/****************************************************************************
*
*    FUNCTION:  InitInstance(HANDLE, int)
*
*    PURPOSE:  Saves instance handle and creates main window
*
*    COMMENTS:
*
*        In this function, we save the instance handle in a static variable and
*        create and display the main program window.
*
****************************************************************************/

BOOL InitInstance(HANDLE hInstance,      // Current instance identifier. 
                  int nCmdShow)          // Param for first ShowWindow() call.
{
  /* Save the instance handle in static variable, which will be used in */
  /* many subsequence calls from this application to Windows.           */
/*
    HPALETTE previous;
    DWORD rgb;
    int colors;
  */  
    hInst = (struct HINSTANCE__ *)hInstance;
    /*    logPal= CreatePalette((LOGPALETTE *)&pal);
    previous = SelectPalette(hDC, logPal, 0);
    chsclr.lStructSize= sizeof(CHOOSECOLOR);
    chsclr.hInstance= hInstance;
    chsclr.hwndOwner= hWnd;
    chsclr.lpCustColors= &rgb;
    
    colors= RealizePalette(hDC);
    ChooseColor(&chsclr);
    */
    return TRUE;
}

BOOL CALLBACK dlgProcCaller(HWND hDlg,     // window handle of the dialog box 
                            UINT message,  // type of message                 
                            WPARAM wParam, // message-specific information    
                            LPARAM lParam) 
{
   switch(message)
    {
    case WM_INITDIALOG:                    // initialize dialog box 
      CheckRadioButton(  hDlg, IDC_SCALE75, IDC_SCALE2400, IDC_SCALE150);
      CheckRadioButton(  hDlg, IDC_ANGLE15, IDC_ANGLE45, IDC_ANGLE15);
      SendDlgItemMessage(hDlg, IDC_SOUND, BM_SETCHECK, BST_CHECKED, 0);
      SendDlgItemMessage(hDlg, IDC_AXES,  BM_SETCHECK, BST_CHECKED, 0);
      SendDlgItemMessage(hDlg, IDC_UNITC, BM_SETCHECK, BST_CHECKED, 0);
      SendDlgItemMessage(hDlg, IDC_TEXT,  BM_SETCHECK, BST_CHECKED, 0);
      SendDlgItemMessage(hDlg, IDC_SPIN1, UDM_SETRANGE, 0, ((-2)<<16)|2);
      SendDlgItemMessage(hDlg, IDC_SPIN1, UDM_SETPOS, 0, 0);
      SendDlgItemMessage(hDlg, IDC_SPIN2, UDM_SETRANGE, 0, ((-2)<<16)|2);
      SendDlgItemMessage(hDlg, IDC_SPIN2, UDM_SETPOS, 0, 0);
      return TRUE;
    case WM_COMMAND:                       // refer to member function
      return Q.dlgProc(hDlg, message, wParam, lParam);
    default:
      return DefWindowProc(hDlg, message, wParam, lParam);
    }
   return FALSE;
}

/****************************************************************************
*
*    FUNCTION: LRESULT CALLBACK WindowFunc(HWND, UINT, WPARAM, LPARAM)
*
*    PURPOSE:  Processes messages
*
*    COMMENTS:
*
*        This function processes all messags sent to the window.  When the
*        user choses one of the options from one of the menus, the command
*        is processed here and passed onto the function for that command.
*        This function also processes the special "FindReplace" message that
*        this application registers for hook processing of the FindText()
*        and ReplaceText() common dialog functions.
*
****************************************************************************/

LRESULT CALLBACK WindowFunc(HWND hWnd, UINT message, WPARAM wParam, 
                            LPARAM lParam) 
{
  PAINTSTRUCT ps;
  static BOOL NewFont;
  HDC hdc, paintDC;
  POINTS point;
  POINT *orbit;
  COMPLEX z, w;
  POLAR pw;
  HPEN linePen;
  char position[40], value[40];
  int n, mixMode;

  switch (message) 
    {
    case WM_CREATE:
      maxX  = GetSystemMetrics(SM_CXSCREEN);
      maxY  = GetSystemMetrics(SM_CYSCREEN);
      hdc   = GetDC(hWnd);
      memDC = CreateCompatibleDC(hdc);
      hbit  = CreateCompatibleBitmap(hdc, maxX, maxY);
      SelectObject(memDC, hbit);
      hbrush = (struct HBRUSH__ *)GetStockObject(WHITE_BRUSH);
      SelectObject(memDC, hbrush);
      PatBlt(memDC, 0, 0, maxX, maxY, PATCOPY);
      ReleaseDC(hWnd, hdc);
      return 0;
    case WM_PAINT:                             // process repaint
      paintDC = BeginPaint (hWnd, &ps);
      BitBlt(paintDC, 0, 0, maxX, maxY, memDC, 0, 0, SRCCOPY);
      EndPaint( hWnd, &ps );                   // end painting and release hDC
      break;
    case WM_COMMAND:                           // cmnd from application menu 
      switch(LOWORD(wParam))
        {
        case IDD_DIALOG1:
          break;
        }
      break;
    case WM_LBUTTONDOWN:                       // delineate orbit
      point.x = LOWORD(lParam);
      point.y = HIWORD(lParam);
      switch(view)
	{
	case DISTANCE:
	  if(Q.getSound())
	    sndPlaySound("ding.wav", SND_ASYNC);
	  z = Q.screen2model(point);
	  n = colorFunction(z);
	  linePen = CreatePen(0, PS_SOLID, 0);
	  SelectObject(hDC, linePen);
	  Polyline(hDC, getOrbit(), n+1);
	  break;
	case MANDELBROT:
	  if(Q.getSound())
	    sndPlaySound("ding.wav", SND_ASYNC);
	  z = Q.screen2model(point);
	  n = colorFunction(z);
	  mixMode = SetROP2(hDC, R2_NOT);
	  linePen = CreatePen(0, PS_SOLID, 0);
	  SelectObject(hDC, linePen);
	  orbit = getOrbit();
	  for(n = 0; orbit[n+1].x || orbit[n+1].y; n++)
	    Polyline(hDC, &orbit[n], 2);
	  SetROP2(hDC, mixMode);
	  break;
	default:
	  break;
	}
      break;
    case WM_MOUSEMOVE:
      point = MAKEPOINTS(lParam);
      z = Q.screen2model(point);
      sprintf(position, "  z: (%2.2f, %2.2f)", z.rpart(), z.ipart());
      SendMessage(hStatusWnd, SB_SETTEXT, (WPARAM) 0, (LPARAM) position);
      switch(view)
	{
	case RELIEF:
	  pw = reliefFunction(z);
	  sprintf(value, "  w: (%2.2f, %2.2f)", 
		  pw.getMod(), 180*pw.getAngle()/pi);
	  break;
	case CARTESIAN:
	  w = cartesianFunction(z);
	  sprintf(value, "  w: (%2.2f, %2.2f)", 
		  w.rpart(), w.ipart());
	  break;
	case DISTANCE:
	  pw = reliefFunction(z);
	  sprintf(value, "  w: (%2.2f, %2.2f)", 
		  pw.getMod(), 180*pw.getAngle()/pi);
	  break;
	}
      SendMessage(hStatusWnd, SB_SETTEXT, (WPARAM) 1, (LPARAM) value);    
      break;
    case WM_SIZE:
      break;
    case WM_DESTROY:                          // window being destroyed 
      DeleteDC(memDC);
      DeleteObject(hbit);
      PostQuitMessage(0);
      break;
    default:
      return DefWindowProc(hWnd, message, wParam, lParam);
    }
  return (0);
}

/****************************************************************************
*
*    FUNCTION: About(HWND, UINT, UINT, LONG)
*
*    PURPOSE:  Processes messages for "About" dialog box
*
*    COMMENTS:
*
*       No initialization is needed for this particular dialog box, but TRUE
*       must be returned to Windows.
*
*       Wait for user to click on "Ok" button, then close the dialog box.
*
****************************************************************************/

BOOL APIENTRY About(HWND hDlg, UINT message, UINT wParam, LONG lParam)
{
  switch (message)
    {
    case WM_INITDIALOG:                     // initialize dialog box 
      return (TRUE);
    case WM_COMMAND:                        // received a command 
      if (LOWORD(wParam) == IDOK || 
	  LOWORD(wParam) == IDCANCEL)
	{                                   // System menu close command?
	  EndDialog(hDlg, TRUE);            // Exits the dialog box 
	  return (TRUE);
	}
      break;
    }
  return (FALSE);                           // Didn't process a message 
  lParam;                                   // avoid compiler warnings at W3
}

void InitStatus(HWND hWnd)
{
  RECT WinDim;
  int i, parts[4];
  
  GetClientRect(hWnd, &WinDim);
  for(i = 1; i <= 4; i++)
      parts[i-1] = WinDim.right/4 * i;
  hStatusWnd = CreateStatusWindow(WS_CHILD | WS_VISIBLE,
				  " Mouse position: ",
				  hWnd,
				  IDD_STATUSWIN);
  SendMessage(hStatusWnd, SB_SETPARTS, (WPARAM) 4, (LPARAM) parts);
}




