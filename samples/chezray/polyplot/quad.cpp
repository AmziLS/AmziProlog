#include <windows.h>
#include <commctrl.h>   // includes common controls
#include <mmsystem.h>   // sound#include "complex.h"
#include <stdio.h>
#include "quad.h"
#include "resource.h"

extern int   colorFunction(COMPLEX);
extern int   view;
extern HDC   hDC;
extern HDC   memDC;
extern HWND  hDlg;
extern HWND  hWnd;   
extern HWND  hStatusWnd;                            // status window handle
extern QUAD  Q;                                         // Quad parameters
extern char  MTitle[100];
extern void  pqInit(int, int);
extern void  pqHeader(char *);
extern char  scaleStr[40];
extern int   tiles, pixels;

DWORD WINAPI doApplication(LPVOID p)
{                                           // plain proc calls member proc
  Q.start(hWnd);
  return(0);
}

QUAD::QUAD()
{
  deltaAngle = 15;
  Epsilon  = 0.0001;
  Quantum  = 10;
  ColorMax = 16;
  Left     = 0;
  Top      = 0;
  Width    = WIDTH;
  Height   = HEIGHT;
  XOrg     = WIDTH>>1;
  YOrg     = HEIGHT>>1;
  Scale    = 150;
  FXOffset = 0.0;
  FYOffset = 0.0;

  lut[0]  =   0x0;        // black
  lut[1]  =   0x00008080; // brown
  lut[2]  =   0x00000080; // dark red
  lut[3]  =   0x000000ff; // red
  lut[4]  =   0x0000ffff; // yellow
  lut[5]  =   0x00008000; // dark green
  lut[6]  =   0x0000ff00; // green
  lut[7]  =   0x00800000; // dark blue
  lut[8]  =   0x00ff0000; // blue
  lut[9]  =   0x00ffff00; // lightblue
  lut[10] =   0x00808080; // dark grey
  lut[11] =   0x00c0c0c0; // light grey
  lut[12] =   0x00ffffff; // white 
  lut[13] =   0x00ffffff; 
  lut[14] =   0x00ffffff; 
  lut[15] =   0x00ffffff; 
}

/****************************************************************************
*
*    FUNCTION: dlgProc(UINT, LONG)
*
*    PURPOSE:  Processes messages for Quad setup
*
*    COMMENTS:
*
*       No initialization is needed for this particular dialog box, but TRUE
*       must be returned to Windows.
*
*       Wait for user to click on "Ok" button, then close the dialog box.
*
****************************************************************************/
BOOL CALLBACK QUAD::dlgProc(HWND hDlg, UINT message, 
             WPARAM wParam, LPARAM lParam)
{
  DWORD  dwThreadId, status;
  HANDLE hThread;

  switch(LOWORD(wParam))
    {
    case IDC_SCALE75:
      Scale = 75;
      break;
    case IDC_SCALE150:
      Scale = 150;
      break;
    case IDC_SCALE300:
      Scale = 300;
      break;
    case IDC_SCALE600:
      Scale = 600;
      break;
    case IDC_SCALE1200:
      Scale = 1200;
      break;
    case IDC_SCALE2400:
      Scale = 2400;
      break;
    case IDC_ANGLE15:
      pqInit(15, view);
      break;
    case IDC_ANGLE30:
      pqInit(30, view);
      break;
    case IDC_ANGLE45:
       pqInit(45, view);
       break;
    case IDOK:
      Sound    = SendDlgItemMessage(hDlg, IDC_SOUND, BM_GETCHECK, 0, 0);
      Axes     = SendDlgItemMessage(hDlg, IDC_AXES,  BM_GETCHECK, 0, 0);
      UnitC    = SendDlgItemMessage(hDlg, IDC_UNITC, BM_GETCHECK, 0, 0);
      Text     = SendDlgItemMessage(hDlg, IDC_TEXT,  BM_GETCHECK, 0, 0);
     FXOffset = 
         (double)((short)(SendDlgItemMessage(hDlg, IDC_SPIN1, UDM_GETPOS, 0, 0) & 0xffff));
      FYOffset = 
         (double)((short)(SendDlgItemMessage(hDlg, IDC_SPIN2, UDM_GETPOS, 0, 0) & 0xffff));

      EndDialog(hDlg, TRUE);       /* Exits the dialog box */
      hThread  = CreateThread(NULL, 0,
               (LPTHREAD_START_ROUTINE) doApplication,
               0, CREATE_SUSPENDED, &dwThreadId);
      SetPriorityClass(hThread, IDLE_PRIORITY_CLASS);
      pqHeader(MTitle);
      SetWindowText(hWnd, MTitle); 
      status = ResumeThread(hThread);
      break;
    case IDCANCEL:
      EndDialog(hDlg, 0);
      break;
    }
  return (TRUE);
  
  // avoid compiler warnings at W3
  //  lParam;
  
};

DWORD WINAPI thread(LPVOID param)
{
  HDC    hDC;
  
  hDC = GetDC(hWnd);
  
  Q.tesselate(hDC, (int)param);
  return(0);
}

void QUAD::start(HWND hWnd)
{
  POINT start, end, center;
  char pixelsstr[40], tilesstr[40];
  
  // DWORD Tid1, Tid2;
  
  background = GetPixel(hDC, 100L, 100L);
  
  setAxes();
  tiles = pixels = 0;
  /* nw:0  ne:1  se:2  sw:3  fencepost:4 */
  //  CreateThread(NULL, 0, 
  //       (LPTHREAD_START_ROUTINE)thread, (void *)4, /* nw */
  //       0, &Tid1);                           
  tesselate(hDC, 4);                                
  //  CreateThread(NULL, 0, 
  //       (LPTHREAD_START_ROUTINE)thread, (void *)5, /* ne */ 
  //       0, &Tid2);                           
  tesselate(hDC, 5);
  
  if(FYOffset != 0)
    {
      tesselate(hDC, 6);
      tesselate(hDC, 7);
    }
  
  if(Sound)
    sndPlaySound("tada.wav", SND_ASYNC);
  
  if(Axes == BST_CHECKED)
    { 
      start.x = 0;                                     // X Axis
      start.y = end.y = origin().y;
      end.x = WIDTH;
      MoveToEx(hDC, start.x, start.y, NULL);
      LineTo(hDC, end.x, end.y);
      
      start.y = 0;                                     // Y Axis
      start.x = end.x = origin().x;
      end.y = HEIGHT;
      MoveToEx(hDC, start.x, start.y, NULL); 
      LineTo(hDC, end.x, end.y); 
    };
  
  if(UnitC == BST_CHECKED)
    {
      int unitBox[] =                                  // Unit Circle
      {   
	origin().x - Scale,
	origin().y - Scale,
   origin().x + Scale,
   origin().y + Scale,
     };
      
      int endPoint[] = {XOrg + (Scale*(int)(FXOffset-1)), YOrg};
      
      Arc(hDC, unitBox[0], unitBox[1], unitBox[2], unitBox[3],
	  endPoint[0], endPoint[1],endPoint[0], endPoint[1]);
    }

  if(Text)
    {
      TextOut(hDC, 700, 20, "Z Plane)", 7);            // Text
      TextOut(hDC, 680, 35, scaleStr, strlen(scaleStr));   
      center = origin();
      TextOut(hDC, center.x-15,       center.y, "(0,0)", 5);   
      TextOut(hDC, center.x-15+Scale, center.y, "(1,0)", 5);
    }
  
  sprintf(pixelsstr, "Function calls: %d", pixels);
  SendMessage(hStatusWnd, SB_SETTEXT, (WPARAM) 2, (LPARAM) pixelsstr);
  sprintf(tilesstr, "Tiles: %d", tiles);
  SendMessage(hStatusWnd, SB_SETTEXT, (WPARAM) 3, (LPARAM) tilesstr);

  BitBlt(memDC, 0, 0, WIDTH, HEIGHT, hDC, 0, 0, SRCCOPY);
};

static int normOf(POINT point)
{
  return point.x*point.x + point.y*point.y;
}

PALOMINE::PALOMINE()
{
  palVersion = 0x300;
  palNumEntries = 16;
  palPalEntry[0] = Q.getLut(0);       
  palPalEntry[1] = Q.getLut(1);
  palPalEntry[2] = Q.getLut(2);
  palPalEntry[3] = Q.getLut(3);
  palPalEntry[4] = Q.getLut(4);
  palPalEntry[5] = Q.getLut(5);
  palPalEntry[6] = Q.getLut(6);
  palPalEntry[7] = Q.getLut(7);
  palPalEntry[8] = Q.getLut(8);
  palPalEntry[9] = Q.getLut(9);
  palPalEntry[10]= Q.getLut(10);
  palPalEntry[11]= Q.getLut(11);
  palPalEntry[12]= Q.getLut(12);
  palPalEntry[13]= Q.getLut(13);
  palPalEntry[14]= Q.getLut(14);
  palPalEntry[15]= Q.getLut(15);
}
