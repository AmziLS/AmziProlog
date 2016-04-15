/****************************************************************************
*
* LLink.h -- the .plm file linker
* 
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: llink.h,v $
* Revision 1.1  2005/12/15 18:21:16  mary
* Add the linker to the engine.
*
****************************************************************************/

#define noBUG_LINK

#define DISTVER      // Puts key stuff into xpl file, must be set

const int INI_BUFLEN = 128;

struct G_ATOM
{ 
   aCHAR *name;          /* symbol */
   int  index;
   G_ATOM()
   { name = NULL; index = 0; }
   ~G_ATOM()
   { delete name; }
};

typedef aCHAR* L_ATOM;
typedef aCHAR* paCHAR;

    /* the op-codes */

#define     Ono_op        '\000'    /*  0 - 0x00 */
#define     Owho_am_i     '\063'    /* 51 - 0x33 */

    /* gets */
#define     Oget_x_var    '\037'    /* 31 - 0x1f */
#define     Oget_y_var    '\043'    /* 35 - 0x23 */
#define     Oget_con      '\001'    /*  1 - 0x01 */
#define     Oget_nil      '\002'    /*  2 - 0x02 */
#define     Oget_struc    '\003'    /*  3 - 0x03 */
#define     Oget_list     '\004'    /*  4 - 0x04 */
#define     Oget_x_val    '\057'    /* 47 - 0x2f */
#define     Oget_y_val    '\060'    /* 48 - 0x30 */

    /* puts */
#define     Oput_x_var    '\061'    /* 49 - 0x31 */
#define     Oput_y_var    '\062'    /* 50 - 0x32 */
#define     Oput_unsafe   '\005'    /*  5 - 0x05 */
#define     Oput_con      '\006'    /*  6 - 0x06 */
#define     Oput_nil      '\007'    /*  7 - 0x07 */
#define     Oput_struc    '\010'    /*  8 - 0x08 */
#define     Oput_list     '\011'    /*  9 - 0x09 */
#define     Oput_x_val    '\054'    /* 44 - 0x2c */
#define     Oput_y_val    '\055'    /* 45 - 0x2d */

    /* sequencing codes */
#define     Oexit           '\042'    /* 34 - 0x22 */
#define     Ofail           '\040'    /* 32 - 0x20 */
#define     Otrust_me_2_else  '\041'  /* 33 - 0x21 */
#define     Ocall           '\012'    /* 10 - 0x0a */
#define     Omod_call       '\067'    // 55 - 0x37
// bigdig #define     Ocall_local     '\050'    /* 40 - 0x28 */
#define     Oproceed        '\013'    /* 11 - 0x0b */
#define     Oexec           '\014'    /* 12 - 0x0c */
// bigdig #define     Oexec_local     '\051'    /* 41 - 0x29 */
#define     Omod_exec       '\070'    // 56 - 0x38 
#define     Oescape         '\015'    /* 13 - 0x0d */
#define     Oalloc          '\016'    /* 14 - 0x0e */
#define     Odealloc        '\017'    /* 15 - 0x0f */
#define     Ocut            '\020'    /* 16 - 0x10 */
#define     Ocutd           '\021'    /* 17 - 0x11 */
#define     Ocut64          '\071'    // 57 - 0x39 // last number
#define     Otry_me_else    '\022'    /* 18 - 0x12 */
#define     Otry_me_or_else '\066'    /* 54 - 0x36 */
#define     Otry            '\023'    /* 19 - 0x13 */
#define     Oretry_me_else  '\024'    /* 20 - 0x14 */
#define     Oretry          '\025'    /* 21 - 0x15 */
#define     Otrust_me_else  '\026'    /* 22 - 0x16 */
#define     Otrust          '\027'    /* 23 - 0x17 */
#define     Oswitch_on_term '\033'    /* 27 - 0x1b */
#define     Ogoto           '\034'    /* 28 - 0x1c */
#define     Oswitch_on_cons '\035'    /* 29 - 0x1d */
#define     Oswitch_on_struc '\036'   /* 30 - 0x1e */
#define     Olabel          '\045'    /* 37 - 0x25 */

    /* Ounify ops */
#define     Ounify_x_var   '\044'     /* 36 - 0x24 */
#define     Ounify_y_var   '\056'     /* 46 - 0x2e */
#define     Ounify_unsafe  '\046'     /* 38 - 0x26 */
#define     Ounify_con     '\030'     /* 24 - 0x18 */
#define     Ounify_void    '\031'     /* 25 - 0x19 */
#define     Ou_var_getlist '\047'     /* 39 - 0x27 */
#define     Ounify_nil     '\032'     /* 26 - 0x1a */
#define     Ounify_x_val   '\052'     /* 42 - 0x2a */
#define     Ounify_y_val   '\053'     /* 43 - 0x2b */

    /* special noops for first clause when switches not needed */
#define     Ono_switch    '\064'    /* 52 - 0x34 */
#define     Ono_try       '\065'    /* 53 - 0x35 */

class Linker
{
#ifdef BUG_LINK
private:
   FILE   *lout;
#endif
private:
//	Keys keys;
   G_ATOM* GAtomTable;                      // Global Atom Table
   L_ATOM* LAtomTable;
   LBOOL isunicode;
   aCHAR msgBuf[512];
   aCHAR  ini_file[INI_BUFLEN];
   intC  eMaxAtoms;
   void (*pfMsg)(aCHAR*);                   // call back function for messages
#ifdef _UNICODE
   void (*pfMsgA)(char*);
#endif
   aCHAR m_ver[256];
public:
   Linker( void(*)(aCHAR*) );
   ~Linker();
   int Link(int, aCHAR**);
#if defined(_UNICODE)
   DLLExport Linker( void(*)(char*) );
   int DLLExport Link(int, char**);
#endif

private:
   void initialize( void(*pfM)(aCHAR*));
#ifdef _UNICODE
   void initialize( void(*pfM)(char*));
#endif
   //void locking();
   void shutdown();
   void zero_fill(aCHAR*, int);

#ifdef __sun
   aINT16 getint16(aBYTE* p)
   {   return( ((aINT16) *(aSBYTE *)(p+1) << 8) | (aINT16) *(aBYTE *)(p) ); }
#else
   aINT16 getint16(aBYTE* p)
   {   return( ((aINT16) *(aSBYTE *)(p+1) << 8) | (aINT16) *(aBYTE *)(p) ); }
#endif
   aINT16 ISUNIGETC(FILE* f)
   {   return( isunicode ? fread_int16(f) : fgetc(f) ); }

   int scampi(aCHAR*, aCHAR*);

   int AtomHash(aCHAR *);
   void EnterGAtom(aCHAR[], int);
   int map_atom(int);
   void read_atom(FILE *, FILE *);
   int read_code(FILE *, FILE *, long, int, aBYTE *);
   void read_const(FILE *, FILE *, long *);
   void read_code_segs(FILE *, FILE *, int);
   void read_g_atoms(FILE *, int);
   void read_l_atoms(FILE *, aUINT16, int);
   void read_linked_segs(FILE *, aUINT16, FILE *, int);
   FILE *ioPathFOpen(aCHAR *, aCHAR *);
   void output(aCHAR* s, ...);
   void abort_linker(aCHAR* s,...);
   void aborteof(aCHAR * s);
   int write_wstring(aCHAR *str, FILE *fp);
   int fread_int16(FILE *f);
   int read_int32(aINT32 *val, FILE *fp);
   int read_int16(aINT16 *val, FILE *fp);
   int read_uint16(aUINT16 *val, FILE *fp);
   int write_int32(int val, FILE *fp);
   int write_int16(aINT16 val, FILE *fp);
};



