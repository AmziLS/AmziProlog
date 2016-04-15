/***************************************************************************\
*
* streams.h -- Stream Definitions
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: streams.h,v $
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.34  2002/11/20 14:54:44  dennis
* added support for user defined streams, fixed java jni bug with
* extended predicates
*
* Revision 1.33  2002/05/15 16:59:09  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.32  2002/05/02 17:39:30  dennis
* various minor bug fixes, added locale as a settable prolog flag
*
* Revision 1.31  2002/03/27 18:26:50  dennis
* fixed bug in read/write binary positioning
*
* Revision 1.30  2002/03/27 02:34:08  dennis
* fixed problems with writing %*g format in lenv_wpf.cpp, need the
* * for implementing decimal_places flag.
*
* Revision 1.29  2002/03/27 02:18:55  dennis
* read/write files working with repositioning
*
* Revision 1.28  2002/03/25 22:24:06  dennis
* Added read/write_binary predicates to replace fwrite, also repositioning
* for binary files. Note that even though the standard says there's a read
* and write position, they are one and the same.
*
* Revision 1.27  2002/03/23 13:11:56  dennis
* fixed Linux version so it can run as a background process.  Background
* processes stop if they try to do terminal I/O, so needed to take
* e_inittty and make sure it was only called when someone first tries
* to do terminal I/O.  That way the listener will still work, but background
* jobs won't call e_inittty and everything works OK.
*
* Revision 1.26  2002/03/22 15:36:27  dennis
* fixed bug
*
* Revision 1.25  2002/03/10 22:18:58  dennis
* fixed some small bugs, updated documentation
*
* Revision 1.24  2002/02/15 02:28:16  dennis
* unicode I/O working again
*
* Revision 1.23  2002/01/20 20:48:06  ray
* revised real divide, printReal
*
* Revision 1.22  2001/12/23 20:08:20  dennis
* made reposition of text files work for both gets and reads
*
* Revision 1.21  2001/12/22 18:50:20  dennis
* allowed for repositioning of text streams, a bit of a tricky business.
*
* Revision 1.20  2001/12/08 03:48:22  dennis
* fixed bugs with reconsult, added unicode support
*
* Revision 1.19  2001/11/17 18:40:25  dennis
* fixed reconsult and error handling in ide
*
* Revision 1.18  2001/11/16 02:11:39  dennis
* fixed bugs in positions, streams, get0 for stdin
*
* Revision 1.17  2001/11/15 13:54:07  dennis
* Fixed logging and apitrace, made logfile into its own entity,
* removed from the Prolog stream IO.
*
* Revision 1.16  2001/11/10 00:36:21  dennis
* fixed reset bug in streams, so reset OK and IDE can post errors.
*
*
\***************************************************************************/

//-*-C++ -*-
#ifndef STREAMS_H
#define STREAMS_H

//#include <cwchar>
#define BREAK_CHAR 3

#define LSTRCMP(A, R, B) (Lstrcmp(A, B) R 0)

class PrologStream;
class LEngine;
class MACRO;
class StreamReader;

//typedef PROSTREAM * STREF;
typedef PrologStream* STREF;

#define LINE_SIZE pLEX->bufSize   // size of line buffers for reader

#define BADSOP(msg) pXCPT->Error(invalid_sopE, id, msg)

/*
enum IOCharType
{
   FILE_ERROR = -1,
   FILE_BINARY,
   FILE_ASCII,
   FILE_ULITTLEENDNOTAG,
   FILE_ULITTLEEND,
   FILE_UBIGEND,
   FILE_UBIGENDNOTAG
};
*/

// NOTE - some of these enums correspond to arrays of atoms used
// to report stream properties.  Make sure they stay synchronized.
// The arrays are in p_stream_props.
enum STREAM_{PROSTREAM_, ISTREAM_, OSTREAM_, IOSTREAM_, MACROSTREAM_, 
				 STRINGSTREAM_};
enum mode_{badmode_, none_, read_, readwrite_, readappend_, write_, append_};
enum type_{text_ = 1, binary_, wide_text_, go_figure_};
enum kind_{TOPLEVEL_ = 1, NONE_, STD_, FILE_, MACRO_,
         STRING_, FUNCTION_, INCLUDE_, USER_};
enum eofAction{error_, eof_code_, reset_};
enum repos_{default_, true_, false_};
enum eoStream{at_, past_, no_};
enum stream_{stdin_, stdout_, stderr_, stdfunction_, file_};
enum origin_{beginning_, current_, end_};
enum char_type_{ byte_, ascii_, little_endian_, big_endian_,
         little_endian_no_tag_, big_endian_no_tag_};

// Repositioning text files poses a problem, as we read them
// a line at a time.  So a position is really the stream position
// plus the offset into the buffer.  Why reposition text files?
// Ray has some interesting applications, including Joli and the
// way the compiler handles discontiguous clauses.
//
// Stream Position needs three parameters for text input:
// line - the current line number (saved as a real tell when first read)
// col - the column the stream thinks it's at
// offset - the column the associated Prolog reader thinks it's at
//
// These are needed for accurately repositioning a text file to
// the state it was at when the last read occurred at that position.
//
// Note: on binary files, the C++ standard says there is a get and
// a put position, so we support both, but it turns out they are
// one and the same, at least in the Windows implementation.
// So when you set one, you set the other; when you read, you move
// the write position as well.

class StreamPosition
{
public:
   type_ type;

   intC line;
   intC col;
   intC offset;

   // need these two boolean to determine if the user actually
   // set one or both, can't use -1 default since its a valid
   // setting
   bool b_read;
   bool b_write;
   int readpos;
   int writepos;
   std::ios::seekdir origin;

   StreamPosition()
   { init(); }
   StreamPosition(LEngine *, TERM t);
   void init()
   { line = 0; col = 0; offset = 0;
     readpos = 0; writepos = 0; origin = std::ios::beg;
     b_read = false; b_write = false; }

   TERM makeTerm(LEngine *);

   // fseek uses enum origin_ which we map to ios::seekdir
   void setOrigin(origin_);
};

// Prolog text stream I/O is on a line-by-line basis.
// This is what the standard recommends, and it is also
// necessary for handling multibyte input files.  A full
// line must first be read and then converted to wide
// characters.  The lines are terminated by '/n' for use
// by the lexical parser.
class PrologStream
{
friend class IO;

protected:
   // Basic Prolog Stream attributes
   LEngine*  m_peng;
   PATOM     aliasA;
   mode_     mode;
   type_     type;
   char_type_ ctype;  // all the Unicode flavors
   bool      reposition;
   eofAction eof_action;
   kind_     kind;
   eoStream  end_of_stream;
   intC       id;     // the number by which IO knows this stream

   // Line buffers used by text streams
   aCHAR *current_line;
   char *mbs_buffer;
   aCHAR *col;
   intC line_no;
   StreamReader *stream_reader;
   intC current_line_file_position;

public:
   // because of preprocessor, a stream might be nested
   STREF     parent;   

public:
   PrologStream(LEngine* peng);
   PrologStream(LEngine* peng, PATOM alias, mode_, type_, bool repos, 
					 eofAction);
   virtual ~PrologStream();

private:
   void init();

public:
   void setAlias(PATOM a)
   { aliasA = a; }
   void setMode(mode_ m)
   { mode = m; }
   void setType(type_ t)
   { type = t; }
   void setCharType(char_type_ ct)
   { ctype = ct; }
   void setReposition(bool r)
   { reposition = r; }
   void setEOFAction(eofAction a)
   { eof_action = a; }
   void setID(int i)
   { id = i; }
   // here to support #line preprocessor directive
   void setLineNo(int i)
   { line_no = i; }
   void setStreamReader(StreamReader *sr)
   { stream_reader = sr; }

   void bump_eos()
   { end_of_stream = ((end_of_stream == no_) ? at_ : past_); }
   void position_eos()
   { end_of_stream = no_; };

   bool isInput()
   { return (mode == read_ || mode == readwrite_ ||
         mode == readappend_); }
   bool isOutput()
   { return (mode == readwrite_ || mode == readappend_ ||
         mode == write_ || mode == append_); }
   bool canReposition()
   { return reposition; }
   bool isFile()
   { return kind == FILE_; }
   bool isBinary()
   { return type == binary_; }
   bool isText()
   { return type == text_ || type == wide_text_; }
   bool isUnicode()
   { return (type == wide_text_); }
   bool isTaggedUnicode()
   { return (ctype == little_endian_ || ctype == big_endian_); }
   bool isEndOfStream()
   { return (end_of_stream == at_ || end_of_stream == past_); }
   bool hasAlias()
   { return aliasA != pATAB->emptyA; }
   bool isOtherEndian()  // i.e., different endian than this machine
   {
#ifdef BIG__ENDIAN
      return (ctype == little_endian_ || ctype == little_endian_no_tag_);
#else
      return (ctype == big_endian_ || ctype == big_endian_no_tag_);
#endif
   }

   eoStream getEndOfStreamStatus()
   { return end_of_stream; }
   PATOM getAlias()
   { return aliasA; }
   mode_ getMode()
   { return mode; }
   type_ getType()
   { return type; }
   char_type_ getCharType()
   { return ctype; }
   intC getLineNo()
   { return line_no; }
   intC getCol()
   { return (intC)(col - current_line + 1); }
   aCHAR *getCurrentLine()
   { return current_line; }
   StreamReader *getStreamReader()
   { return stream_reader; }

   void clearCurrentLine()
   { col = current_line; *col = EOS; }

   // This is only used by scanNumber which wants to go back
   // when an integer is too big.
   void setCol(int c)
   { col = current_line + c - 1; }

   // Prolog Stream Interface
public:
   virtual bool eof() = 0;
   virtual bool open() = 0;
   virtual void close() = 0;
   virtual void set_position(StreamPosition) = 0;
   virtual StreamPosition get_position() = 0;
   virtual aCHAR get_char() = 0;
   virtual void unget_char() = 0;
   virtual void put_char(aCHAR c) = 0;
   virtual void put_string(STRptr s) = 0;
   virtual void get_line(STRptr s, int n) = 0;
   virtual void read(char *buf, int n) = 0;
   virtual void write(char *buf, int n) = 0;
   virtual void flush() = 0;
   virtual PATOM get_name() = 0;

#ifdef LANDFILL
   void Dump();
#endif
};

class stdinPStream: public PrologStream
{
   bool b_tty_initialized;

public:
   stdinPStream(LEngine* peng) : PrologStream(peng)
   {  mode = read_;
      type = text_;
      reposition = false;
      eof_action = reset_;
      aliasA = pATAB->userInA;
      kind = STD_;
      b_tty_initialized = false; }

   // PrologStream Interface Implementation
public:
   bool eof() { return true;}
   bool open() { STUB(aS("open")); return false;}
   void close() {};
   void set_position(StreamPosition) { BADSOP(aS("position")); }
   StreamPosition get_position() { BADSOP(aS("position")); return StreamPosition(); }
   aCHAR get_char();
   void unget_char() { STUB(aS("unget_char")); }
   void put_char(aCHAR c) { STUB(aS("put_char")); }
   void put_string(STRptr s) { STUB(aS("put_string")); }
   void get_line(STRptr s, int n);
   void read(char *buf, int n) { STUB(aS("read")); }
   void write(char *buf, int n) { STUB(aS("write")); }
   void flush()
   { clearCurrentLine(); }
   PATOM get_name() { return aliasA; }
};

class stdoutPStream: public PrologStream
{
public:
   stdoutPStream(LEngine* peng) : PrologStream(peng)
   {  mode = append_;
      type = text_;
      reposition = false;
      eof_action = reset_;
      aliasA = pATAB->userOutA;
      kind = STD_; }

   // PrologStream Interface Implementation
public:
	bool eof() { return true;}
   bool open() { STUB(aS("open")); return false; }
   void close() {};
   void set_position(StreamPosition) { BADSOP(aS("position")); }
   StreamPosition get_position() { BADSOP(aS("position")); return StreamPosition(); }
   aCHAR get_char() { STUB(aS("get_char")); return 0;}
   void unget_char() { STUB(aS("unget_char")); }
   void put_char(aCHAR c);
   void put_string(STRptr s);
   void get_line(STRptr s, int n) { STUB(aS("get_line")); }
   void read(char *buf, int n) { STUB(aS("read")); }
   void write(char *buf, int n) { STUB(aS("write")); }
   void flush()
   { std::cout.flush(); }
   PATOM get_name() { return aliasA; }
};

class stderrPStream: public PrologStream
{
public:
   stderrPStream(LEngine* peng) : PrologStream(peng)
   {  mode = append_;
      type = text_;
      reposition = false;
      eof_action = reset_;
      aliasA = pATAB->userErrA;
      kind = STD_; }

   // PrologStream Interface Implementation
public:
	bool eof() { return true;}
   bool open() { STUB(aS("open")); return false; }
   void close() {};
   void set_position(StreamPosition) { BADSOP(aS("position")); }
   StreamPosition get_position() { BADSOP(aS("position")); return StreamPosition(); }
   aCHAR get_char() { STUB(aS("get_char")); return 0;}
   void unget_char() { STUB(aS("unget_char")); }
   void put_char(aCHAR c) { STUB(aS("put_char")); }
   void put_string(STRptr s) { STUB(aS("put_string")); }
   void get_line(STRptr s, int n) { STUB(aS("get_line")); }
   void read(char *buf, int n) { STUB(aS("read")); }
   void write(char *buf, int n) { STUB(aS("write")); }
   void flush() { STUB(aS("flush")); }
   PATOM get_name() { return aliasA; }
};

class FilePStream: public PrologStream
{
private:
   std::fstream  sio;
   PATOM         nameA;

public:
   FilePStream(LEngine* peng, PATOM name) : PrologStream(peng)
   { kind = FILE_; nameA = name; }
   ~FilePStream() {};

   // PrologStream Interface Implementation
public:
	bool eof()
	  { return sio.eof();}
   bool open();
   void close()
   { sio.close(); }
   void set_position(StreamPosition);
   StreamPosition get_position();
   aCHAR get_char();
   void unget_char() { STUB(aS("unget_char")); }
   void put_char(aCHAR c); 
   void put_string(STRptr s);
   void get_line(STRptr s, int n);
   void read(char *buf, int n);
   void write(char *buf, int n);
   void flush() 
   { sio.flush(); }
   PATOM get_name() { return nameA; }

private:
   void next_line();
};

class FunctionPStream: public PrologStream
{
public:
   // IO function pointers from lsapi via IO functions
   VOIDptr    m_xarg;   // extra argument used by I/O callbacks
   pX_GETC    m_xfgetc;
   pX_UNGETC  m_xfungetc;
   pX_PUTSA   m_xfputsA;
#ifdef _UNICODE
   pX_PUTSW   m_xfputsW;
#endif
   pX_PUTC    m_xfputc;

public:
   FunctionPStream(LEngine* peng);
   ~FunctionPStream() {};

   // PrologStream Interface Implementation
public:
	bool eof() { STUB(aS("eof")); return false; }
   bool open() { BADSOP(aS("open a function stream")); return false; }
   void close() {};
   void set_position(StreamPosition) { BADSOP(aS("position a function stream")); }
   StreamPosition get_position() { BADSOP(aS("position a function stream")); return StreamPosition(); }
   aCHAR get_char();
   void unget_char();
   void put_char(aCHAR c);
   void put_string(STRptr s);
   void get_line(STRptr s, int n);
   void read(char *buf, int n) { BADSOP(aS("binary read")); }
   void write(char *buf, int n) { BADSOP(aS("binary write")); }
   void flush() {};
   PATOM get_name() { return aliasA; }

private:
   void next_line();

#ifdef LANDFILL
public:
   void Dump();
#endif
};

class StringPStream: public PrologStream
{
private:
   int m_length;
   aCHAR *external_output;  // output directly to callers string

public:
   StringPStream(LEngine *peng, STRptr s, mode_ m, int length, bool add_period);
   ~StringPStream();

   // PrologStream Interface Implementation
public:
	bool eof() { STUB(aS("eof")); return false; }
   bool open() { BADSOP(aS("open")); return false;}
   void close() {};
   void set_position(StreamPosition) { BADSOP(aS("set_position")); }
   StreamPosition get_position() { BADSOP(aS("get_position")); return StreamPosition(); }
   aCHAR get_char();
   void unget_char();
   void put_char(aCHAR c);
   void put_string(STRptr s);
   void get_line(STRptr s, int n);
   void read(char *buf, int n) { BADSOP(aS("read")); }
   void write(char *buf, int n) { BADSOP(aS("write")); }
   void flush() { BADSOP(aS("flush")); }
   PATOM get_name() { return aliasA; }
};

class UserPStream: public PrologStream
{
public:
   // IO function pointers from lsapi via IO functions
   paUSER_GET_LINE   mfa_getline;
   paUSER_PUT_STRING mfa_putstring;
#ifdef _UNICODE
   pwUSER_GET_LINE   mfw_getline;
   pwUSER_PUT_STRING mfw_putstring;
#endif
   VOIDptr  m_ioarg;

public:
   UserPStream(LEngine* peng);
   ~UserPStream() {};

   // PrologStream Interface Implementation
public:
	bool eof() { STUB(aS("eof")); return false; }
   bool open() { BADSOP(aS("open a function stream")); return false; }
   void close() {};
   void set_position(StreamPosition) { BADSOP(aS("position a function stream")); }
   StreamPosition get_position() { BADSOP(aS("position a function stream")); return StreamPosition(); }
   aCHAR get_char();
   void unget_char();
   void put_char(aCHAR c);
   void put_string(STRptr s);
   void get_line(STRptr s, int n);
   void read(char *buf, int n) { BADSOP(aS("binary read")); }
   void write(char *buf, int n) { BADSOP(aS("binary write")); }
   void flush() {};
   PATOM get_name() { return aliasA; }

private:
   void next_line();

#ifdef LANDFILL
public:
   void Dump();
#endif
};


class IO
{
private:
   LEngine *m_peng;

   STREF std_in;
   STREF std_out;
   STREF std_err;
   STREF std_function;

   int current_input;
   int current_output;
   int current_error;

   int user_input;
   int user_output;
   int user_error;

   //int current_log;

public:
   std::vector<STREF> stream;

public:
   IO(LEngine *);
   ~IO();
   void Init();
   void Reset();

   int makeStdStream(stream_);
   int makeFileStream(PATOM name, PATOM alias, mode_ m, type_ t, repos_ repos, eofAction ea);
   int makeStringStream(STRptr s, mode_ m, int length, bool add_period);

   void CloseStreams();

   int add_open_stream(STREF s);
   void remove_stream(int h);
   int streamIndex(TERM);
   char_type_ figureCharType(PATOM name);
   //bool pop_stream(int h);
   //void push_stream(int h, STREF strm);

   // called from lsapi
   void set_stream(int iStream, int j);
   int get_stream(int iStream);
   void SetGetC(pX_GETC fpGetc)
   { ((FunctionPStream*)stream[3])->m_xfgetc = fpGetc; }
   void SetUngetC(pX_UNGETC fpUngetc)
   { ((FunctionPStream*)stream[3])->m_xfungetc = fpUngetc; }
   void SetPutC(pX_PUTC fpPutc)
   { ((FunctionPStream*)stream[3])->m_xfputc = fpPutc; }
   void SetPutS(pX_PUTSA fpPuts)
   { ((FunctionPStream*)stream[3])->m_xfputsA = fpPuts; }
#ifdef _UNICODE
   void SetPutS(pX_PUTSW fpPuts)
   { ((FunctionPStream*)stream[3])->m_xfputsW = fpPuts; }
#endif
   void SetIOArg(VOIDptr vp)
   { ((FunctionPStream*)stream[3])->m_xarg = vp; }

   int OpenUserStream(char* alias, paUSER_GET_LINE ugl,
         paUSER_PUT_STRING ups, VOIDptr vp);
#ifdef _UNICODE
   int OpenUserStream(aCHAR* alias, pwUSER_GET_LINE ugl,
         pwUSER_PUT_STRING ups, VOIDptr vp);
#endif

   // Logging Support

   //void openLog(PATOM name);
   //void closeLog();
   //bool termWriteLog(TERM t);

#ifdef LANDFILL
   void DumpStreams();
#endif

private:
   // utilities for creating list items
   TERM bumpNiladic(TERM list, PATOM functr);
   TERM bumpMonadic(TERM list, PATOM functr, PATOM arg);
   TERM bumpMonadic(TERM list, PATOM functr, int arg);
   TERM bumpMonadic(TERM list, PATOM functr, TERM arg);


public:
   // Builtin Predicates

   TF p_cur_streams();
   TF p_cur_user();
   TF p_cur_input();
   TF p_cur_output();
   TF p_set_input();
   TF p_set_output();
   TF p_set_streampos();
   TF p_open();
   TF p_close();
   TF p_at_eos();
   TF p_geto();
   TF p_putc();
   TF p_fread();
   TF p_fseek();
   TF p_streamList();
   TF p_stream_props();
   TF p_flewrite();
   TF p_fflush();
   TF p_handle_name();
   TF p_read_binary();
   TF p_write_binary();
};

#endif















