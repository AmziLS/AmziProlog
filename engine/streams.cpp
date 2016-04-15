/***************************************************************************\
*
* streams.cpp -- Stream Definitions
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
*
\***************************************************************************/


#include <fstream>
#include <vector>
#include <cctype>
#include <string>
#include <typeinfo>
#include "inc.h"
#include "streams.h"

#ifdef LANDFILL
#define noBUG_IO
#define noBUG_USER_STREAM
#endif

// separate from landfill, for Avery
#define noBUG_FUNCTION_STREAM

//---------------------------------------
// StreamPosition
//

StreamPosition::StreamPosition(LEngine *m_peng, TERM posT)
// variables indicate no change to position parameter
{
   init();

   TERM t, t2;

   t = posT->dref();

   if (! t->IsStruct())
      throw LExcept(sysargE);

   t = t->getTerm();
   if (t->getAtom() == pATAB->binaryA && t->getArity() == 3)
   // binary(origin, readpos, writepos)
   {
      t++;   // first arg
      t2 = t->dref();
      if (! t2->IsAtom())
         throw LExcept(sysargE);
      else
      {
         if (t2->getAtom() == pATAB->beginningA)
            origin = std::ios::beg;
         else if (t2->getAtom() == pATAB->currentA)
            origin = std::ios::cur;
         else if (t2->getAtom() == pATAB->endA)
            origin = std::ios::end;
         else
            throw LExcept(sysargE);
      }

      t++;
      t2 = t->dref();
      if (t2->IsInt())
      {
         readpos = t2->getInt();
         b_read = true;
      }
      else if (! t2->IsVar())
         throw LExcept(sysargE);

      t++;
      t2 = t->dref();
      if (t2->IsInt())
      {
         writepos = t2->getInt();
         b_write = true;
      }
      else if (! t2->IsVar())
         throw LExcept(sysargE);
   }
   else if (t->getAtom() == pATAB->textA && t->getArity() == 5)
   // text(origin, linepos, stream_offset, reader_offset, writepos)
   {
      t++;   // first arg
      t2 = t->dref();
      if (! t2->IsAtom())
         throw LExcept(sysargE);
      else
      {
         if (t2->getAtom() == pATAB->beginningA)
            origin = std::ios::beg;
         else if (t2->getAtom() == pATAB->currentA)
            origin = std::ios::cur;
         else if (t2->getAtom() == pATAB->endA)
            origin = std::ios::end;
         else
            throw LExcept(sysargE);
      }

      t++;
      t2 = t->dref();
      if (t2->IsInt())
      {
         line = t2->getInt();
         b_read = true;
      }
      else if (! t2->IsVar())
         throw LExcept(sysargE);

      t++;
      t2 = t->dref();
      if (t2->IsInt())
         col = t2->getInt();
      else if (b_read || (!b_read && ! t2->IsVar()))
         throw LExcept(sysargE);

      t++;
      t2 = t->dref();
      if (t2->IsInt())
         offset = t2->getInt();
      else if (b_read || (!b_read && ! t2->IsVar()))
         throw LExcept(sysargE);

      t++;
      t2 = t->dref();
      if (t2->IsInt())
      {
         writepos = t2->getInt();
         b_write = true;
      }
      else if (! t2->IsVar())
         throw LExcept(sysargE);
   }
   else
      throw LExcept(sysargE);
}

TERM StreamPosition::makeTerm(LEngine *m_peng)
{
   TERM spT, t;

   if (type == text_)
   {
      // text(origin, linepos, stream_offset, reader_offset, writepos)
      spT = pHXL->heapGETN(7);
      t = spT;
      spT->setStruct(++t);
      t++->setFA(pATAB->textA, 5);
      switch(origin)
      {
      case std::ios::beg:
         t++->setAtom(pATAB->beginningA);
         break;
      case std::ios::cur:
         t++->setAtom(pATAB->currentA);
         break;
      case std::ios::end:
         t++->setAtom(pATAB->endA);
         break;
      default:
         throw LExcept(internalE, aS("unexpected file position origin"));
      }
      t++->setInt(line);
      t++->setInt(col);
      t++->setInt(offset);
      t->setInt(writepos);
   }
   else if (type == binary_)
   {
      // binary(origin, readpos, writepos)
      spT = pHXL->heapGETN(5);
      t = spT;
      spT->setStruct(++t);
      t++->setFA(pATAB->binaryA, 3);
      switch(origin)
      {
      case std::ios::beg:
         t++->setAtom(pATAB->beginningA);
         break;
      case std::ios::cur:
         t++->setAtom(pATAB->currentA);
         break;
      case std::ios::end:
         t++->setAtom(pATAB->endA);
         break;
      default:
         throw LExcept(internalE, aS("unexpected file position origin"));
      }
      t++->setInt(readpos);
      t->setInt(writepos);
   }
   else
      throw LExcept(internalE, aS("unexpected type in StreamPosition"));

   return spT;
}

   // fseek uses enum origin_ which we map to ios::seekdir
void StreamPosition::setOrigin(origin_ orig)
{
   switch(orig)
   {
   case beginning_:
      origin = std::ios::beg;
      break;
   case current_:
      origin = std::ios::cur;
      break;
   case end_:
      origin = std::ios::end;
      break;
   }
}

//-------------------------------------------------------



PrologStream::PrologStream(LEngine* peng)
{
   m_peng = peng;
   aliasA = pATAB->emptyA;
   mode = none_;
   type = text_;
   reposition = false;
   eof_action = reset_;

   init();
}

PrologStream::PrologStream(LEngine* peng,
      PATOM alias, mode_ m, type_ t, bool repos, eofAction ea)
{
   m_peng = peng;
   aliasA = alias;
   mode = m;
   type = t;
   reposition = repos;
   eof_action = ea;

   init();
}

PrologStream::~PrologStream()
{
   delete current_line;
   delete mbs_buffer;
   delete stream_reader;
}

void PrologStream::init()
{
   kind = NONE_;
   end_of_stream = no_;
   parent = NULL;
   id = -1;
   line_no = 0;

   LNEW(current_line, aCHAR[LINE_SIZE], aS("stream"));
   LNEW(mbs_buffer, char[LINE_SIZE], aS("stream"));
   col = current_line;
   *col = EOS;
   stream_reader = NULL;
   current_line_file_position = 0;
}

#ifdef LANDFILL
void PrologStream::Dump()
{
   DUMP << "name = " << get_name() << SP2 <<
      "type = " << getType() << NL;
}
#endif

aCHAR stdinPStream::get_char()
{
   if (! isInput())
      BADSOP(aS("input"));

   if (*col == EOS)
   {
      // needed for Linux, to set terminal backspace use, and to
      // make key$b work, but only do when stdin actually used,
      // otherwise it breaks Logic Server programs run in the background.
      if (! b_tty_initialized)
      {
         g_lenv.e_inittty();
         b_tty_initialized = true;
      }
      std::cin.getline(mbs_buffer, LINE_SIZE);
      // if running as an embedded application under Windows,
      // then cin just keeps returning empty strings, and it
      // appears the only way to tell is to just get a few.
      if (strlen(mbs_buffer) == 0)
      {
         int count = 7;
         while(strlen(mbs_buffer) == 0 && count > 0)
         {
            std::cin.getline(mbs_buffer, LINE_SIZE);
            count--;
         }
         if (count == 0)
            pXCPT->Error(no_cinE);
      }
      if ((int)strlen(mbs_buffer) > LINE_SIZE-2)
         pXCPT->Error(longlineE, LINE_SIZE, line_no);
      g_lenv.e_mbs_in(m_peng, current_line, mbs_buffer, LINE_SIZE);
      if (NULL != Lstrchr(current_line, BREAK_CHAR))  // break hit?
         pXCPT->Error(breakE);
      col = current_line;
   }

   if (*col == EOS)
      return 0;
   else
      return *col++;
}

void stdinPStream::get_line(STRptr s, int n)
{
   if (! isInput())
      BADSOP(aS("input"));

   if (*col == EOS)
   {
      // needed for Linux, to set terminal backspace use, and to
      // make key$b work, but only do when stdin actually used,
      // otherwise it breaks Logic Server programs run in the background.
      if (! b_tty_initialized)
      {
         g_lenv.e_inittty();
         b_tty_initialized = true;
      }
      std::cin.getline(mbs_buffer, LINE_SIZE);

      // if running as an embedded application under Windows,
      // then cin just keeps returning empty strings, and it
      // appears the only way to tell is to just get a few.
      // BUT this breaks programs that want to read an empty
      // string, so let the other bug lurk...
      /*
      if (strlen(mbs_buffer) == 0)
      {
         int count = 7;
         while(strlen(mbs_buffer) == 0 && count > 0)
         {
            std::cin.getline(mbs_buffer, LINE_SIZE);
            count--;
         }
         if (count == 0)
            pXCPT->Error(no_cinE);
      }
      */

      if ((int)strlen(mbs_buffer) > LINE_SIZE-2)
         pXCPT->Error(longlineE, LINE_SIZE, line_no);
      g_lenv.e_mbs_in(m_peng, current_line, mbs_buffer, LINE_SIZE);
      if (NULL != Lstrchr(current_line, BREAK_CHAR))  // break hit?
         pXCPT->Error(breakE);
      col = current_line;
   }

   int i;
   STRptr ss = s;
   for (i = 0; i < n - 2, *col != EOS; i++)
      *ss++ = *col++;

   *ss = EOS;
}

void stdoutPStream::put_char(aCHAR c)
{
   int len = wctomb(mbs_buffer, c);
   if (len > 0)
      std::cout.write(mbs_buffer, len);
}

void stdoutPStream::put_string(STRptr s)
{
   if ((int)g_lenv.e_mbs_out(m_peng, mbs_buffer, s, LINE_SIZE) >= LINE_SIZE)
      mbs_buffer[LINE_SIZE-1] = 0;
   std::cout << mbs_buffer;
   return;
}

bool FilePStream::open()
{
   char *aname;
   LPathString paname = (LPathString)*nameA;
   paname.TiltSlashes();
   //aname = g_lenv.new_mbs_dup(*nameA);
   aname = g_lenv.new_mbs_dup(paname);
   std::ios::openmode om;
   aCHAR c;
   aCHAR tag = 0xfeff;
   //bool exists;

   if (isInput() && isOutput())
      om = std::ios::in | std::ios::out;
   else if (isInput())
      om = std::ios::in;
   else if (isOutput())
      om = std::ios::out;

   if (isBinary() || isUnicode())
      om |= std::ios::binary;

   if (mode == append_)
      om |= std::ios::app;

   sio.open(aname, om);
   // if it didn't open, and its a read/write, then create it
   if ( ! sio.is_open() && isInput() && isOutput() )
   {
      om |= std::ios::trunc;
      sio.open(aname, om);
   }
   delete aname;

   if (isTaggedUnicode() && isInput())
      sio.read((char*)&c, 2);  // skip the tag
   if (isUnicode() && isOutput() && mode != append_)
      sio.write((char*)&tag, 2);  // add a tag

	return sio.is_open();
}

void FilePStream::get_line(STRptr s, int n)
{
   if (! isInput())
      BADSOP(aS("input"));

   if (*col == EOS)
      next_line();

   int i;
   STRptr ss = s;
   for (i=0; i < n-2, *col != EOS; i++)
      *ss++ = *col++;

   *ss = EOS;
}

aCHAR FilePStream::get_char()
{
   if (! isInput())
      BADSOP(aS("input"));

   if (*col == EOS)
   {
      if (sio.eof())
      {
         bump_eos();
         return LEOF;
      }
      else
         next_line();
   }

   return *col++;
}


void FilePStream::set_position(StreamPosition sp)
{
   //if (!isBinary())
   //   BADSOP(aS("position non-binary file"));
   //if (isEndOfStream())
   //   BADSOP(aS("Attempt to position file after end_of_file reached"));

   if (isText())   // includes wide text
   {
      if (isInput() && sp.b_read)
      {
         sio.seekg(sp.line, sp.origin);
         next_line();
         if (getStreamReader())
         {
            getStreamReader()->getLine();
            getStreamReader()->setCol(sp.offset);
         }
      col = current_line + sp.col;
      }
      if (isOutput() && sp.b_write)
         sio.seekp(sp.writepos, sp.origin);
   }
   else
   {
      if (isInput() && sp.b_read)
         sio.seekg(sp.readpos, sp.origin);
      if (isOutput() && sp.b_write)
         sio.seekp(sp.writepos, sp.origin);
   }

   position_eos();
}

StreamPosition FilePStream::get_position()
{
   //if (!isBinary())
   //   BADSOP(aS("position non-binary file"));
   StreamPosition sp;
   sp.origin = std::ios::beg;

   if (isText())
   // includes unicode as text, even though opened as binary
   {
      sp.type = text_;
      if (isInput())
      {
         sp.line = current_line_file_position;
         sp.col = (intC)(col - current_line);
         if (getStreamReader())
            sp.offset = getStreamReader()->getCol();
         else
            sp.offset = 0;
      }
      if (isOutput())
         sp.writepos = (intC)sio.tellp();
   }
   else
   {
      sp.type = binary_;
      if (isInput())
         sp.readpos = (intC)sio.tellg();
      if (isOutput())
         sp.writepos = (intC)sio.tellp();
   }

   return sp;
}

void FilePStream::put_char(aCHAR c)
{
   switch (ctype)
   {
   case big_endian_:
   case big_endian_no_tag_:
   case little_endian_:
   case little_endian_no_tag_:
      sio.write((char*)&c, 2);
      break;
   case ascii_:
   default:
      int len = wctomb(mbs_buffer, c);
      if (len > 0)
         sio.write(mbs_buffer, len);
   }
   return;
}

void FilePStream::put_string(STRptr s)
{
   aCHAR *p = s;
   aCHAR c;

   // output will always be in correct endian
   // for the platform

   switch (ctype)
   {
   case big_endian_:
   case big_endian_no_tag_:
   case little_endian_:
   case little_endian_no_tag_:
      while (*p != EOS)
      {
         if (*p == aS('\n'))
         {
#ifdef WINDOWS
            c = 0x000d;
            sio.write((char*)&c, 2);
#endif
            c = 0x000a;
            sio.write((char*)&c, 2);
         }
         else
            sio.write((char*)p, 2);
         p++;
      }
      break;
   case ascii_:
   default:
      if ((int)g_lenv.e_mbs_out(m_peng, mbs_buffer, s, LINE_SIZE) >= LINE_SIZE)
         mbs_buffer[LINE_SIZE-1] = 0;
      sio << mbs_buffer;
   }
   return;
}

void FilePStream::read(char *buf, int n)
{
   if (!isBinary())
      BADSOP(aS("binary read of non-binary file"));
   if (!isInput())
      BADSOP(aS("input"));

   sio.read(buf, n); 
}

void FilePStream::write(char *buf, int n)
{
   if (!isBinary())
      BADSOP(aS("binary write of non-binary file"));
   if (!isOutput())
      BADSOP(aS("output"));

   sio.write(buf, n);
}

void FilePStream::next_line()
{
   aCHAR *p = current_line;
   intC len = 0;
   aCHAR c;

   // in case we want to go back
   current_line_file_position = (intC)sio.tellg();
   // there was some real strangeness here, but it seems to
   // have gone away, tell was bumping the position by one...
   // so this test
   if (isUnicode())
   {
      LASSERT((0 == current_line_file_position % 2), aS("odd tell in unicode file"));
   }

   if (isEndOfStream())
   {
      current_line[0] = EOS;
      col = current_line;
      return;
   }

   // input might be in other endian from
   // this platform
   bool other_endian = isOtherEndian();

   switch (ctype)
   {
   case big_endian_:
   case big_endian_no_tag_:
   case little_endian_:
   case little_endian_no_tag_:
      // note that some compilers (gcc) represent wide chars as full
      // 4 byte ints, so when we read 2 bytes, we really need
      // to ensure the other were 0.
      *p = 0;
      sio.read((char*)p, 2);
      if (other_endian) FlipEndian16(*p);
      if (sio.eof())
      {
         current_line[0] = EOS;
         col = current_line;
         bump_eos();
         return;
      }
      while (!sio.eof() && *p != 0x000a && *p != 0x000d && *p != 0x2028)
      {
         p++;
         if (++len > LINE_SIZE-4)
            pXCPT->Error(longlineE, LINE_SIZE, line_no);
         *p = 0;
         sio.read((char*)p, 2);
         if (other_endian) FlipEndian16(*p);
      }
      if (*p == 0x000d)
      {
         sio.read((char*)&c, 2);
         if (other_endian) FlipEndian16(c);
         if (c != 0x000a)    // Unix
            sio.seekg(-2, std::ios::cur);
      }
      *p++ = aS('\n');
      *p = EOS;
      line_no++;
      break;
   case ascii_:
   default:
      sio.getline(mbs_buffer, LINE_SIZE);
      len = (intC)strlen(mbs_buffer);
      if (len > LINE_SIZE-3)
         pXCPT->Error(longlineE, LINE_SIZE, line_no);
      mbs_buffer[len] = '\n';
      mbs_buffer[len+1] = EOS;
      g_lenv.e_mbs_in(m_peng, current_line, mbs_buffer, LINE_SIZE);
      line_no++;
   }

   if (sio.eof())
      bump_eos();
   col = current_line;

   return;
}

FunctionPStream::FunctionPStream(LEngine* peng) : PrologStream(peng)
{
   // common stuff
   mode = readwrite_;
   type = text_;
   reposition = false;
   eof_action = reset_;
   aliasA = pATAB->userFuncA;
   kind = FUNCTION_;

   m_xfgetc = NULL;
   m_xfungetc = NULL;
   m_xfputsA = NULL;
#ifdef _UNICODE
   m_xfputsW = NULL;
#endif
   m_xfputc = NULL;
}

aCHAR FunctionPStream::get_char()
{
#ifdef BUG_FUNCTION_STREAM
 LOG( "function get_char, buffer: " << current_line );
#endif
   if (*col == EOS)
      next_line();
#ifdef BUG_FUNCTION_STREAM
 LOG( "function get_char, returning: " << *col );
#endif
   return *col++;
}

void FunctionPStream::unget_char()
{
   if (col > current_line)
      col--;
}

void FunctionPStream::put_char(aCHAR c)
{
   if (! m_xfputc)
      BADSOP(aS("Attempt to use uninitialized function stream"));
   (*m_xfputc)(m_xarg, c);
}

void FunctionPStream::put_string(STRptr s)
{
#ifdef _UNICODE
      if (m_xfputsW )
         (*m_xfputsW)(m_xarg, s);
      else
      {
         if (! m_xfputsA)
            BADSOP(aS("Attempt to use uninitialized function stream"));
         int len = (intC)Lstrlen(s)+1;
         char *sa;
         LNEW(sa, char[len*2], aS("temporary"));
         g_lenv.e_mbs_out(m_peng, sa, s, len*2);
         (*m_xfputsA)(m_xarg, sa);
         delete sa;
      }
#else
      if (! m_xfputsA)
         BADSOP(aS("Attempt to use uninitialized function stream"));
      (*m_xfputsA)(m_xarg, s);
#endif
}

void FunctionPStream::get_line(STRptr s, int n)
{
   if (*col == EOS)
      next_line();

   int i;
   STRptr ss = s;
   for (i=0; i < n-2, *col != EOS; i++)
      *ss++ = *col++;

   *ss = EOS;
}

void FunctionPStream::next_line()
{
#ifdef BUG_FUNCTION_STREAM
 LOG( "function next_line" );
#endif
   if (!m_xfgetc)
      BADSOP(aS("Attempt to use uninitialized function stream"));
   int i = 0;
   current_line[i] = (aCHAR) (*m_xfgetc)(m_xarg);
#ifdef BUG_FUNCTION_STREAM
 LOG( "function next_line, character [" << i << "] read: " << current_line[i] );
#endif

   while(current_line[i] != '\n' && i < LINE_SIZE-2)
   {
      current_line[++i] = (aCHAR) (*m_xfgetc)(m_xarg);
#ifdef BUG_FUNCTION_STREAM
 LOG( "function next_line, character [" << i << "] read: " << current_line[i] );
#endif
   }

   if (i >= LINE_SIZE-2)
      pXCPT->Error(longlineE, LINE_SIZE, line_no);

   current_line[++i] = '\n';
   current_line[++i] = EOS;
   if (NULL != Lstrchr(current_line, BREAK_CHAR))  // break hit?
      pXCPT->Error(breakE);
#ifdef BUG_FUNCTION_STREAM
 LOG( "function next_line, new buffer: " << current_line );
#endif
   col = current_line;
}

#ifdef LANDFILL
void FunctionPStream::Dump()
{
   DUMP << "name = " << get_name() << SP2 <<
      "type = " << getType() << NL;
   DUMP << SP2 << "fgetc = " << m_xfgetc << NL;
#ifdef _UNICODE
   DUMP << SP2 << "fputs = " << m_xfputsW << NL;
#else
   DUMP << SP2 << "fputs = " << m_xfputsA << NL;
#endif
}
#endif

// User Streams

UserPStream::UserPStream(LEngine* peng) : PrologStream(peng)
{
   // common stuff
   mode = readwrite_;
   type = text_;
   reposition = false;
   eof_action = reset_;
   kind = USER_;

   mfa_getline = NULL;
   mfa_putstring = NULL;
#ifdef _UNICODE
   mfw_getline = NULL;
   mfw_putstring = NULL;
#endif
}

aCHAR UserPStream::get_char()
{
   if (! isInput())
      BADSOP(aS("input"));

#ifdef BUG_USER_STREAM
 DUMP << "user get_char, buffer: " << current_line ;
#endif
   if (*col == EOS)
      next_line();
#ifdef BUG_USER_STREAM
 DUMP << "user get_char, returning: " << *col ;
#endif
   return *col++;
}

void UserPStream::unget_char()
{
   if (! isInput())
      BADSOP(aS("input"));

   if (col > current_line)
      col--;
}

void UserPStream::put_char(aCHAR c)
{
   if (! isOutput())
      BADSOP(aS("output"));

   aCHAR s[2];
   s[0] = c;
   s[1] = EOS;

   put_string(s);
}

void UserPStream::put_string(STRptr s)
{
   if (! isOutput())
      BADSOP(aS("output"));

#ifdef _UNICODE
      if (mfw_putstring)
         (*mfw_putstring)(m_ioarg, s);
      else
      {
         if (! mfa_putstring)
            BADSOP(aS("Attempt to use uninitialized function stream"));
         intC len = (intC)Lstrlen(s)+1;
         char *sa;
         LNEW(sa, char[len*2], aS("temporary"));
         g_lenv.e_mbs_out(m_peng, sa, s, len*2);
         (*mfa_putstring)(m_ioarg, sa);
         delete sa;
      }
#else
      if (! mfa_putstring)
         BADSOP(aS("Attempt to use uninitialized function stream"));
      (*mfa_putstring)(m_ioarg, s);
#endif
}

void UserPStream::get_line(STRptr s, int n)
{
   if (! isInput())
   BADSOP(aS("output"));

   if (*col == EOS)
      next_line();

   int i;
   STRptr ss = s;
   for (i=0; i < n-2, *col != EOS; i++)
      *ss++ = *col++;

   *ss = EOS;
}

void UserPStream::next_line()
{
   intC len;
   aCHAR* wline;
   char*  aline;
#ifdef _UNICODE
   if (mfw_getline)
   {
      wline = (*mfw_getline)(m_ioarg);
      len = (intC)Lstrlen(wline);
      if (len >= LINE_SIZE-2)
         pXCPT->Error(longlineE, LINE_SIZE, line_no);
      Lstrcpy(current_line, wline);
   }
   else if (mfa_getline)
   {
      aline = (*mfa_getline)(m_ioarg);
      len = (intC)strlen(aline);
      if (len >= LINE_SIZE-2)
         pXCPT->Error(longlineE, LINE_SIZE, line_no);
      g_lenv.e_mbs_in(m_peng, current_line, aline, len);
   }
   else
      BADSOP(aS("Attempt to use uninitialized user stream"));
#else
   if (mfa_getline)
   {
      aline = (*mfa_getline)(m_ioarg);
      len = strlen(aline);
      if (len >= LINE_SIZE-2)
         pXCPT->Error(longlineE, LINE_SIZE, line_no);
      Lstrcpy(current_line, aline, len);
   }
   else
      BADSOP(aS("Attempt to use uninitialized user stream"));
#endif

   current_line[len] = '\n';
   current_line[len+1] = EOS;

#ifdef BUG_USER_STREAM
 DUMP << "user next_line, new buffer: " << NL;
 aCHAR* cl_ptr = current_line;
 while(*cl_ptr != 0)
 {
    DUMP << (int)(*cl_ptr) << NL;
    cl_ptr++;
 }
 DUMP << "----------";
#endif

 if (NULL != Lstrchr(current_line, BREAK_CHAR))  // break hit?
      pXCPT->Error(breakE);
   col = current_line;
   line_no++;

   return;
}

// String Streams, internal use only

StringPStream::StringPStream(LEngine* peng, STRptr s, mode_ m, int length, bool add_period)
      : PrologStream(peng)
{
   // common stuff
   if (! (m == read_ || m == write_))
      BADSOP(aS("string stream not read or write"));
   mode = m;
   type = text_;
   reposition = false;
   eof_action = reset_;
   aliasA = pATAB->stringA;
   kind = STRING_;
   // string stuff
   m_length = length;
   if (isInput())
   {
      intC len = (intC)Lstrlen(s);
      if (len > m_length - 2)
         BADSOP(aS("string stream larger than read buffer"));
      Lstrcpy(current_line, s);
      if (add_period)
      {
         int i;
         for (i=len; i>0, current_line[i] <= 32; i--) ;  // last non-white char
         //if (current_line[i] != '.')
         if (true)  // the extra . doesn't seem to hurt, and lets =.. work
         {
            current_line[i+1] = ' ';  // space, so that '*'. and '=..' work OK
            current_line[i+2] = '.';
            current_line[i+3] = EOS;
         }
      }
      col = current_line;
   }
   else
   {
      // redirect output stream to caller's
      delete current_line;
      current_line = s;
      col = current_line;
   }
}

StringPStream::~StringPStream()
{
   // output string was caller's responsibility.
   if (isOutput())
      current_line = NULL;
}

aCHAR StringPStream::get_char()
{
   if (! isInput())
      BADSOP(aS("input"));

   if (*col == EOS)
   {
      bump_eos();
      return LEOF;
   }
   else
      return *col++;
}

void StringPStream::unget_char()
{
   if (! isInput())
      BADSOP(aS("input"));

   if (col > current_line && *col != EOS)
      col--;
}

void StringPStream::get_line(STRptr s, int n)
{
   if (! isInput())
      BADSOP(aS("input"));

   int i;
   STRptr ss = s;
   for (i=0; i < n-2, *col != EOS; i++)
      *ss++ = *col++;

   *ss = EOS;
   bump_eos();
}

void StringPStream::put_char(aCHAR c)
{
   if (! isOutput())
      BADSOP(aS("output"));

   if (isEndOfStream())
      return;

   if (getCol() >= m_length)
   {
      *col = EOS;
      bump_eos();
   }
   else
      *col++ = c;
}

void StringPStream::put_string(STRptr s)
{
   if (! isOutput())
      BADSOP(aS("output"));

   if (isEndOfStream())
      return;

   intC l = (intC)Lstrlen(s);
   if (l >= (m_length - getCol()))
   {
      l = m_length - getCol();  // getCol() starts at 1, not 0
      Lstrncpy(col, s, l);
      col += l;
      *col = EOS;
      bump_eos();
      //BADSOP(aS("write string overflows buffer"));
   }
   else
   {
      Lstrcpy(col, s);
      col += l;
   }
}

IO::IO(LEngine * peng)
{
  m_peng  = peng;
  std_in  = NULL;
  std_out = NULL;
  std_err = NULL;
  std_function = NULL;
}

void IO::Init()
{
   if (stream.size() == 0)
   {
      if (!std_in) 
		  makeStdStream(stdin_);
      if (!std_out) 
		  makeStdStream(stdout_);
      if (!std_err) 
		  makeStdStream(stderr_);
      if (!std_function) 
		  makeStdStream(stdfunction_);

      std_in  = stream[0];
      std_out = stream[1];
      std_err = stream[2];
      std_function = stream[3];

      std_in->setID(0);
      std_out->setID(1);
      std_err->setID(2);
      std_function->setID(3);
   }

  //LNEW(std_in, STDINSTREAM(m_peng, pATAB->stdinA, pATAB->stdinA, read_, 
  //                         text_, pLEX->bufSize), aS("initialization"));
  //std_in->kind = TOPLEVEL_;
  //LNEW(std_out, STDOUTSTREAM(m_peng, pATAB->stdoutA, pATAB->stdoutA, write_, 
  //                               text_, pLEX->bufSize), aS("initialization"));
  //std_out->kind = TOPLEVEL_;
  //LNEW(std_err, OSTREAM(m_peng, pATAB->stderrA, pATAB->stderrA, write_, 
  //              text_, pLEX->bufSize), aS("initialization"));
  //std_err->kind = TOPLEVEL_;
  //LNEW(std_function, FUNCTIONSTREAM(m_peng, pATAB->functionA, pATAB->functionA, readwrite_, 
  //               text_, pLEX->bufSize), aS("initialization"));
  //std_function->kind = FUNCTION_;

//  g_lenv.e_inittty();  // no, breaks Linux background, only do as needed in stdin
  user_input  = current_input  = 0;
  user_output = current_output = 1;
  user_error  = current_error  = 2;

  //current_log = -1;

  //bInitIO = TRUE;
}

IO::~IO()
{
   CloseStreams();
   g_lenv.e_resettty();    // in case set in Linux reading stdin
}

void IO::Reset()
{
   CloseStreams();
   Init();
#ifdef BUG_IO
   DUMP << "==> IO::Reset" << NL;
   DumpStreams();
   DUMP << "<== IO::Reset" << NL << FLUSH;
#endif
}


int IO::OpenUserStream(char* alias, paUSER_GET_LINE ugl,
            paUSER_PUT_STRING ups, VOIDptr ioarg)
{
   UserPStream* us;
   LNEW(us, UserPStream(m_peng), aS("stream"));

   us->m_ioarg = ioarg;
   if (alias)
      us->setAlias(pATAB->EnterAtom(alias));
   if (ugl)
   {
      us->mfa_getline = ugl;
      us->setMode(read_);
   }
   if (ups)
   {
      us->mfa_putstring = ups;
      us->setMode(write_);
   }
   if (ugl && ups)
      us->setMode(readwrite_);

   return add_open_stream((PrologStream*)us);
}
#ifdef _UNICODE
int IO::OpenUserStream(aCHAR* alias, pwUSER_GET_LINE ugl,
            pwUSER_PUT_STRING ups, VOIDptr ioarg)
{
   UserPStream* us;
   LNEW(us, UserPStream(m_peng), aS("stream"));

   us->m_ioarg = ioarg;
   if (alias)
      us->setAlias(pATAB->EnterAtom(alias));
   if (ugl)
   {
      us->mfw_getline = ugl;
      us->setMode(read_);
   }
   if (ups)
   {
      us->mfw_putstring = ups;
      us->setMode(write_);
   }
   if (ugl && ups)
      us->setMode(readwrite_);

   return add_open_stream(us);
}
#endif

// The stream factory
int IO::makeStdStream(stream_ stream_class)
{
   STREF  ps;

   switch(stream_class)
   {
   case stdin_:
      LNEW(ps, stdinPStream(m_peng), aS("stream"));
      break;
   case stdout_:
      LNEW(ps, stdoutPStream(m_peng), aS("stream"));
      break;
   case stderr_:
      LNEW(ps, stderrPStream(m_peng), aS("stream"));
      break;
   case stdfunction_:
      LNEW(ps, FunctionPStream(m_peng), aS("stream"));
      break;
   default:
      break;
   }

   return add_open_stream(ps);
}

int IO::makeFileStream(PATOM name, PATOM alias, mode_ mode, type_ type, 
							  repos_ repos, eofAction eofa)
{
   STREF  strmp;
   char_type_ ctype;

   LNEW(strmp, FilePStream(m_peng, name), aS("stream"));
   strmp->setAlias(alias);
   strmp->setMode(mode);

   switch(type)
   {
   case binary_:
      ctype = byte_;
      break;
   case wide_text_:
      if (mode == read_ || mode == readwrite_)
      {
         ctype = figureCharType(name);
         if (ctype == ascii_)
            ctype = little_endian_no_tag_;
      }
      else
      {
#ifdef BIG__ENDIAN
         ctype = big_endian_;
#else
         ctype = little_endian_;
#endif
      }
      break;
   case go_figure_:
      if (mode == read_ || mode == readwrite_)
      {
         ctype = figureCharType(name);
         if (ctype == ascii_)
            type = text_;
         else
            type = wide_text_;
      }
      else
      {
         ctype = ascii_;
         type = text_;
      }
      break;
   case text_:
   default:
      ctype = ascii_;
   }
   strmp->setCharType(ctype);

   strmp->setType(type);
   if (repos == false_)
      strmp->setReposition(false);
   else
      strmp->setReposition(true);
   strmp->setEOFAction(eofa);

   if (! strmp->open())
      pXCPT->Error(fopenE, (STRptr)(*name));

   int h = add_open_stream(strmp);
   strmp ->setID(h);

   return h;
}

int IO::makeStringStream(STRptr s, mode_ m, int length, bool add_period)
{
   STREF  strmp;

   LNEW(strmp, StringPStream(m_peng, s, m, length, add_period), aS("string_stream"));

   int h = add_open_stream(strmp);
   strmp ->setID(h);
   return h;
}

void IO::CloseStreams()
{
//   std::vector<STREF>::iterator p;
   for (size_t i=0; i < stream.size(); i++)
//   for (p = stream.begin(); p != stream.end(); p++)
   {                                        // put out entire string
      if(stream[i] != NULL)
      {
         stream[i]->close();
         delete stream[i];
         stream[i] = NULL;
      }
   }
   std_in = NULL;
   std_out = NULL;
   std_err = NULL;
   std_function = NULL;
   stream.clear();
}

void IO::set_stream(int iStream, int j)
{
   switch (iStream)
   {
   case CUR_IN:    current_input  = j;  break;
   case CUR_OUT:   current_output = j;  break;
   case CUR_ERR:   current_error  = j;  break;
   case USER_IN:   user_input     = j;  break;
   case USER_OUT:  user_output    = j;  break;
   case USER_ERR:  user_error     = j;  break;
   default:
      pXCPT->Error(badstreamE);
   }
}

int IO::get_stream(int iStream)
{
   int rv;

   switch (iStream)
   {
   case CUR_IN:    rv = current_input;  break;
   case CUR_OUT:   rv = current_output; break;
   case CUR_ERR:   rv = current_error;  break;
   case USER_IN:   rv = user_input;     break;
   case USER_OUT:  rv = user_output;    break;
   case USER_ERR:  rv = user_error;     break;
   default:
      pXCPT->Error(badstreamE);
   }

   return rv;
}

TF IO::p_cur_streams()
// used by see/tell to set/report on streams, along with cur_user.
{
   //TERM    stream_T[3];
   TERM  x0, x1, x2;
   //int     i, h; //, varstat[3];
   //PATOM   a;

   x0 = X(0);
   x1 = X(1);
   x2 = X(2);

   if (x0->IsRef())
      pTSVC->UnifyInt(current_input, pHXL->XVar(0));
   else if (x0->IsInt())
      current_input = x0->getInt();
   else
      pXCPT->Error(argE, 
         aS("stream argument must be var or integer"));

   if (x1->IsRef())
      pTSVC->UnifyInt(current_output, pHXL->XVar(1));
   else if (x1->IsInt())
      current_output = x1->getInt();
   else
      pXCPT->Error(argE, 
         aS("stream argument must be var or integer"));

   if (x2->IsRef())
      pTSVC->UnifyInt(current_error, pHXL->XVar(2));
   else if (x2->IsInt())
      current_error = x2->getInt();
   else
      pXCPT->Error(argE, 
         aS("stream argument must be var or integer"));

   return TRUE;
}

int IO::p_cur_user()
{
  TERM    stream_T[3];
  intC     i, varstat[3];
  intC  h;
  PATOM   a;

  for (i = 0; i < 3; i++)
    {
      stream_T[i] = (pHXL->XVar(i))->dref();
      varstat[i] = stream_T[i]->IsRef();
    }
  
  for (i = 0; i < 3; i++)
    {
      if (varstat[i])
          pTSVC->UnifyInt(i == 0 ? user_input : 
                          (i == 1 ? user_output : user_error),
                          pHXL->XVar(i));
      
      if (stream_T[i]->IsInt())
        {
          h = stream_T[i]->getInt();
        }
      else 
        if (stream_T[i]->IsAtom())
          {
            a = stream_T[i]->getAtom();
            for (h = 0; h < stream.size(); ++h)
              if (h >= stream.size()) 
                pXCPT->Error(unopenedE, (STRptr)*(a));
          }
        else 
          pXCPT->Error(argE, 
                     aS("stream argument must be var, integer, or atom"));
      
      switch(i)
        {
        case(0): user_input  = h; break;
        case(1): user_output = h; break;
        case(2): user_error  = h; break;
        }
    }
  
  return(TRUE);
}
  
TF IO::p_cur_input()
{
  TERM T = X(0);
  if(T->IsRef() || T->IsInt())
    return pTSVC->UnifyInt(current_input,  pHXL->XVar(0));
  return FALSE;
}

TF IO::p_cur_output()
{
  TERM T = (pHXL->XVar(0))->dref();
  if(T->IsRef() || T->IsInt())
    return pTSVC->UnifyInt(current_output,  pHXL->XVar(0)); 
  return FALSE;
}

TF IO::p_set_input()
{                                            // must specify an open stream
  int h;
  TERM T = X(0);

  h = streamIndex(T);

  if (stream[h]->isInput())
  {
     current_input = h;
     return TRUE;
  }
  else
     pXCPT->Error(setinputE, h);

  return FALSE;
}

TF IO::p_set_output()
{
  int h;
  TERM T = X(0);

  h = streamIndex(T);

  if (stream[h]->isOutput())
  {
     current_output = h;
     return TRUE;
  }
  else
     pXCPT->Error(setoutputE, h);

  return FALSE;
}

TF IO::p_set_streampos()
// set_stream_position(H, POS)
{
  int h;
  TERM t0 = X(0);
  //if(!streamIndex(t0, h))
  //  return FALSE;
  h = streamIndex(t0);

  TERM t1 = X(1);
  if(!(t1->IsInt()) && !(t1->IsStruct()))
     pXCPT->Error(instanceE, aS("Stream position must be integer or structure"));

  //int pos = t1->getInt();
  StreamPosition sp(m_peng, t1);
  //LNEW(sp, StreamPosition(t1), aS("stream"));
  stream[h]->set_position(sp);
  //delete sp;

  return TRUE;
}

//   The document keeps the file type which can be
// 1- ASCII,  a b c
// 2- Unicode, a 0 b 0 c 0
// 3- Tagged Unicode, ff fe a 0 b 0 c 0
// 4- Reverse Unicode, or 0 a 0 b 0 c
// 5- Tagged Reverse Unicode. fe ff 0 a 0 b 0 c
// The number corresponds to the number returned from the
// saveas file dialog as file type. 

char_type_ IO::figureCharType(PATOM name)
{  // this is not wide chars
   char_type_  result;
   aBYTE* buf = NULL;
   std::ifstream in;

   LPathString paname = (LPathString)*name;
   paname.TiltSlashes();
   //aname = g_lenv.new_mbs_dup(*nameA);
   char *aname = g_lenv.new_mbs_dup(paname);
   //char *aname;
   //aname = g_lenv.new_mbs_dup(*name);

   try
   {
      in.open(aname, std::ios::in | std::ios::binary);
      if(!in.is_open())
         pXCPT->Error(fopenE, (STRptr)(*name));

      in.seekg(0, std::ios::end);
      intC nLen = (intC)in.tellg();

      if (nLen & 1)
      {               // Odd length files are ascii.
         result = ascii_;
         goto done;
      }

      in.seekg(std::ios::beg);

      nLen = nLen < 888 ? nLen : 888;
      LNEW(buf, aBYTE[nLen], aS("initialization"));
      in.read((aSBYTE *)buf, nLen);

      if (buf[0] == 0xff && buf[1] == 0xfe)
      {
         result = little_endian_;
         goto done;
      }

      if (buf[0] == 0xfe && buf[1] == 0xff)
      {
         result = big_endian_;
         goto done;
      }
   
      // This is program text, so there will not be
      // any 0's naturally occurring in ASCII files.
      // Further, a Unicode file will have spaces and
      // carriage return line feeds which come from
      // the lower 256, so there will be 0's in Unicode.

      for (int i = 0; i < nLen; i += 2)
      {
         if (buf[i + 1] == 0)
         {
            result = little_endian_no_tag_;
            goto done;
         }

         if (buf[i] == 0)
         {
            result = big_endian_no_tag_;
            goto done;
         }
      }
    
      result = ascii_;
   }
   catch(aCHAR* s)
   {
      delete buf;
      in.close();
      pXCPT->Error(ioE, s);
   }   

done:
   delete buf;
   delete aname;
   in.close();
   return result;
}



TF IO::p_open()
// open$(H, Name, Mode, options(Type, Repos, Alias, Action))
// Mode = read, write, append, readappend, readwrite, readwritesys
// Type = 2 (binary), 1 (text)
// Repos = default (whatever stream defaults to), true, false
// Alias = atom alias for stream
// Action = error, eof_code, reset
//      eof_code is the default (not sure others are handled...)
{ 
   intC        h;
   TERM handle_T  = X(0);
   TERM fname_T   = X(1); 
   TERM mode_T    = X(2);
   TERM options_T = X(3);
   TERM x;
   PATOM action;

   // the initialization parameters:
   PATOM name;
   PATOM alias = pATAB->emptyA;
   mode_ mode;
   type_ type;
   repos_ repos;
   eofAction eofa;
   
#ifdef BUG_IO
 DUMP << "p_open file: " << *fname_T << NL;
 DUMP << SP2 << "mode = " << *mode_T << NL;
#endif

   if (fname_T->IsAtom())
      name = fname_T->getAtom();
   else
      pXCPT->Error(argE, aS("name must be atom"));

   // Get the mode (read, write, ...)
   PATOM mode_A  = mode_T->getAtom();
   mode = 
     mode_A == pATAB->readA ? read_ :  
     (mode_A == pATAB->writeA ? write_ :  
      (mode_A == pATAB->appendA ? append_ :
       (mode_A == pATAB->readappendA ? readappend_ :
        (mode_A == pATAB->readwriteA ? readwrite_ :
        badmode_ ))));
   if(mode == badmode_)
     pXCPT->Error(argE, aS("unknown mode"));

   for(h = 4; h < stream.size(); h++) // Look 4 open stream of same name & mode
   {
      if(stream[h] && stream[h]->isFile() &&
            (stream[h])->get_name() == name)
       //if((stream[h])->mode == mode)           // found with this name & mode
       //  return pTSVC->UnifyInt(h, handle_T);
       //else
         pXCPT->Error(prevopenE, (STRptr)*name);
   }
   
   if (! handle_T->IsRef()) // We're returning the handle, better not be bound
      pXCPT->Error(sysargE);
   
   if(! options_T->IsStruct())
      pXCPT->Error(sysargE);

   // Get the type (text, binary, wide_text)
   x = options_T->getTerm() + 1;           // arg 1
   type = (type_)x->getInt();
   x++;
#ifdef BUG_IO
 if (type == binary_)
    DUMP << SP2 << "binary" << NL;
 else
    DUMP << SP2 << "text" << NL;
 DUMP << FLUSH;
#endif

   // get reposition(true,false,default), alias and eof_action properties 
   // (if given)
   PATOM reposA = x->getAtom();
   repos = reposA != pATAB->falseA ? true_ : false_;
   /*
   if(reposA == pATAB->defaultA)
      repos = default_;
   else if (reposA == pATAB->trueA)
      repos = true_;
   else
      repos = false_;
   */
   x++;

   if(x->getType() == atomS)               // arg 3
      alias = x->getAtom();

   x++;
   if(x->getType() == atomS)               // arg 4
   {
      action = x->getAtom();
      eofa =  ( action == pATAB->error_A ? error_ : 
            (action == pATAB->reset_A ? reset_ : eof_code_) );
   }
   else
      eofa = eof_code_;

   x++;
   //       m_peng->GetArg(options_T, 4, cINT,   &(strmp->end_of_stream));

   //h = add_open_stream(strmp);
   //strmp->setID(h);
   h = makeFileStream(name, alias, mode, type, repos, eofa);

   return pTSVC->UnifyInt(h, handle_T);
}


int IO::add_open_stream(STREF s)
{
   intC h;
   for (h = 0; h < stream.size(); h++)
   {
      if (stream[h] == NULL)
      {
         stream[h] = s;
         break;
      }
   }
   if (h == stream.size())
   {
      stream.push_back(s);
   }
   return h;
}

void IO::remove_stream(int h)
{
#ifdef xBUG_IO
 DUMP << "remove_stream(" << h << ")" << NL;
 if (stream[h]->isFile())
    DUMP << SP2 << (stream[h])->get_name() << NL;
 else
    DUMP << SP2 << stream[h]->getAlias() << NL;
 DUMP << FLUSH;
#endif
   PrologStream *pstr = stream[h];
   pstr->close();
   delete pstr;
   stream[h] = NULL;
}

#ifdef LANDFILL
void IO::DumpStreams()
{
   DUMP << NL << "----- Streams -----" << NL;
   for (size_t h=0; h < stream.size(); h++)
   {
      DUMP << h << ": ";
      if (stream[h] == NULL)
         DUMP << "NULL" << NL;
      else
      {
         if (h == 3)
            ((FunctionPStream*)stream[h])->Dump();
         else
            stream[h]->Dump();
      }
   }
   DUMP << "user_input = " << user_input << NL;
   DUMP << "user_output = " << user_output << NL;
   DUMP << "user_error = " << user_error << NL;
   DUMP << "current_input = " << current_input << NL;
   DUMP << "current_output = " << current_output << NL;
   DUMP << "current_error = " << current_error << NL;
   DUMP << "----- -----" << NL << FLUSH;
}
#endif


TF IO::p_close()
{
  intC h = streamIndex(X(0));
  
  if (h >= (int)stream.size() || h < 4) 
    pXCPT->Error(sysargE);

  remove_stream(h);
  return TRUE;
}       

int IO::streamIndex(TERM tid)
{       // In tid the stream is identified either by numeric index or by alias
   PATOM fname_A;
   intC h;

   if (tid->IsInt())
   {                                         // numeric index supplied
      h = tid->getInt();
      switch(h)
      {
      case(-1): 
         h = current_output; 
         break;
      case(-2): 
         h = current_input;  
         break;
      case(-3): 
         h = current_error;  
         break;
      default:
         break;
      }
      if (h >=0 && h < stream.size() && stream[h])
         return h;
      else
         pXCPT->Error(handleE, h);
   }

   if (tid->IsAtom())
   {                                       // alias supplied
      fname_A = tid->getAtom();
      for (h = 0 ; h < stream.size(); ++h)
         if (stream[h] && stream[h]->aliasA == fname_A) 
            break;
      if (h < stream.size())
         return h;
      else
         pXCPT->Error(aliasE, (STRptr)*(fname_A));
   }

   pXCPT->Error(instanceE, aS("I/O id must be integer or atom"));
	return h;                              // for compiler
}

TF IO::p_at_eos()
{
  intC h;
  TERM T = (pHXL->XVar(0))->dref();
 
  //if (! streamIndex(T, h))
  //  return false;                             // no error mandated
  h = streamIndex(T);

  if (stream[h]->isEndOfStream())
     return TRUE;
  else
     return FALSE;
}

TF IO::p_geto()
{  // prolog's get_char: get the next char from input. get0_(Term, Handle)
  intC    h;
  Cell   x;
  aCHAR  c;
  
  h = streamIndex(X(1));

  c = stream[h]->get_char();
  if (c == LEOF)
    x.setAtom(pATAB->eofA);
  else
    x.setChar(c);

  return pTSVC->Unify(pHXL->XVar(0), &x);
}


TF IO::p_putc()
{                // putc(H, Char, charsOnly)
   aCHAR     c;
   TF        wasChar;
   STRptr    name;

   int h = streamIndex(X(0));

   TERM t1 = X(1);           // the char to put
   TERM t2 = X(2);           // char/code switch
   TF wasputchar = (TF)t2->getInt();          // put_char or else put_code

   if (t1->IsInt())
     {
       wasChar = 0;
       c = (aCHAR)t1->getInt();
     }
   else 
     if (t1->IsChar())
       {
         wasChar = 1;
         c = (aCHAR)t1->getChar();
       }
     else
       if(t1->IsAtom())
         {
           wasChar = 1;
           name = *(t1->getAtom());
           if(Lstrlen(name) == 1)
             c = (aCHAR)*name;
           else
             pXCPT->Error(sysargE);
         }
       else
         pXCPT->Error(sysargE);

   stream[h]->put_char(c);
   return true;
}



//
//    fread_(Handle, Value, Type)
//      Type = 0 read char
//      Type = 1 read short
//      Type = 2 read float
//      Type = 3 read long
//      Type = 4 read double
//
TF IO::p_fread()
{    // pro tem  fread(Stream, Value, Type)

   long       i = 0;
   short      shrt;
   char       uch;
   Cell       x;
   TERM       t2;
   int        type;
   float      f;
   double     df;
   int        h;
   GCDouble   *gcf;

   h = streamIndex(X(0));

   t2 = X(2);
   if (! t2->IsInt())
     pXCPT->Error(sysargE);

   type = t2->getInt();         // fread Type, not stream type
   //current_input = t0->getInt();
   //h = t0->getInt();
   //FISTREAM *readerp = (FISTREAM *)(stream[current_input]);
   //FISTREAM *readerp = (FISTREAM *)(stream[h]);
   switch( type )
   {
   case 0:                                    // char
      stream[h]->read(&uch, 1);
      i = (unsigned char)uch;
      x.setInt(i);
      break;

   case 1:                                    // short
      stream[h]->read((char *)&shrt, 2);
      RevEndian16(shrt);
      i = shrt;
      Lprintf(aS("Read short %d\n"), i);      // ray
      x.setInt(i);
      break;

   case 2:                                    // float
      stream[h]->read((char *)&f, 4);
      memcpy(&i, &f, 4);
      RevEndian32(i);
      memcpy(&f, &i, 4);
      df = (double) f;
      LNEW(gcf, GCDouble(df), aS("gc_object"));
      x.setDouble(gcf);
      break;

   case 3:                                    // long
      stream[h]->read((char *)&i, 4);
      RevEndian32(i);
      x.setInt(i);
      break;

   case 4:                                    // double
      stream[h]->read((char *)&df, 8);
      LNEW(gcf, GCDouble(df), aS("gc_object"));
      x.setDouble(gcf);
      break;

   default:
      pXCPT->Error(sysargE);
   }

	if(stream[h]->eof())
		  x.setAtom(pATAB->eofA);

   return( pTSVC->Unify(pHXL->XVar(1), &x));
}

   /* Old Alan comment left in for literary value...
    * Arghhhhhh we have a problem (only took a week of head banging).
    * Microsoft version V C seems to have screwed up the interaction
    * of fseek(), getc() and ungetc(). The net result of this is that
    * if you are using read() (Prolog) to read a term from a file,
    * and fseek() (Prolog) to find out where you are in the file, the
    * file pointer gets clobbered (because the parser uses ungetc()).
    * So we have to check to see if you are useing fseek() purely
    * as an ftell() kind of thing i.e. fseek(H, 0, 1, Pos), var(Pos).
    * In this case we DO NOT DO A SEEK. As a benefit it is somewhat 
    * faster (compiles gain a little)
    */
      
TF IO::p_fseek()
// fseek(Id, Offset, Method, NewOffset)
// for backward compatibility, uses integers for
// offsets, not position structure, so not reliable
// for text streams.
{
   Cell    x;
   long    offset;
   StreamPosition sp;

   int h = streamIndex(X(0));

   TERM offsetT = X(1);
   TERM methodT = X(2);

   if (! methodT->IsInt() )
       pXCPT->Error(sysargE);

   //mode_ mode = strmp->getMode();
   origin_ method = (origin_)methodT->getInt();
   sp.setOrigin( method );
   offset = sp.readpos = sp.writepos = offsetT->forceInt();
   sp.b_read = sp.b_write = true;

   // fseek(H,0,1,Pos) is how to get/tell the current position,
   // that is, it doesn't do a seek.
   if (! (offset == 0 && method == 1))
   {
      stream[h]->set_position(sp);
   }

   sp.init();
   sp = stream[h]->get_position();
   x.setInt(sp.writepos);
   return( pTSVC->Unify(pHXL->XVar(3), &x));
}

TF IO::p_streamList()
// create a list of streams for generating stream properties
{
   TERM list, head, tail;

   TERM t0 = X(0);
   if(!t0->IsRef())
     pXCPT->Error(sysargE);

   //TERM list = pHXL->heapGETN(3*stream.size());
   list = pHXL->heapGET();
   //TERM head = list + 1;
   //TERM tail = head + 1;
   //head->setInt(0);
   //list->setList(head);

   //std::vector<STREF>::iterator p;
   //int i = 1;
   //for (p = &stream[1]; p != stream.end(); p++)
   //  {
   //    if(*p)
   //      {
   //        head += 2;
   //        tail->setList(head);
   //        head->setInt(i);
   //        tail += 2;
   //      }
   //    i++;
   //  }

   tail = list;
   for (intC h=0; h < stream.size(); h++)
   {
      if (stream[h] != NULL)
      {
         head = pHXL->heapGETN(2);
         tail->setList(head);
         head->setInt(h);
         tail = head + 1;
      }
   }
   tail->setAtom(pATAB->nilA);
   return(pTSVC->Unify(list, pHXL->XVar(0)));
}

TERM IO::bumpNiladic(TERM list, PATOM functr)
{   // add an atom to a list
   // next list cells
   TERM head, tail;
   head = pHXL->heapGETN(2);
   list->setList(head);
   head->setAtom(functr);
   tail = head + 1;

   return tail;
//  head += 2;
//  tail->setList(head);
//  tail += 2;
//  head->setAtom(functr);
}

TERM IO::bumpMonadic(TERM list, PATOM functr, PATOM a)
{   // add a structure/1 to a list
   // make the structure
   TERM str = pHXL->heapGETN(2);
   str->setFA(functr, 1);
   TERM arg = str+1;
   arg->setAtom(a);

   // next list cells
   TERM head, tail;
   head = pHXL->heapGETN(2);
   list->setList(head);
   head->setStruct(str);
   tail = head + 1;

   return tail;
//  head += 2;
//  tail->setList(head);
//  tail += 2;
//  TERM fn = pTSVC->MakeStruc(functr, 1);
//  TERM strucName = fn->t;
//  (strucName + 1)->setAtom(arg);
//  head->setStruct(strucName);
}

TERM IO::bumpMonadic(TERM list, PATOM functr, int i)
{   // add a structure/1 to a list
   // make the structure
   TERM str = pHXL->heapGETN(2);
   str->setFA(functr, 1);
   TERM arg = str+1;
   arg->setInt(i);

   // next list cells
   TERM head, tail;
   head = pHXL->heapGETN(2);
   list->setList(head);
   head->setStruct(str);
   tail = head + 1;

   return tail;

  //head += 2;
  //tail->setList(head);
  //tail += 2;
  //TERM fn = pTSVC->MakeStruc(functr, 1);
  //TERM strucName = fn->t;
  //(strucName + 1)->setInt(arg);
  //head->setStruct(strucName);
}

TERM IO::bumpMonadic(TERM list, PATOM functr, TERM t)
{   // add a structure/1 to a list
   // make the structure
   TERM str = pHXL->heapGETN(2);
   str->setFA(functr, 1);
   TERM arg = str+1;
   //arg->setInt(i);
   arg->setTerm(t);

   // next list cells
   TERM head, tail;
   head = pHXL->heapGETN(2);
   list->setList(head);
   head->setStruct(str);
   tail = head + 1;

   return tail;

  //head += 2;
  //tail->setList(head);
  //tail += 2;
  //TERM fn = pTSVC->MakeStruc(functr, 1);
  //TERM strucName = fn->t;
  //(strucName + 1)->setInt(arg);
  //head->setStruct(strucName);
}


TF IO::p_stream_props()
{
   //int props = 10;
   TERM list, tail;
   //TERM t0 = X(0);
   //if(!t0->IsInt())
   //   pXCPT->Error(sysargE);

   int h = streamIndex(X(0));
   //int h = t0->getInt();
   STREF strmp = stream[h];
  
// These correspond to enums in streams.h, make sure they stay
// coordinated with them.

   PATOM modeName[] =
   {  pATAB->error_A, pATAB->emptyA, pATAB->readA, pATAB->readwriteA, 
      pATAB->readappendA, pATAB->writeA, pATAB->appendA, };

   PATOM typeName[] =
   {  pATAB->emptyA, pATAB->textA, pATAB->binaryA, pATAB->wide_textA };

   PATOM eoStreamName[] =
   {  pATAB->at_A, pATAB->past_A, pATAB->no_A };

   PATOM eofActionName[] =
   {  pATAB->error_A, pATAB->eof_code_A, pATAB->reset_A };

   //list = pHXL->heapGETN(2*props + 3);
   list = pHXL->heapGET();
   tail = list;
   //head = list + 1;
   //tail = head + 1;
   //list->setList(head);

   //if (strmp->isFile())
   //{
   //  fn = pTSVC->MakeStruc(pATAB->file_nameA, 1);
   //  strucName = fn->t;
   //  (strucName + 1)->setAtom(strmp->path);
   //  head->setStruct(strucName);
   //}
   if (strmp->isFile())
      tail = bumpMonadic(tail, pATAB->file_nameA, strmp->get_name());

   if(strmp->hasAlias())
      tail = bumpMonadic(tail, pATAB->aliasA, strmp->getAlias());

   if(strmp->isInput())
   {
      tail = bumpNiladic(tail, pATAB->inputA);
      tail = bumpMonadic(tail, pATAB->line_numberA, strmp->getLineNo());
   }

   if(strmp->isOutput())
      tail = bumpNiladic(tail, pATAB->outputA);

   tail = bumpMonadic(tail, pATAB->modeA, modeName[strmp->getMode()]);
   tail = bumpMonadic(tail, pATAB->typeA, typeName[strmp->getType()]);
   tail = bumpMonadic(tail, pATAB->end_of_streamA, 
              eoStreamName[strmp->getEndOfStreamStatus()]);
   tail = bumpMonadic(tail, pATAB->eofActionA, 
              eofActionName[strmp->eof_action]);
   tail = bumpMonadic(tail, pATAB->repositionA, 
              strmp->reposition ? pATAB->trueA : pATAB->falseA);
   if(strmp->reposition)
      tail = bumpMonadic(tail, pATAB->positionA, 
                strmp->get_position().makeTerm(m_peng));

   tail->setAtom(pATAB->nilA);
   return(pTSVC->Unify(list, pHXL->XVar(1)));
}

// This version is used by the compiler and unlock code only and
// writes binary files in little Endian format, no matter what machine.
//    flewrite(Handle, Value, Type)
//    Type = 0 char, 1 short, 2 float, 3 long(integer), 4 double, 5 fixed

int IO::p_flewrite()
{
   int      h, i, lb;
   aBYTE    b1;
   aINT16   i2;
   float    f4;
   double   f8;
   fixedC   fx;
   aINT32   i4;
   aBYTE   *b;

   TERM t2 = X(2);            // type
   TERM t1 = X(1);            // value

   if (! t2->IsInt())
     pXCPT->Error(sysargE);
   if ( !( t1->IsNumber()))
     pXCPT->Error(sysargE);
   
   //streamIndex(pHXL->XVar(0), h);              // handle
   h = streamIndex(X(0));
   //if(strmp->kind != FILE_)
   //  return FALSE;

   switch(t2->getInt()) 
     {
     case 0:                                   // char
       lb = 1;
       b1 = (aBYTE)t1->getInt();
       b = (aBYTE*) &b1;
       break;
       
     case 1:                                   // short
       lb = 2;
       i2 = (aINT16)t1->getInt();
       b = (aBYTE*) &i2;
       break;
       
     case 2:                                   // C float
       lb = 4;
       if (t1->IsSingle())
          f4 = t1->getSingle();
       else
         f4 = (float)t1->getDouble();

       b = (aBYTE *) &f4;
       break;
       
     case 3:                                   // long
       lb = 4;
       //i4 = pTSVC->XValue(t);
       i4 = (long)t1->getInt();
       b = (aBYTE*) &i4;
       break;
       
     case 4:                                   // C double (prolog float)
       lb = 8;
       if (t1->IsSingle())
          f8 = (double)t1->getSingle();
       else
          f8 = (double)t1->getDouble();
       b = (aBYTE *) &f8;
       break;
       
     case 5:                                   // fixed
       lb = 8;
       fx = t1->getFixed(); 
       b = (aBYTE *) &fx;
       break;

     default:  
       pXCPT->Error(sysargE);
     }
#ifdef BIG__ENDIAN 
   for (i = lb - 1; i >= 0; i--)
#else
     for (i = 0; i < lb; i++)
#endif
       {                                      // write byte by byte
         stream[h]->write((char *)(b+i), 1);
       }
   return TRUE;
}


TF IO::p_fflush()  
{                                              // fflush(Handle) 
  int h;
  TERM T = (pHXL->XVar(0))->dref();
  //if(!streamIndex(T, h))
  //  pXCPT->Error(sysargE);
  h = streamIndex(T);
  
  stream[h]->flush();
  return true;
}

TF IO::p_handle_name()
{     // handle_name(H,N) - get the handle for a name or the name for a handle 
   intC     h;
   PATOM   name_A;

   TERM handle_T = X(0);
   TERM name_T   = X(1);

   if (handle_T->IsRef() && name_T->IsAtom())
     {                                     // handle wanted
       name_A = name_T->getAtom();
       
       for(h = 4; h < stream.size(); h++)
         if(stream[h]->isFile() && (stream[h])->get_name() == name_A)
           break;
       
       if (h < stream.size())       
         return(pTSVC->UnifyInt(h, pHXL->XVar(0)));

       return(FALSE);
     }

   if (name_T->IsRef() && handle_T->IsInt())
     {                                      // name wanted
       h = handle_T->getInt();
       if (h >= stream.size() || h < 0) 
         pXCPT->Error(sysargE);
//       if (! stream[h]->isFile())
//          pXCPT->Error(invalid_sopE, h, aS("name of non-file stream"));   
//       name_T->setAtom(stream[h]->get_name());
         if (stream[h]->isFile())
            name_T->setAtom(stream[h]->get_name());
         else
            name_T->setAtom(stream[h]->getAlias());

       return(pTSVC->Unify(name_T, pHXL->XVar(1)));
     }

   pXCPT->Error(sysargE);
   return FALSE;        // a formality 
}

TF IO::p_read_binary()
// read_binary(FileID, TYPE, VAR)
//   where TYPE = char, wide_char, integer, single_float, double_float
{
   int h;
   PATOM type;
   TERM x1;
   Cell c;

   h = streamIndex(X(0));

   x1 = X(1);
   if (! x1->IsAtom())
      pXCPT->Error(sysargE);
   type = x1->getAtom();

   if (type == pATAB->charA)
   {
      char ch;
      stream[h]->read((char*)&ch, sizeof(unsigned char));
      c.setInt(ch);
      return pTSVC->UnifyConst(X(2), &c);
   }
   else if (type == pATAB->wide_charA)
   {
      aCHAR wch;
      stream[h]->read((char*)&wch, sizeof(aCHAR));
      c.setChar(wch);
      return pTSVC->UnifyConst(X(2), &c);
   }
   else if (type == pATAB->short_integerA)
   {
      intCH si;
      stream[h]->read((char*)&si, sizeof(intCH));
      c.setInt(si);
      return pTSVC->UnifyConst(X(2), &c);
   }
   else if (type == pATAB->integerA)
   {
      intC i;
      stream[h]->read((char*)&i, sizeof(intC));
      c.setInt(i);
      return pTSVC->UnifyConst(X(2), &c);
   }
   else if (type == pATAB->single_floatA)
   {
      float f;
      stream[h]->read((char*)&f, sizeof(float));
      c.setSingle(f);
      return pTSVC->UnifyConst(X(2), &c);
   }
   else if (type == pATAB->double_floatA)
   {
      double d;
      stream[h]->read((char*)&d, sizeof(double));
      c.setDouble(pGCTH->make_float(d));
      return pTSVC->UnifyConst(X(2), &c);
   }
   else
      pXCPT->Error(sysargE);

   return TRUE;
}

TF IO::p_write_binary()
// write_binary(FileID, TYPE, VAR)
//   where TYPE = char, wide_char, short_integer, integer, single_float, double_float
{
   int h;
   PATOM type;
   TERM x1;

   h = streamIndex(X(0));

   x1 = X(1);
   if (! x1->IsAtom())
      pXCPT->Error(sysargE);
   type = x1->getAtom();

   if (type == pATAB->charA)
   {
      unsigned char c = (unsigned char)X(2)->forceInt();
      stream[h]->write((char*)&c, sizeof(unsigned char));
   }
   else if (type == pATAB->wide_charA)
   {
      aCHAR wc = (aCHAR)X(2)->forceInt();
      stream[h]->write((char*)&wc, sizeof(aCHAR));
   }
   else if (type == pATAB->short_integerA)
   {
      intCH si = (intCH)X(2)->forceInt();
      stream[h]->write((char*)&si, sizeof(intCH));
   }
   else if (type == pATAB->integerA)
   {
      intC i = (intC)X(2)->forceInt();
      stream[h]->write((char*)&i, sizeof(intC));
   }
   else if (type == pATAB->single_floatA)
   {
      float f = (float)X(2)->forceDouble();
      stream[h]->write((char*)&f, sizeof(float));
   }
   else if (type == pATAB->double_floatA)
   {
      double d = (double)X(2)->forceDouble();
      stream[h]->write((char*)&d, sizeof(double));
   }
   else
      pXCPT->Error(sysargE);

   return TRUE;
}

//
// end of IO 
//---------------











