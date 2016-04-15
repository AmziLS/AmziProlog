/*****************************************************************\
*
* stash.h - A stash is an in memory list.
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: stash.h,v $
* Revision 1.2  2003/10/14 01:45:09  dennis
* keys/license stuff in engine
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.4  2002/05/15 16:59:09  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.3  2002/01/06 20:31:28  dennis
* put in new memory leak LNEW that reports on lines of code, had
* to take out std:: classes from engine and replace with pointers
* to classes to get it to work, due to various news and deletes in
* the stl class libraries.
*
* Revision 1.2  2001/08/24 16:06:44  dennis
* fixed header file comments at end of files, removed ostream include
* which isn't apparently necessary
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.3  2000/08/26 00:32:08  dennis
* Merged with ray's changes for new numbers and ISO standard features.
* Added new AtomTable, Dynamic Database and operators based on STL
* maps as basis for ISO modules.
*
* Revision 1.2  2000/03/28 01:05:17  dennis
* merged Ray's changes with bigdig.  bigdig is at point where
* new Cell class is used, but there are no modules in the system.
*
* Revision 1.1.2.3  2000/03/23 12:36:11  dennis
* *** empty log message ***
*
* Revision 1.1.2.2  2000/03/22 23:21:24  dennis
* findall example working
*
* Revision 1.1.2.1  2000/03/13 06:07:45  dennis
* got cell, stash and termsvcs to compile
*
*
*
\******************************************************************/

/* Stashing functions

   A stash is a linked list, dynamically allocated as needed
   to store a Prolog list.  Each stash node contains the normal
   cells for a list term, and the term at that node of the list.
   
   Stashes are an alternative to using the heap, and are only used
   internally at this time.  The users are the compiler and the
   built-in findall function.

   */
  

#ifndef STASH_H
#define STASH_H

class StashNode
{
private:
   Cell *cells;
   StashNode *next;

public:
   StashNode(PATOM a);
   StashNode(int n);

   ~StashNode()
   { //delete[] cells;
      delete[] cells; }

   TERM getHead()
   { return cells; }
   TERM getTail()
   { return cells+1; }
   TERM getTerm()
   { return cells+2; }

   StashNode *getNext()
   { return next; }
   void setNext(StashNode *n)
   { next = n; }
};


class Stash
{
private:
   LEngine *m_peng;
   StashNode *first;
   StashNode *last;

public:
   Stash(LEngine *peng)
   { m_peng = peng; init(); }
   //{ first = new StashNode(); last = first; }

   ~Stash()
   { deleteNodes(); }
   //{ clearTerm(); //delete first;
   //   delete first; }

   // start the chain
   void init();

   // Add a term to the end of the list
   void addTerm(int n);
   // Get the full list term from the stash.
   // It starts in the tail cell of the first
   // stash node.
   TERM getTerm()
   {   return first->getTail(); }
   // clear out the old
   void deleteNodes();
   // free up the nodes used in the stash and
   // initialize to start again
   void clearTerm();
   // get the head cell of a newly created
   // last stash node for someone outside
   // to copy into.
   TERM getHead()
   {   return last->getHead(); }
   // get where the term itself goes in support
   // of getHead above.
   TERM getHeadTerm()
   {   return last->getTerm(); }

//private:
   // Free the storage for the list
//   void freeNodes();
};

#endif //STASH_H
