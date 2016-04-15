/*****************************************************************\
*
* stash.cpp - the chunks that make up a stash list
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: stash.cpp,v $
* Revision 1.2  2003/10/14 01:45:09  dennis
* keys/license stuff in engine
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.3  2002/05/15 16:59:09  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.2  2002/01/06 20:31:28  dennis
* put in new memory leak LNEW that reports on lines of code, had
* to take out std:: classes from engine and replace with pointers
* to classes to get it to work, due to various news and deletes in
* the stl class libraries.
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
* Revision 1.1.2.2  2000/03/23 12:36:11  dennis
* *** empty log message ***
*
* Revision 1.1.2.1  2000/03/13 06:07:45  dennis
* got cell, stash and termsvcs to compile
*
*
\******************************************************************/

#include "inc.h"
#include "pch.h"

// Used for the first StashNode, called with a = pATAB->nilA
StashNode::StashNode(PATOM a)
{
   //cells = new Cell[3];
   LNEWX(cells, Cell[3]);
   cells[1].setAtom(a);
   next = NULL;
}

// Subsequent StashNodes, allocate enough space
// for the term
StashNode::StashNode(int n)
{
   //cells = new Cell[n+2];
   LNEWX(cells, Cell[n+2]);
   next = NULL;
}

void Stash::init()
{
   //first = new StashNode(pATAB->nilA);
   LNEWX(first, StashNode(pATAB->nilA));
   last = first;
}

// Add a term to the end of the list
void Stash::addTerm(int n)
{
   //StashNode *newnode = new StashNode(n);
   StashNode *newnode;
   LNEWX(newnode, StashNode(n));
   last->getTail()->setList(newnode->getHead());
   newnode->getTail()->setAtom(pATAB->nilA);
   last->setNext(newnode);
   last = newnode;
}

// Free all the nodes for the stash and get ready
// to start again.
void Stash::clearTerm()
{
   deleteNodes();
   init();
}

void Stash::deleteNodes()
{
   StashNode *node = first;
   StashNode *next;

   while(node != NULL)
   {
      next = node->getNext();
      //delete node;
      delete node;
      node = next;
   }
}

