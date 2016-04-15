/***************************************************************************\
*
* module.cpp -- various module class implementations
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: module.cpp,v $
* Revision 1.4  2006/12/19 04:58:47  dennis
* module names are just atoms
*
* Revision 1.3  2004/01/30 16:33:44  mary
* Added argument and description strings to PredicateHead and alib.
* Added predicate_info and predicate$enginfo to return this information.
*
* Revision 1.2  2003/12/10 22:29:04  dennis
* built dll with copy all feature for the dynamic db for now
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.28  2002/12/11 17:29:42  dennis
* new build for 6-3-2, using visual studio .net
*
* Revision 1.27  2002/12/06 17:37:36  dennis
* a6-3-2, finally got rid of mscw tagging of heap cells to maintain
* state of dynamic clause backtracking, put backtracking info in
* the choice point structure instead.  much cleaner.  retract can now
* just analyze the control stack to see if a retract is safe.  heapgc
* no longer has to worry about those wierd cells.
*
* Revision 1.26  2002/12/02 18:25:12  dennis
* Converted to Visual Studio .NET.
*
* Revision 1.25  2002/06/23 20:01:30  dennis
* fixed some gc issues
*
* Revision 1.24  2002/05/15 16:59:09  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.23  2002/05/01 02:26:33  dennis
* minor fixes
*
* Revision 1.22  2002/04/19 19:41:43  dennis
* fixed retract bug with sorted/indexed clauses, implemented abolish for
* those types as well
*
* Revision 1.21  2002/03/10 22:18:58  dennis
* fixed some small bugs, updated documentation
*
* Revision 1.20  2002/03/04 03:46:12  dennis
* fixed bug in sorted predicates
*
* Revision 1.19  2002/01/28 06:29:19  dennis
* changes for parsing numbers, handling different options
*
* Revision 1.18  2002/01/06 20:31:28  dennis
* put in new memory leak LNEW that reports on lines of code, had
* to take out std:: classes from engine and replace with pointers
* to classes to get it to work, due to various news and deletes in
* the stl class libraries.
*
* Revision 1.17  2001/12/08 03:48:22  dennis
* fixed bugs with reconsult, added unicode support
*
* Revision 1.16  2001/12/06 16:43:21  dennis
* fixed heapgc bug and allowed for unlimited vars in copyterm
*
* Revision 1.15  2001/11/09 02:28:10  dennis
* Finished, I hope, sorted and indexed predicates. Needed to rethink
* how mscws worked, given the new itertors for moving from clause to
* clause.
*
* Revision 1.14  2001/10/30 01:01:04  dennis
* sorted and indexed clauses supported in dynamic database
*
* Revision 1.13  2001/10/27 03:24:37  dennis
* sorted dynamic predicates working, with optimized queries
* when they fit the right pattern, of vars last
*
* Revision 1.12  2001/10/26 01:28:16  dennis
* sorted predicates partially implemented
*
* Revision 1.11  2001/10/24 14:19:48  dennis
* changed a name on buttercup, just testing really
*
* Revision 1.10  2001/10/24 05:06:34  dennis
* changed linked list of clauses in ddb to use STL list, so links
* no longer in clause, allowing for different types of iterators on
* clauses
*
* Revision 1.9  2001/10/19 01:38:00  dennis
* compiler bugs, still not found, but noted that X registers
* are really restricted to 255 because of flewrite in assemb.pro,
* should change some day.
*
* Revision 1.8  2001/10/13 02:58:13  dennis
* see/tell bugs, used to close function streams
*
* Revision 1.7  2001/07/21 00:39:47  dennis
* added garbage collector for strings and things
*
* Revision 1.6  2001/07/10 16:51:32  dennis
* added more memory leak checking tools, all clean so far
*
* Revision 1.5  2001/06/27 15:15:10  dennis
* miscellaneous changes and bug fixes, work with leak detection
*
* Revision 1.4  2001/02/13 03:42:13  dennis
* trying to fix bug in lsExecStr, cleaning up user:
*
* Revision 1.3  2001/01/11 01:49:23  dennis
* Implemented rest of import/export and metapredicates, working
* for test cases.
*
* Revision 1.2  2001/01/05 06:05:55  dennis
* fixed minor discrepancies
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.4  2000/09/27 01:42:03  dennis
* implemented listing, needed current_predicate, predicate_property,
* and current_module
*
* Revision 1.3  2000/09/15 21:42:25  dennis
* 12->13
*
* Revision 1.2  2000/09/02 02:10:20  dennis
* new version of get$db replacing dbrefgen for getting clauses
* from dynamic database
*
* Revision 1.1  2000/08/26 20:24:10  dennis
* added missing module files
*
*
\***************************************************************************/

#include "inc.h"
#include "pch.h"

#ifdef LANDFILL
#define noBUG_MOD
#define noBUG_MSCW
#endif

KeyRep::KeyRep(LEngine *peng, TERM key)
{
   m_peng = peng;
   m_count = 1;
   //b_original = true;
   int len = key->term_size();
#ifdef BUG_MOD
 DUMP << "KeyCode key = ";
 pTSVC->Dump(key);
 DUMP << "  length = " << len << NL;
#endif
//std::cout << '+';
   LNEW(m_code, Cell[len], aS("module"));
   TERM next = m_code + 1;
   pTSVC->Copy(m_code, next, &next, key, GC_NO);
#ifdef BUG_MOD
 DUMP << "KeyCode m_code = ";
 pTSVC->Dump(m_code);
 DUMP << NL;
#endif
}

KeyRep::~KeyRep()
{
//std::cout << '-';
   delete[] m_code;
}

KeyCode::KeyCode(LEngine *m_peng, TERM key)
{
   LNEW(m_rep, KeyRep(m_peng, key), aS("module"));
}

KeyCode::~KeyCode()
{
   if (--m_rep->m_count <= 0)
   {
      delete m_rep;
   }
}

//---------------------------------------------
// ClauseCode is used as a key for STL map
//

bool operator<(ClauseCode a, ClauseCode b)
{
 LEngine *m_peng = a.m_peng;
#ifdef BUG_MOD
 DUMP << "==>operator< " << NL;
 pTSVC->DumpWrite(a.m_code);
 DUMP << SP2;
 pTSVC->DumpWrite(b.m_code);
 DUMP << NL;
#endif
  bool rc = pTSVC->compare(a.m_code,b.m_code,true) < 0;
#ifdef BUG_MOD
  DUMP << "<==operator< returns: " << (rc ? "true" : "false") << NL;
#endif
  return rc;
}

bool operator<(KeyCode a, KeyCode b)
{
 LEngine *m_peng = a.getPEng();
#ifdef BUG_MOD
 DUMP << "==>operator< " << NL;
 pTSVC->DumpWrite(a.getCode());
 DUMP << SP2;
 pTSVC->DumpWrite(b.getCode());
 DUMP << NL;
#endif
  bool rc = pTSVC->compare(a.getCode(),b.getCode(),true) < 0;
#ifdef BUG_MOD
  DUMP << "<==operator< returns: " << (rc ? "true" : "false") << NL;
#endif
  return rc;
}

//---------------------------------------------
// PredicateHead
//

Lostream& operator<<( Lostream &os, PredicateHead &p )
{
   os << "  " << p.m_name << "/" << p.m_arity;

   if (p.IsBuiltIn())
      os << " built_in";
   if (p.IsExtended())
      os << " extended";
   if (p.IsCompiled())
      os << " compiled";
   if (p.IsDynamic())
      os << " dynamic";

   if (p.IsExported())
      os << " exported";
   if (p.IsImported())
      os << " imported";
   if (p.IsMeta())
      os << " metapredicate";

    return os;
}

// create a head for a builtin predicate
PredicateHead::PredicateHead(LEngine *peng, BuiltInPredicate *pbip, MODIX imod, PATOM name, ARITY arity, bool b_export, STRptr args, STRptr desc)
{
   m_peng = peng;
   m_imod = imod;
   m_name = name;
   m_arity = arity;
   m_code.m_builtin = pbip;
   //m_info = sysC | protecC | mfC;
   m_info = sysC | mfC;
   if (b_export) setExport();
   m_args = args;
   m_desc = desc;
}

// create a head for a builtin predicate
PredicateHead::PredicateHead(LEngine *peng, ExtendedPredicate *pext, MODIX imod, PATOM name, ARITY arity, bool b_export, STRptr args, STRptr desc)
{
   m_peng = peng;
   m_imod = imod;
   m_name = name;
   m_arity = arity;
   m_code.m_extended = pext;
   //m_info = sysC | protecC | extC;
   m_info = sysC | extC;
   if (b_export) setExport();
   m_args = args;
   m_desc = desc;
}

// create a head for a builtin predicate
//PredicateHead::PredicateHead(CODEptr ccode, PATOM name, ARITY arity)
PredicateHead::PredicateHead(LEngine *peng, CompiledPredicate *cp, MODIX imod, PATOM name, ARITY arity, bool b_export, STRptr args, STRptr desc)
{
   m_peng = peng;
   m_imod = imod;
   m_name = name;
   m_arity = arity;
   m_code.m_compiled = cp;
   m_info = compC;
   if (b_export) setExport();
   m_args = args;
   m_desc = desc;
}

PredicateHead::PredicateHead(LEngine *peng, DynamicPredicate *pdp, MODIX imod, PATOM name, ARITY arity, bool b_export, STRptr args, STRptr desc)
{
   m_peng = peng;
   m_imod = imod;
   m_name = name;
   m_arity = arity;
   m_code.m_dynamic = pdp;
   m_info = dynC;
   if (b_export) setExport();
   m_args = args;
   m_desc = desc;
}

PredicateHead::PredicateHead(LEngine *peng, ImportPredicate *ip, MODIX imod, PATOM name, ARITY arity, bool b_export, STRptr args, STRptr desc)
{
   m_peng = peng;
   m_imod = imod;
   m_name = name;
   m_arity = arity;
   m_code.m_import = ip;
   m_info = importC;
   if (b_export) setExport();
   m_args = args;
   m_desc = desc;
}

PredicateHead::~PredicateHead()
{
   if (IsBuiltIn())
   {
      delete m_code.m_builtin;
   }
   else if (IsExtended())
   {
      delete m_code.m_extended;
   }
   else if (IsCompiled())
   {
      delete m_code.m_compiled;
   }
   else if (IsDynamic())
   {
      delete m_code.m_dynamic;
   }
   else if (IsImported())
   {
      delete m_code.m_import;
   }
}

#ifdef LANDFILL
void PredicateHead::Dump()
{
   DUMP << "===PredicateHead===" << NL;
   DUMP << MODPREDAR(m_imod, m_name, m_arity) << NL;
   DUMP << "  info = " << m_info << NL;
   DUMP << "=== ===" << NL << FLUSH;
}
#endif

//---------------------------------
// CompiledPredicate
//

CompiledPredicate::~CompiledPredicate()
{
   // if discontiguous, need to follow links...
   delete m_code;
   delete m_clink;
};

//----------------------------------
// EscapePredicate
//

Lostream& operator<<( Lostream &os, EscapePredicate &p )
{
   os << p.getName() << "/" << p.getArity();
   return os;
}

//---------------------
// DynamicPredicate
//

Lostream& operator<<( Lostream &os, DynamicPredicate &dp )
{
   DynamicClause *dc;

   os << *(dp.m_pph) << NL;
   
   DynamicClauseIterator *dci = dp.getIterator(NULL);

   for ( dc = dci->getClause(); ! dci->isDone(); dci->setNext() )
      os << dc << ": " << *dc;

   return os;
}

//------------------------------------------
// ListDynamicPredicate
//

ListDynamicPredicate::~ListDynamicPredicate()
{
   DynamicClause *pdc;
   ClauseListIterator cli = m_clauses.begin();
   while (cli != m_clauses.end())
   {
      pdc = *cli;
      cli++;
      delete pdc;
   }
}

void ListDynamicPredicate::Abolish()
{
   if (m_clauses.empty())
      return;

   DynamicClause *pdc;
   ClauseListIterator cli = m_clauses.begin();
   while (cli != m_clauses.end())
   {
      pdc = *cli;
      delete pdc;
      cli = m_clauses.erase(cli);
   }
}

DynamicClauseIterator *ListDynamicPredicate::getIterator(TERM head)
{
   if (m_clauses.empty())
      return NULL;
   LEngine *m_peng = getPEng();
   DCListIterator *pdci;
   LNEW(pdci, DCListIterator(this), aS("module"));
   return pdci;
}

#ifdef LANDFILL
void ListDynamicPredicate::Dump()
{
   LEngine *m_peng = getPEng();
   DUMP << "ListDynamicPredicate::Dump()" << NL;
   ClauseListIterator lci = m_clauses.begin();
   while(lci != m_clauses.end())
   {
      (*lci)->Dump();
      lci++;
   }
}
#endif

//------------------------------------------
// SortedDynamicPredicate
//

SortedDynamicPredicate::~SortedDynamicPredicate()
{
   DynamicClause *pdc;
   ClauseMapIterator cmi = m_clauses.begin();
   while (cmi != m_clauses.end())
   {
      pdc = cmi->second;
      cmi++;
      delete pdc;
   }
}

DynamicClauseIterator *SortedDynamicPredicate::getIterator(TERM head)
{
   if (m_clauses.empty())
      return NULL;

   LEngine *m_peng = getPEng();
#ifdef BUG_MOD
 DUMP << "==>SortedDynamicPredicate::getIterator()" << NL;
 DUMP << "  head = ";
 pTSVC->DumpWrite(head);
 DUMP << NL;
#endif
   DCSortedIterator *pdci;
   LNEW(pdci, DCSortedIterator(this, head, b_poisoned), aS("module"));
#ifdef BUG_MOD
 DUMP << "  iter = ";
 if (pdci->isDone())
  DUMP << "Done";
 else
  pTSVC->DumpWrite(pdci->getClause()->getCode());
 DUMP << NL;
 DUMP << "<==SortedDynamicPredicate::getIterator()" << NL;
#endif
   return pdci;
}

void SortedDynamicPredicate::Abolish()
{
   if (m_clauses.empty())
      return;

   DynamicClause *pdc;
   ClauseMapIterator cmi = m_clauses.begin();
   ClauseMapIterator temp_cmi;
   // another example of ms going non-standard, but right.
   // erase for maps, unlike erase for other collections, is not
   // supposed to return the next iterator.  so you need to go
   // through the contortions below to get it to work on standard
   // complying gcc, although for ms the more intuitive approach works.
   while (cmi != m_clauses.end())
   {
      temp_cmi = cmi;
      cmi++;
      pdc = temp_cmi->second;
      delete pdc;
      m_clauses.erase(temp_cmi);
   }
}

void SortedDynamicPredicate::Insert(DynamicClause *pdc, int az)
{
   LEngine *m_peng = getPEng();
#ifdef BUG_MOD
 DUMP << "==>SortedDynamicPredicate::Insert" << NL;
 pdc->Dump();
#endif
   m_clauses.insert(ClauseMapPair(ClauseCode(m_peng,pdc->getCode()), pdc));
   // if a sorted asserted clause has variables, then its no good
   // for optimization of queries.
   if (pdc->getNumVars() > 0)
      b_poisoned = true;
#ifdef BUG_MOD
 DUMP << "  DB after insert: " << NL;
 Dump();
 DUMP << "<==SortedDynamicPredicate::Insert" << NL;
#endif
}

#ifdef LANDFILL
void SortedDynamicPredicate::Dump()
{
   LEngine *m_peng = getPEng();
   DUMP << "SortedDynamicPredicate::Dump()" << NL;
   ClauseMapIterator mci = m_clauses.begin();
   while(mci != m_clauses.end())
   {
      mci->second->Dump();
      mci++;
   }
}
#endif

DCSortedIterator::DCSortedIterator(SortedDynamicPredicate *sdp, TERM head, bool poisoned)
{
   m_sdp = sdp;
   LEngine *m_peng = sdp->getPEng();
   if (poisoned)
   {
      m_cmi = m_sdp->m_clauses.begin();
      m_cmiend = m_sdp->m_clauses.end();
   }
   else
   {
#ifdef BUG_MOD
 DUMP << "==>DCSortedIterator constructor" << NL;
 DUMP << "  head before = ";
 pTSVC->DumpWrite(head);
#endif
      TERM query = pTSVC->query_copy(head);
#ifdef BUG_MOD
 DUMP << "  head after = ";
 pTSVC->DumpWrite(head);
 DUMP << NL;
 DUMP << "  query = ";
 pTSVC->DumpWrite(query);
 DUMP << NL;
#endif
      m_cmi = m_sdp->m_clauses.lower_bound(ClauseCode(m_peng, query));
      pTSVC->setVarsFirst(false);
      m_cmiend = m_sdp->m_clauses.upper_bound(ClauseCode(m_peng, query));
      pTSVC->setVarsFirst(true);
   }
   m_cmilast = m_cmiend;
   m_cmilast--;
#ifdef BUG_MOD
 DUMP << "m_cmi's clause = " << NL;
 m_cmi->second->Dump();
 DUMP << "m_cmilast's clause = " << NL;
 m_cmilast->second->Dump();
 DUMP << "<==DCSortedIterator constructor" << NL;
#endif
}

//------------------------------------------
// IndexedDynamicPredicate
//

IndexedDynamicPredicate::~IndexedDynamicPredicate()
{
   DynamicClause *pdc;
   delete[] m_index_term;
   ClauseListIterator cli = m_clauses.begin();
   while (cli != m_clauses.end())
   {
      pdc = *cli;
      cli++;
      delete pdc;
   }
}

void IndexedDynamicPredicate::Init(TERM ipat)
{
   LEngine *m_peng = getPEng();
   Goal g(m_peng, ipat);
   TERM tipat = ipat;
   TERM x;
#ifdef BUG_MOD
 DUMP << "==>IndexedDynamicPredicate::Init" << NL;
 DUMP << "   ipat = ";
 pTSVC->Dump(ipat);
 DUMP << NL << FLUSH;
#endif
   LNEW(m_index_term, Cell[g.arity + 1], aS("module"));
   TERM mit = m_index_term;
   tipat = tipat->getTerm();
   // we dispense with the formality of the structure cell,
   // and just store the f/a + args.
   *mit = *tipat;
   indexes = 0;
   for (intC i=0; i < (intC)g.arity; i++)
   {
      mit++; tipat++;
      x = tipat->dref();
      if (! x->IsInt())
         pXCPT->Error(instanceE, aS("index args must be integers"));
      if (0 != x->getInt())
         indexes++;
      *mit = *x;
   }
#ifdef BUG_MOD
 DUMP << "<==IndexedDynamicPredicate::Init" << NL;
 DUMP << "   m_index_term = ";
 pTSVC->DumpWrite(m_index_term);
 DUMP << NL;
#endif
}

TERM IndexedDynamicPredicate::getQueryPattern(TERM head)
{
   LEngine *m_peng = getPEng();
#ifdef BUG_MOD
 DUMP << "  QueryPattern head = ";
 pTSVC->Dump(head);
 DUMP << NL;
#endif
   TERM in = head;
   in = in->getTerm();
   TERM pat = m_index_term;
   LASSERT( (in->getAtom() == pat->getAtom() && in->getArity() == pat->getArity()),
         aS("bad query for indexed predicate"));
   TERM query = pHXL->heapGETN(indexes + 2);
//std::cout << pHXL->heapUSE() << SP;
   TERM q = query;
   q++;
   query->setStruct(q);
   //*q = *pat;
   q->setAtom(pat->getAtom());
   q->setArity(indexes);
   ARITY a = pat->getArity();
   for (intC i=0; i< (intC)a; i++)
   {
      pat++; in++;
      if (0 != pat->getInt())
         *(++q) = *(in->dref());
   }
   return query;
}

DynamicClauseIterator *IndexedDynamicPredicate::getIterator(TERM head)
{
   if (m_clauses.empty())
      return NULL;

   LEngine *m_peng = getPEng();
   DCIndexedIterator *pdci;
   LNEW(pdci, DCIndexedIterator(this, head), aS("module"));
   return pdci;
}

void IndexedDynamicPredicate::Abolish()
{
   if (m_clauses.empty())
      return;

   DynamicClause *pdc;
   if (! m_index.empty())
   {
      // Get rid of the index entries

      IndexMapIterator imi = m_index.begin();
      IndexMapIterator temp_imi;
      // another example of ms going non-standard, but right.
      // erase for maps, unlike erase for other collections, is not
      // supposed to return the next iterator.  so you need to go
      // through the contortions below to get it to work on standard
      // complying gcc, although for ms the more intuitive approach works.
      while (imi != m_index.end())
      {
         temp_imi = imi;
         imi++;
         m_index.erase(temp_imi);
      }
   }

   // Get rid of the clauses

   ClauseListIterator cli = m_clauses.begin();
   while (cli != m_clauses.end())
   {
      pdc = *cli;
      delete pdc;
      cli = m_clauses.erase(cli);
   }
}

void IndexedDynamicPredicate::Insert(DynamicClause *pdc, int az)
{
   LEngine *m_peng = getPEng();
   Goal g(m_peng, pdc->getCode());

   TERM key = getQueryPattern(g.head);

   // Shouldn't can't allow this, because even if we
   // add to all known indices, future indices will
   // miss it.
   if (pTSVC->hasVar(key))
      pXCPT->Error(instanceE, aS("can't assert indexed clause with variable in key"));

   KeyCode kc(m_peng, key);
#ifdef BUG_MOD
 DUMP << "Inserting: ";
 pTSVC->Dump(key);
 DUMP << NL;
#endif
   IndexMapIterator ii = m_index.find(kc);
   if (ii == m_index.end())
      ii = (m_index.insert(IndexMapPair(kc, ClauseList()))).first;

   ClauseList *pcl = &(ii->second);
   if (az == 'z')
   {
      m_clauses.push_back(pdc);
      pcl->push_back(pdc);
   }
   else
   {
      m_clauses.push_front(pdc);
      pcl->push_front(pdc);
   }
#ifdef BUG_MOD
   Dump();
#endif
}

void DCIndexedIterator::remove()
{
   LEngine *m_peng = m_idp->getPEng();
   // note this doesn't work, need to find the index
   // the clause is in as well and remove from index
   // when its removed via pathological retract(pet(X,gus))
   // where the index is pet(1,0).  so fix it someday,
   // but for now flag an error
   if (m_pcl == &(m_idp->m_clauses))  // not from an index
   {
      //m_cli = m_pcl->erase(m_cli);
      pXCPT->Error(instanceE, aS("full index must be bound to retract indexed clause"));
   }
   else
   {
      m_idp->m_clauses.remove(*m_cli);
      m_cli = m_pcl->erase(m_cli);
   }
}

#ifdef LANDFILL
void IndexedDynamicPredicate::Dump()
{
   LEngine *m_peng = getPEng();
   DUMP << "==>IndexedDynamicPredicate::Dump()" << NL;
   IndexMapIterator imi = m_index.begin();
   ClauseList *pcl;
   ClauseListIterator cli;
   while(imi != m_index.end())
   {
      DUMP << "  key = ";
	  // this causes compiler complaints since new KeyCode
      //pTSVC->DumpWrite((imi->first).getCode());
      DUMP << NL;
      pcl = &(imi->second);
      cli = pcl->begin();
      while(cli != pcl->end())
      {
         DUMP << "    ";
         pTSVC->DumpWrite((*cli)->getCode());
         DUMP << NL;
         cli++;
      }
      imi++;
   }
   DUMP << "<==IndexedDynamicPredicate::Dump()" << NL;
   DUMP << FLUSH;
}
#endif

DCIndexedIterator::DCIndexedIterator(IndexedDynamicPredicate *idp, TERM head)
{
   m_idp = idp;
   LEngine *m_peng = idp->getPEng();

   // build an index term from the head and the stored pattern.
   // if the index term has variables, then we just start from
   // the beginning, otherwise we can use the index list

   TERM query = idp->getQueryPattern(head);

   if (pTSVC->hasVar(query))
   {
      m_pcl = &idp->m_clauses;
      m_cli = m_pcl->begin();
      return;
   }

   //m_pcl = &(m_idp->m_index.find(KeyCode(m_peng, query))->second);
   //m_cli = m_pcl->begin();
   IndexMapIterator imi = m_idp->m_index.find(KeyCode(m_peng, query));
   if (imi == m_idp->m_index.end())
   {
      // there were no index entries for this query, but we still
      // have to leave space for clauseZdb to call and find out
      m_pcl = NULL;
   }
   else
   {
      m_pcl = &(imi->second);
      m_cli = m_pcl->begin();
   }
}

//---------------------
// DynamicClause
//

Lostream& operator<<( Lostream &os, const DynamicClause &dc )
{
   os << " clause output not implemented ";
   return os;
}

DynamicClause::DynamicClause(DynamicPredicate *pdp, TERM t)
{
   m_pdp = pdp;
   //m_mscw_next = NULL;
   //m_mscw_inuse = NULL;
   m_deleted = false;

   LEngine *m_peng = getPEng();
   if (pTSVC->is_rule(t))
      m_is_rule = true;
   else
      m_is_rule = false;

   int size = t->term_size();
   LNEW(dbcode, Cell[size+1], aS("module"));  // one extra because Copy looks ahead

   TERM dest = dbcode + 1;

   // a GC Thing event, so increment counts for the things
   // in the clause, that is creating a non-heap copy, so
   // must note that in things used in clause.
   m_numvars = pTSVC->Copy(dbcode, dest, &dest, t, GC_INC);
   m_size = size;
}

DynamicClause::~DynamicClause()
{
   // first free up any references to gc things in the clause
   LEngine *m_peng = getPEng();
   pTSVC->UpdateGCThings(dbcode, GC_DEC);
   delete[] dbcode;
}

/*
// Called by p_clauseZdb and p_retractZdb to clear the
// the mscw_next pointer, indicating the clause is no
// longer on the heap in a mscw waiting for the next
// iteration.
void DynamicClause::clearNextMSCW(TERM mscw)
{
   if (m_mscw_next == NULL)
      return;
   // if the new is the same as the old,
   // then clear it
   if (mscw == m_mscw_next)
   {
      m_mscw_next = NULL;
      return;
   }
   // if the new wasn't this, and the old is still valid,
   // don't do anything.
   if (m_mscw_next < mscw && m_mscw_next->IsMSCW() &&
         m_mscw_next->getMSCWDBRef()->getClause() == this)
      return;

   // all other cases, clear it.
   m_mscw_next = NULL;
   return;
}
*/
/*
// Called by get$db and associates when trying to
// clear the mscw indicator that a clause is
// in use.
void DynamicClause::clearInUseMSCW(TERM mscw)
{
   if (m_mscw_inuse == NULL)
      return;
   // if the new is the same as the old,
   // then clear it
   if (mscw == m_mscw_inuse)
   {
      m_mscw_inuse = NULL;
      return;
   }
   // if the new wasn't this, and the old is still valid,
   // don't do anything.
   if ( m_mscw_inuse < mscw && m_mscw_inuse->IsMSCW() &&
         m_mscw_inuse->getMSCWDBRef()->getPrev() == this )
      return;

   // all other cases, clear it.
   m_mscw_inuse = NULL;
   return;
}
*/

/*
void DynamicClause::setNextMSCW(TERM mscw)
{
   if (m_mscw_next == NULL)
   {
      // don't really need to do this, do we?  as mscw's
      // live and die on the heap...
      //mscw->getGCThing()->inc_dbuse();
      m_mscw_next = mscw;
      return;
   }
   // if the old is still valid,
   // don't do anything.
   if (m_mscw_next < mscw && m_mscw_next->IsMSCW()
         && m_mscw_next->getMSCWDBRef()->getClause() == this)
      return;

   // all other cases, set it.
   // see comment above, on dbuse - mscws live and die on heap
   //if( m_mscw_next->IsMSCW() )
   //   m_mscw_next->getGCThing()->dec_dbuse();
   //mscw->getGCThing()->inc_dbuse();

   m_mscw_next = mscw;
   return;
}
*/

/*
void DynamicClause::setInUseMSCW(TERM mscw)
{
   if (m_mscw_inuse == NULL)
   {
      // mscws live and die on heap, so maybe don't
      // need this?
      //mscw->getGCThing()->inc_dbuse();
      m_mscw_inuse = mscw;
      return;
   }
   // if the old is still valid,
   // don't do anything.
   if (m_mscw_inuse < mscw && m_mscw_inuse->IsMSCW()
         && m_mscw_inuse->getMSCWDBRef()->getPrev() == this)
      return;

   // all other cases, set it.
   // mscws live and die on heap, maybe don't need this?
   //if( m_mscw_inuse->IsMSCW() )
   //   m_mscw_inuse->getGCThing()->dec_dbuse();
   //mscw->getGCThing()->inc_dbuse();
   m_mscw_inuse = mscw;
   return;
}
*/

/*
// if its set, make sure its still current, and clear it
// if not.
bool DynamicClause::inUse(LEngine *m_peng)
{
   if (!m_mscw_inuse)
      return false;

   if (!m_mscw_inuse->IsMSCW())
   {
      m_mscw_inuse = NULL;
      return false;
   }

#ifdef BUG_MSCW
 DUMP << "inUse this = " << this << " that = " <<
   m_mscw_inuse->getMSCWDBRef()->getClause() << NL << FLUSH;
#endif

   if (m_mscw_inuse < pHXL->HTop() && m_mscw_inuse->IsMSCW() &&
         m_mscw_inuse->getMSCWDBRef()->getPrev() == this)
      return true;

   m_mscw_inuse = NULL;
   return false;
}

bool DynamicClause::isNext(LEngine *m_peng)
{
   if (!m_mscw_next)
      return false;

   if (m_mscw_next < pHXL->HTop() && m_mscw_next->IsMSCW() &&
         m_mscw_next->getMSCWDBRef()->getClause() == this)
      return true;

   m_mscw_next = NULL;
   return false;
}
*/

#ifdef LANDFILL
void DynamicClause::Dump()
{
   LEngine *m_peng = getPEng();
   DUMP << SP2 << (void*)this << SP2;
   pTSVC->DumpWrite(getCode());
   DUMP << NL;
   //DUMP << SP2 << SP2 << "m_mscw_inuse = " << m_mscw_inuse
   //      << "  m_mscw_next = " << m_mscw_next;
   if (m_deleted)
      DUMP << "  deleted";
   DUMP << NL;
   //DUMP << this;
}
#endif

//------------------------------------------
// Module
//

Module::Module(MODIX imod, PATOM nameA, LEngine *peng)
{
//   m_name = name;
   m_imod = imod;
   m_peng = peng;
   m_nameA = nameA;
}
