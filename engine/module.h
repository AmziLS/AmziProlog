/***************************************************************************\
*
* module.h -- Module Definitions
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: module.h,v $
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
* Revision 1.30  2002/12/06 17:37:36  dennis
* a6-3-2, finally got rid of mscw tagging of heap cells to maintain
* state of dynamic clause backtracking, put backtracking info in
* the choice point structure instead.  much cleaner.  retract can now
* just analyze the control stack to see if a retract is safe.  heapgc
* no longer has to worry about those wierd cells.
*
* Revision 1.29  2002/12/02 18:25:12  dennis
* Converted to Visual Studio .NET.
*
* Revision 1.28  2002/09/27 00:27:23  dennis
* fix another retract pulls the rug out from goal bug
*
* Revision 1.27  2002/09/20 20:59:40  dennis
* changes to get Solaris to build with gcc 3.2
*
* Revision 1.26  2002/05/15 16:59:09  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.25  2002/04/19 19:41:43  dennis
* fixed retract bug with sorted/indexed clauses, implemented abolish for
* those types as well
*
* Revision 1.24  2002/03/10 22:18:58  dennis
* fixed some small bugs, updated documentation
*
* Revision 1.23  2002/03/07 04:37:43  dennis
* updated tutorial and duckworld samples
*
* Revision 1.22  2002/03/04 04:22:53  dennis
* changed sorted iterator erase again, as gcc follows the stl standard,
* which is dumb but standard, and ms, hate to see them as the good guys,
* does the right thing, which is not standard
*
* Revision 1.21  2002/03/04 03:46:12  dennis
* fixed bug in sorted predicates
*
* Revision 1.20  2002/01/06 20:31:28  dennis
* put in new memory leak LNEW that reports on lines of code, had
* to take out std:: classes from engine and replace with pointers
* to classes to get it to work, due to various news and deletes in
* the stl class libraries.
*
* Revision 1.19  2001/12/08 03:48:22  dennis
* fixed bugs with reconsult, added unicode support
*
* Revision 1.18  2001/12/06 16:43:21  dennis
* fixed heapgc bug and allowed for unlimited vars in copyterm
*
* Revision 1.17  2001/11/11 19:03:54  dennis
* STL erase doesn't return iterator in Linux, which is correct
* by the standard.  so fixed it for sorted predicates.
*
* Revision 1.16  2001/11/09 02:28:10  dennis
* Finished, I hope, sorted and indexed predicates. Needed to rethink
* how mscws worked, given the new itertors for moving from clause to
* clause.
*
* Revision 1.15  2001/10/30 01:01:04  dennis
* sorted and indexed clauses supported in dynamic database
*
* Revision 1.14  2001/10/27 03:24:37  dennis
* sorted dynamic predicates working, with optimized queries
* when they fit the right pattern, of vars last
*
* Revision 1.13  2001/10/26 01:28:16  dennis
* sorted predicates partially implemented
*
* Revision 1.12  2001/10/24 14:19:48  dennis
* changed a name on buttercup, just testing really
*
* Revision 1.11  2001/10/24 05:06:34  dennis
* changed linked list of clauses in ddb to use STL list, so links
* no longer in clause, allowing for different types of iterators on
* clauses
*
* Revision 1.10  2001/10/13 02:58:13  dennis
* see/tell bugs, used to close function streams
*
* Revision 1.9  2001/08/05 19:11:20  dennis
* made unload work for .plm files again
*
* Revision 1.8  2001/07/24 02:52:47  dennis
* discontiguous/multifile working again for 6.1
*
* Revision 1.7  2001/07/10 16:51:32  dennis
* added more memory leak checking tools, all clean so far
*
* Revision 1.6  2001/06/27 15:15:10  dennis
* miscellaneous changes and bug fixes, work with leak detection
*
* Revision 1.5  2001/04/02 21:50:13  dennis
* got debugger working again
*
* Revision 1.4  2001/02/05 03:11:44  dennis
* Fixed nasty heap GC bug, other minor fixes
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
* Revision 1.9  2000/10/30 15:11:45  ray
* Extended compiler, loader and linker for real data.
* Added arithmetic primitives for rational data in alib.pro
*
* Revision 1.8  2000/10/01 17:02:12  dennis
* cleaned up bigdig comments
*
* Revision 1.7  2000/10/01 16:20:04  dennis
* cleaned up modules, ddb, got defined, abolish and friends working
*
* Revision 1.6  2000/09/27 01:42:03  dennis
* implemented listing, needed current_predicate, predicate_property,
* and current_module
*
* Revision 1.5  2000/09/15 21:42:25  dennis
* 12->13
*
* Revision 1.4  2000/09/02 02:10:20  dennis
* new version of get$db replacing dbrefgen for getting clauses
* from dynamic database
*
* Revision 1.3  2000/08/26 00:32:07  dennis
* Merged with ray's changes for new numbers and ISO standard features.
* Added new AtomTable, Dynamic Database and operators based on STL
* maps as basis for ISO modules.
*
* Revision 1.2  2000/05/23 03:37:44  dennis
* changed name of various variables that used to be called mod this
* and that for keeping track of load modules.  Those are now 'file' variables
* and 'mod' is used for real modules.
*
* Revision 1.1  2000/05/14 03:52:34  dennis
* a5-1-8 replaced ddbatom with atomdb and one 'user' module
*
*
\***************************************************************************/

#ifndef MODULE_H
#define MODULE_H

typedef TF (BuiltIns::*pBIP)();

class PredicateHead;
class BuiltInPredicate;   // builtin predicate
class ExtendedPredicate;  // extended predicate (from an LSX)
class CompiledPredicate;  // compiled predicate
class DynamicPredicate;   // dynamic predicate
class ImportPredicate;    // imported predicate
class DynamicClause;
class ClauseCode;
class DCListIterator;
class DCSortedIterator;
class DCIndexedIterator;
class Goal;

// Used as key for sorted predicates
class ClauseCode
{
public:
   // engine needed for comparing clause codes
   LEngine *m_peng;
   TERM m_code;

   ClauseCode() {};
   ClauseCode(LEngine *peng, TERM code)
   { m_peng = peng; m_code = code; }
   ClauseCode(const ClauseCode &cc)
   {
      m_peng = cc.m_peng;
      m_code = cc.m_code;
   }
   ~ClauseCode() {};
};

// Clause codes, used in sorted predicates, are pointers to the
// actual ddb code, so no need for a second copy.  Key codes,
// on the other hand, are generated and thus need a copy to preserve
// themselves, and reference counting because of all the copies
// generated calling operator< from the STL map container.
class KeyCode;

class KeyRep
{
public:
   LEngine *m_peng;
   TERM m_code;
   int m_count;
   KeyRep(LEngine *peng, TERM key);
   ~KeyRep();
};

class KeyCode
{
public:
   // engine needed for comparing clause codes
   KeyRep  *m_rep;

   KeyCode(LEngine *peng, TERM code);
   KeyCode(const KeyCode &kc)
   {
      m_rep = kc.m_rep;
      m_rep->m_count++;
   }
   ~KeyCode();
   LEngine *getPEng()
   { return m_rep->m_peng; }
   TERM getCode()
   { return m_rep->m_code; }
};

bool operator<(ClauseCode a, ClauseCode b);
bool operator<(KeyCode a, KeyCode b);

typedef std::list<DynamicClause*> ClauseList;
typedef ClauseList::iterator ClauseListIterator;

typedef std::map<ClauseCode, DynamicClause*> ClauseMap;
typedef ClauseMap::iterator ClauseMapIterator;
typedef std::pair<ClauseCode, DynamicClause*> ClauseMapPair;

//typedef std::vector<ClauseListIterator> ClauseIndex;
//typedef ClauseIndex::iterator ClauseIndexIterator;
typedef std::map<KeyCode, ClauseList> IndexMap;
typedef IndexMap::iterator IndexMapIterator;
typedef std::pair<KeyCode, ClauseList> IndexMapPair;

typedef std::map<LPredicate, PredicateHead*> PredMap;
typedef PredMap::iterator PredIter;
typedef std::pair<LPredicate, PredicateHead*> PredPair;

#define   ctypeM    0x000F
#define   importC   0x0001   // clause is imported
#define   sysC      0x0002   // system
#define   compC     0x0004   // clause is compiled 
#define   extC      0x0008   // clause is externally defined
#define   mfC       0x0010   // member function system predicate
#define   dynC      0x0020   // dynamic predicate
#define   disconC   0x0040   // predicate is discontiguous
#define   exportC   0x0100   // predicate is exported
#define   metaC     0x0200   // predicate is meta predicate
#define   sortedC   0x0400   // predicate is sorted
#define   indexedC  0x0800   // predicate is indexed
//#define   protecC   0x2000   // clause is protected from modification 

//--------------------------------------------------------------
// PredicateHead is what is stored directly in the dynamic database.
// It is then used to access the various sorts of predicates
// the system may have.
//

class PredicateHead
{
private:
   union
   {
      BuiltInPredicate   *m_builtin;   // builtin predicate
      ExtendedPredicate  *m_extended;  // extended predicate (from an LSX)
      CompiledPredicate  *m_compiled;  // compiled predicate
      ImportPredicate    *m_import;    // imported predicate
      DynamicPredicate   *m_dynamic;   // dynamic predicate
   } m_code;
   PATOM  m_name;
   ARITY  m_arity;
   MODIX  m_imod;
   intCH  m_info;
   STRptr m_args;
   STRptr m_desc;
   LEngine *m_peng;
public:
   PredicateHead(LEngine *peng, BuiltInPredicate *pbip, MODIX imod, PATOM name, ARITY ar, bool b_export, STRptr args, STRptr desc);
   PredicateHead(LEngine *peng, ExtendedPredicate *extp, MODIX imod, PATOM name, ARITY ar, bool b_export, STRptr args, STRptr desc);
   PredicateHead(LEngine *peng, CompiledPredicate *cp, MODIX imod, PATOM name, ARITY ar, bool b_export, STRptr args, STRptr desc);
   PredicateHead(LEngine *peng, DynamicPredicate *dp, MODIX imod, PATOM name, ARITY ar, bool b_export, STRptr args, STRptr desc);
   PredicateHead(LEngine *peng, ImportPredicate *ip, MODIX imod, PATOM name, ARITY ar, bool b_export, STRptr args, STRptr desc);

   ~PredicateHead();

   friend Lostream& operator<<( Lostream &os, PredicateHead &p );

   bool IsBuiltIn()
   { return (m_info & mfC) ? true : false; }
   bool IsExtended()
   { return (m_info & extC) ? true : false; }
   bool IsCompiled()
   { return (m_info & compC) ? true : false; }
   bool IsDynamic()
   { return (m_info & dynC) ? true : false; }
   //bool IsProtected()
   //{ if (m_info & protecC) return true; else return false; }
   bool IsImported()
   { return (m_info & importC) ? true : false; }
   bool IsExported()
   { return (m_info & exportC) ? true : false; }
   bool IsMeta()
   { return (m_info & metaC) ? true : false; }
   bool IsDiscon()
   { return ( m_info & disconC ) ? true : false; }
   bool IsSorted()
   { return ( m_info & sortedC ) ? true : false; }
   bool IsIndexed()
   { return ( m_info & indexedC ) ? true : false; }

   bool IsEscape()
   { return IsBuiltIn() || IsExtended(); }

   //void setProtected()
   //{ m_info |= protecC; }
   void setExport()
   { m_info |= exportC; }
   void setMeta()
   { m_info |= metaC; }
   void setDiscon()
   { m_info |= disconC; }
   void setSorted()
   { m_info |= sortedC; }
   void setIndexed()
   { m_info |= indexedC; }

   void setCompiledPredicate(CompiledPredicate *pcp)
   {
      LASSERT(IsCompiled(), aS("invalid set compiled predicate"));
      m_code.m_compiled = pcp;
   }

   LEngine *getPEng()
   { return m_peng; }
   PATOM getName()
   { return m_name; }
   ARITY getArity()
   { return m_arity; }
   MODIX getModuleIX()
   { return m_imod; }
   STRptr getArgsStr()
   { return m_args; }
   STRptr getDescStr()
   { return m_desc; }

   BuiltInPredicate *getBuiltInPredicate()
   { return m_code.m_builtin; }
   ExtendedPredicate *getExtendedPredicate()
   { return m_code.m_extended; }
   CompiledPredicate *getCompiledPredicate()
   { return m_code.m_compiled; }
   DynamicPredicate *getDynamicPredicate()
   { return m_code.m_dynamic; }
   ImportPredicate *getImportPredicate()
   { return m_code.m_import; }

   EscapePredicate *getEscapePredicate();

#ifdef LANDFILL
   void Dump();
#endif
};

class CompiledPredicate
{
private:
   CompiledPredicate *m_clink;
   CODEptr            m_code;
   int                m_fileix;
   //bool               m_continued;
   PredicateHead     *m_pph;
public:
   CompiledPredicate(CODEptr code, int fileix)
   { 
	  m_code = code; 
	  m_clink = NULL; 
	  m_fileix = fileix; 
	  m_pph = NULL; 
   }
   ~CompiledPredicate();

   bool IsContinued()
   { return m_clink != NULL; }

   CODEptr getCode()
   { return m_code; }
   CompiledPredicate *getNext()
   { return m_clink; }
   int getFileIX()
   { return m_fileix; }

   PATOM getName()
   { return m_pph->getName(); }
   ARITY getArity()
   { return m_pph->getArity(); }
   MODIX getModuleIX()
   { return m_pph->getModuleIX(); }

   void setNext(CompiledPredicate *next)
   { m_clink = next; }
   void setPredicateHead(PredicateHead *pph)
   { m_pph = pph; }
};

//--------------------------------------------------------
// EscapePredicate are not Prolog.  They can have
// multiple flavors.  So far they are builtins, part
// of the core C++ engine, and extended, part of
// external application definitions.
//

class EscapePredicate
{
protected:
   PredicateHead *m_pph;
public:
   EscapePredicate()
   { m_pph = NULL; }
   friend Lostream& operator<<( Lostream &os, EscapePredicate &p );
   virtual TF execute() = 0;

   PATOM getName()
   { return m_pph->getName(); }
   ARITY getArity()
   { return m_pph->getArity(); }

   void setPredicateHead(PredicateHead *pph)
   { m_pph = pph; }
};

class BuiltInPredicate : public EscapePredicate
{
private:
   LEngine *m_peng;
   pBIP  m_pbip;
public:
   BuiltInPredicate(LEngine *peng, pBIP pbip)
         : EscapePredicate()
   { m_peng = peng; m_pbip = pbip; }
   TF execute()
   {
//      DUMP << "executing bip" << this << PREDAR(getName(), getArity()) << NL << FLUSH;
      return (pBIPS->*m_pbip)(); }
};

class ExtendedPredicate : public EscapePredicate
{
private:
   ExtPred m_func;
   void   *m_arg;
public:
   ExtendedPredicate(ExtPred func, void *arg)
      : EscapePredicate()
   { m_func = func; m_arg = arg; }
   TF execute()
   { return (*m_func)(m_arg); }
};

//---------------------------------------------
// Predicates imported into a module from
// a different module
//

class ImportPredicate
{
private:
   MODIX m_modix;
   PredicateHead *m_pph;

public:
   ImportPredicate(MODIX modix)
   { m_modix = modix; }

   void setPredicateHead(PredicateHead *pph)
   { m_pph = pph; }

   MODIX getModix()
   { return m_modix; }
};

//---------------------------------------------------------------
// Dynamic Predicates can come in a variety of flavors.
// So DynamicPredicate is an abstract class defining the
// interface to the predicate.  The underlying implementation
// might be a linked list, or b-tree or whatever.  This
// allows us to easily increase the options available for
// dynamic data, including extensions to external data stores.
//

class DynamicPredicate
// replace direct links with separate lists
// that are walked instead.
{
private:
   PredicateHead *m_pph;
public:
   DynamicPredicate() {};
   virtual ~DynamicPredicate() {};

   // return NULL if no clauses
   virtual DynamicClauseIterator *getIterator(TERM head) = 0;
   virtual void Insert(DynamicClause *pnewdc, int az) = 0;
   virtual void Abolish() = 0;

   friend Lostream & operator<<( Lostream &, DynamicPredicate & );

   //void setFirst(DynamicClause *pdc)
   //{  first = pdc; }
   //void setLast(DynamicClause *pdc)
   //{  last = pdc; }
   void setPredicateHead(PredicateHead *pph)
   { m_pph = pph; }
   PredicateHead *getPredicateHead()
   { return m_pph; }
   LEngine *getPEng()
   { return m_pph->getPEng(); }
#ifdef LANDFILL
   virtual void Dump() = 0;
#endif
};

//--------------------------------------------------------------
// A DynamicPredicate will have an iterator associated with
// it.  The backtracking predicates keep a pointer to that
// iterator in the various mscw heap control stack cells.
// This is our own iterator definition, supporting the
// functions needed for the ddb predicates.  For those
// predicates that are stored using STL containers, the
// DynamicClauseIterator will wrap an STL iterator.
//

class DynamicClauseIterator
{
public:
   virtual ~DynamicClauseIterator() {};
   // returns current clause
   virtual DynamicClause* getClause() = 0;
   // increments iterator to next
   virtual void setNext() = 0;
   // leaves iterator as it was, returns previous clause
   virtual DynamicClause* getPrev() = 0;
   // true if current clause is last one
   virtual bool isLast() = 0;
   virtual bool isFirst() = 0;
   // true if past the end
   virtual bool isDone() = 0;
   // remove the current clause
   virtual void remove() = 0;
   virtual DynamicPredicate *getDynamicPredicate() = 0;
};

//--------------------------------------------
// Clauses stored in a list
//

class ListDynamicPredicate : public DynamicPredicate
{
friend class DCListIterator;

private:
   ClauseList m_clauses;
public:
   ListDynamicPredicate() {};
   ~ListDynamicPredicate();

   DynamicClauseIterator *getIterator(TERM head);
   void Insert(DynamicClause *pdc, int az)
   { if (az == 'z') m_clauses.push_back(pdc);
     else m_clauses.push_front(pdc); }
   void Abolish();

#ifdef LANDFILL
   void Dump();
#endif
};

class DCListIterator : public DynamicClauseIterator
{
private:
   ListDynamicPredicate *m_ldp;
   ClauseListIterator m_cli;
public:
   DCListIterator(ListDynamicPredicate *ldp)
   { m_ldp = ldp; m_cli = m_ldp->m_clauses.begin(); }
   ~DCListIterator() {};

   DynamicClause *getClause()
   { return *m_cli; }
   void setNext()
   { m_cli++; }
   DynamicClause *getPrev()
   { m_cli--; return *m_cli++; }
   bool isLast()
   { return m_cli == (--(m_ldp->m_clauses.end())) ? true : false; }
   bool isDone()
   { return m_cli == m_ldp->m_clauses.end() ? true : false; }
   bool isFirst()
   { return m_cli == m_ldp->m_clauses.begin() ? true : false; }
   void remove()
   { m_cli = m_ldp->m_clauses.erase(m_cli); }
   DynamicPredicate *getDynamicPredicate()
   { return m_ldp; }
};

//-------------------------------------------------------
// Clauses stored in a map (b-tree and sorted)
//

class SortedDynamicPredicate : public DynamicPredicate
{
friend class DCSortedIterator;

private:
   ClauseMap m_clauses;
   // If the clauses to be sorted have any clause with
   // a variable in it, then it is no longer possible
   // to optimize queries, thus the poisoned flag.
   bool b_poisoned;
public:
   SortedDynamicPredicate()
   { b_poisoned = false; };
   ~SortedDynamicPredicate();

   // return NULL if no clauses
   DynamicClauseIterator *getIterator(TERM head);
   void Insert(DynamicClause *pdc, int az);
   void Abolish();

#ifdef LANDFILL
   void Dump();
#endif
};

class DCSortedIterator : public DynamicClauseIterator
{
private:
   SortedDynamicPredicate *m_sdp;
   ClauseMapIterator m_cmi;
   ClauseMapIterator m_cmiend;
   ClauseMapIterator m_cmilast;
public:
   DCSortedIterator(SortedDynamicPredicate *sdp, TERM head, bool poisoned);
   DCSortedIterator() {};

   DynamicClause *getClause()
   { return m_cmi->second; }
   void setNext()
   { m_cmi++; }
   DynamicClause *getPrev()
   { m_cmi--; return m_cmi++->second; }
   bool isLast()
   //{ return m_cmi == (--(m_sdp->m_clauses.end())) ? true : false; }
   { return m_cmi == m_cmilast ? true : false; }
   bool isDone()
   { return (m_cmi == m_cmiend || m_cmi == m_sdp->m_clauses.end()) ? true : false; }
   bool isFirst()
   { return (m_cmi == m_sdp->m_clauses.begin()); }
   // another example of ms going non-standard, but right.
   // erase for maps, unlike erase for other collections, is not
   // supposed to return the next iterator.  so you need to go
   // through the contortions below to get it to work on standard
   // complying gcc, although for ms the more intuitive approach works.
   void remove()
   //{ m_cmi = m_sdp->m_clauses.erase(m_cmi); }   // works ms only, but erase should return void for maps according to docs
   { ClauseMapIterator temp_cmi = m_cmi;
     m_cmi++;
     m_sdp->m_clauses.erase(temp_cmi); }
   DynamicPredicate *getDynamicPredicate()
   { return m_sdp; }
};

//-------------------------------------------------------
// Clauses stored with an index on the keyed arguments
//
// Using a clause list in the index, and a clause list
// for all the clauses as well.  This is because there
// might be variables in queries, requiring non-indexed
// walk, but if we could guarantee no-variables, we 
// could eliminate the main list.  Hmmm, I suspect its
// better to keep both, because there might be variables
// in key elements.
//

class IndexedDynamicPredicate : public DynamicPredicate
{
friend class DCIndexedIterator;

private:
   ClauseList m_clauses;
   IndexMap m_index;
   Cell *m_index_term;  // e.g.   pet(1,0)
   int indexes;
public:
   IndexedDynamicPredicate() {};
   ~IndexedDynamicPredicate();
   void Init(TERM index_pattern);

   DynamicClauseIterator *getIterator(TERM head);
   void Insert(DynamicClause *pdc, int az);
   void Abolish();

private:
//   TERM IndexedDynamicPredicate::getQueryPattern(TERM head);
   TERM getQueryPattern(TERM head);

public:
#ifdef LANDFILL
   void Dump();
#endif
};

class DCIndexedIterator : public DynamicClauseIterator
{
private:
   IndexedDynamicPredicate *m_idp;
   // will be NULL if no clause list for query index entry
   ClauseList *m_pcl;
   ClauseListIterator m_cli;
public:
   DCIndexedIterator(IndexedDynamicPredicate *idp, TERM head);
   DCIndexedIterator() {};

   DynamicClause *getClause()
   { return *m_cli; }
   void setNext()
   { m_cli++; }
   DynamicClause *getPrev()
   { m_cli--; return *m_cli++; }
   bool isLast()
   { return m_cli == (--(m_pcl->end())) ? true : false; }
   bool isDone()
   { return (m_pcl == NULL || m_cli == m_pcl->end()) ? true : false; }
   bool isFirst()
   { return (m_cli == m_pcl->begin()); }
   void remove();
   DynamicPredicate *getDynamicPredicate()
   { return m_idp; }
};

//------------------------------------------------
// Dynamic Clause is the actual clause stored
// in the dynamic database
//

class DynamicClause
{
//friend bool operator<(DynamicClause a, DynamicClause b);

private:
   DynamicPredicate *m_pdp;
   Cell          *dbcode;
   intC           m_size;
   intCH          m_numvars;
   //TERM           m_mscw_inuse;
   //TERM           m_mscw_next;
   //CHOICE_POINTptr m_b_inuse;
   //CHOICE_POINTptr m_b_next;
   bool           m_deleted;
   bool           m_is_rule;   // we will protect rules from being actually deleted

public:
   DynamicClause(DynamicPredicate *pdp, TERM t);
   ~DynamicClause();

   friend Lostream & operator<<( Lostream &, const DynamicClause & );

   Cell *getCode()
   { return dbcode; }
   intCH getNumVars()
   { return m_numvars; }
   intC getSize()
   { return m_size; }
   DynamicPredicate *getDynamicPredicate()
   { return m_pdp; }

   bool isDeleted()
   { return m_deleted; }
   void setDeleted()
   { m_deleted = true; }
   bool is_rule()
   { return m_is_rule; }

   LEngine *getPEng()
   { return m_pdp->getPEng(); }

   //void clearNextMSCW(TERM mscw);
   //void setNextMSCW(TERM mscw);
   //bool inUse(LEngine *m_peng);
   //bool isNext(LEngine *m_peng);
   //TERM getNextMSCW()
   //{ return m_mscw_next; }
   // more straight-forward, because can't have
   // two retracts looking at same next clause.
   //void clearInUseMSCW(TERM mscw);
   //void setInUseMSCW(TERM mscw);
   //TERM getInUseMSCW()
   //{ return m_mscw_inuse; }
   //bool inUseRetract(LEngine *m_peng);

   void setInUse(CHOICE_POINTptr);

#ifdef LANDFILL
   void Dump();
#endif
};

//--------------------------------------------------
// The base module class, which uses the concepts
// above.  Specific types of modules will be derived
// from this class.  The classic, ModClassic, is the
// standard Prolog dynamic database.  Others slated
// for the future are database and persistent modules.
//

class Module
{
protected:
//cim   STRptr      m_name;
   PATOM       m_nameA;
   LEngine    *m_peng;
   MODIX       m_imod;

public:
//cim   Module(MODIX imod, STRptr name, LEngine *peng);
   Module(MODIX imod, PATOM nameA, LEngine *peng);
   virtual ~Module() {};

//cim   STRptr getModuleName()
//cim      { return m_name; }
   PATOM getModuleNameA()
   { return m_nameA; }
   MODIX getModuleIX()
   { return m_imod; }

   virtual void AddImportModule(PATOM modA) = 0;
   virtual void AddImportModule(MODIX imod) = 0;
   virtual void AddImportPredicate(MODIX modix, PATOM pred, ARITY ar) = 0;

   virtual void MarkExportPredicate(PATOM pred, ARITY ar) = 0;
   virtual void MarkMetaPredicate(PATOM pred, ARITY ar) = 0;
   virtual void MarkDisconPredicate(PATOM pred, ARITY ar) = 0;
   virtual void MarkSortedPredicate(PATOM pred, ARITY ar) = 0;

   virtual bool CheckExport(PATOM pred, ARITY ar) = 0;
   virtual bool CheckMeta(PATOM pred, ARITY ar) = 0;
   virtual bool CheckDiscon(PATOM pred, ARITY ar) = 0;
   virtual bool CheckSorted(PATOM pred, ARITY ar) = 0;

   virtual void EnterBIP(PATOM a, ARITY ar, pBIP bip, bool b_export, STRptr args, STRptr desc) = 0;
   virtual void AddCompiledPredicate(PATOM a, ARITY ar, CODEptr code, 
												 int fileix) = 0;
   virtual void AddIndexedPredicate(TERM index_pattern) = 0;

   virtual PredicateHead *getPredicateHead(PATOM a, ARITY ar) = 0;
   virtual PredicateHead *getVisiblePredicate(PATOM a, ARITY ar) = 0;
   virtual PredicateHead *getExportedPredicate(PATOM a, ARITY ar) = 0;

   virtual bool IsEscapePred(PATOM pred, ARITY ar) = 0;
   virtual EscapePredicate *getEscapePredicate(PATOM name, ARITY arity) = 0;

   virtual void UnloadFile(int fileix) = 0;

//   virtual bool IsSpying(PATOM name, ARITY ar) = 0;
   virtual bool IsDefined(PATOM name, ARITY ar) = 0;

//--- mason dixon ---

   // Atom table functions

   virtual void Init() = 0;

   // Extended predicate functions, these can be stubs for
   // modules not used to define 'user'.

#ifdef _UNICODE
   virtual void InitPredTab(PRED_INITWptr p) = 0;
#endif
   virtual void InitPredTab(PRED_INITAptr p) = 0;
   virtual void AddPred(STRptr s, ARITY a, ExtPred ep, VOIDptr vp) = 0;

   // Dynamic database functions

   virtual CompiledPredicate *getCompiledPredicate(PATOM name, ARITY arity)= 0;

   virtual RC Assert(int az, Goal g, TERM t) = 0;
   virtual TF Retract(TERM t) = 0;
   virtual void Abolish(PATOM name, ARITY arity) = 0;
   virtual void GC() = 0;

   virtual void remove_ddb(DynamicClauseIterator* pdci) = 0;

   // functions used by DDB to get all the predicates
   // in a module, for use by listing and the like.
   virtual LPredicate get_first_pred(PredIter *ppi) = 0;
   virtual LPredicate get_next_pred(PredIter *ppi) = 0;

   // Debugging functions

   virtual void Dump() = 0;
};

class ModClassic : public Module
{
private:
   PredMap *m_predmap;
   std::vector<MODIX> *m_import_mods;

   // a temporary storage place for exports that have not
   // been defined yet
   std::vector<LPredicate> *m_exports;
   std::vector<LPredicate> *m_metas;
   std::vector<LPredicate> *m_discons;
   std::vector<LPredicate> *m_sorteds;

   // a linked list of retracted clauses that
   // couldn't be actually deleted.
   //DynamicClause *m_kill_list;
   ClauseList m_kill_list;
   int  recent_purgatories;

public:
   ModClassic(MODIX imod, PATOM nameA, LEngine *peng)
         : Module(imod, nameA, peng)  { recent_purgatories = 0; }
   virtual ~ModClassic();

   void Init();

   void AddImportModule(PATOM modA);
   void AddImportModule(MODIX imod);
   void AddImportPredicate(MODIX modix, PATOM pred, ARITY ar);

   void MarkExportPredicate(PATOM pred, ARITY ar);
   void MarkMetaPredicate(PATOM pred, ARITY ar);
   void MarkDisconPredicate(PATOM pred, ARITY ar);
   void MarkSortedPredicate(PATOM pred, ARITY ar);

   bool CheckExport(PATOM pred, ARITY ar);
   bool CheckMeta(PATOM pred, ARITY ar);
   bool CheckDiscon(PATOM pred, ARITY ar);
   bool CheckSorted(PATOM pred, ARITY ar);

   void EnterBIP(PATOM a, ARITY ar, pBIP bip, bool b_export, STRptr args, STRptr desc);

   // Add a compiled predicate block, which might be chained to a
   // previous block for the same predicate.
   void AddCompiledPredicate(PATOM a, ARITY ar, CODEptr code, int fileix);
   void AddIndexedPredicate(TERM index_pattern);

   CompiledPredicate *getCompiledPredicate(PATOM name, ARITY arity);

   PredicateHead *getPredicateHead(PATOM a, ARITY ar);
   PredicateHead *getVisiblePredicate(PATOM a, ARITY ar);
   PredicateHead *getExportedPredicate(PATOM a, ARITY ar);

   bool IsEscapePred(PATOM pred, ARITY ar); // true if pred builtin or extended

   EscapePredicate *getEscapePredicate(PATOM name, ARITY arity);

   void UnloadFile(int fileix);

//   bool IsSpying(PATOM name, ARITY ar)
//   { return getPredicateHead(name,ar)->IsSpying(); }
   bool IsDefined(PATOM name, ARITY ar)
   { return getPredicateHead(name,ar) ? TRUE : FALSE; }

   // Extended predicate functions

#ifdef _UNICODE
   void InitPredTab(PRED_INITWptr p);
#endif
   void InitPredTab(PRED_INITAptr p);
   void AddPred(STRptr s, ARITY a, ExtPred ep, VOIDptr vp);

   // Dynamic database functions

   RC Assert(int az, Goal g, TERM t);
   TF Retract(TERM t);
   void Abolish(PATOM name, ARITY arity);
   void GC();

   void remove_ddb(DynamicClauseIterator* pdci);

   // functions used by DDB to get all the predicates
   // in a module, for use by listing and the like.
   LPredicate get_first_pred(PredIter *ppi);
   LPredicate get_next_pred(PredIter *ppi);

   // Debugging functions

   void Dump();

private:
   void   MakeExtSys(STRptr, ARITY, ExtPred, VOIDptr);
   LBOOL  inuse(DynamicClause*);
};


// Inline functions that couldn't be declared in
// the header due to definition ordering.

inline EscapePredicate *PredicateHead::getEscapePredicate()
{
   return IsBuiltIn() ?  m_code.m_builtin :
		 (IsExtended() ? (EscapePredicate *)m_code.m_extended : NULL);
}

#endif  // MODULE_H
