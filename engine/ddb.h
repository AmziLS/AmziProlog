/***************************************************************************\
*
* ddb.h --    Dynamic Database Interface
*             Created as a copy of old ddbatom.h, but now
*             acting as dispatcher for modules.
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: ddb.h,v $
* Revision 1.5  2006/12/19 04:58:47  dennis
* module names are just atoms
*
* Revision 1.4  2006/12/15 04:05:55  dennis
* undecapitalization of module names
*
* Revision 1.3  2005/04/25 23:55:37  dennis
* merged sync and apitrace changes
*
* Revision 1.2  2004/01/30 16:33:44  mary
* Added argument and description strings to PredicateHead and alib.
* Added predicate_info and predicate$enginfo to return this information.
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.26  2002/12/06 17:37:36  dennis
* a6-3-2, finally got rid of mscw tagging of heap cells to maintain
* state of dynamic clause backtracking, put backtracking info in
* the choice point structure instead.  much cleaner.  retract can now
* just analyze the control stack to see if a retract is safe.  heapgc
* no longer has to worry about those wierd cells.
*
* Revision 1.25  2002/07/04 16:20:25  dennis
* support academic registration
*
* Revision 1.24  2002/05/15 16:59:07  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.23  2002/04/25 03:42:23  dennis
* more documentation, logicbase.htm, and some fiddling with sources
*
* Revision 1.22  2002/04/19 19:41:43  dennis
* fixed retract bug with sorted/indexed clauses, implemented abolish for
* those types as well
*
* Revision 1.21  2002/04/02 22:52:43  dennis
* Moved the hotel two feet to the right, changing arity and xi
* in .plm files to be 2 bytes rather than 1.
*
* Revision 1.20  2001/10/30 01:01:04  dennis
* sorted and indexed clauses supported in dynamic database
*
* Revision 1.19  2001/10/27 03:24:37  dennis
* sorted dynamic predicates working, with optimized queries
* when they fit the right pattern, of vars last
*
* Revision 1.18  2001/10/24 05:06:34  dennis
* changed linked list of clauses in ddb to use STL list, so links
* no longer in clause, allowing for different types of iterators on
* clauses
*
* Revision 1.17  2001/10/13 02:58:12  dennis
* see/tell bugs, used to close function streams
*
* Revision 1.16  2001/08/05 19:11:20  dennis
* made unload work for .plm files again
*
* Revision 1.15  2001/07/24 02:52:47  dennis
* discontiguous/multifile working again for 6.1
*
* Revision 1.14  2001/04/02 21:50:13  dennis
* got debugger working again
*
* Revision 1.13  2001/03/26 02:29:35  dennis
* Ray's fixed numbers in.
*
* Revision 1.12  2001/03/16 00:29:06  dennis
* compiled metapredicates
*
* Revision 1.11  2001/03/13 20:05:18  dennis
* added meta$call to try and isolate meta calls.
*
* Revision 1.10  2001/02/21 04:46:42  dennis
* debugger working, updated documentation for 6.1
*
* Revision 1.9  2001/02/13 03:42:13  dennis
* trying to fix bug in lsExecStr, cleaning up user:
*
* Revision 1.8  2001/02/05 03:11:43  dennis
* Fixed nasty heap GC bug, other minor fixes
*
* Revision 1.7  2001/01/30 16:47:28  dennis
* Made, after many trials, alib into amzi_system module.
*
* Revision 1.6  2001/01/11 03:39:00  dennis
* Made metapredicates more efficient with is$meta/3
*
* Revision 1.5  2001/01/11 01:49:23  dennis
* Implemented rest of import/export and metapredicates, working
* for test cases.
*
* Revision 1.4  2001/01/06 03:46:02  dennis
* listener working with import/export
*
* Revision 1.3  2001/01/05 06:05:54  dennis
* fixed minor discrepancies
*
* Revision 1.2  2000/12/29 22:39:48  dennis
* added first cut at a system module
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.8  2000/12/28 16:30:35  dennis
* fixed nasty pets bug, true/false on CallInterp()
* and other changes, merged with ray's new stuff
* calling this a6-1-6
*
* Revision 1.7  2000/10/30 15:11:45  ray
* Extended compiler, loader and linker for real data.
* Added arithmetic primitives for rational data in alib.pro
*
* Revision 1.6  2000/10/01 16:20:03  dennis
* cleaned up modules, ddb, got defined, abolish and friends working
*
* Revision 1.5  2000/09/27 01:42:03  dennis
* implemented listing, needed current_predicate, predicate_property,
* and current_module
*
* Revision 1.4  2000/09/25 02:11:19  dennis
* first version of modules working, runs the modular version of
* duck world.  still needs import and export.  release 6.1.1
*
* Revision 1.3  2000/09/15 21:42:24  dennis
* 12->13
*
* Revision 1.2  2000/09/02 02:10:19  dennis
* new version of get$db replacing dbrefgen for getting clauses
* from dynamic database
*
* Revision 1.1  2000/08/26 20:24:10  dennis
* added missing module files
*
* Revision 1.2  2000/05/15 11:24:47  dennis
* fixed some minor bugs, started modules
*
* Revision 1.1  2000/05/14 03:52:32  dennis
* a5-1-8 replaced ddbatom with atomdb and one 'user' module
*
*
\***************************************************************************/

#ifndef DDB_H
#define DDB_H

#include "module.h"

#define UNSPECIFIED_MODULE -2
#define UNDEFINED_MODULE -1
#define SYSTEM_MODULE 0
#define USER_MODULE 1
//#define SYSUSER_MODULE 0
#define N_MODULES 0xfff   // 24 bits in cell - fairly hard limit

class Goal
{
public:
   MODIX imod;
   PATOM functor;
   ARITY arity;
   TERM  modterm;    // for Assert, the term without the mod qualification?
   TERM  head;  // the structure which is the head
   bool  explicit_modix;

   Goal(LEngine *m_peng, TERM t);
};

class DDB
{
private:
   LEngine *m_peng;
   // The modules for this application,
   // and the current module.
   Module *modules[N_MODULES];
   // Not to be confused, this is the current module open for reading,
   // that is, this is set by module, body, end_module, etc. directives.
   // It is different from the calling context module at runtime.
   MODIX   current_imod;
   int     nmods;
   PredIter  pi;  // used for holding iterator

public:
   DDB(LEngine *peng);
   ~DDB();

   void   Init();

   MODIX   NewModule(STRptr modname);
   MODIX   NewModule(PATOM modatom);

   // Add an import module to the current module
   void AddImportModule(PATOM modnameA)
   { modules[current_imod]->AddImportModule(modnameA); }
   void AddImportModule(MODIX imod, PATOM modnameA)
   { modules[imod]->AddImportModule(modnameA); }

   // Add a predicate as import to the current module
   void AddImportPredicate(MODIX modix, PATOM pred, ARITY ar)
   { modules[current_imod]->AddImportPredicate(modix, pred, ar); }

   void MarkExportPredicate(PATOM pred, ARITY ar)
   { modules[current_imod]->MarkExportPredicate(pred, ar); }
   void MarkMetaPredicate(PATOM pred, ARITY ar)
   { modules[current_imod]->MarkMetaPredicate(pred, ar); }
   void MarkDisconPredicate(PATOM pred, ARITY ar)
   { modules[current_imod]->MarkDisconPredicate(pred, ar); }
   void MarkSortedPredicate(PATOM pred, ARITY ar)
   { modules[current_imod]->MarkSortedPredicate(pred, ar); }

   // Enter a built-in predicate, called at startup
   void    EnterBIP(PATOM a, ARITY ar, pBIP bip, bool b_export, STRptr args, STRptr desc)
   { modules[SYSTEM_MODULE]->EnterBIP(a, ar, bip, b_export, args, desc); }

   void AddCompiledPredicate(MODIX imod, PATOM a, ARITY ar, CODEptr code, int fileix)
   { modules[imod]->AddCompiledPredicate(a, ar, code, fileix); }

   bool IsEscapePred(MODIX imod, PATOM pred, ARITY ar)
   { return modules[imod]->IsEscapePred(pred, ar); }
   bool IsDefined(MODIX imod, PATOM pred, ARITY ar)
   { return modules[imod]->IsDefined(pred, ar); }

   CompiledPredicate *getCompiledPredicate(MODIX imod, PATOM at, ARITY ar)
      { return modules[imod]->getCompiledPredicate(at, ar); }
   EscapePredicate *getEscapePredicate(MODIX imod, PATOM at, ARITY ar)
      { return modules[imod]->getEscapePredicate(at, ar); }
   PredicateHead *getExportedPredicate(MODIX imod, PATOM at, ARITY ar)
      { return modules[imod]->getExportedPredicate(at, ar); }

   // Delete all the compiled code that was loaded from the given fileix,
   // walking each module, looking for code from that file.
   void UnloadFile(int fileix);

//   bool IsSpying(MODIX imod, PATOM name, ARITY ar)
//   { return modules[imod]->IsSpying(name, ar); }

//cim   MODIX   getModuleIX(STRptr s);
   MODIX  getModuleIX(PATOM a);
   MODIX   getCurrentMIX()
   { return current_imod; }
//cim   STRptr  getModuleName(MODIX imod)
//cim   { return modules[imod]->getModuleName(); }
   PATOM  getModuleNameA(MODIX imod)
   { return modules[imod]->getModuleNameA(); }
   bool validModix(MODIX imod)
   { return (imod >= 0 && imod < nmods); }

   PredicateHead *getPredicateHead(MODIX imod, PATOM a, ARITY ar)
      { return modules[imod]->getPredicateHead(a, ar); }
   PredicateHead *getPredicateHead(PATOM a, ARITY ar)
      { return modules[current_imod]->getPredicateHead(a, ar); }
   PredicateHead *getVisiblePredicate(MODIX imod, PATOM a, ARITY ar)
   { return (imod >= 0) ? modules[imod]->getVisiblePredicate(a, ar) : NULL; }

   //bool IsSystem(PATOM a, ARITY ar)
   //{ return modules[SYSTEM_MODULE]->IsDefined(a, ar); }
   //bool IsUser(PATOM a, ARITY ar)
   //{ return modules[USER_MODULE]->IsDefined(a, ar); }

   bool CheckExport(PATOM pred, ARITY ar)
   { return modules[current_imod]->CheckExport(pred, ar); }
   bool CheckMeta(PATOM pred, ARITY ar)
   { return modules[current_imod]->CheckMeta(pred, ar); }

   // Extended predicate functions

#ifdef _UNICODE
   void   InitPredTab(PRED_INITWptr p)
      //{ modules[USER_MODULE]->InitPredTab(p); }
      { modules[current_imod]->InitPredTab(p); }
#endif
   void   InitPredTab(PRED_INITAptr p)
      //{ modules[USER_MODULE]->InitPredTab(p); }
      { modules[current_imod]->InitPredTab(p); }
   void   AddPred(STRptr s, ARITY a, ExtPred ep, VOIDptr vp)
      //{ modules[USER_MODULE]->AddPred(s, a, ep, vp); }
      { modules[current_imod]->AddPred(s, a, ep, vp); }

   // Assert, used by p_assert and LSAPI calls
   RC Assert(int az, TERM t);
   RC Assert(MODIX imod, int az, TERM t);

   // Used by logic server
   TF Retract(TERM t);

   // Not used now, maybe make a user callable predicate?  or
   // call from memory failures?  The problem is, there won't
   // be much garbage in the new system.
   void GC()
      { for (int i= 0; i < nmods; i++) modules[i]->GC(); }

   // Interpreter Call functions

   CODEptr CallInterp(CODEptr, TF);
   CODEptr DoCall(CODEptr, TF);
   CODEptr DoCallNoMeta(CODEptr, TF);
   CODEptr DoCallMeta(CODEptr, TF);

   // predicates

   TF p_moduleZ(void);
   TF p_end_moduleZ(void);

   TF p_asserta(void);
   TF p_assertz(void);
   TF p_assertZ(void);
   TF p_debug_data(void);

   TF p_abolish(void);
   TF p_iscode(void);
   TF p_isZcode(void);
   TF p_definedZ(void);
   TF p_setZdiscontiguous(void);

   TF p_import_1(void);
   TF p_importZ2(void);
   TF p_importZmod(void);
   TF p_exportZ(void);
   TF p_setZmeta(void);
   TF p_setZsorted(void);
   TF p_setZindexed(void);
   TF p_retractZdb(void);
   TF p_clauseZdb(void);
   TF p_isZmeta(void);

   TF p_currentZmodule(void);
   TF p_loading_module(void);
   TF p_getZpred(void);
   TF p_predicateZproperty(void);
   TF p_predicateZenginfo(void);
   TF p_moduleZindex(void);
   TF p_remove_module(void);

private:
   void remove_ddb(MODIX imod, DynamicClauseIterator* pdci)
      { modules[imod]->remove_ddb(pdci); }

#ifdef LANDFILL
public:
   TF p_dumpdb(void);
   void Dump();
   void Dump(MODIX i)
   { modules[i]->Dump(); }
   //void mscw_heap_walk();
   void predicate_dump(DynamicPredicate *pdp);
#endif
};

#endif // DDB_H





