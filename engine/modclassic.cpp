/***************************************************************************\
*
* modclassic.cpp -- Classic Prolog Module Implementation
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
\***************************************************************************/
#include "inc.h"
#include "pch.h"

#ifdef LANDFILL
#define noBUG_MCDDB
#define MODTEST(IMOD) IMOD > SYSTEM_MODULE
#define noMODTEST(IMOD) true
#define noBUG_EXTENDED
#define noBUG_ABOLISH
#define noBUG_MCKILL
#endif

//-------------------------------------------

ModClassic::~ModClassic()
{
   PredIter pi = m_predmap->begin();
   while (pi != m_predmap->end())
   {
      delete pi->second;
      pi++;
   }

   delete m_predmap;
   delete m_import_mods;
   delete m_exports;
   delete m_metas;
   delete m_discons;
   delete m_sorteds;
}

void ModClassic::Init()
{
   LNEW(m_predmap, PredMap, aS("module"));
   LNEW(m_import_mods, std::vector<MODIX>, aS("module"));
   LNEW(m_exports, std::vector<LPredicate>, aS("module"));
   LNEW(m_metas, std::vector<LPredicate>, aS("module"));
   LNEW(m_discons, std::vector<LPredicate>, aS("module"));
   LNEW(m_sorteds, std::vector<LPredicate>, aS("module"));

   if (getModuleIX() == SYSTEM_MODULE)
      AddImportModule(USER_MODULE);
   else if (getModuleIX() == USER_MODULE)
      AddImportModule((MODIX)SYSTEM_MODULE);
   else
   {
      AddImportModule(USER_MODULE);
      AddImportModule((MODIX)SYSTEM_MODULE);
   }
}

void ModClassic::AddImportModule(PATOM modA)
// NOTE - for now, if its undefined we create a new module,
// which will be a ModClassic.  This is OK for now, but
// when we have different types of modules, we'll need a
// to make 'stub' modules in this case.
{
   MODIX modix = pDDB->getModuleIX(modA);
   if (modix == UNDEFINED_MODULE)
      modix = pDDB->NewModule(modA);
   AddImportModule(modix);
}

void ModClassic::AddImportModule(MODIX imod)
{
   for (size_t i = 0; i < m_import_mods->size(); i++)
      if ((*m_import_mods)[i] == imod) 
		  return;

   m_import_mods->push_back(imod);

//   PATOM modnameA;
//   if (imod == USER_MODULE) modnameA = pATAB->userA;
//   else if (imod == SYSTEM_MODULE) modnameA = pATAB->amzi_systemA;

//   LNEW(modules[imod],ModClassic(imod, modnameA, m_peng), aS("module"));
//   modules[imod]->Init();
//   nmods++;

}

void ModClassic::AddImportPredicate(MODIX modix, PATOM a, ARITY ar)
{
   bool b_export;
   ImportPredicate *ip;
   LNEW(ip, ImportPredicate(modix), aS("module"));
   //if (!ip)
   //   pXCPT->Error(outofmemE, aS("ImportPredicate"));
   b_export = CheckExport(a, ar) ? true : false;
   MODIX imod = getModuleIX();
   PredicateHead *prhead;
   LNEW(prhead, PredicateHead(m_peng, ip, imod, a, ar, b_export, NULL, NULL), aS("module"));
   //if (!prhead)
   //   pXCPT->Error(outofmemE, aS("PredicateHead"));
   ip->setPredicateHead(prhead);
   std::pair<PredIter,bool> pip = m_predmap->insert(PredPair(LPredicate(a, ar), prhead));
   LASSERT(pip.second, aS("Add Import Predicate found duplicate"));
}

void ModClassic::MarkExportPredicate(PATOM pred, ARITY ar)
{
   PredicateHead *pph = getPredicateHead(pred, ar);
   if (pph == NULL)
      m_exports->push_back(LPredicate(pred, ar));
   else
      pph->setExport();
}

void ModClassic::MarkMetaPredicate(PATOM pred, ARITY ar)
{
   PredicateHead *pph = getPredicateHead(pred, ar);
   if (pph == NULL)
      m_metas->push_back(LPredicate(pred, ar));
   else
      pph->setMeta();
}

void ModClassic::MarkDisconPredicate(PATOM pred, ARITY ar)
{
   PredicateHead *pph = getPredicateHead(pred, ar);
   if (pph == NULL)
      m_discons->push_back(LPredicate(pred, ar));
   else
      pph->setDiscon();
}

void ModClassic::MarkSortedPredicate(PATOM pred, ARITY ar)
{
   PredicateHead *pph = getPredicateHead(pred, ar);
   if (pph == NULL)
      m_sorteds->push_back(LPredicate(pred, ar));
   else
      pph->setSorted();
}


PredicateHead *ModClassic::getPredicateHead(PATOM a, ARITY ar)
{
   PredIter pi = m_predmap->find(LPredicate(a, ar));
	return pi == m_predmap->end() ? NULL : pi->second;
}

// Assumes everything in user module is exported, but lets not
// assume that for the system module anymore.  We can only see
// what we want to let people see.
PredicateHead *ModClassic::getExportedPredicate(PATOM a, ARITY ar)
{
   PredicateHead *pph = getPredicateHead(a, ar);

   if (! pph)
      return NULL;

   //if (getModuleIX() == USER_MODULE || getModuleIX() == SYSTEM_MODULE)
   if (getModuleIX() == USER_MODULE)
      return pph;

   if (pph->IsExported())
      return pph;

   return NULL;
}

// Visible predicates are the ones that can be seen from a
// module.  They might be defined here, or explicitly mentioned
// and thus in the map as ImportPredicates or they might be
// found from the list of imported modules.  If in an import
// module, they must be exported from that module, with the exception
// of the user module which automatically exports everything.
PredicateHead *ModClassic::getVisiblePredicate(PATOM a, ARITY ar)
{
   PredicateHead *pph = getPredicateHead(a, ar);
   if (pph && !pph->IsImported())
      return pph;
   else if (pph && pph->IsImported())
   {
      ImportPredicate *pip = pph->getImportPredicate();
      pph = pDDB->getExportedPredicate(pip->getModix(), a, ar);
      return pph;
   }
   else
   {
      for (size_t i=0; i < m_import_mods->size(); i++)
      {
         pph = pDDB->getExportedPredicate((*m_import_mods)[i], a, ar);
         if (pph)
            return pph;
      }
   }
   return NULL;
}

bool ModClassic::IsEscapePred(PATOM pred, ARITY ar)
{                      // true if the predicate is either builtin or extended
   PredicateHead *pph = getPredicateHead(pred, ar);
	return pph ? pph->IsBuiltIn() || pph->IsExtended() : false;
}

EscapePredicate *ModClassic::getEscapePredicate(PATOM name, ARITY arity)
{
   PredicateHead *pph = getPredicateHead(name, arity);
   if (!pph)
      return NULL;

   EscapePredicate *pep = pph->getBuiltInPredicate();
   if ( pep )
      return pep;
   else
      return pph->getExtendedPredicate();
}
   
void ModClassic::UnloadFile(int fileix)
{  // delete all the compiled code that has the given file index
   PredicateHead *pph;
   CompiledPredicate *pcp;
   CompiledPredicate *pcp2;
   CompiledPredicate *pcp1;
   CompiledPredicate *pcp_prev;
   std::vector<LPredicate> to_be_erased;
   size_t i;

   PredIter pi = m_predmap->begin();
   while (pi != m_predmap->end())
   {
      pph = pi->second;
      if (pph->IsCompiled())
      {
         pcp1 = pph->getCompiledPredicate();
         pcp = pcp1;
         pcp_prev = NULL;
         while(pcp)
         {
            pcp2 = pcp->getNext();
            if (pcp->getFileIX() == fileix)
            {
               if (pcp == pcp1)  // first one

               {
                  if (pcp->IsContinued())
                  {
                     pph->setCompiledPredicate(pcp2);
                     pcp1 = pcp2;
                     delete pcp;
                  }
                  else
                  {
                     //m_predmap->erase(pi->first);
                     to_be_erased.push_back(pi->first);
                     delete pph;
                  }
               }
               else
               {  // not the first one
                  pcp_prev->setNext( pcp2 );
                  delete pcp;
               }
            }
            else
               pcp_prev = pcp;

            pcp = pcp2;
         }
      }
      pi++;
   }
   for (i=0; i<to_be_erased.size(); i++)
   {
      m_predmap->erase(to_be_erased[i]);
   }
}

//--------------------------------------------------
// Functions to add extended & builtin predicates
//

// enter a built-in predicate, last argument indicates
// whether or not its exported from amzi_system or not
void ModClassic::EnterBIP(PATOM a, ARITY ar, pBIP pbip, bool b_export, STRptr args, STRptr desc)
{
   BuiltInPredicate *bip;
   LNEW(bip, BuiltInPredicate(m_peng, pbip), aS("module"));
   //if (!bip)
   //   pXCPT->Error(outofmemE, aS("BuiltInPredicate"));
   //bool b_export = CheckExport(a, ar) ? true : false;
   //bool b_export = true;
   MODIX imod = getModuleIX();
   PredicateHead *prhead;
   LNEW(prhead, PredicateHead(m_peng, bip, imod, a, ar, b_export, args, desc), aS("module"));
   //if (!prhead)
   //   pXCPT->Error(outofmemE, aS("PredicateHead"));
   if (CheckMeta(a, ar))
      prhead->setMeta();
   bip->setPredicateHead(prhead);
   std::pair<PredIter,bool> pip = m_predmap->insert(PredPair(LPredicate(a, ar), prhead));
   LASSERT(pip.second, aS("EnterBIP found duplicate"));
}

void ModClassic::InitPredTab(PRED_INITAptr pint)
{             // Used for user-defined predicate tables, loaded through the API
#ifdef _UNICODE
   aCHAR *buf;
   intC len;

   while( pint->Pname) 
   {
      len = 1 + (intC)strlen(pint->Pname);
      LNEW(buf, aCHAR[len], aS("module"));
      mbstowcs(buf, pint->Pname, len);
      MakeExtSys(buf, pint->Parity, pint->Pfunc, m_peng);
      delete buf;
      ++pint;
   }
#else
   while( pint->Pname) 
   {
      MakeExtSys((STRptr) (pint->Pname), pint->Parity, pint->Pfunc, m_peng);
      ++pint;
   }
#endif
}

#ifdef _UNICODE
void ModClassic::InitPredTab(PRED_INITWptr pint)
{            // Used for user-defined predicate tables, loaded through the API
   while( pint->Pname) 
   {
      MakeExtSys((STRptr) (pint->Pname), pint->Parity, pint->Pfunc, m_peng);
      ++pint;
   }
}
#endif

void ModClassic::AddPred(STRptr name, ARITY arity, ExtPred func, VOIDptr vp)
{
   MakeExtSys(name, arity, func, vp);
}

void ModClassic::MakeExtSys(STRptr name, ARITY ar, ExtPred func, VOIDptr vp)
// make predicate name of arity as an external predicate which calls func
// int  (*func)(); third argument 
{
   PATOM a = pATAB->EnterAtom(name);
#ifdef BUG_EXTENDED
 DUMP << "MC Adding Extended Predicate: " << MODPREDAR(m_imod, a, ar) << NL;
#endif

   if (getVisiblePredicate(a, ar) != NULL)
   {
      if (pDDB->getExportedPredicate(SYSTEM_MODULE, a, ar) != NULL)
         pXCPT->Error(syspredE, (STRptr)*a, ar);
      else
         pXCPT->Error(redefinitionE, (STRptr)*a, ar);
   }

   ExtendedPredicate *pep;
   LNEW(pep, ExtendedPredicate(func, vp), aS("module"));
   //if (!pep)
   //   pXCPT->Error(outofmemE, aS("ExtendedPredicate"));
   bool b_export = CheckExport(a, ar) ? true : false;
   MODIX imod = getModuleIX();
   PredicateHead *prhead;
   LNEW(prhead, PredicateHead(m_peng, pep, imod, a, ar, b_export, NULL, NULL), aS("module"));
   //if (!prhead)
   //   pXCPT->Error(outofmemE, aS("PredicateHead for ExtendedPredicate"));
   if (CheckMeta(a, ar))
      prhead->setMeta();
   pep->setPredicateHead(prhead);
   std::pair<PredIter,bool> pip = 
	  m_predmap->insert(PredPair(LPredicate(a, ar), prhead));
   LASSERT(pip.second, aS("MakeExtSys found duplicate"));
}


//----------------------------------
// Functions for clauses
//

// Adds a compiled predicate block either new or to a chain already
// existing.  Note that it cross-references itself back into the code,
// so lexec can know something about the code.
void ModClassic::AddCompiledPredicate(PATOM a, ARITY ar, CODEptr code, 
												  int fileix)
{
   CompiledPredicate *cpprev;
   CompiledPredicate *cp;
   PredicateHead *prhead;

   // See if the predicate is already defined as compiled, 
   // if not add it, if so create an extension for it.
   if (! IsDefined(a, ar))
   {  // Add a new one
      if (getVisiblePredicate(a, ar) != NULL)
      {
         if (pDDB->getExportedPredicate(SYSTEM_MODULE, a, ar) != NULL)
            pXCPT->Error(syspredE, (STRptr)*a, ar);
         else
            pXCPT->Error(redefinitionE, (STRptr)*a, ar);
      }
      LNEW(cp, CompiledPredicate(code, fileix), aS("module"));
      //if (!cp)
      //   pXCPT->Error(outofmemE, "CompiledPredicate");

      bool b_export = CheckExport(a, ar) ? true : false;
      MODIX imod = getModuleIX();
      LNEW(prhead, PredicateHead(m_peng, cp, imod, a, ar, b_export, NULL, NULL), aS("module"));
      if (CheckMeta(a, ar))
         prhead->setMeta();
      if (CheckDiscon(a, ar))
         prhead->setDiscon();
      cp->setPredicateHead(prhead);
      std::pair<PredIter,bool> pip = 
		  m_predmap->insert(PredPair(LPredicate(a, ar), prhead));
   }
   else
   {
      prhead = getPredicateHead(a, ar);
      if (! prhead->IsCompiled())
         pXCPT->Error(add_compiledE, (STRptr)*(a), ar);
      if (! prhead->IsDiscon())
         pXCPT->Error(disconE, (STRptr)*(a), ar);
      cpprev = prhead->getCompiledPredicate();
      LASSERT( (cpprev != NULL), aS("Unexpected missing compiled predicate"));
      while (cpprev->getNext() != NULL)
         cpprev = cpprev->getNext();
      LNEW(cp, CompiledPredicate(code, fileix), aS("module"));
      cpprev->setNext(cp);
   }
   // lexec will need to know if this block of code is
   // continued or not, so this lets it find out
   ((CODE_HEADERptr)code)->block = cp;
}

void ModClassic::AddIndexedPredicate(TERM ipat)
{ 
   Goal g(m_peng, ipat);
   DynamicPredicate *pdp;
   PredicateHead *pph;

   pph = getPredicateHead(g.functor, g.arity);
   if (pph && !pph->IsIndexed())
      pXCPT->Error(badindexedE, (aCHAR*)(g.functor->get_display_name(m_peng)), g.arity);
   if (!pph)
   {
      bool b_export = CheckExport(g.functor, g.arity) ? true : false;
      LNEW(pdp, IndexedDynamicPredicate(), aS("module"));
      LNEW(pph, PredicateHead(m_peng, pdp, m_imod, g.functor, g.arity, b_export, NULL, NULL), 
			  aS("module"));
      pph->setIndexed();
      if (CheckMeta(g.functor, g.arity))
         pph->setMeta();
      pdp->setPredicateHead(pph);
      ((IndexedDynamicPredicate*)pdp)->Init(ipat);  // couldn't init until pph set for m_peng references
      std::pair<PredIter,bool> pip = 
		  m_predmap->insert(PredPair(LPredicate(g.functor, g.arity), pph));
      LASSERT(pip.second, aS("Assert error inserting"));
   }
}

CompiledPredicate* ModClassic::getCompiledPredicate(PATOM name, ARITY arity)
{
   PredIter pi = m_predmap->find(LPredicate(name, arity));

   if (pi == m_predmap->end())
      return NULL;                              // not in map

   return pi->second->getCompiledPredicate();
}

bool ModClassic::CheckExport(PATOM pred, ARITY ar)
{
   LPredicate lp(pred, ar);
   std::vector<LPredicate>::iterator it;

   for(it = m_exports->begin(); it != m_exports->end(); it++)
      if (*it == lp)
      {
         m_exports->erase(it);
         return true;
		}
   return false;
}

bool ModClassic::CheckMeta(PATOM pred, ARITY ar)
{
   LPredicate lp(pred, ar);
   std::vector<LPredicate>::iterator it;

   for(it = m_metas->begin(); it != m_metas->end(); it++)
	  if (*it == lp)
		 {
         m_metas->erase(it);
         return true;
		 }
   return false;
}

bool ModClassic::CheckDiscon(PATOM pred, ARITY ar)
{
   LPredicate lp(pred, ar);
   std::vector<LPredicate>::iterator it;

   for(it = m_discons->begin(); it != m_discons->end(); it++)
	  if (*it == lp)
		 {
         m_discons->erase(it);
         return true;
		 }
   return false;
}

bool ModClassic::CheckSorted(PATOM pred, ARITY ar)
{
   LPredicate lp(pred, ar);
   std::vector<LPredicate>::iterator it;

   for(it = m_sorteds->begin(); it != m_sorteds->end(); it++)
	  if (*it == lp)
		 {
         m_sorteds->erase(it);
         return true;
		 }
   return false;
}


void ModClassic::Abolish(PATOM name, ARITY arity)
{ // abolish a dynamic predicate, note that this might not be safe,
  // as the clauses might have been unified with at some point.
   PredicateHead *pph = getPredicateHead(name, arity);
   if (! pph)
      return;

#ifdef BUG_ABOLISH
 DUMP << "==> ModClassic::Abolish" << NL;
 pph->Dump();
 DUMP << "<== ModClassic::Abolish" << NL << FLUSH;
#endif

   if (! pph->IsDynamic())
      pXCPT->Error(abolishE, (aCHAR*)m_nameA->get_display_name(m_peng), (aCHAR*)name->get_display_name(m_peng), arity);

   pph->getDynamicPredicate()->Abolish();

   //m_predmap->erase(LPredicate(name, arity));
   
   //delete pph;    // this is done as well, deleting the predicate head
}


//----------------------------------------------------
// Functions that manipulate the dynamic database.
//

RC ModClassic::Assert(int az, Goal g, TERM t)
// Assert - assert a term to the database, az indicates front or back
// returns the key atom if it's protected
{
   PredicateHead *pph;
   //DynamicClause *pdc;
   DynamicClause *pnewdc;
   DynamicPredicate *pdp;

   pph = getPredicateHead(g.functor, g.arity);

   if (!pph)
   {
      if (getVisiblePredicate(g.functor, g.arity) != NULL)
      {
         if (pDDB->getExportedPredicate(SYSTEM_MODULE, g.functor, g.arity) != NULL)
            pXCPT->Error(syspredE, (STRptr)*(g.functor), g.arity);
         else
            pXCPT->Error(redefinitionE, (STRptr)*(g.functor), g.arity);
      }
      // default for now is a list dynamic predicate
      MODIX imod = getModuleIX();
      if (CheckSorted(g.functor, g.arity))
      {
         LNEW(pdp, SortedDynamicPredicate(), aS("module"));
      }
      else
      {
         LNEW(pdp, ListDynamicPredicate(), aS("module"));
      }

      bool b_export = CheckExport(g.functor, g.arity) ? true : false;
      LNEW(pph, PredicateHead(m_peng, pdp, imod, g.functor, g.arity, b_export, NULL, NULL), 
			  aS("module"));
      if (CheckMeta(g.functor, g.arity))
         pph->setMeta();
      pdp->setPredicateHead(pph);
      std::pair<PredIter,bool> pip = 
		  m_predmap->insert(PredPair(LPredicate(g.functor, g.arity), pph));
      LASSERT(pip.second, aS("Assert error inserting"));
   }
   else if (pph->IsDynamic())
   {
      pdp = pph->getDynamicPredicate();
   }
   else
      pXCPT->Error(sysassertE, (STRptr)*(g.functor));

   // record the term under the functor key, front or back as required
   // (default should be assertz not asserta. reeves)

#ifdef BUG_MCDDB
 if (MODTEST(m_imod))
 {
  DUMP << "MC Asserting: " << MODPREDAR(m_imod, g.functor, g.arity) << NL << FLUSH;
  pTSVC->Dump(t);
  DUMP << NL << FLUSH;
  pTSVC->DumpWrite(t);
 }
#endif
   LNEW(pnewdc, DynamicClause(pdp, t), aS("module"));
#ifdef BUG_MCDDB
 if (MODTEST(m_imod))
 {
  DUMP << "MC Asserted at: " << pnewdc;
 TERM end = pnewdc->getCode() + pnewdc->getSize();
 DUMP << " code from: " << pnewdc->getCode() << " to: " << end << NL << FLUSH;
 }
#endif

   pdp->Insert(pnewdc, az);

   return OK;

}


TF ModClassic::Retract(TERM rett)
{
   STUB(aS("Retract")); 
	return FALSE;
}

void ModClassic::remove_ddb(DynamicClauseIterator* pdci)
{
   PROBE_FUNCTION_START("remove_ddb");
   //TERM  h;

   DynamicClause *pdc = pdci->getClause();

   //DynamicClause *prev = pdc->prev();
   //DynamicClause *next = pdc->next();
#ifdef BUG_MCDDB
 DynamicPredicate *pdp = pdci->getClause()->getDynamicPredicate();
 if (MODTEST(m_imod))
 {
 FILL("==>ModClassic::remove_ddb " << pdc << ": ");
 pdci->getClause()->Dump();
 DUMP << "  Predicate before clause removal: " << NL;
 pdp->Dump();
 }
#endif

   CHOICE_POINTptr Bt;
   DynamicClauseIterator* Bpdci;
   bool protect = false;

   // Defend against pathological programs that try to delete clauses
   // and backtrack into goals that are using those clauses.  Everyone
   // slows down so those programs will work.  Sigh.
   //
   // I think we can get rid of the protect stuff now that we use
   // unify copy, but I'm not sure.  Leave as is for now.
   for (Bt = pCNTL->BTop(); Bt != NULL; Bt = Bt->Bc)
   {
      if (Bt->gc_pdci)
      {
         Bpdci = Bt->gc_pdci->pdci;
         //if (Bpdci && ! Bpdci->isDone())
         if (! Bpdci->isDone())
         {
            if (Bpdci != pdci && Bpdci->getClause() == pdc)
            {
               //BUGOUT << "adjusting\n";
               Bpdci->setNext();
            }
            else if (! protect && ! Bpdci->isFirst() && Bpdci->getPrev() == pdc)
            {
               protect = true;
            }
         }
      }
      //i++;
   }

   pdci->remove();
   //BUGOUT << "stack depth = " << i << '\n';
   if (protect)
   {
      //BUGOUT << "protecting\n";
      m_kill_list.push_back(pdc);
      pdc->setDeleted();  // set the deleted flag on clause
      recent_purgatories++;
      if (recent_purgatories > 10000)
         GC();
   }
   else
   {
      //BUGOUT << "removing\n";
      delete pdc;
   }
}

/*
// this is the case in which the clause is next
   // for either a clause or retract iteration
   if (pdc->isNext(m_peng))
   {
#ifdef BUG_MCDDB
 if (MODTEST(m_imod))
 FILL(pdc << " was in use");
#endif
      // starting from the oldest use,
      // stored in mscwNext, walk the heap to
      // the current heap top, updating all
      // mscw pointers.
      for (h=pdc->getNextMSCW(); h<pHXL->HTop(); h++)
      {
         if (h->IsMSCW() &&
               h->getMSCWDBRef() != pdci &&
               h->getMSCWDBRef()->getClause() == pdc)
         {

#ifdef BUG_MCDDB
 if (MODTEST(m_imod))
 {
 DUMP << "adjusting mscw for: ";
 pTSVC->DumpWrite(pdc->getCode());
 DUMP << NL;
 }
#endif

            //pdci->setNext();
            //h->setMSCWDBRef(next);
            h->getMSCWDBRef()->setNext();  // set iterator to next clause

#ifdef BUG_MCDDB
 if (MODTEST(m_imod))
 {
 if (! h->getMSCWDBRef()->isDone())
 {
  DUMP << "new mscw is: ";
  pTSVC->DumpWrite(h->getMSCWDBRef()->getClause()->getCode());
  DUMP << NL;
 }
 else
  FILL("new mscw is: NULL");
 }
#endif

            if (! h->getMSCWDBRef()->isDone())
               h->getMSCWDBRef()->getClause()->setNextMSCW(h);
         }
      }

      //if (next) next->setMSCWretract(pdc->getMSCWretract());
   }
*/
/*
// Make sure its safe to delete, if not,
   // put it on a kill list for gc to clean up
   // if the user wants.  This is the situation where
   // the clause has been unified with, and the
   // unification is still active.
   if (pdc->inUse(m_peng))
   {

#ifdef BUG_MCDDB
 if (MODTEST(m_imod))
 DUMP << "put on kill list: " << pdc << NL;
#endif

      // link it into the kill list chain
      m_kill_list.push_back(pdc);
      pdc->setDeleted();  // set the deleted flag on clause
#ifdef BUG_MCKILL
 std::cout << 'K';
#endif
   }
   // its safe to delete
   else
   {
#ifdef BUG_MCDDB
 if (MODTEST(m_imod))
 {
 DUMP << "killing: " << pdc;
 TERM end = pdc->getCode() + pdc->getSize();
 DUMP << " code from: " << pdc->getCode() << " to: " << end << NL;
 }
#endif
      // remove from clauses in list first because
      // might be an error as some iterators might not
      // like it and throw an error, frinstance, indexed
      // clauses with variables don't work as of this writing.
#ifdef BUG_MCDDB
 DUMP << "pdci before remove: " << NL;
 pdci->getClause()->Dump();
#endif
      pdci->remove();
#ifdef BUG_MCDDB
 DynamicClause *pdc2;
 if (pdci->isDone())
  DUMP << "pdci after remove is done" << NL;
 else
 {
 pdc2 = pdci->getClause();
 DUMP << "pdci after remove: " << pdc2 << NL;
 }
#endif
      delete pdc;
   }


#ifdef BUG_MCDDB
 if (MODTEST(m_imod))
 {
 //DUMP << "  Predicate after clause removal: " << NL;
 //pdp->Dump();
 DUMP << "<==ModClassic::remove_ddb finished" << NL << NL;
 }
#endif
   PROBE_FUNCTION_END("remove_ddb");
   return;
}
*/

// functions used by DDB to get all the predicates
// in a module, for use by listing and the like.  the
// caller has allocated space for ppi.
LPredicate ModClassic::get_first_pred(PredIter *ppi)
{
   *ppi = m_predmap->begin();
   if (*ppi == m_predmap->end())
      return LPredicate();                   // a null LPredicate

	return LPredicate((*ppi)->second->getName(), (*ppi)->second->getArity() );
}

LPredicate ModClassic::get_next_pred(PredIter *ppi)
{
   if (*ppi == m_predmap->end())
      return LPredicate();                  // a null LPredicate

	return LPredicate((*ppi)->second->getName(), (*ppi)->second->getArity() );
}

//-----------------------
// Garbage collection
//

// There might have been some retracts that couldn't be
// physically retracted because of references somewhere.
// They were put on a kill list.

// This whole problem is more insidious than that, things
// are now killed because the pointers for a particular
// clause are updated so the clause could be removed.
// The only reason we might not want to kill a clause
// is because something is unified with a structure in
// it.  We could flag those clauses as inuse, but its a
// nasty problem deciding later if we can delete it or
// not, as we'd have to walk the heap looking for references
// in its address range.

void ModClassic::GC()
{
   //int ix = 17;
   //if (ix == 17)
   //   return;

   DynamicClause *pdc;
   DynamicClauseIterator* Bpdci;
   CHOICE_POINTptr Bt;
   bool protect = false;

   ClauseListIterator cli = m_kill_list.begin();
   while (cli != m_kill_list.end())
   {
      protect = false;
      pdc = *cli;

      for (Bt = pCNTL->BTop(); Bt != NULL; Bt = Bt->Bc)
      {
         if (Bt->gc_pdci)
         {
            Bpdci = Bt->gc_pdci->pdci;
            //if (! protect && Bt->pdci && ! Bt->pdci->isDone() && Bt->pdci->getPrev() == pdc)
            if (! protect && Bpdci && ! Bpdci->isDone() && ! Bpdci->isFirst() && Bpdci->getPrev() == pdc)
            {
               protect = true;
               break;
            }
         }
      }

      if ( ! protect )
      {
         //BUGOUT << "gc_kill\n";
         cli = m_kill_list.erase(cli);
         delete pdc;
      }
      else
      {
         //BUGOUT << "gc_protect\n";
         cli++;
      }
   }
   recent_purgatories = 0;
}
//   STUB("ModClassic::GC()");
/* don't need this if always retracting!
   //DynamicClause *p;
   //DynamicClause *pnext;
   //int kill_count = 0;
#ifdef BUG_MCDDB
   DUMP << "*** GC DDB ***" << NL << FLUSH;
#endif

   ClauseListIterator cli = m_kill_list.begin();
   //for (p = m_kill_list; p ; p = pnext)
   while (cli != m_kill_list.end())
   {
      //pnext = p->next();
      //if (!p->inUse(m_peng))
      //{
      //   if (p->prev())
      //      p->prev()->setNext(p->next());
      //   if (p->next())
      //      p->next()->setPrev(p->prev());
      //  delete p;
      //}
      if ( ! (*cli)->inUse(m_peng) )
      {
         cli = m_kill_list.erase(cli);
         delete *cli;
         //kill_count++;
      }
      else
         cli++;
   }
   //std::cout << "killed = " << kill_count << NL;
   */


//------------------------------
// LANDFILL functions
//

#ifndef LANDFILL
void ModClassic::Dump()
{
}
#else
void ModClassic::Dump()
{
   size_t i;

   DUMP << NL << "---------------------------------------" << NL;
   DUMP << "Dumping: " << getModuleNameA()->get_display_name(m_peng) << NL;
   DUMP << "size: " << m_predmap->size() << NL;

   DUMP << "Imported Modules:" << NL;
   for (i = 0; i < m_import_mods->size(); i++)
      DUMP << "  " << pDDB->getModuleNameA((*m_import_mods)[i])->get_display_name(m_peng) << NL;

   if (m_exports->size() > 0)
   {
      DUMP << "Unresolved exported predicates:" << NL;
      for (i = 0; i < m_exports->size(); i++)
         DUMP << "  " << (*m_exports)[i] << NL;
   }

   if (m_metas->size() > 0)
   {
      DUMP << "Unresolved meta predicates:" << NL;
      for (i = 0; i < m_metas->size(); i++)
         DUMP << "  " << (*m_metas)[i] << NL;
   }

   if (m_discons->size() > 0)
   {
      DUMP << "Unresolved discontiguous predicates:" << NL;
      for (i = 0; i < m_discons->size(); i++)
         DUMP << "  " << (*m_discons)[i] << NL;
   }

   if (m_sorteds->size() > 0)
   {
      DUMP << "Unresolved sorted predicates:" << NL;
      for (i = 0; i < m_sorteds->size(); i++)
         DUMP << "  " << (*m_sorteds)[i] << NL;
   }

   DUMP << "Predicates:" << NL;
   PredIter pi = m_predmap->begin();
   while (pi != m_predmap->end())
   {
      //DUMP << "key: " << pi->first << NL;
      DUMP << *(pi->second) << NL;
      pi++;
   }

   DUMP << "---------------------------------------" << NL;
   DUMP << FLUSH;
}

#endif   // LANDFILL





































