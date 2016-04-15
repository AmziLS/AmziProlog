/****************************************************************************
*
* ddb.cpp -- database support routines
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
****************************************************************************/

#include "inc.h"
#include "pch.h"
#include <assert.h>


#ifdef BUG_LEAK

Leak *LeakStart = NULL;
Leak *LeakEnd = NULL;

char *g_file = "unknown";
int g_line = 0;
aCHAR *g_note = NULL;

int g_count = 0;

void *leak_malloc(size_t size)
{
   //if (g_line == 0)
      //g_line = -1;
      //return malloc(size);
   // note that we add a long on as well, as it seems
   // new[] puts the size of the array at -1.
   size_t leak_size = size + sizeof(Leak);
   void *p = malloc(leak_size);
   if (! p)
   {
      //std::bad_alloc ba;
      //throw ba;
      return NULL;
   }
   ((Leak*)p)->size = size;
   ((Leak*)p)->file = g_file;
   ((Leak*)p)->note = g_note;
   ((Leak*)p)->line = g_line;
   ((Leak*)p)->count = ++g_count;
   ((Leak*)p)->id = 0xfade;
   if (!LeakStart)
   {
      LeakStart = (Leak*)p;
      LeakEnd = (Leak*)p;
      ((Leak*)p)->prev = NULL;
      ((Leak*)p)->next = NULL;
   }
   else
   {
      ((Leak*)p)->prev = LeakEnd;
      ((Leak*)p)->next = NULL;
      LeakEnd->next = (Leak*)p;
      LeakEnd = (Leak*)p;
   }
   return (char*)p + sizeof(Leak);
}

void *operator new(size_t size)
{
   return leak_malloc(size);
}

void *operator new[](size_t size)
{
   return leak_malloc(size);
}

void leak_free(void *p)
{
   if (!p)
      return;
   Leak *l = (Leak*)((char*)p - sizeof(Leak));

   // could be some deletes call from STL routines, where news
   // were called before our new was standard, or some such
   // rot is the theory anyway.
   if (l->id != 0xfade)
   {
      free(p);
      return;
   }

   if (l->prev)
      l->prev->next = l->next;    // eotek crashes here....
   else
      LeakStart = l->next;

   if (l->next)
      l->next->prev = l->prev;
   else
      LeakEnd = l->prev;

   free(l);
}

void operator delete(void *p)
{
   leak_free(p);
}

void operator delete[](void *p)
{
   leak_free(p);
}

void leak_report(char *description)
{
   int mem = 0;
   int blocks = 0;
   FILE *f = fopen("c:\\temp\\leak.txt", "w");
   fprintf(f, "Leak Report from %s\n", description);
   fprintf(f, "%ls  %ls\n\n", (STRptr)LDateString(), (STRptr)LTimeString());
   Leak *l = LeakStart;
   while (l)
   {
      fprintf(f, "undeleted(%d): %s:%d %ls\n", l->count, l->file, l->line, l->note);
      mem += (int)l->size;
      blocks++;
      l = l->next;
   }
   fprintf(f, "  memory = %d\n", mem);
   fprintf(f, "  blocks = %d\n", blocks);
   fclose(f);
}

void leak_report(char *description, char *filename)
{
   int mem = 0;
   int blocks = 0;
   FILE *f = fopen(filename, "w");
   fprintf(f, "Leak Report from %s\n", description);
   fprintf(f, "%ls  %ls\n\n", (STRptr)LDateString(), (STRptr)LTimeString());
   Leak *l = LeakStart;
   while (l)
   {
      fprintf(f, "undeleted(%d): %s:%d %ls\n", l->count, l->file, l->line, l->note);
      mem += (int)l->size;
      blocks++;
      l = l->next;
   }
   fprintf(f, "  memory = %d\n", mem);
   fprintf(f, "  blocks = %d\n", blocks);
   fclose(f);
}

#endif

