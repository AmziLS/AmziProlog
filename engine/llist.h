// List classes

// Links in a list, specific types of lists will be
// derived from slink.

#ifndef LIST_DEFINED

class LsLink
{
public:
   LsLink *m_next;
public:
   LsLink() { m_next = 0; }
   LsLink(LsLink *p) { m_next = p; }
};

// Links with some substance to them.

template <class T>
class LTLink : public LsLink
{
public:
   T  m_elem;
public:
   LTLink(const T &elem) : m_elem(elem) {};
};

// Base class for lists.

class LsListBase
{
public:
   LsLink *m_head;
public:
   LsListBase() { m_head = NULL; }
   LsListBase(LsLink *elem)
   {   elem->m_next = NULL; m_head = elem; }

   void Push(LsLink *elem)
   {   elem->m_next = m_head; m_head = elem; }
   LsLink* Pop()
   {
      LsLink *retval = m_head;
      if (m_head)
         m_head = m_head->m_next;
      return retval;
   }   
};

// List template for generic lists.

template <class T>
class LsList : public LsListBase
{
public:
   LsList() {};
   ~LsList() {};
   void Push(const T &elem)
   {
      LTLink<T>* tlnk;
      LNEWX(tlnk, LTLink<T>(elem));  // can't use with gcc 3
      //tlnk = new LTLink<T>(elem);
      //LsListBase::Push(new LTLink<T>(elem));
   }
   T Pop()
   {
      LTLink<T> *tlnk = (LTLink<T>*) LsListBase::Pop();
      if (tlnk)
      {
         T elem = tlnk->m_elem;
         delete  tlnk;
         return elem;
      }
      else
         return (T)0;
   }
};

class LsListIterBase
{
public:
   LsLink *m_pos;
public:
   LsListIterBase()
   {   m_pos = NULL; }
   LsListIterBase(LsListBase &lb)
   {   m_pos = lb.m_head; }
   void Init(LsListBase &lb)
   {   m_pos = lb.m_head; }

   LsLink* Next()
   {
      LsLink *tmp = m_pos;
      if (m_pos)
         m_pos = m_pos->m_next;
      return tmp;
   }
};


template <class T>
class LsListIter : public LsListIterBase
{
public:
   LsListIter(LsList<T> &list) : LsListIterBase(list) {};
   T Next()
   {
      LTLink<T> *tlnk = (LTLink<T>*) LsListIterBase::Next();
      if (tlnk)
         return tlnk->m_elem;
      else
         return (T)0;
   }
};

#define LIST_DEFINED
#endif


