//-*-C++-*-
#ifndef ERAPQ_H
#define ERAPQ_H
    /**************************************************************\
    *                                                              *
    * era.h - primes library                                       *
    *                                                              *
    * Copyright (c) 1997 by Ray Reeves.  All Rights Reserved.      *
    *                                                              *
    \**************************************************************/

/**************************************************************************\
* These primitives form the basis for heap processing.                     *
* The terms 'heap' and 'pq'(priority queue) mean the same thing.           *
* era->pq is a pqtype array containing the elements to be sorted.          *
* enforce swaps items that are out of order.                               *
* swap swaps elements.                                                     *
* siftUpPn and siftDownPn are the fundamental operations of heap sorting.  *
* See CACM Vol. 28 #3 March 1985 pp 245-250.                               *
\**************************************************************************/
/*
#define SWAP(X, Y) temp = pq[Y]; pq[Y] = pq[X]; pq[X] = temp;

#define COMPAREPN(X, REL, Y)  pq[X]->pn REL pq[Y]->pn 

#define ENFORCEPN(X, REL, Y) \
        if(COMPAREPN(X ,REL, Y)) break; SWAP(X, Y) 
		  */

class PQELEMENT
{
friend class ERAPQ;
public:

void update()
{  pn += p;}                                     // p is twice the prime

private:
  intC pn;
  intC p;
};

class ERAPQ                                      // priority queue
{  
private:
  LEngine  *m_peng;
  unsigned int length;
  unsigned int free;
  unsigned int maxSize;
  PQELEMENT **pq;
  PQELEMENT *factor;

public:
  ERAPQ(const int, LEngine *);
  ~ERAPQ()
    {delete [] pq; delete [] factor; }

  intC &operator[](int Index) { return pq[Index]->p; }

  unsigned int getLen() const
    { return length; }

  void update(intC);
  
  void push(intC, intC);

  intC top() const
    { return pq[1] ? pq[1]->pn : 0; }

  void Insert(PQELEMENT *element);

  intC pop();

  bool heapEmpty() const
    {return  pq[1] == 0; }
  
  void swap(intC X, intC Y)
  { PQELEMENT *temp = pq[Y]; pq[Y] = pq[X]; pq[X] = temp;}

  bool enforce(intC, intC);
  void SiftUpPn();
  void SiftDownPn(); 
  void dumpERAPQ();
};
#endif








