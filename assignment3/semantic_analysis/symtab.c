/****************************************************/
/* File: symtab.c                                   */
/* Symbol table implementation for the TINY compiler*/
/* (allows only one symbol table)                   */
/* Symbol table is implemented as a chained         */
/* hash table                                       */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "symtab.h"

/* SIZE is the size of the hash table */
#define SIZE 211

/* SHIFT is the power of two used as multiplier
   in hash function  */
#define SHIFT 4

//static ScopeList scopeStackTop[MAXSCOPE]; // for build symtab
//static ScopeList scopSt

/* the hash function */
static int hash ( char * key )
{ int temp = 0;
  int i = 0;
  while (key[i] != '\0')
  { temp = ((temp << SHIFT) + key[i]) % SIZE;
    ++i;
  }
  return temp;
}

/* The record in the bucket lists for
 * each variable, including name, 
 * assigned memory location, and
 * the list of line numbers in which
 * it appears in the source code
 */
/*
typedef struct BucketListRec
   { char * name;
     LineList lines;
     int memloc ;
     struct BucketListRec * next;
   } * BucketList; */

ScopeList newScope(char * name)
{ ScopeList scope = (ScopeList)malloc(sizeof(struct ScopeListRec));
  scope->name = name;
  scope->depth = scopeStackTop;
  
  if(scopeStackTop == 0)
    scope->parent = NULL;
  else
    scope->parent = scopeStack[scopeStackTop - 1];

  scopeAll[scopeSize++] = scope;
  return scope;
}

void pushScope(ScopeList scope)
{ scopeStack[scopeStackTop++] = scope;
}

void popScope()
{ ScopeList tmp = scopeStack[scopeStackTop--];
  free(tmp);
  //free(scopeStack[scopeStackTop--]);
  scopeStack[scopeStackTop] = NULL;
}

/* Procedure st_insert inserts line numbers and
 * memory locations into the symbol table
 * loc = memory location is inserted only the
 * first time, otherwise ignored
 */
void st_insert( char * name, ExpType type, int lineno, TreeNode * t/*, int loc */)
{ int h = hash(name);
  ScopeList scope = scopeStack[scopeStackTop - 1];
  BucketList l = scope->bucket[h];

  while ((l != NULL) && (strcmp(name,l->name) != 0))
    l = l->next;

  if (l == NULL) /* variable not yet in table */
  { l = (BucketList) malloc(sizeof(struct BucketListRec));
    l->name = name;
    l->type = type;
    l->lines = (LineList) malloc(sizeof(struct LineListRec));
    l->lines->lineno = lineno;
    //l->memloc = loc;
    l->lines->next = NULL;
    l->next = scope->bucket[h];
    scope->bucket[h] = l;
    l->node = t;
    //if(parmaK != -1)
      //l->param = paramK;
  }
  else /* found in table, so just add line number */
  { LineList t = l->lines;
    while (t->next != NULL) t = t->next;
    t->next = (LineList) malloc(sizeof(struct LineListRec));
    t->next->lineno = lineno;
    t->next->next = NULL;
  }
} /* st_insert */

/* Function st_lookup returns the memory 
 * location of a variable or -1 if not found
 */
/*int st_lookup ( char * name )
{ int h = hash(name);
  BucketList l =  hashTable[h];
  while ((l != NULL) && (strcmp(name,l->name) != 0))
    l = l->next;
  if (l == NULL) return -1;
  else return l->memloc;
}*/

BucketList st_lookup(ScopeList scope, char * name)
{ int h = hash(name);
  BucketList l =  scope->bucket[h];

  while(scope != NULL)
  { while ((l != NULL) && (strcmp(name,l->name) != 0))
      l = l->next;
    
    if (l != NULL) return l;

    scope = scope->parent;
  }

  return NULL;
}

BucketList st_lookup_noparent(ScopeList scope, char * name)
{ int h = hash(name);

  if(scope == NULL)
    return NULL;

  BucketList l =  scope->bucket[h];
  
  while ((l != NULL) && (strcmp(name,l->name) != 0))
      l = l->next;
    
  return l;
}

void st_addLineno(BucketList l, int lineno)
{ LineList line = l->lines;
  while (line->next != NULL)
    line = line->next;

  line->next = (LineList) malloc(sizeof(struct LineListRec));
  line->next->lineno = lineno;
  line->next->next = NULL;
}

void _printType(TreeNode * t)
{ switch(t->type)
  { case Void:
      fprintf(listing, "%-13s", "VOID");
      break;
    case Integer:
      fprintf(listing, "%-13s", "INT");
      break;
    default:
      break;
  }
}

void printSymTabRow(FILE * listing, BucketList * bucket)
{ int i; BucketList b; LineList l; TreeNode * t;

  for(i = 0; i < SIZE; i++)
  { b = bucket[i];
    l = b->lines;
    t = b->node;

    while(b != NULL)
    { fprintf(listing, "%-13s", b->name);

      if(t->nodekind == DclK)
      { switch(t->kind.dcl)
        { case FuncK:
            fprintf(listing, "%-13s", "Function");
            _printType(t);
            break;
          case VarK:
            fprintf(listing, "%-13s", "Var");
            _printType(t);
            break;
          case ArrVarK:
            fprintf(listing, "%-13s", "ArrVar");
            _printType(t);
            break;
          default:
            break;
        }
      }
      else if(t->nodekind == ParamK)
      { switch(t->kind.param)
        { case ParK:
            fprintf(listing, "%-13s", "VarPar");
            _printType(t);
            break;
          case ArrParK:
            fprintf(listing, "%-13s", "ArrVarPar");
            _printType(t);
            break;
          deault:
            break;
        }
      }

      while(l != NULL)
      { fprintf(listing, "%3d ", t->lineno);
        l = l->next;
      }

      fprintf(listing, "\n");
      b = b->next;
    }
  }
}


/* Procedure printSymTab prints a formatted 
 * listing of the symbol table contents 
 * to the listing file
 */
void printSymTab(FILE * listing)
{ int i; ScopeList scope; BucketList * b;

  fprintf(listing, "-----Symbol Table-----\n");
  fprintf(listing,"Symbol Name  Symbol Type   Data Type    Line Numbers\n");
  fprintf(listing,"-----------  -----------  -----------  --------------\n");

  for (i=0;i<scopeSize;i++)
  { scope = scopeAll[i];
    b = scope->bucket;
    
    if(i == 0)
      fprintf(listing, "global  depth: %d\n", scope->depth);
    else
      fprintf(listing, "%s  depth: %d\n", scope->name, scope->depth);

    printSymTabRow(listing, b);
    fprintf(listing, "\n");
  }
} /* printSymTab */
