/****************************************************/
/* File: symtab.h                                   */
/* Symbol table interface for the TINY compiler     */
/* (allows only one symbol table)                   */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#ifndef _SYMTAB_H_
#define _SYMTAB_H_

#include "globals.h"

#define SIZE 211
#define MAXSCOPE 211
#define SHIFT 4

/* Procedure st_insert inserts line numbers and
 * memory locations into the symbol table
 * loc = memory location is inserted only the
 * first time, otherwise ignored
 */
void st_insert( char * name, ExpType type, int lineno, TreeNode * t/*, int loc */);

/* Function st_lookup returns the memory 
 * location of a variable or -1 if not found
 */
//int st_lookup ( char * name );


typedef struct LineListRec
  { int lineno;
    struct LineListRec * next;
  } * LineList;

typedef struct BucketListRec
  { char * name;
    ExpType type;
    LineList lines;
    TreeNode * node;
    int memloc; /*memory location for variable*/
    struct BucketListRec * next;
    //ParamKind param;
  } * BucketList;

typedef struct ScopeListRec
  { char * name;
    BucketList bucket[SIZE];
    struct ScopeListRec * parent;
    int depth;
  } * ScopeList;

static ScopeList scopeStack[MAXSCOPE]; // for build symtable
static ScopeList scopeAll[MAXSCOPE]; // save all scope
static int scopeStackTop = 0;
static int scopeSize = 0; // scope #

ScopeList newScope(char*);
void pushScope(ScopeList);
void popScope();


BucketList st_lookup ( ScopeList, char * );
BucketList st_lookup_noparent ( ScopeList, char * );
void st_addLineno(BucketList, int);

/* Procedure printSymTab prints a formatted 
 * listing of the symbol table contents 
 * to the listing file
 */
void printSymTab(FILE * listing);

#endif
