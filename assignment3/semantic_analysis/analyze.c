/****************************************************/
/* File: analyze.c                                  */
/* Semantic analyzer implementation                 */
/* for the TINY compiler                            */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "symtab.h"
#include "analyze.h"
#include "util.h"

/* counter for variable memory locations */
static int location = 0;

//for compK, do not push scope for function comp
int isFunc = 0;

/* Procedure traverse is a generic recursive 
 * syntax tree traversal routine:
 * it applies preProc in preorder and postProc 
 * in postorder to tree pointed to by t
 */
static void traverse( TreeNode * t,
               void (* preProc) (TreeNode *),
               void (* postProc) (TreeNode *) )
{ if (t != NULL)
  { preProc(t);
    { int i;
      for (i=0; i < MAXCHILDREN; i++)
        traverse(t->child[i],preProc,postProc);
    }
    postProc(t);
    traverse(t->sibling,preProc,postProc);
  }
}

/* nullProc is a do-nothing procedure to 
 * generate preorder-only or postorder-only
 * traversals from traverse
 */
static void nullProc(TreeNode * t)
{ if (t==NULL) return;
  else return;
}

//pop scope after insert
static void afterInsert(TreeNode * t)
{ if (t->nodekind == StmtK)
  { if (t->kind.stmt == CompK)
      popScope();
  }
}

//print symbol error type
static void symbolError(char * errorName, int lineno)
{ fprintf(listing, "Semantic Error: %s at line %d/n", errorName, lineno);
  Error = TRUE;
}

//check return type error
static void checkRetK(TreeNode * t)
{ BucketList l = st_lookup(scopeStack[scopeStackTop - 1], t->attr.name);
  if (l != NULL)
  { if (l->type == Void && t->child[0] != NULL)
      symbolError("Void should be returned", t->lineno);
    else if (l->type == Integer)
    { if (t->child[0] == NULL)
        symbolError("Integer should be returned", t->lineno);
      else if (t->child[0]->kind.exp == IdK)
      { if (st_lookup(scopeStack[scopeStackTop - 1], t->child[0]->attr.name) == NULL)
          symbolError("Integer should be returned", t->lineno);
      }
      else if (t->child[0]->kind.exp != ConstK)
        symbolError("Integer should be returned", t->lineno);
    }
  }
  else //something weird
  { symbolError("No function declare", t->lineno);
    return;
  }
}

/* Procedure insertNode inserts 
 * identifiers stored in t into 
 * the symbol table 
 */
static void insertNode( TreeNode * t )
{ BucketList l; TreeNode * tmp1, * tmp2; char * name; int isArr = 0;

  switch (t->nodekind)
  { case DclK:
      switch (t->kind.dcl)
      { case FuncK:
          l = st_lookup(scopeStack[scopeStackTop - 1], t->attr.name);
              
          if(l != NULL)
          { symbolError("Redefined Function", t->lineno);
            return;
          }

          st_insert(t->attr.name, t->type, t->lineno, t);
            //ScopeList scope = newScope(t->attr.name);
          pushScope(newScope(t->attr.name));
          isFunc = 1;

          break;
        case VarK:
        case ArrVarK:
          if(t->type == Void)
          { symbolError("Variable cannot be void", t->lineno);
            return;
          }
          if(t->kind.dcl == VarK)
            name = t->attr.name;
          else
            name = t->attr.arr.name;

          l = st_lookup_noparent(scopeStack[scopeStackTop - 1], name);
          if(l != NULL)
          { symbolError("Redefined Variable", t->lineno);
            return;
          }

          st_insert(name, t->type, t->lineno, t);

          break;
        default:
          //U shouldn't be here
          break;
      }
      break;
    case ParamK:
      if(t->kind.param == ParK)
        name = t->attr.name;
      else if(t->kind.param == ArrParK)
        name = t->attr.arr.name;

      if(st_lookup_noparent(scopeStack[scopeStackTop - 1], name) == NULL)
        st_insert(name, t->type, t->lineno, t);
      else
      { symbolError("Redefined Parameter", t->lineno);
        return;
      }
      /*
      switch (t->kind.param)
      { case ParK:
        case ArrParK:
          if()

          if(st_lookup_noparent(scopeStack[scopeStackTop - 1], ))
          {
          }
          break;
        default:
          //U shouldn't be here
          break;
      }*/
      break;
    case StmtK:
      switch (t->kind.stmt)
      { case CompK:
          if (isFunc == 1)
            isFunc = 0;
          else
          { ScopeList scope = newScope(scopeStack[scopeStackTop - 1]->name);
            pushScope(scope);
          }
          break;
        case IfK:
        case IterK:
          //nothing
          break;
        case RetK:
          checkRetK(t);
          break;
          /*
        case AssignK:
        case ReadK:
          if (st_lookup(t->attr.name) == -1)
          not yet in table, so treat as new definition
            st_insert(t->attr.name,t->lineno,location++);
          else
          already in table, so ignore location, 
             add line number of use only
            st_insert(t->attr.name,t->lineno,0);
          break; */
        default:
          break;
      }
      break;
    case ExpK:
      switch (t->kind.exp)
      { case ArrIdK:
          name = t->attr.arr.name; isArr = 1;
        case IdK:
        case CallK:
          if(isArr == 0)
            name = t->attr.name;

          l = st_lookup(scopeStack[scopeStackTop - 1], name); 
          if (l == NULL) {
          /* not yet in table */
            symbolError("Undeclared symbol", t->lineno);
            return;
          }
          else {
          /* already in table add line number of use only */
            if(t->kind.exp == CallK)
            { tmp1 = t->child[0];
              tmp2 = l->node->child[0];

              while(tmp1 != NULL)
              { if(tmp2->type == Void || tmp2 == NULL)
                  symbolError("Wrong parameter type", t->lineno);

                if(tmp1->kind.param != tmp2->kind.param)
                { symbolError("Wrong parameter type", t->lineno);
                  return;
                }
                //paranNum++;
                tmp1 = tmp1->sibling;
                tmp2 = tmp2->sibling;

                if(tmp1 == NULL  && tmp2 != NULL)
                  symbolError("Wrong parameter type", t->lineno);
              }
            }

            st_addLineno(l, t->lineno);
          }

          break;
        case AssignK:
          if(t->child[1]->type == Void)
          { symbolError("Cannot assign Void type", t->lineno);
            return;
          }

          if(t->child[0]->kind.exp != t->child[1]->kind.exp)
          { symbolError("Assign different type", t->lineno);
            return;
          }
          /*
          l = st_lookup(scopeStackTop[scopeStackTop - 1], t->child[0]->attr.name);

          if(l == NULL)
          { symbolError("Undefined variable", t->lineno);
            return;
          }
          else
          { if(t->child[0]->kind.exp == ArrIdK)
          }*/
          break;
        case OpK:
          tmp1 = t->child[0];
          tmp2 = t->child[1];
          if(tmp1->type != tmp2->type)
          { symbolError("Need to be same type to operate", t->lineno);
            return;
          }

          if(tmp1->type == Void || tmp2->type == Void)
          { symbolError("Void type cannot be operated", t->lineno);
            return;
          }

          if(tmp1->kind.exp == IdK)
            name = tmp1->attr.name;
          else if(tmp1->kind.exp == ArrIdK)
            name = tmp1->attr.arr.name;
          l = st_lookup(scopeStack[scopeStackTop - 1], name);
          if(tmp1->kind.exp != l->node->kind.exp)
          { symbolError("Use unavaliable type", t->lineno);
            return;
          }

          if(tmp2->kind.exp == IdK)
            name = tmp2->attr.name;
          else if(tmp2->kind.exp == ArrIdK)
            name = tmp2->attr.arr.name;
          l = st_lookup(scopeStack[scopeStackTop - 1], name);
          if(tmp2->kind.exp != l->node->kind.exp)
          { symbolError("Use unavaliable type", t->lineno);
            return;
          }

          break;
        default:
          break;
      }
      break;
    case TypeK:
      break;
    default:
      //U shouldn't be here
      break;
  }
}

//insert builtin IOfunnction
void builtinFunc()
{ TreeNode *d, *t, *p, *c;

  d = newDclNode(FuncK);
  d->attr.name = "input";

  t = newTypeNode(TypenameK);
  d->child[0] = t;
  //t->type = Integer;
  d->type = Integer;

  p = newParamNode(ParK);
  p->type = Void;
  d->child[1] = p;

  c = newStmtNode(CompK);
  c->child[0] = NULL;
  c->child[1] = NULL;
  d->child[2] = c;

  st_insert("input", Integer, 0, d);

  d = newDclNode(FuncK);
  d->attr.name = "output";

  t = newTypeNode(TypenameK);
  d->child[0] = t;
  d->type = Void;

  p = newParamNode(ParK);
  p->child[0] = newTypeNode(TypenameK);
  //p->child[0]->attr.type = Integer;
  p->attr.name = "param";
  p->type = Integer;
  d->child[1] = p;

  c = newStmtNode(CompK);
  c->child[0] = NULL;
  c->child[1] = NULL;
  d->child[2] = c;

  st_insert("output", Void, 0, d);
}

/* Function buildSymtab constructs the symbol 
 * table by preorder traversal of the syntax tree
 */
void buildSymtab(TreeNode * syntaxTree)
{ ScopeList scope = newScope("global");
  pushScope(scope);
  builtinFunc();

  traverse(syntaxTree,insertNode,afterInsert);
  if (TraceAnalyze)
  { fprintf(listing,"\nSymbol table:\n\n");
    printSymTab(listing);
  }
}

static void typeError(TreeNode * t, char * message)
{ fprintf(listing,"Type error at line %d: %s\n",t->lineno,message);
  Error = TRUE;
}
