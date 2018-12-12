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
//for check return type;
char * funcName;
//for checking error;
int isErr = 0;

/* Procedure traverse is a generic recursive 
 * syntax tree traversal routine:
 * it applies preProc in preorder and postProc 
 * in postorder to tree pointed to by t
 */
static void traverse( TreeNode * t,
               void (* preProc) (TreeNode *),
               void (* postProc) (TreeNode *) )
{ if (t != NULL)
  { fprintf(listing, "DEBUG ME\n");
    preProc(t);
    //fprintf(listing, "DEBUG2\n");

    { int i;
      for (i=0; i < MAXCHILDREN; i++){
        fprintf(listing, "DEBUG child\n"); traverse(t->child[i],preProc,postProc); }
    }
    postProc(t);
    fprintf(listing, "DEBUG sibling`\n");
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
{ fprintf(listing, "Semantic Error: %s at line %d\n", errorName, lineno);
  Error = TRUE;
  isErr++;
}

//check return type error
void checkRetK(TreeNode * t)
{ char * name; BucketList l; TreeNode * tmp;
  l = st_lookup(scopeStack[0], funcName);
  fprintf(listing, "DEBUG8 %s %d\n", funcName, l->type);
  
  if(t->child[0] == NULL) //treenode return void
  { if(l->type != Void)
      symbolError("Integer should be returned", t->lineno);

    return;
  }
  else // treenode return something
  { 
    tmp = t->child[0];
    switch(tmp->kind.exp)
    { case AssignK:
        symbolError("return wrong type", t->lineno);
        return;
        break;
      case OpK:
      case ConstK: //declared function must return int
        if(l->type != Integer)
        { symbolError("Void should be returned", t->lineno);
          return;
        }
        break;
      case IdK:
      case CallK:
        name = tmp->attr.name;
        break;
      case ArrIdK:
        name = tmp->attr.arr.name;
        break;
    }
    
    if (l != NULL)
    { if (l->type == Void)
      { if(tmp->kind.exp == CallK)
        { l = st_lookup(scopeStack[scopeStackTop - 1], tmp->attr.name);
          if(l == NULL)
            symbolError("return unknown function", t->lineno);
          else if(l->type != Void)
            symbolError("Void should be returned", t->lineno);
        }
        else
          symbolError("Void should be returned", t->lineno);
        return;
      }
      else if (l->type == Integer)
      { switch(tmp->kind.exp)
        { case CallK:
          case IdK:
            fprintf(listing, "DEBUG NAME %s\n", tmp->attr.name);

            l = st_lookup(scopeStack[scopeStackTop - 1], tmp->attr.name);
            
            if(l == NULL)
            { symbolError("return unknown type", t->lineno);
              return;
            }

            if(tmp->kind.exp == CallK)
            { if(l->type == Void)
              { symbolError("Integer should be returned", t->lineno);
                return;
              }
            }
            break;
          case ArrIdK:
            if (st_lookup(scopeStack[scopeStackTop - 1], tmp->attr.arr.name) == NULL)
            { symbolError("return unknown type", t->lineno);
              return;
            }
            break;
        }
      }
    }
    else//something wierd
      symbolError("Function does not exist", t->lineno);
  }
}
//function for check op exptype return type and -1 if error
int checkOpType(TreeNode * t)
{ char * name; BucketList m;
  
  if(t->kind.exp == ConstK || t->kind.exp == OpK)
    {return 1;}
  else
  { if(t->kind.exp == IdK || t->kind.exp == CallK)
      name = t->attr.name;
    else if(t->kind.exp == ArrIdK)
      name = t->attr.arr.name;
    m = st_lookup(scopeStack[scopeStackTop - 1], name);
    if(m == NULL)
    { symbolError("Undefined symbol", t->lineno);
      return -1;
    }
    fprintf(listing, "tkind %d %d\n", t->nodekind, m->node->nodekind);
    if(m->node->type == Void)
    { symbolError("Void type cannot be operated or assigned", t->lineno);
    }

    if(m->node->nodekind == ParamK)
    { if(t->kind.exp != (m->node->kind.param + 3)) // check arrvar or var
      { symbolError("Use unavaliable type", t->lineno);
        return -1;
      }
    }
    else if(m->node->nodekind == DclK)
    { if((t->kind.exp == IdK && m->node->kind.dcl == VarK) ||
        (t->kind.exp == ArrIdK && m->node->kind.dcl == ArrVarK)) // check arrvar or var
      {}
      else if(t->kind.exp == CallK)
        return m->node->type;
      else
      { symbolError("Use unavaliable type", t->lineno);
        return -1;
      }
    }
  }
  return 1;
}
/* Procedure insertNode inserts 
 * identifiers stored in t into 
 * the symbol table 
 */
static void insertNode( TreeNode * t )
{ BucketList l, m; TreeNode * tmp1, * tmp2; char * name; int isArr = 0;

  //fprintf(listing, "DEBUG2 %d %d\n", t->nodekind, t->type);
  switch (t->nodekind)
  { case DclK:
      fprintf(listing, "DEBUG DclK %d\n", t->kind.dcl);
      switch (t->kind.dcl)
      { case FuncK:
          //fprintf(listing, "DEBUG3 %d\n", scopeStackTop);    
          l = st_lookup(scopeStack[scopeStackTop - 1], t->attr.name);
          if(l != NULL)
          { symbolError("Redefined Function", t->lineno);
            return;
          }

          st_insert(t->attr.name, t->type, t->lineno, t);
            //ScopeList scope = newScope(t->attr.name);
          pushScope(newScope(t->attr.name));
          isFunc = 1; funcName = t->attr.name;

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
      fprintf(listing, "DEBUG ParamK %d\n", t->kind.param);
      if(t->type == Void)
        return;

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
      fprintf(listing, "DEBUG StmtK %d\n", t->kind.stmt);
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
      fprintf(listing, "DEBUG ExpK %d\n", t->kind.exp);

      switch (t->kind.exp)
      { case ArrIdK:
          name = t->attr.arr.name; isArr = 1;
        case IdK:
        case CallK:
          if(isArr == 0)
            name = t->attr.name;
          else
            isArr = 0;

          fprintf(listing, "DEBUG10 %s\n", name);
          l = st_lookup(scopeStack[scopeStackTop - 1], name); 
          if (l == NULL) {
            symbolError("Undeclared symbol", t->lineno);
            return;
          }
          else {
            if(t->kind.exp == CallK)
            { tmp1 = t->child[0];
              tmp2 = l->node->child[1];
              
              if(tmp1 == NULL && tmp2 != NULL)
              { if(tmp2->nodekind == ParamK && tmp2->type == Void) {}
                else
                { symbolError("Different parameter #", t->lineno);
                  return;
                }
              }

              while(tmp1 != NULL)
              { if(tmp2->type == Void || tmp2 == NULL)
                { if(tmp1->kind.exp == CallK) // for dangling void func
                  { m = st_lookup(scopeStack[scopeStackTop - 1], tmp1->attr.name);

                    if(m == NULL)
                    { symbolError("Undefined function", t->lineno);
                      return;
                    }
                    else if(m->type == Void)
                    {
                      if(tmp1->sibling == NULL)
                        return;
                    }

                    symbolError("Different parameter type", t->lineno);
                    return;
                  }
                  else
                  { symbolError("Different parameter #", t->lineno);
                    return;
                  }
                }
                else if(tmp1->kind.exp == OpK || tmp1->kind.exp == ConstK) 
                { if(tmp2->kind.param == ArrParK)
                  { symbolError("Wrong parameter type", t->lineno);
                    return;
                  }
                }
                else if(tmp1->kind.exp == AssignK)
                { symbolError("Wrong parameter type", t->lineno);
                  return;
                }
                else if(tmp1->kind.exp == IdK)
                { m = st_lookup(scopeStack[scopeStackTop - 1], tmp1->attr.name);
                  //fprintf(listing, "DEBUG Parameter\n %s\n", m->name);

                  if(m == NULL)
                  { symbolError("Undefined parameter", t->lineno);
                    return;
                  }

                  if(m->type == Void)
                  { //variable cannot be void, maybe error occurs early
                    symbolError("Wrong parameter, Var cannot be Void", t->lineno);
                    return;
                  }
                  else
                  {
                    if(m->node->nodekind == DclK)
                    { if(m->node->kind.dcl == VarK)
                      {
                        if(tmp2->kind.param == ArrParK) // declare Arrvar but use var
                        { symbolError("Wrong parameter type", t->lineno);
                          return;
                        }
                      }
                      else if(m->node->kind.dcl == ArrVarK) // vice versa
                      { if(tmp2->kind.param == ParK)
                        { symbolError("Wrong paramter type", t->lineno);
                          return;
                        }
                      }
                    }
                    else if(m->node->nodekind == ParamK)
                    { if(m->node->kind.param == ParK)
                      {
                        if(tmp2->kind.param == ArrParK) // declare Arrvar but use var
                        { symbolError("Wrong parameter type", t->lineno);
                          return;
                        }
                      }
                      else if(m->node->kind.param == ArrParK) // vice versa
                      { if(tmp2->kind.param == ParK)
                        { symbolError("Wrong paramter type", t->lineno);
                          return;
                        }
                      }
                    }
                    else //maybe not happen, declare must be DclK or ParamK
                    {
                      symbolError("fatal error", t->lineno);
                      return;
                    }
                  }
                  /*
                  if(tmp2->kind.param == ParK)
                  { symbolError("Wrong parameter type", t->lineno);
                    return;
                  }*/
                }
                else if(tmp1->kind.exp == CallK)
                { m = st_lookup(scopeStack[scopeStackTop - 1], tmp1->attr.name); 
                  if (m == NULL)
                  { symbolError("Undeclared function", t->lineno);
                    return;
                  }
                  else if(m->type != tmp2->type)
                  { symbolError("Unmatched return type", t->lineno);
                    return;
                  }
                  else if(m->type == Integer && tmp2->kind.param == ArrParK)
                  { symbolError("Unmatched return type", t->lineno);
                    return;
                  }

                  //fprintf(listing, "DEBUG13 %d %d\n", tmp1->kind.exp, tmp2->kind.param);

                  /*if(tmp1->kind.exp != (tmp2->kind.param + 3))
                  { symbolError("Wrong parameter type", t->lineno);
                      return;
                  }*/
                }
                else if(tmp1->kind.exp == ArrIdK)
                { //this is syntax error, U shouldn't be here
                  symbolError("Wrong parameter type", t->lineno);
                  return;
                }

                tmp1 = tmp1->sibling;
                tmp2 = tmp2->sibling;

                if(tmp1 == NULL  && tmp2 != NULL)
                { symbolError("Different paramter #", t->lineno);
                  return;
                }
              }
            }

            st_addLineno(l, t->lineno);
          }

          break;
        case AssignK:
          tmp1 = t->child[0];
          tmp2 = t->child[1];

          if(checkOpType(tmp1) == -1)
            return;

          if(tmp2->kind.exp != OpK || tmp2->kind.exp != ConstK)
          { if(checkOpType(tmp2) == -1)
              return;
           }/*
          else
          {
            fprintf(listing, "DEBUG6 %d %d %d\n", tmp1->nodekind, tmp2->nodekind, tmp2->type);
            if(tmp1->type != tmp2->type)
            { symbolError("Need to be same type to assign", t->lineno);
              return;
            }

            if(tmp1->type == Void || tmp2->type == Void)
            { symbolError("Void type cannot be assigned or assign", t->lineno);
              return;
            }
          }*/
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
          fprintf(listing, "DEBUG7 %d\n", scopeStackTop);
          
          tmp1 = t->child[0];
          tmp2 = t->child[1];
          if(tmp1->kind.exp != OpK && tmp1->kind.exp != ConstK)
          { if(checkOpType(tmp1) == -1)
              return;
          }
          else if(tmp2->kind.exp != OpK && tmp2->kind.exp != ConstK)
          { if(checkOpType(tmp2) == -1)
              return;
          }/*
          else
          {
            fprintf(listing, "DEBUG6 %d %d\n", tmp1->nodekind, tmp2->nodekind);
            if(tmp1->type != tmp2->type)
            { symbolError("Need to be same type to operate", t->lineno);
              return;
            }

            if(tmp1->type == Void || tmp2->type == Void)
            { symbolError("Void type cannot be operated", t->lineno);
              return;
            }
          }*/
          /*
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
          }*/

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
  d->lineno = 0;

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
  d->lineno = 0;

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
//modified, return error numbers when err occurs
int buildSymtab(TreeNode * syntaxTree)
{ ScopeList scope = newScope("global");
  pushScope(scope);
  //fprintf(listing, "DEBUG4 %d\n", scopeStackTop);
  builtinFunc();
  traverse(syntaxTree,insertNode,afterInsert);
  return isErr;

  //fprintf(listing, "DEBUG1\n");
  /*
  if (TraceAnalyze)
  { fprintf(listing,"\nSymbol table:\n\n");
    printSymTab(listing);
  }*/
}

static void typeError(TreeNode * t, char * message)
{ fprintf(listing,"Type error at line %d: %s\n",t->lineno,message);
  Error = TRUE;
}
