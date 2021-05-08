// tcltk.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"

#ifdef _UNICODE
#ifndef UNICODE
#define UNICODE
#endif
#endif

#include "stdio.h"
#include "amzi.h"
#include <tchar.h>
#include "tcl.h"
#include "tk.h"
#include <vector>

TF EXPFUNC p_tcl_main(ENGid);
TF EXPFUNC p_tcl_init(ENGid);
TF EXPFUNC p_tk_mainloop(ENGid);
TF EXPFUNC p_tcl_eval(ENGid);
TF EXPFUNC p_tcl_evalfile(ENGid);
TF EXPFUNC p_tcl_finish(ENGid);
TF EXPFUNC p_tcl_exit(ENGid);

PRED_INIT extPreds[] =
{
   {_T("tcl_init"), 3, p_tcl_init},
   {_T("tk_mainloop"), 0, p_tk_mainloop},
   {_T("tcl_finish"), 1, p_tcl_finish},
   {_T("tcl_eval"), 3, p_tcl_eval},
   {_T("tcl_evalfile"), 3, p_tcl_evalfile},
   {NULL, 0, NULL}
};

#ifdef _WIN32
BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD dwReason, LPVOID lpRes)
{
    switch (dwReason)
    {
    case DLL_PROCESS_ATTACH:
    case DLL_THREAD_ATTACH:
    case DLL_THREAD_DETACH:
    case DLL_PROCESS_DETACH:
        break;
    }
    return TRUE;
}
#endif /* _WIN32 */

int Tcl_GetIntPtrFromObj(Tcl_Interp *interp, Tcl_Obj *objPtr,
    intptr_t *intptr_tPtr)
{
#ifdef _WIN64
    return Tcl_GetWideIntFromObj(interp, objPtr, intptr_tPtr);
#else
    return Tcl_GetIntPtrFromObj(interp, objPtr, intptr_tPtr);
#endif
}

void Tcl_SetIntPtrObj(Tcl_Obj *objPtr, intptr_t intptr_tValue)
{
#ifdef _WIN64
    return Tcl_SetWideIntObj(objPtr, intptr_tValue);
#else
    return Tcl_SetIntObj(objPtr, intptr_tValue);
#endif
}

//-------------------------------------------
// Required LSX Initialization Function
//

#ifdef _WIN32
extern "C" __declspec(dllexport) RC EXPFUNC InitPreds(ENGid eid, void* p)
#else
extern "C" RC EXPFUNC InitPreds(ENGid eid, void* p)
#endif
{
    RC rc;
    _TCHAR buf[80];

    rc = lsInitPreds(eid, extPreds);
    if (rc)
    {
#ifdef _WIN32
        _stprintf(buf, _T("Error #%d Loading atcltk Predicates"), rc);
        MessageBox(NULL, buf, "atcltk lsx", MB_OK);
#else
        printf(_T("Error #%d Loading atcltk Predicates"), rc);
#endif
    }
    //else
    //   MessageBox(NULL, "ATclTk Predicates Loaded", "ATCLTK LSX", MB_OK);

    return 0;
}

//-----------------------------------------------------------
// Tcl/Tk -> Prolog functions
//

std::vector<TERM> g_called_terms;   // A stack of called terms


// ls_call

int tcl_ls_call(ClientData eng, Tcl_Interp *terp, int objc, Tcl_Obj *CONST objv[])
{
    TERM t;
    char *query;
    Tcl_Obj *result;

    query = Tcl_GetString(objv[1]);
    RC rc = lsCallStr(eng, &t, query);
    result = Tcl_GetObjResult(terp);
    if (rc == TRUE)
    {
        Tcl_SetIntPtrObj(result, (intptr_t)t);
        return TCL_OK;
    }
    else if (rc == FALSE)
    {
        Tcl_SetIntPtrObj(result, 0);
        return TCL_OK;
    }
    else
    {
        char *message = Tcl_Alloc(256);
        lsGetExceptMsg(eng, message, 255);
        Tcl_ResetResult(terp);
        Tcl_AppendStringsToObj(result, "Prolog error: ", message, NULL);
        Tcl_Free(message);
        return TCL_ERROR;
    }
}

// ls_exec

int tcl_ls_exec(ClientData eng, Tcl_Interp *terp, int objc, Tcl_Obj *CONST objv[])
{
    TERM t;
    char *query;
    Tcl_Obj *result;

    query = Tcl_GetString(objv[1]);
    RC rc = lsExecStr(eng, &t, query);
    result = Tcl_GetObjResult(terp);
    if (rc == TRUE)
    {
        Tcl_SetIntPtrObj(result, (intptr_t)t);
        return TCL_OK;
    }
    else if (rc == FALSE)
    {
        Tcl_SetIntPtrObj(result, 0);
        return TCL_OK;
    }
    else
    {
        char *message = Tcl_Alloc(256);
        lsGetExceptMsg(eng, message, 255);
        Tcl_ResetResult(terp);
        Tcl_AppendStringsToObj(result, "Prolog error: ", message, NULL);
        Tcl_Free(message);
        return TCL_ERROR;
    }
}

// ls_redo

int tcl_ls_redo(ClientData eng, Tcl_Interp *terp, int objc, Tcl_Obj *CONST objv[])
{
    Tcl_Obj *result;
    int it;

    result = Tcl_GetObjResult(terp);
    Tcl_GetIntFromObj(terp, objv[1], &it);
    if (it == 0)
    {
        Tcl_SetIntObj(result, 0);
        return TCL_OK;
    }
    else
    {
        TF tf = lsRedo(eng);
        if (tf == TRUE)
        {
            Tcl_SetIntObj(result, it);
            return TCL_OK;
        }
        else if (tf == FALSE)
        {
            Tcl_SetIntObj(result, 0);
            return TCL_OK;
        }
        else
        {
            char message[256];
            lsGetExceptMsg(eng, message, 255);
            Tcl_ResetResult(terp);
            Tcl_AppendStringsToObj(result, "Prolog error: ", message, NULL);
            return TCL_ERROR;
        }
    }
}

// ls_clearcall

int tcl_ls_clearcall(ClientData eng, Tcl_Interp *terp, int objc, Tcl_Obj *CONST objv[])
{
    Tcl_Obj *result;
    int it;

    result = Tcl_GetObjResult(terp);
    Tcl_GetIntFromObj(terp, objv[1], &it);
    if (it == 0)
    {
        Tcl_SetIntObj(result, 0);
        return TCL_OK;
    }
    else
    {
        TF tf = lsClearCall(eng);
        Tcl_SetIntObj(result, 0);
        return TCL_OK;
    }
}

// ls_getstrarg term i
// returns string value of term's ith argument (starting at 1)
int tcl_ls_getstrarg(ClientData eng, Tcl_Interp *terp, int objc, Tcl_Obj *CONST objv[])
{
    TERM t, targ;
    intptr_t it;
    int i;
    char s[512];
    Tcl_Obj *result;

    Tcl_GetIntPtrFromObj(terp, objv[1], &it);
    Tcl_GetIntFromObj(terp, objv[2], &i);
    result = Tcl_GetObjResult(terp);
    if (it == 0)
    {
        Tcl_SetStringObj(result, "", -1);
        return TCL_OK;
    }
    else
    {
        t = (TERM)it;
        lsGetArg(eng, t, i, cTERM, &targ);
        lsTermToStr(eng, targ, s, 511);
        Tcl_SetStringObj(result, s, -1);
        return TCL_OK;
    }
}

// ls_termtostr term
// returns string value of term
int tcl_ls_termtostr(ClientData eng, Tcl_Interp *terp, int objc, Tcl_Obj *CONST objv[])
{
    TERM t;
    intptr_t it;
    char s[512];
    Tcl_Obj *result;

    Tcl_GetIntPtrFromObj(terp, objv[1], &it);

    result = Tcl_GetObjResult(terp);
    if (it == 0)
    {
        Tcl_SetStringObj(result, "", -1);
        return TCL_OK;
    }
    else
    {
        t = (TERM)it;
        lsTermToStr(eng, t, s, 511);
        Tcl_SetStringObj(result, s, -1);
        return TCL_OK;
    }
}

// ls_termtostrq term
// returns quoted string value of term suitable for sending back to Prolog
int tcl_ls_termtostrq(ClientData eng, Tcl_Interp *terp, int objc, Tcl_Obj *CONST objv[])
{
    TERM t;
    intptr_t it;
    char s[512];
    Tcl_Obj *result;

    Tcl_GetIntPtrFromObj(terp, objv[1], &it);
    result = Tcl_GetObjResult(terp);
    if (it == 0)
    {
        Tcl_SetStringObj(result, "", -1);
        return TCL_OK;
    }
    else
    {
        t = (TERM)it;
        lsTermToStrQ(eng, t, s, 511);
        Tcl_SetStringObj(result, s, -1);
        return TCL_OK;
    }
}

// The Prolog query returned a list of the form [varname, value, ...]
// if there were any variables bindings.  Map them to a Tcl list,
// and return.  The first name=value pair is result and either true/false.
int map_prolog_tcl_lists(Tcl_Interp *terp, ClientData eng, TF tf, TERM vqt)
{
    TERM varlist;  // the [varname, value, ...] list
    TERM elem;     // an element in the var list
    int i;
    int rlen;     // length of result varname, value list
    char *answer;
    char elemstr[512];
    Tcl_Obj *result;
    Tcl_Obj **tclobjs;

    // Get the result object for this interpreter.
    result = Tcl_GetObjResult(terp);

    // The query succeeded
    if (tf == TRUE)
    {
        // Get the length of the result list
        lsGetArg(eng, vqt, 2, cINT, &rlen);
        // Allocate an array of Tcl_Objects to put in the list
        tclobjs = (Tcl_Obj**)Tcl_Alloc((rlen + 2) * sizeof(Tcl_Obj*));
        // Set the first name value pair to 'result' 'true'
        tclobjs[0] = Tcl_NewStringObj("result", -1);
        tclobjs[1] = Tcl_NewStringObj("true", -1);
        // Get the [varname, value, ...] list from Prolog
        lsGetArg(eng, vqt, 3, cTERM, &varlist);
        // Populate the array of Tcl objects with the Prolog
        // list elements.
        for (i = 0; i < rlen; i++)
        {
            // Get the next element, might be a name or a
            // value but treat them alike.
            lsGetHead(eng, varlist, cTERM, &elem);
            lsTermToStr(eng, elem, elemstr, 511);
            // Put it in the array.
            tclobjs[i + 2] = Tcl_NewStringObj(elemstr, -1);
            // Get the tail of the list for next iteration.
            varlist = lsGetTail(eng, varlist);
        }
        Tcl_SetListObj(result, rlen + 2, tclobjs);
        return TCL_OK;
    }
    else if (tf == FALSE)
    {
        g_called_terms.pop_back();   // remove the last called term
        // Allocate an array of Tcl_Objects to put in the list
        tclobjs = (Tcl_Obj**)Tcl_Alloc(2 * sizeof(Tcl_Obj*));
        // Set the first name value pair to 'result' 'false'
        tclobjs[0] = Tcl_NewStringObj("result", -1);
        tclobjs[1] = Tcl_NewStringObj("false", -1);
        Tcl_SetListObj(result, 2, tclobjs);
        return TCL_OK;
    }
    else
    {
        answer = Tcl_Alloc(300);
        strcpy(answer, "Logic Server Error: ");
        char *message = Tcl_Alloc(256);
        lsGetExceptMsg(eng, message, 255);
        strcat(answer, message);
        Tcl_SetStringObj(result, answer, -1);
        Tcl_Free(message);
        return TCL_ERROR;
    }
}

// ls_query option string
// This is the high-level query command that returns a list of variable names
// and values.  It is thought this approach will replace other logic server
// calls in the future.  If there are no variables, it returns "yes",
// and if the query failed, it returns "no".

int tcl_ls_query(ClientData eng, Tcl_Interp *terp, int objc, Tcl_Obj *CONST objv[])
{
    TERM vqt;      // the varlist query term
    char *varlist_query;
    char *query;
    char *option;
    int qlen;
    TF tf;

    // Get the option, either once, first, next or clear
    option = Tcl_GetStringFromObj(objv[1], NULL);

    if (0 == strcmp(option, "once"))
    {
        // Get the user's query and embedded it in a call to
        // varlist_query/3 which will return a list of variables
        // and their bindings.
        query = Tcl_GetStringFromObj(objv[2], &qlen);
        varlist_query = Tcl_Alloc(qlen + 50);
        sprintf(varlist_query, "varlist_query(`%s`, L, V)", query);  // don't make variable names longer unless you up varlist query
        tf = lsExecStr(eng, &vqt, varlist_query);
        Tcl_Free(varlist_query);

        return map_prolog_tcl_lists(terp, eng, tf, vqt);
    }

    else if (0 == strcmp(option, "first"))
    {
        // Get the user's query and embedded it in a call to
        // varlist_query/3 which will return a list of variables
        // and their bindings.
        query = Tcl_GetStringFromObj(objv[2], &qlen);
        varlist_query = Tcl_Alloc(qlen + 50);
        sprintf(varlist_query, "varlist_query(`%s`, L, V)", query);  // don't make variable names longer unless you up varlist query
        tf = lsCallStr(eng, &vqt, varlist_query);
        Tcl_Free(varlist_query);
        g_called_terms.push_back(vqt);  // save as the last called term, for ls_query_next

        return map_prolog_tcl_lists(terp, eng, tf, vqt);
    }

    else if (0 == strcmp(option, "next"))
    {
        if (g_called_terms.empty() == true)
        {
            Tcl_Obj *result = Tcl_GetObjResult(terp);
            Tcl_SetStringObj(result, "No active query to redo", -1);
            return TCL_ERROR;
        }

        vqt = g_called_terms.back();  // from the saved list
        tf = lsRedo(eng);

        return map_prolog_tcl_lists(terp, eng, tf, vqt);
    }

    else if (0 == strcmp(option, "clear"))
    {
        if (g_called_terms.empty() == true)
        {
            Tcl_Obj *result = Tcl_GetObjResult(terp);
            Tcl_SetStringObj(result, "No active query to clear", -1);
            return TCL_ERROR;
        }

        g_called_terms.pop_back();   // clear the query term
        lsClearCall(eng);
        return TCL_OK;
    }

    else
    {
        Tcl_Obj *result = Tcl_GetObjResult(terp);
        Tcl_SetStringObj(result, "Unknown ls_query option", -1);
        return TCL_ERROR;
    }
}



//------------------------------------------------------------
// Prolog -> Tck/Tk extended predicates
//

// tcl_init(-TclInterpreter, +TclLib, -Result)
//    TclInterpreter - The address returned for future calls.
//    TclLib - The location of the Tcl libraries.  Standards
//             are 'amzi', 'tcl', 'here'.  Otherwise, the path.
//             The directory structure is TclLib/tcl and
//             TclLib/tk, except if 'tcl', in which case it uses
//             whereever Tcl is installed on the machine.
//    Result - The return result from the interpreter.
TF EXPFUNC p_tcl_init(ENGid CurEng)
{
    Tcl_Interp *terp;
    char msg[1000];
    const char *amzi_dir;
    char library[700];
    char tcl_lib[500];

    Tcl_FindExecutable("atcltk.lsx");
    terp = Tcl_CreateInterp();

    // New query interface
    Tcl_CreateObjCommand(terp, "ls_query", tcl_ls_query, CurEng, NULL);
    // Classic Logic Server calls
    Tcl_CreateObjCommand(terp, "ls_call", tcl_ls_call, CurEng, NULL);
    Tcl_CreateObjCommand(terp, "ls_exec", tcl_ls_exec, CurEng, NULL);
    Tcl_CreateObjCommand(terp, "ls_redo", tcl_ls_redo, CurEng, NULL);
    Tcl_CreateObjCommand(terp, "ls_clearcall", tcl_ls_clearcall, CurEng, NULL);
    Tcl_CreateObjCommand(terp, "ls_termtostr", tcl_ls_termtostr, CurEng, NULL);
    Tcl_CreateObjCommand(terp, "ls_termtostrq", tcl_ls_termtostrq, CurEng, NULL);
    Tcl_CreateObjCommand(terp, "ls_getstrarg", tcl_ls_getstrarg, CurEng, NULL);

    lsGetParm(CurEng, 2, cSTR, tcl_lib);

    if (0 == strcmp(tcl_lib, "amzi"))
    {
        amzi_dir = Tcl_GetVar(terp, "env(AMZI_DIR)", TCL_GLOBAL_ONLY);
        strcpy(library, amzi_dir);
        strcat(library, "\\langbindings\\tcltk\\lib\\tcl");
        Tcl_SetVar(terp, "tcl_library", library, TCL_GLOBAL_ONLY);
        strcpy(library, amzi_dir);
        strcat(library, "\\langbindings\\tcltk\\lib\\tk");
        Tcl_SetVar(terp, "tk_library", library, TCL_GLOBAL_ONLY);
    }
    else if (0 == strcmp(tcl_lib, "here"))
    {
        strcpy(library, ".\\lib\\tcl");
        Tcl_SetVar(terp, "tcl_library", library, TCL_GLOBAL_ONLY);
        strcpy(library, ".\\lib\\tk");
        Tcl_SetVar(terp, "tk_library", library, TCL_GLOBAL_ONLY);
    }
    else if (0 == strcmp(tcl_lib, "tcl"))
        ;
    else
    {
        strcpy(library, tcl_lib);
        strcat(library, "\\lib\\tcl");
        Tcl_SetVar(terp, "tcl_library", library, TCL_GLOBAL_ONLY);
        strcpy(library, tcl_lib);
        strcat(library, "\\lib\\tk");
        Tcl_SetVar(terp, "tk_library", library, TCL_GLOBAL_ONLY);
    }

    if (Tcl_Init(terp) == TCL_ERROR)
    {
        strcpy(msg, "Tcl Init: ");
        strncat(msg, Tcl_GetStringResult(terp), 1000);
        lsErrRaise(CurEng, msg);
    }

    if (Tk_Init(terp) == TCL_ERROR)
    {
        strcpy(msg, "Tk Init: ");
        strncat(msg, Tcl_GetStringResult(terp), 1000);
        lsErrRaise(CurEng, msg);
    }

    lsUnifyParm(CurEng, 1, cADDR, &terp);
    lsUnifyParm(CurEng, 3, cASTR, terp->result);

    return TRUE;
}


TF EXPFUNC p_tk_mainloop(ENGid CurEng)
{
    Tk_MainLoop();
    return TRUE;
}

TF EXPFUNC p_tcl_eval(ENGid CurEng)
{
    Tcl_Interp *terp = NULL;
    size_t len;
    char *buffer;
    //char buffer[10000];
    int rc;
    lsGetParm(CurEng, 1, cADDR, &terp);
    len = 3 * lsStrParmLen(CurEng, 2);  // blatent hack in case utf8io flag set
    buffer = (char*)malloc((len + 1) * sizeof(char));
    lsGetParm(CurEng, 2, cASTR, buffer);
    Tcl_Preserve(terp);
    rc = Tcl_Eval(terp, buffer);
    Tcl_Release(terp);
    if (rc == TCL_OK)
    {
        lsUnifyParm(CurEng, 3, cASTR, terp->result);
        return TRUE;
    }
    else
    {
        lsErrRaise(CurEng, terp->result);
        return FALSE;
    }
}

TF EXPFUNC p_tcl_evalfile(ENGid CurEng)
{
    Tcl_Interp *terp = NULL;
    char *buffer;
    int rc;
    size_t len;
    char msg[120];

    lsGetParm(CurEng, 1, cADDR, &terp);
    len = lsStrParmLen(CurEng, 2);
    buffer = (char*)malloc((len + 1) * sizeof(char));
    lsGetParm(CurEng, 2, cASTR, buffer);
    Tcl_Preserve(terp);
    rc = Tcl_EvalFile(terp, buffer);
    Tcl_Release(terp);
    if (rc == TCL_OK)
    {
        lsUnifyParm(CurEng, 3, cASTR, terp->result);
        return TRUE;
    }
    else
    {
        strcpy(msg, "Tcl_EvalFile: ");
        strncat(msg, Tcl_GetStringResult(terp), 1000);
        lsErrRaise(CurEng, msg);
        return FALSE;
    }
}


// tcl_finish(TclInterpreter)
//   Just wind down the interpreter, DON'T exit, as that
//   will kill the calling process as well.  This, the
//   atcltk LSX, stays memory resident until the Amzi!
//   engine actually shuts down.
TF EXPFUNC p_tcl_finish(ENGid CurEng)
{
    Tcl_Interp *terp = NULL;
    lsGetParm(CurEng, 1, cADDR, &terp);
    Tcl_DeleteInterp(terp);
    return TRUE;
}
