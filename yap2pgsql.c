#include "../yap-6.3/include/YapInterface.h"
#include <libpq-fe.h>
#include <string.h>
#include <stdlib.h>

static PGconn *conn = NULL;

// db_connect(Host, Port, DBName, User, Password)
static bool db_connect(void) {
    YAP_Term host = YAP_ARG1;
    YAP_Term port = YAP_ARG2;
    YAP_Term dbname = YAP_ARG3;
    YAP_Term user = YAP_ARG4;
    YAP_Term pass = YAP_ARG5;
    
    const char *host_str = YAP_AtomName(YAP_AtomOfTerm(host));
    long port_num = YAP_IntOfTerm(port);
    const char *dbname_str = YAP_AtomName(YAP_AtomOfTerm(dbname));
    const char *user_str = YAP_AtomName(YAP_AtomOfTerm(user));
    const char *pass_str = YAP_AtomName(YAP_AtomOfTerm(pass));

    char conninfo[1024];
    snprintf(conninfo, sizeof(conninfo),
             "host=%s port=%ld dbname=%s user=%s password=%s",
             host_str, port_num, dbname_str, user_str, pass_str);

    conn = PQconnectdb(conninfo);
    
    if (PQstatus(conn) != CONNECTION_OK) {
        fprintf(stderr, "Connection to database failed: %s",
                PQerrorMessage(conn));
        PQfinish(conn);
        conn = NULL;
        return false;
    }
    
    return true;
}

// db_disconnect
static bool db_disconnect(void) {
    if (conn) {
        PQfinish(conn);
        conn = NULL;
    }
    return true;
}

// db_query(Query, Params, Result)
static bool db_query(void) {
    YAP_Term query = YAP_ARG1;
    YAP_Term params = YAP_ARG2;
    YAP_Term result = YAP_ARG3;

    const char *query_str = YAP_AtomName(YAP_AtomOfTerm(query));

    // Convert Prolog list of parameters to C array
    int param_count = YAP_ListLength(params);
    const char *param_values[param_count];
    YAP_Term list = params;
    
    for (int i = 0; i < param_count; i++) {
        YAP_Term head = YAP_HeadOfTerm(list);
        param_values[i] = YAP_AtomName(YAP_AtomOfTerm(head));
        list = YAP_TailOfTerm(list);
    }

    PGresult *res = PQexecParams(conn, 
                                query_str,
                                param_count,
                                NULL,  // Let the backend figure out parameter types
                                param_values,
                                NULL,  // Don't need parameter lengths for text
                                NULL,  // Don't need parameter formats
                                0);    // Want text results

    if (PQresultStatus(res) != PGRES_TUPLES_OK) {
        fprintf(stderr, "Query failed: %s", PQerrorMessage(conn));
        PQclear(res);
        return false;
    }

    // Convert result to Prolog terms
    int nrows = PQntuples(res);
    int ncols = PQnfields(res);
    
    YAP_Term rows = YAP_MkAtomTerm(YAP_LookupAtom("[]"));

    for (int i = nrows-1; i >= 0; i--) {
        YAP_Term row_args[ncols];
        for (int j = 0; j < ncols; j++) {
            char *value = PQgetvalue(res, i, j);
            row_args[j] = YAP_MkAtomTerm(YAP_LookupAtom(value));
        }
        
        YAP_Term row = YAP_MkApplTerm(YAP_MkFunctor(YAP_LookupAtom("row"), ncols), ncols, row_args);
        rows = YAP_MkPairTerm(row, rows);
    }

    PQclear(res);
    return YAP_Unify(result, rows);
}

// Initialize the module
void init_predicates(void) {
    YAP_UserCPredicate("db_connect", db_connect, 5);
    YAP_UserCPredicate("db_disconnect", db_disconnect, 0);
    YAP_UserCPredicate("db_query", db_query, 3);
} 