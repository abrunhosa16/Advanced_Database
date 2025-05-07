#include <stdio.h>
#include <stdlib.h>
#include "db_utils.h"

#define CONNINFO "host=localhost dbname=postgres user=postgres password=1234"

PGconn* connect_db() {
    PGconn *conn = PQconnectdb(CONNINFO);
    if (PQstatus(conn) != CONNECTION_OK)
        handle_error(conn);
    return conn;
}

void handle_error(PGconn *conn) {
    fprintf(stderr, "Error: %s\n", PQerrorMessage(conn));
    PQfinish(conn);
    exit(EXIT_FAILURE);
}

