#include <stdio.h>
#include <stdlib.h>
#include "db_utils.h"


void drop_table_puzzles(PGconn *conn) {
    const char *query = 
    "DROP TABLE if exists Puzzles;";

    PGresult *res = PQexec(conn, query);
    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
        fprintf(stderr, "Drop table failed (puzzles): %s\n", PQerrorMessage(conn));
        PQclear(res);
        PQfinish(conn);
        exit(EXIT_FAILURE);
    }

    PQclear(res);   
    fprintf(stderr, "Drop table successfully (puzzles)\n");

}


void create_puzzles_table(PGconn *conn) {
    const char *query = 
        "CREATE TABLE IF NOT EXISTS Puzzles ("
        "name VARCHAR(50),"
        "color VARCHAR(50),"
        "type VARCHAR(50),"
        "geom GEOMETRY);";

    PGresult *res = PQexec(conn, query);
    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
        fprintf(stderr, "Table creation failed (puzzles): %s\n", PQerrorMessage(conn));
        PQclear(res);
        PQfinish(conn);
        exit(EXIT_FAILURE);
    }
    PQclear(res);   
    fprintf(stderr, "Create successfully (puzzles)\n");
     
}

void populate_puzzles(PGconn *conn) {
    const char *query =    
    "INSERT INTO puzzles (name, color, type, geom) VALUES"
    "('Board1', 'black', 'Polygon', 'POLYGON((0 0, 6 0, 6 5, 0 5, 0 0), (2 2, 3 2, 3 3, 2 3, 2 2), (5 3, 6 3, 6 4, 5 4, 5 3))'),"
    "('Board2', 'black', 'Polygon', 'POLYGON((0 0, 6 0, 6 5, 0 5, 0 0), (2 2, 3 2, 3 3, 2 3, 2 2), (2 4, 3 4, 3 5, 2 5, 2 4))')"
    ;
   

    PGresult *res = PQexec(conn, query);
    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
        fprintf(stderr, "Insert failed (puzzles): %s\n", PQerrorMessage(conn));
        PQclear(res);
        exit(EXIT_FAILURE);
    }
    PQclear(res);
    fprintf(stderr, "Insert successfully populated(puzzles)\n");

}