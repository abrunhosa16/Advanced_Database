#include <stdio.h>
#include <stdlib.h>
#include "db_utils.h"

void drop_table_tetrominoes(PGconn *conn) {
    const char *query = 
    "DROP TABLE if exists tetrominoes;";

    PGresult *res = PQexec(conn, query);
    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
        fprintf(stderr, "Drop table failed (tetrominoes): %s\n", PQerrorMessage(conn));
        PQclear(res);
        PQfinish(conn);
        exit(EXIT_FAILURE);
    }

    PQclear(res);   
    fprintf(stderr, "Drop table successfully (tetrominoes)\n");

}


void create_tetrominoes_table(PGconn *conn) {
    const char *query = 
        "CREATE TABLE IF NOT EXISTS Tetrominoes ("
        "name VARCHAR(50),"
        "color VARCHAR(50),"
        "type VARCHAR(50),"
        "geom GEOMETRY(Polygon));";  

    PGresult *res = PQexec(conn, query);
    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
        fprintf(stderr, "Table creation failed (tetrominoes): %s\n", PQerrorMessage(conn));
        PQclear(res);
        PQfinish(conn);
        exit(EXIT_FAILURE);
    }
    PQclear(res);   
    fprintf(stderr, "Create successfully (tetrominoes)\n");
}


void populate_tetrominoes(PGconn *conn) {
    const char *query =
        "INSERT INTO Tetrominoes (name, color, type, geom) VALUES "
        "('I', '0,255,255,255', 'Polygon', 'POLYGON((0 0, 4 0, 4 1, 0 1, 0 0))'),"
        "('O', '255,255,0,255', 'Polygon', 'POLYGON((0 0, 2 0, 2 2, 0 2, 0 0))'),"
        "('T', '128,0,128,255', 'Polygon', 'POLYGON((1 0, 2 0, 2 1, 3 1, 3 2, 0 2, 0 1, 1 1, 1 0))'),"
        "('J', '0,0,255,255', 'Polygon', 'POLYGON((0 0, 2 0, 2 3, 1 3, 1 1, 0 1, 0 0))'),"
        "('L', '255,165,0,255', 'Polygon', 'POLYGON((0 0, 2 0, 2 1, 1 1, 1 3, 0 3, 0 0))'),"
        "('S', '0,255,0,255', 'Polygon', 'POLYGON((0 0, 2 0, 2 1, 3 1, 3 2, 1 2, 1 1, 0 1, 0 0))'),"
        "('Z', '255,0,0,255', 'Polygon', 'POLYGON((1 0, 3 0, 3 1, 2 1, 2 2, 0 2, 0 1, 1 1, 1 0))');";
    

    PGresult *res = PQexec(conn, query);
    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
        fprintf(stderr, "Insert failed (tetrominoes): %s\n", PQerrorMessage(conn));
        PQclear(res);
        PQfinish(conn);
        exit(EXIT_FAILURE);
    }
    PQclear(res);
    fprintf(stderr, "Insert successfully populated(tetrominoes)\n");

}