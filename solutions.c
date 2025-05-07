#include <stdio.h>
#include <stdlib.h>
#include "db_utils.h"

void drop_table_solutions(PGconn *conn) {
    const char *query = 
    "DROP TABLE IF EXISTS solutions;";

    PGresult *res = PQexec(conn, query);
    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
        fprintf(stderr, "Drop table failed (solutions): %s\n", PQerrorMessage(conn));
        PQclear(res);
        PQfinish(conn);
        exit(EXIT_FAILURE);
    }

    PQclear(res);   
    fprintf(stderr, "Drop table successfully (solutions)\n");

}


void create_solutions_table(PGconn *conn) {
    const char *query = 
        "CREATE TABLE IF NOT EXISTS solutions ("
        "name VARCHAR(50),"
        "board GEOMETRY,"
        "type VARCHAR(50),"
        "tetro GEOMETRY" 
        ");";

    
    PGresult *res = PQexec(conn, query);
    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
        fprintf(stderr, "Table creation failed (solutions): %s\n", PQerrorMessage(conn));
        PQclear(res);
        PQfinish(conn);
        exit(EXIT_FAILURE);
    }
    PQclear(res);   
    fprintf(stderr, "Create successfully (solutions)\n");
     
}

  
void populate_solutions(PGconn *conn) {
    const char *query =
    "WITH board1 AS (SELECT geom AS board_geom FROM Puzzles WHERE name = 'Board1'), "
    "     board2 AS (SELECT geom AS board_geom FROM Puzzles WHERE name = 'Board2') "
    "INSERT INTO Solutions (name, board, type, tetro) "
    "SELECT 'Solution2', b2.board_geom, 'MultiPolygon', ST_Translate(ST_Rotate(t.geom, radians(90)), 6, 1) "
    "FROM Tetrominoes t, board2 b2 WHERE t.name = 'I' "
    "UNION ALL "
    "SELECT 'Solution2', b2.board_geom, 'MultiPolygon', ST_Translate(t.geom, 2, 0) "
    "FROM Tetrominoes t, board2 b2 WHERE t.name = 'J' "
    "UNION ALL "
    "SELECT 'Solution2', b2.board_geom, 'MultiPolygon', ST_Translate(t.geom, 0, 0) "
    "FROM Tetrominoes t, board2 b2 WHERE t.name = 'S' "
    "UNION ALL "
    "SELECT 'Solution2', b2.board_geom, 'MultiPolygon', ST_Translate(t.geom, 0, 3) "
    "FROM Tetrominoes t, board2 b2 WHERE t.name = 'Z' "
    "UNION ALL "
    "SELECT 'Solution2', b2.board_geom, 'MultiPolygon', ST_Translate(ST_Rotate(t.geom, radians(90)), 2, 1) "
    "FROM Tetrominoes t, board2 b2 WHERE t.name = 'T' "
    "UNION ALL "
    "SELECT 'Solution2', b2.board_geom, 'MultiPolygon', ST_Translate(t.geom, 4, 0) "
    "FROM Tetrominoes t, board2 b2 WHERE t.name = 'L' "
    "UNION ALL "
    "SELECT 'Solution2', b2.board_geom, 'MultiPolygon', ST_Translate(t.geom, 3, 3) "
    "FROM Tetrominoes t, board2 b2 WHERE t.name = 'O'"

    "UNION ALL "

    "SELECT 'Solution1', b1.board_geom, 'MultiPolygon', ST_Translate(ST_Rotate(t.geom, radians(180)), 6, 5) "
    "FROM Tetrominoes t, board1 b1 WHERE t.name = 'I' "
    "UNION ALL "
    "SELECT 'Solution1', b1.board_geom, 'MultiPolygon', ST_Translate(t.geom, 4, 0) "
    "FROM Tetrominoes t, board1 b1 WHERE t.name = 'J' "
    "UNION ALL "
    "SELECT 'Solution1', b1.board_geom, 'MultiPolygon', ST_Translate(ST_Rotate(t.geom, radians(90)), 3, 0) "
    "FROM Tetrominoes t, board1 b1 WHERE t.name = 'S' "
    "UNION ALL "
    "SELECT 'Solution1', b1.board_geom, 'MultiPolygon', ST_Translate(ST_Rotate(t.geom, radians(90)), 5, 0) "
    "FROM Tetrominoes t, board1 b1 WHERE t.name = 'Z' "
    "UNION ALL "
    "SELECT 'Solution1', b1.board_geom, 'MultiPolygon', ST_Translate(t.geom, 2, 2) "
    "FROM Tetrominoes t, board1 b1 WHERE t.name = 'T' "
    "UNION ALL "
    "SELECT 'Solution1', b1.board_geom, 'MultiPolygon', t.geom "
    "FROM Tetrominoes t, board1 b1 WHERE t.name = 'L' "
    "UNION ALL "
    "SELECT 'Solution1', b1.board_geom, 'MultiPolygon', ST_Translate(t.geom, 0, 3) "
    "FROM Tetrominoes t, board1 b1 WHERE t.name = 'O';";  
    PGresult *res = PQexec(conn, query);

    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
        fprintf(stderr, "Insert with CTE failed: %s\n", PQerrorMessage(conn));
        PQclear(res);
        PQfinish(conn);
        exit(EXIT_FAILURE);
    }

    PQclear(res);
    fprintf(stderr, "Solutions inserted using CTE successfully.\n");
}
