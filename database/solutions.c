#include <stdio.h>
#include <stdlib.h>
#include "../db_utils.h"

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
        "tName VARCHAR(50),"
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
    "     board2 AS (SELECT geom AS board_geom FROM Puzzles WHERE name = 'Board2'), "
    "     board3 AS (SELECT geom AS board_geom FROM Puzzles WHERE name = 'Board3'), "
    "     board4 AS (SELECT geom AS board_geom FROM Puzzles WHERE name = 'Board4'), "
    "     board5 AS (SELECT geom AS board_geom FROM Puzzles WHERE name = 'Board5'), "
    "     board6 AS (SELECT geom AS board_geom FROM Puzzles WHERE name = 'Board6'), "
    "     board7 AS (SELECT geom AS board_geom FROM Puzzles WHERE name = 'Board7') "

    "INSERT INTO Solutions (name, board, type, tName,tetro) "
    "SELECT 'Solution2', b2.board_geom, 'MultiPolygon', t.name,ST_Translate(ST_Rotate(t.geom, radians(90)), 6, 1) "
    "FROM Tetrominoes t, board2 b2 WHERE t.name = 'I' "
    "UNION ALL "
    "SELECT 'Solution2', b2.board_geom, 'MultiPolygon', t.name, ST_Translate(t.geom, 2, 0) "
    "FROM Tetrominoes t, board2 b2 WHERE t.name = 'J' "
    "UNION ALL "
    "SELECT 'Solution2', b2.board_geom, 'MultiPolygon',  t.name, ST_Translate(t.geom, 0, 0) "
    "FROM Tetrominoes t, board2 b2 WHERE t.name = 'S' "
    "UNION ALL "
    "SELECT 'Solution2', b2.board_geom, 'MultiPolygon',  t.name,ST_Translate(t.geom, 0, 3) "
    "FROM Tetrominoes t, board2 b2 WHERE t.name = 'Z' "
    "UNION ALL "
    "SELECT 'Solution2', b2.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(90)), 2, 1) "
    "FROM Tetrominoes t, board2 b2 WHERE t.name = 'T' "
    "UNION ALL "
    "SELECT 'Solution2', b2.board_geom, 'MultiPolygon',  t.name,ST_Translate(t.geom, 4, 0) "
    "FROM Tetrominoes t, board2 b2 WHERE t.name = 'L' "
    "UNION ALL "
    "SELECT 'Solution2', b2.board_geom, 'MultiPolygon',  t.name,ST_Translate(t.geom, 3, 3) "
    "FROM Tetrominoes t, board2 b2 WHERE t.name = 'O'"

    "UNION ALL "

    "SELECT 'Solution1', b1.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(180)), 6, 5) "
    "FROM Tetrominoes t, board1 b1 WHERE t.name = 'I' "
    "UNION ALL "
    "SELECT 'Solution1', b1.board_geom, 'MultiPolygon',  t.name,ST_Translate(t.geom, 4, 0) "
    "FROM Tetrominoes t, board1 b1 WHERE t.name = 'J' "
    "UNION ALL "
    "SELECT 'Solution1', b1.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(90)), 3, 0) "
    "FROM Tetrominoes t, board1 b1 WHERE t.name = 'S' "
    "UNION ALL "
    "SELECT 'Solution1', b1.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(90)), 5, 0) "
    "FROM Tetrominoes t, board1 b1 WHERE t.name = 'Z' "
    "UNION ALL "
    "SELECT 'Solution1', b1.board_geom, 'MultiPolygon',  t.name,ST_Translate(t.geom, 2, 2) "
    "FROM Tetrominoes t, board1 b1 WHERE t.name = 'T' "
    "UNION ALL "
    "SELECT 'Solution1', b1.board_geom, 'MultiPolygon',  t.name,t.geom "
    "FROM Tetrominoes t, board1 b1 WHERE t.name = 'L' "
    "UNION ALL "
    "SELECT 'Solution1', b1.board_geom, 'MultiPolygon',  t.name,ST_Translate(t.geom, 0, 3) "
    "FROM Tetrominoes t, board1 b1 WHERE t.name = 'O'"

    "UNION ALL "

    "SELECT 'Solution3', b3.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(180)), 6, 5) "
    "FROM Tetrominoes t, board3 b3 WHERE t.name = 'I' "
    "UNION ALL "
    "SELECT 'Solution3', b3.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(90)), 3, 1)"
    "FROM Tetrominoes t, board3 b3 WHERE t.name = 'J' "
    "UNION ALL "
    "SELECT 'Solution3', b3.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(0)), 3, 1) "
    "FROM Tetrominoes t, board3 b3 WHERE t.name = 'S' "
    "UNION ALL "
    "SELECT 'Solution3', b3.board_geom, 'MultiPolygon',  t.name,ST_Translate(t.geom, 0, 0) "
    "FROM Tetrominoes t, board3 b3 WHERE t.name = 'Z' "
    "UNION ALL "
    "SELECT 'Solution3', b3.board_geom, 'MultiPolygon',  t.name,ST_Translate(t.geom, 2, 2) "
    "FROM Tetrominoes t, board3 b3 WHERE t.name = 'T' "
    "UNION ALL "
    "SELECT 'Solution3', b3.board_geom, 'MultiPolygon',  t.name, ST_Translate(ST_Rotate(t.geom, radians(90)), 6, 0) "
    "FROM Tetrominoes t, board3 b3 WHERE t.name = 'L' "
    "UNION ALL "
    "SELECT 'Solution3', b3.board_geom, 'MultiPolygon',  t.name,ST_Translate(t.geom, 0, 3) "
    "FROM Tetrominoes t, board3 b3 WHERE t.name = 'O'"

    "UNION ALL "

    "SELECT 'Solution4', b4.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(90)), 6, 1) "
    "FROM Tetrominoes t, board4 b4 WHERE t.name = 'I' "
    "UNION ALL "
    "SELECT 'Solution4', b4.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(-90)), 0, 2)"
    "FROM Tetrominoes t, board4 b4 WHERE t.name = 'J' "
    "UNION ALL "
    "SELECT 'Solution4', b4.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(0)), 2, 2) "
    "FROM Tetrominoes t, board4 b4 WHERE t.name = 'S' "
    "UNION ALL "
    "SELECT 'Solution4', b4.board_geom, 'MultiPolygon',  t.name,ST_Translate(t.geom, 0, 1) "
    "FROM Tetrominoes t, board4 b4 WHERE t.name = 'Z' "
    "UNION ALL "
    "SELECT 'Solution4', b4.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(180)), 6, 2) "
    "FROM Tetrominoes t, board4 b4 WHERE t.name = 'T' "
    "UNION ALL "
    "SELECT 'Solution4', b4.board_geom, 'MultiPolygon',  t.name, ST_Translate(ST_Rotate(t.geom, radians(270)), 2, 5) "
    "FROM Tetrominoes t, board4 b4 WHERE t.name = 'L' "
    "UNION ALL "
    "SELECT 'Solution4', b4.board_geom, 'MultiPolygon',  t.name,ST_Translate(t.geom, 0, 3) "
    "FROM Tetrominoes t, board4 b4 WHERE t.name = 'O'"

    "UNION ALL "
  
    "SELECT 'Solution5', b5.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(0)), 0, 0) "
    "FROM Tetrominoes t, board5 b5 WHERE t.name = 'I' "
    "UNION ALL "
    "SELECT 'Solution5', b5.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(0)), 4, 0)"
    "FROM Tetrominoes t, board5 b5 WHERE t.name = 'J' "
    "UNION ALL "
    "SELECT 'Solution5', b5.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(90)), 5, 1) "
    "FROM Tetrominoes t, board5 b5 WHERE t.name = 'S' "
    "UNION ALL "
    "SELECT 'Solution5', b5.board_geom, 'MultiPolygon',  t.name,ST_Translate(t.geom, 1, 1) "
    "FROM Tetrominoes t, board5 b5 WHERE t.name = 'Z' "
    "UNION ALL "
    "SELECT 'Solution5', b5.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(0)), 0, 3) "
    "FROM Tetrominoes t, board5 b5 WHERE t.name = 'T' "
    "UNION ALL "
    "SELECT 'Solution5', b5.board_geom, 'MultiPolygon',  t.name, ST_Translate(ST_Rotate(t.geom, radians(0)), 0, 1) "
    "FROM Tetrominoes t, board5 b5 WHERE t.name = 'L' "
    "UNION ALL "
    "SELECT 'Solution5', b5.board_geom, 'MultiPolygon',  t.name,ST_Translate(t.geom, 4, 3) "
    "FROM Tetrominoes t, board5 b5 WHERE t.name = 'O'"  

    "UNION ALL "

    "SELECT 'Solution6', b6.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(0)), 1, 4) "
    "FROM Tetrominoes t, board6 b6 WHERE t.name = 'I' "
    "UNION ALL "
    "SELECT 'Solution6', b6.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(0)), 4, 2)"
    "FROM Tetrominoes t, board6 b6 WHERE t.name = 'J' "
    "UNION ALL "
    "SELECT 'Solution6', b6.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(0)), 2, 2) "
    "FROM Tetrominoes t, board6 b6 WHERE t.name = 'S' "
    "UNION ALL "
    "SELECT 'Solution6', b6.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(90)), 2, 0)"
    "FROM Tetrominoes t, board6 b6 WHERE t.name = 'Z' "
    "UNION ALL "
    "SELECT 'Solution6', b6.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(90)), 2, 2) "
    "FROM Tetrominoes t, board6 b6 WHERE t.name = 'T' "
    "UNION ALL "
    "SELECT 'Solution6', b6.board_geom, 'MultiPolygon',  t.name, ST_Translate(ST_Rotate(t.geom, radians(90)), 4, 0) "
    "FROM Tetrominoes t, board6 b6 WHERE t.name = 'L' "
    "UNION ALL "
    "SELECT 'Solution6', b6.board_geom, 'MultiPolygon',  t.name,ST_Translate(t.geom, 4, 0) "
    "FROM Tetrominoes t, board6 b6 WHERE t.name = 'O'"

    "UNION ALL "

    "SELECT 'Solution7', b7.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(0)), 2, 0) "
    "FROM Tetrominoes t, board7 b7 WHERE t.name = 'I' "
    "UNION ALL "
    "SELECT 'Solution7', b7.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(90)), 6, 3)"
    "FROM Tetrominoes t, board7 b7 WHERE t.name = 'J' "
    "UNION ALL "
    "SELECT 'Solution7', b7.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(0)), 0, 0) "
    "FROM Tetrominoes t, board7 b7 WHERE t.name = 'S' "
    "UNION ALL "
    "SELECT 'Solution7', b7.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(0)), 0, 3)"
    "FROM Tetrominoes t, board7 b7 WHERE t.name = 'Z' "
    "UNION ALL "
    "SELECT 'Solution7', b7.board_geom, 'MultiPolygon',  t.name,ST_Translate(ST_Rotate(t.geom, radians(90)), 2, 1) "
    "FROM Tetrominoes t, board7 b7 WHERE t.name = 'T' "
    "UNION ALL "
    "SELECT 'Solution7', b7.board_geom, 'MultiPolygon',  t.name, ST_Translate(ST_Rotate(t.geom, radians(90)), 6, 1) "
    "FROM Tetrominoes t, board7 b7 WHERE t.name = 'L' "
    "UNION ALL "
    "SELECT 'Solution7', b7.board_geom, 'MultiPolygon',  t.name,ST_Translate(t.geom, 3, 2) "
    "FROM Tetrominoes t, board7 b7 WHERE t.name = 'O';";  


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
