#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../db_utils.h"

#define MAX_QUERY_SIZE 32768 
#define MAX_PART_SIZE 512

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

typedef struct {
    const char *name;
    const char *rotation;
    int dx;
    int dy;
} TetrominoPlacement;

typedef struct {
    const char *solution_name;
    const char *board_alias;
    const char *board_cte;
    TetrominoPlacement pieces[7];
} SolutionPlan;

void populate_solutions(PGconn *conn) {
    char query[MAX_QUERY_SIZE];
    char temp[MAX_PART_SIZE];
    query[0] = '\0';  // initialize

    strcat(query,
        "WITH board1 AS (SELECT geom AS board_geom FROM Puzzles WHERE name = 'Board1'), "
        "     board2 AS (SELECT geom AS board_geom FROM Puzzles WHERE name = 'Board2'), "
        "     board3 AS (SELECT geom AS board_geom FROM Puzzles WHERE name = 'Board3'), "
        "     board4 AS (SELECT geom AS board_geom FROM Puzzles WHERE name = 'Board4'), "
        "     board5 AS (SELECT geom AS board_geom FROM Puzzles WHERE name = 'Board5'), "
        "     board6 AS (SELECT geom AS board_geom FROM Puzzles WHERE name = 'Board6'), "
        "     board7 AS (SELECT geom AS board_geom FROM Puzzles WHERE name = 'Board7') " 
    );

    SolutionPlan solutions[] = {
        {
            "Solution1", "b1", "board1", {
                {"I", "180", 6, 5},
                {"J", "0",   4, 0},
                {"S", "90",  3, 0},
                {"Z", "90",  5, 0},
                {"T", "0",   2, 2},
                {"L", "0",   0, 0},
                {"O", "0",   0, 3}
            }
        },
        {
            "Solution2", "b2", "board2", {
                {"I", "90", 6, 1},
                {"J", "0",  2, 0},
                {"S", "0",  0, 0},
                {"Z", "0",  0, 3},
                {"T", "90", 2, 1},
                {"L", "0",  4, 0},
                {"O", "0",  3, 3}
            }
        },
        {
        "Solution3", "b3", "board3", {
            {"I", "180", 6, 5},
            {"J", "90",  3, 1},
            {"S", "0",   3, 1},
            {"Z", "0",   0, 0},
            {"T", "0",   2, 2},
            {"L", "90",  6, 0},
            {"O", "0",   0, 3}
        }
        },
        {
            "Solution4", "b4", "board4", {
                {"I", "90",   6, 1},
                {"J", "-90",  0, 2},
                {"S", "0",    2, 2},
                {"Z", "0",    0, 1},
                {"T", "180",  6, 2},
                {"L", "270",  2, 5},
                {"O", "0",    0, 3}
            }
        },
        {
            "Solution5", "b5", "board5", {
                {"I", "0",  0, 0},
                {"J", "0",  4, 0},
                {"S", "90", 5, 1},
                {"Z", "0",  1, 1},
                {"T", "0",  0, 3},
                {"L", "0",  0, 1},
                {"O", "0",  4, 3}
            }
        },
        {
            "Solution6", "b6", "board6", {
                {"I", "0",  1, 4},
                {"J", "0",  4, 2},
                {"S", "0",  2, 2},
                {"Z", "90", 2, 0},
                {"T", "90", 2, 2},
                {"L", "90", 4, 0},
                {"O", "0",  4, 0}
            }
        },
        {
            "Solution7", "b7", "board7", {
                {"I", "0",  2, 0},
                {"J", "90", 6, 3},
                {"S", "0",  0, 0},
                {"Z", "0",  0, 3},
                {"T", "90", 2, 1},
                {"L", "90", 6, 1},
                {"O", "0",  3, 2}
            }
        }
    };

    strcat(query, " INSERT INTO Solutions (name, board, type, tName, tetro) ");

    for (int i = 0; i < sizeof(solutions) / sizeof(solutions[0]); i++) {
        SolutionPlan s = solutions[i];
        for (int j = 0; j < 7; j++) {
            TetrominoPlacement p = s.pieces[j];
            snprintf(temp, sizeof(temp),
                "SELECT '%s', %s.board_geom, 'MultiPolygon', t.name, "
                "ST_Translate(ST_Rotate(t.geom, %s), %d, %d) "
                "FROM Tetrominoes t, %s %s WHERE t.name = '%s' ",
                s.solution_name, s.board_alias, p.rotation, p.dx, p.dy,
                s.board_cte, s.board_alias, p.name
            );

            strcat(query, temp);

            // Add UNION ALL if not the last one
            if (!(i == sizeof(solutions)/sizeof(solutions[0]) - 1 && j == 6)) {
                strcat(query, "UNION ALL ");
            }
        }
    }

    strcat(query, ";");

    PGresult *res = PQexec(conn, query);
    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
        fprintf(stderr, "Insert with dynamic SQL failed: %s\n", PQerrorMessage(conn));
        PQclear(res);
        PQfinish(conn);
        exit(EXIT_FAILURE);
    }

    PQclear(res);
    fprintf(stderr, "Solutions inserted dynamically.\n");
}
