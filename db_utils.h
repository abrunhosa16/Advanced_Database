#ifndef DB_UTILS_H
#define DB_UTILS_H

#include <libpq-fe.h>

PGconn* connect_db();
void handle_error(PGconn *conn);

void drop_table_puzzles(PGconn *conn);
void drop_table_tetrominoes(PGconn *conn);
void drop_table_solutions(PGconn *conn);



void create_tetrominoes_table(PGconn *conn);
void populate_tetrominoes(PGconn *conn);

void create_puzzles_table(PGconn *conn);
void populate_puzzles(PGconn *conn);

void create_solutions_table(PGconn *conn);
void populate_solutions(PGconn * conn);

#endif
