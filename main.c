#include "db_utils.h"

int main() {
    PGconn *conn = connect_db();

    drop_table_puzzles(conn);
    drop_table_solutions(conn);
    drop_table_tetrominoes(conn);

    create_tetrominoes_table(conn);
    populate_tetrominoes(conn);


    create_puzzles_table(conn);
    populate_puzzles(conn);

    create_solutions_table(conn);
    populate_solutions(conn);

    PQfinish(conn);
    return 0;
}
