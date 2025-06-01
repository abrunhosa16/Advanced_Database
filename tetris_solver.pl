:- module(tetris_solver, [solve_puzzle/2]).
:- use_module(library(lists)).

% Helper predicate to extract geometry from row format
extract_geometry(row(_, Geom), Geom).

% Debug predicate to print geometry
debug_print(Label, Geom) :-
    write(Label), write(': '), write(Geom), nl.

% Database verification
verify_database :-
    write('Verifying database structure...'), nl,
    write('Checking if Puzzles table exists...'), nl,
    db_import('SELECT table_name FROM information_schema.tables WHERE table_schema = \'public\'', [], Tables),
    write('Available tables: '), write(Tables), nl,
    
    write('Checking Puzzles table structure...'), nl,
    db_import('SELECT column_name, data_type FROM information_schema.columns WHERE table_name = \'puzzles\'', [], PuzzleColumns),
    write('Puzzles table columns: '), write(PuzzleColumns), nl,
    
    write('Checking Tetrominoes table structure...'), nl,
    db_import('SELECT column_name, data_type FROM information_schema.columns WHERE table_name = \'tetrominoes\'', [], TetrominoColumns),
    write('Tetrominoes table columns: '), write(TetrominoColumns), nl,
    
    write('Checking Puzzles data...'), nl,
    db_import('SELECT name, ST_AsText(geom) as geom FROM Puzzles', [], PuzzleData),
    write('Puzzles data: '), write(PuzzleData), nl,
    
    write('Checking Tetrominoes data...'), nl,
    db_import('SELECT name, ST_AsText(geom) as geom FROM Tetrominoes', [], TetrominoData),
    write('Tetrominoes data: '), write(TetrominoData), nl.

% Main solving predicate
solve_puzzle(PuzzleName, Solution) :-
    write('Starting solve_puzzle for '), write(PuzzleName), nl,
    verify_database,
    get_puzzle(PuzzleName, InitialPuzzle),
    (InitialPuzzle = [] -> 
        write('Error: Could not find puzzle '), write(PuzzleName), nl, fail
    ; true),
    debug_print('Initial Puzzle', InitialPuzzle),
    get_tetrominoes(Tetrominoes),
    (Tetrominoes = [] ->
        write('Error: No tetrominoes found in database'), nl, fail
    ; true),
    debug_print('Tetrominoes', Tetrominoes),
    write('Starting backtracking...'), nl,
    solve_puzzle_backtrack(InitialPuzzle, Tetrominoes, [], Solution).

% Base case - all tetrominoes placed
solve_puzzle_backtrack(_, [], Solution, Solution) :-
    write('All tetrominoes placed successfully!'), nl.

% Recursive case - try to place next tetromino
solve_puzzle_backtrack(CurrentPuzzle, [Tetromino|Rest], PartialSolution, FinalSolution) :-
    write('Trying to place next tetromino...'), nl,
    debug_print('Trying tetromino', Tetromino),
    debug_print('Current puzzle', CurrentPuzzle),
    try_place_tetromino(CurrentPuzzle, Tetromino, NewPuzzle, Placement),
    debug_print('Placement found', Placement),
    debug_print('New puzzle', NewPuzzle),
    solve_puzzle_backtrack(NewPuzzle, Rest, [Placement|PartialSolution], FinalSolution).

% Try to place a tetromino in different positions and rotations
try_place_tetromino(Puzzle, Tetromino, NewPuzzle, Placement) :-
    get_rotation(Rotation),
    debug_print('Trying rotation', Rotation),
    get_translation(Dx, Dy),
    debug_print('Trying translation', [Dx, Dy]),
    rotate_tetromino(Tetromino, Rotation, RotatedPiece),
    debug_print('Rotated piece', RotatedPiece),
    translate_tetromino(RotatedPiece, Dx, Dy, PlacedPiece),
    debug_print('Placed piece', PlacedPiece),
    check_fit(Puzzle, PlacedPiece),
    debug_print('Piece fits', true),
    check_contiguous_space(Puzzle, PlacedPiece),
    debug_print('Contiguous space', true),
    calculate_difference(Puzzle, PlacedPiece, NewPuzzle),
    debug_print('New puzzle after placement', NewPuzzle),
    Placement = [Tetromino, Rotation, Dx, Dy].

% Check if there is enough contiguous space for the piece
check_contiguous_space(Puzzle, Piece) :-
    atomic_concat('WITH piece_buffer AS (SELECT ST_Buffer(geom, 0.5) as buffered_piece FROM (SELECT ST_GeomFromText(\'', Piece, P1),
    atomic_concat(P1, '\') as geom) as piece), available_space AS (SELECT ST_Difference(geom, buffered_piece) as space FROM (SELECT ST_GeomFromText(\'', P2),
    atomic_concat(P2, Puzzle, P3),
    atomic_concat(P3, '\') as geom) as puzzle, piece_buffer) SELECT ST_Area(space) >= 4 FROM available_space WHERE ST_IsEmpty(space) = false', Query),
    write('Executing query: '), write(Query), nl,
    db_import(Query, [], [Row]),
    extract_geometry(Row, true).

% Database interaction predicates
get_puzzle(PuzzleName, Puzzle) :-
    atomic_concat('SELECT ST_AsText(geom) FROM Puzzles WHERE name = \'', PuzzleName, N1),
    atomic_concat(N1, '\'', Query),
    write('Executing query: '), write(Query), nl,
    db_import(Query, [], [Row]),
    extract_geometry(Row, Puzzle).

get_tetrominoes(Tetrominoes) :-
    write('Getting tetrominoes from database...'), nl,
    db_import('SELECT name, ST_AsText(geom) as geom FROM Tetrominoes', [], Rows),
    write('Raw tetrominoes: '), write(Rows), nl,
    maplist(extract_geometry, Rows, Tetrominoes).

% Geometric operations
rotate_tetromino(Tetromino, Rotation, Rotated) :-
    atomic_concat('WITH tetromino AS (SELECT ST_GeomFromText(\'', Tetromino, T1),
    atomic_concat(T1, '\') as geom) SELECT ST_AsText(ST_Rotate(geom, ', T2),
    atomic_concat(T2, Rotation, T3),
    atomic_concat(T3, ')) FROM tetromino', Query),
    write('Executing query: '), write(Query), nl,
    db_import(Query, [], [Row]),
    extract_geometry(Row, Rotated).

translate_tetromino(Tetromino, Dx, Dy, Translated) :-
    atomic_concat('WITH tetromino AS (SELECT ST_GeomFromText(\'', Tetromino, T1),
    atomic_concat(T1, '\') as geom) SELECT ST_AsText(ST_Translate(geom, ', T2),
    atomic_concat(T2, Dx, T3),
    atomic_concat(T3, ', ', T4),
    atomic_concat(T4, Dy, T5),
    atomic_concat(T5, ')) FROM tetromino', Query),
    write('Executing query: '), write(Query), nl,
    db_import(Query, [], [Row]),
    extract_geometry(Row, Translated).

calculate_difference(Puzzle, Piece, NewPuzzle) :-
    atomic_concat('WITH puzzle_geom AS (SELECT ST_GeomFromText(\'', Puzzle, P1),
    atomic_concat(P1, '\') as geom), piece_geom AS (SELECT ST_GeomFromText(\'', P2),
    atomic_concat(P2, Piece, P3),
    atomic_concat(P3, '\') as geom) SELECT ST_AsText(ST_Difference(puzzle_geom.geom, piece_geom.geom)) FROM puzzle_geom, piece_geom', Query),
    write('Executing query: '), write(Query), nl,
    db_import(Query, [], [Row]),
    extract_geometry(Row, NewPuzzle).

% Validation predicates
check_fit(Puzzle, Piece) :-
    atomic_concat('WITH puzzle_geom AS (SELECT ST_GeomFromText(\'', Puzzle, P1),
    atomic_concat(P1, '\') as geom), piece_geom AS (SELECT ST_GeomFromText(\'', P2),
    atomic_concat(P2, Piece, P3),
    atomic_concat(P3, '\') as geom) SELECT ST_Contains(puzzle_geom.geom, piece_geom.geom) FROM puzzle_geom, piece_geom', Query),
    write('Executing query: '), write(Query), nl,
    db_import(Query, [], [Row]),
    extract_geometry(Row, true).

check_no_overlap(Puzzle, Piece) :-
    atomic_concat('WITH puzzle_geom AS (SELECT ST_GeomFromText(\'', Puzzle, P1),
    atomic_concat(P1, '\') as geom), piece_geom AS (SELECT ST_GeomFromText(\'', P2),
    atomic_concat(P2, Piece, P3),
    atomic_concat(P3, '\') as geom) SELECT NOT ST_Intersects(puzzle_geom.geom, piece_geom.geom) FROM puzzle_geom, piece_geom', Query),
    write('Executing query: '), write(Query), nl,
    db_import(Query, [], [Row]),
    extract_geometry(Row, true).

% Helper predicates for generating possible moves
get_rotation(0).
get_rotation(90).
get_rotation(180).
get_rotation(270).

get_translation(Dx, Dy) :-
    between(0, 6, Dx),
    between(0, 6, Dy). 