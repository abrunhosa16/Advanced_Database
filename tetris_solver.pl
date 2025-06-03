:- module(tetris_solver, [test_db/0, test_geometries/0, test_translate/0, test_rotate/0, test_difference/0, test_fit/0]).
:- use_module(yap2pgsql).
:- use_module(library(lists)).

% Simple test predicate to verify database connection
test_db :-
    write('Testing database connection...'), nl,
    db_import('SELECT current_database()', [], [DB]),
    write('Connected to database: '), write(DB), nl,
    write('Testing Puzzles table...'), nl,
    db_import('SELECT name FROM puzzles', [], Puzzles),
    write('Puzzles found: '), write(Puzzles), nl,
    write('Testing Tetrominoes table...'), nl,
    db_import('SELECT name FROM tetrominoes', [], Tetrominoes),
    write('Tetrominoes found: '), write(Tetrominoes), nl.

% Test predicate to verify geometries
test_geometries :-
    write('Testing puzzle geometries...'), nl,
    db_import('SELECT name, ST_AsText(geom) as geom FROM puzzles', [], PuzzleGeoms),
    write('Puzzle geometries: '), nl,
    write_geometries(PuzzleGeoms),
    nl,
    write('Testing tetromino geometries...'), nl,
    db_import('SELECT name, ST_AsText(geom) as geom FROM tetrominoes', [], TetrominoGeoms),
    write('Tetromino geometries: '), nl,
    write_geometries(TetrominoGeoms).

% Test predicate for translation
test_translate :-
    write('Testing translation of tetromino I...'), nl,
    db_import('SELECT ST_AsText(geom) as geom FROM tetrominoes WHERE name = \'I\'', [], [row(IGeom)]),
    write('Original I geometry: '), write(IGeom), nl,
    atomic_concat('WITH tetromino AS (SELECT ST_GeomFromText(\'', IGeom, T1),
    atomic_concat(T1, '\') as geom) SELECT ST_AsText(ST_Translate(geom, 1, 1)) FROM tetromino', Query),
    db_import(Query, [], [row(TranslatedGeom)]),
    write('Translated I geometry (dx=1, dy=1): '), write(TranslatedGeom), nl.

% Test predicate for rotation
test_rotate :-
    write('Testing rotation of tetromino I...'), nl,
    db_import('SELECT ST_AsText(geom) as geom FROM tetrominoes WHERE name = \'I\'', [], [row(IGeom)]),
    write('Original I geometry: '), write(IGeom), nl,
    st_rotate_for_solver(IGeom, 90, RotatedGeom),
    write('Rotated I geometry (90 degrees): '), write(RotatedGeom), nl.

% Test predicate for difference
test_difference :-
    write('Testing difference operation...'), nl,
    db_import('SELECT ST_AsText(geom) as geom FROM puzzles WHERE name = \'Board1\'', [], [row(BoardGeom)]),
    write('Board1 geometry: '), write(BoardGeom), nl,
    db_import('SELECT ST_AsText(geom) as geom FROM tetrominoes WHERE name = \'I\'', [], [row(IGeom)]),
    write('I geometry: '), write(IGeom), nl,
    st_difference_for_solver(BoardGeom, IGeom, DiffGeom),
    write('Difference geometry: '), write(DiffGeom), nl.

% Test predicate for fit check
test_fit :-
    write('Testing if tetromino I fits in Board1...'), nl,
    db_import('SELECT ST_AsText(geom) as geom FROM puzzles WHERE name = \'Board1\'', [], [row(BoardGeom)]),
    write('Board1 geometry: '), write(BoardGeom), nl,
    db_import('SELECT ST_AsText(geom) as geom FROM tetrominoes WHERE name = \'I\'', [], [row(IGeom)]),
    write('I geometry: '), write(IGeom), nl,
    st_translate_for_solver(IGeom, 1, 1, TranslatedGeom),
    write('Translated I geometry: '), write(TranslatedGeom), nl,
    check_fit_for_solver(BoardGeom, TranslatedGeom, Fits),
    write('Fits: '), write(Fits), nl.

% Spatial operation predicates
st_rotate_for_solver(Geom, Angle, Result) :-
    atomic_concat('\'', Geom, G1),
    atomic_concat(G1, '\'', G1Quote),
    atomic_concat('SELECT ST_AsText(ST_MakeValid(ST_SnapToGrid(ST_Rotate(ST_GeomFromText(', G1Quote, Q1),
    atomic_concat(Q1, '), radians(', Q2),
    number_atom(Angle, AngleAtom),
    atomic_concat(Q2, AngleAtom, Q3),
    atomic_concat(Q3, '), ST_MakePoint(0,0)), 0.0001)))', Query),
    write('Debug - Query: '), write(Query), nl,
    db_import(Query, [], [row(Result)]).

st_difference_for_solver(Geom1, Geom2, Result) :-
    atomic_concat('\'', Geom1, G1),
    atomic_concat(G1, '\'', G1Quote),
    atomic_concat('\'', Geom2, G2),
    atomic_concat(G2, '\'', G2Quote),
    atomic_concat('SELECT ST_AsText(ST_Difference(ST_GeomFromText(', G1Quote, Q1),
    atomic_concat(Q1, '), ST_GeomFromText(', Q2),
    atomic_concat(Q2, G2Quote, Q3),
    atomic_concat(Q3, ')))', Query),
    write('Debug - Query: '), write(Query), nl,
    db_import(Query, [], [row(Result)]).

st_translate_for_solver(Geom, X, Y, Result) :-
    atomic_concat('\'', Geom, G1),
    atomic_concat(G1, '\'', G1Quote),
    atomic_concat('SELECT ST_AsText(ST_Translate(ST_GeomFromText(', G1Quote, Q1),
    atomic_concat(Q1, '), ', Q2),
    number_atom(X, XAtom),
    atomic_concat(Q2, XAtom, Q3),
    atomic_concat(Q3, ', ', Q4),
    number_atom(Y, YAtom),
    atomic_concat(Q4, YAtom, Q5),
    atomic_concat(Q5, '))', Query),
    db_import(Query, [], [row(Result)]).

check_fit_for_solver(Geom1, Geom2, Result) :-
    atomic_concat('\'', Geom1, G1),
    atomic_concat(G1, '\'', G1Quote),
    atomic_concat('\'', Geom2, G2),
    atomic_concat(G2, '\'', G2Quote),
    atomic_concat('SELECT ST_Contains(ST_GeomFromText(', G1Quote, Q1),
    atomic_concat(Q1, '), ST_GeomFromText(', Q2),
    atomic_concat(Q2, G2Quote, Q3),
    atomic_concat(Q3, '))', Query),
    write('Debug - Query: '), write(Query), nl,
    db_import(Query, [], [row(Result)]).

% Helper predicates for geometry testing
write_geometries([]).
write_geometries([row(Name, Geom)|Rest]) :-
    write(Name), write(': '), write(Geom), nl,
    write_geometries(Rest).

% Commented out original code
/*
:- use_module(library(lists)).

% Helper predicate to extract geometry from row format
extract_geometry(row(_, Geom), Geom) :-
    write('Debug: Extracting geometry from row: '), write(Geom), nl.

% Debug predicate to print geometry
debug_print(Label, Geom) :-
    write(Label), write(': '), write(Geom), nl.

% Database verification
verify_database :-
    write('Debug: Starting database verification...'), nl,
    write('Checking if Puzzles table exists...'), nl,
    db_import('SELECT table_name FROM information_schema.tables WHERE table_schema = \'public\'', [], Tables),
    write('Debug: Available tables: '), write(Tables), nl,
    
    write('Debug: Checking database connection...'), nl,
    db_import('SELECT current_database()', [], [DB]),
    write('Debug: Connected to database: '), write(DB), nl,
    
    write('Checking Puzzles table structure...'), nl,
    db_import('SELECT column_name, data_type FROM information_schema.columns WHERE table_name = \'puzzles\'', [], PuzzleColumns),
    write('Debug: Puzzles table columns: '), write(PuzzleColumns), nl,
    
    write('Checking Tetrominoes table structure...'), nl,
    db_import('SELECT column_name, data_type FROM information_schema.columns WHERE table_name = \'tetrominoes\'', [], TetrominoColumns),
    write('Debug: Tetrominoes table columns: '), write(TetrominoColumns), nl,
    
    write('Checking Puzzles data...'), nl,
    db_import('SELECT name, ST_AsText(geom) as geom FROM Puzzles', [], PuzzleData),
    write('Debug: Puzzles data: '), write(PuzzleData), nl,
    
    write('Checking Tetrominoes data...'), nl,
    db_import('SELECT name, ST_AsText(geom) as geom FROM Tetrominoes', [], TetrominoData),
    write('Debug: Tetrominoes data: '), write(TetrominoData), nl.

% Main solving predicate
solve_puzzle(PuzzleName, Solution) :-
    write('Debug: Starting solve_puzzle for '), write(PuzzleName), nl,
    verify_database,
    get_puzzle(PuzzleName, InitialPuzzle),
    write('Debug: After get_puzzle, InitialPuzzle = '), write(InitialPuzzle), nl,
    (InitialPuzzle = [] -> 
        write('Error: Could not find puzzle '), write(PuzzleName), nl, fail
    ; write('Debug: Puzzle found successfully'), nl),
    debug_print('Initial Puzzle', InitialPuzzle),
    get_tetrominoes(Tetrominoes),
    write('Debug: After get_tetrominoes, Tetrominoes = '), write(Tetrominoes), nl,
    (Tetrominoes = [] ->
        write('Error: No tetrominoes found in database'), nl, fail
    ; write('Debug: Tetrominoes found successfully'), nl),
    debug_print('Tetrominoes', Tetrominoes),
    write('Starting backtracking...'), nl,
    solve_puzzle_backtrack(InitialPuzzle, Tetrominoes, [], Solution).

% Base case - all tetrominoes placed
solve_puzzle_backtrack(_, [], Solution, Solution) :-
    write('Debug: Base case reached - all tetrominoes placed'), nl.

% Recursive case - try to place next tetromino
solve_puzzle_backtrack(CurrentPuzzle, [Tetromino|Rest], PartialSolution, FinalSolution) :-
    write('Debug: Trying to place next tetromino...'), nl,
    debug_print('Trying tetromino', Tetromino),
    debug_print('Current puzzle', CurrentPuzzle),
    try_place_tetromino(CurrentPuzzle, Tetromino, NewPuzzle, Placement),
    write('Debug: After try_place_tetromino'), nl,
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
    write('Debug: Getting puzzle '), write(PuzzleName), nl,
    atomic_concat('SELECT ST_AsText(geom) FROM Puzzles WHERE name = \'', PuzzleName, N1),
    atomic_concat(N1, '\'', Query),
    write('Debug: Formatted query: '), write(Query), nl,
    db_import(Query, [], [Row]),
    write('Debug: Query result: '), write(Row), nl,
    extract_geometry(Row, Puzzle).

get_tetrominoes(Tetrominoes) :-
    write('Debug: Getting tetrominoes from database...'), nl,
    db_import('SELECT name, ST_AsText(geom) as geom FROM Tetrominoes', [], Rows),
    write('Debug: Raw tetrominoes: '), write(Rows), nl,
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
*/ 