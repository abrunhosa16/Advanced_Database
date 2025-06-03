:- module(tetris_solver, [
    test_db/0, 
    test_geometries/0, 
    test_yap_translate/0, 
    test_yap_rotate/0, 
    test_yap_difference/0, 
    test_yap_fit/0,
    get_puzzle/2,
    get_tetrominoes/1,
    try_place_piece/4,
    test_place_piece/0,
    calculate_remaining_space/3,
    try_place_piece_with_rotation/5
]).
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

% Test predicates using yap2pgsql functions
test_yap_translate :-
    write('Testing yap2pgsql translation of tetromino I...'), nl,
    db_import('SELECT ST_AsText(geom) as geom FROM tetrominoes WHERE name = \'I\'', [], [row(IGeom)]),
    write('Original I geometry: '), write(IGeom), nl,
    st_translate(IGeom, 1, 1, TranslatedGeom),
    write('Translated I geometry (dx=1, dy=1): '), write(TranslatedGeom), nl.

test_yap_rotate :-
    write('Testing yap2pgsql rotation of tetromino I...'), nl,
    db_import('SELECT ST_AsText(geom) as geom FROM tetrominoes WHERE name = \'I\'', [], [row(IGeom)]),
    write('Original I geometry: '), write(IGeom), nl,
    st_rotate(IGeom, 90, RotatedGeom),
    write('Rotated I geometry (90 degrees): '), write(RotatedGeom), nl.

test_yap_difference :-
    write('Testing yap2pgsql difference operation...'), nl,
    db_import('SELECT ST_AsText(geom) as geom FROM puzzles WHERE name = \'Board1\'', [], [row(BoardGeom)]),
    write('Board1 geometry: '), write(BoardGeom), nl,
    db_import('SELECT ST_AsText(geom) as geom FROM tetrominoes WHERE name = \'I\'', [], [row(IGeom)]),
    write('I geometry: '), write(IGeom), nl,
    st_difference(BoardGeom, IGeom, DiffGeom),
    write('Difference geometry: '), write(DiffGeom), nl.

test_yap_fit :-
    write('Testing yap2pgsql if tetromino I fits in Board1...'), nl,
    db_import('SELECT ST_AsText(geom) as geom FROM puzzles WHERE name = \'Board1\'', [], [row(BoardGeom)]),
    write('Board1 geometry: '), write(BoardGeom), nl,
    db_import('SELECT ST_AsText(geom) as geom FROM tetrominoes WHERE name = \'I\'', [], [row(IGeom)]),
    write('I geometry: '), write(IGeom), nl,
    st_translate(IGeom, 1, 1, TranslatedGeom),
    write('Translated I geometry: '), write(TranslatedGeom), nl,
    st_contains(BoardGeom, TranslatedGeom, Fits),
    write('Fits: '), write(Fits), nl.

% Helper predicates for geometry testing
write_geometries([]).
write_geometries([row(Name, Geom)|Rest]) :-
    write(Name), write(': '), write(Geom), nl,
    write_geometries(Rest).

% Get puzzle geometry from database
get_puzzle(PuzzleName, Puzzle) :-
    db_import('SELECT ST_AsText(geom) as geom FROM puzzles WHERE name = $1', [PuzzleName], [row(Puzzle)]).

% Get all tetrominoes from database
get_tetrominoes(Tetrominoes) :-
    db_import('SELECT name, ST_AsText(geom) as geom FROM tetrominoes', [], Tetrominoes).

% Try to place a piece at a specific position
try_place_piece(Puzzle, Piece, Dx, Dy) :-
    write('Trying to place piece at ('), write(Dx), write(','), write(Dy), write(')'), nl,
    st_translate(Piece, Dx, Dy, TranslatedPiece),
    write('Translated piece: '), write(TranslatedPiece), nl,
    st_contains(Puzzle, TranslatedPiece, Fits),
    write('Fits: '), write(Fits), nl,
    Fits = t.  % PostgreSQL returns 't' for true

% Calculate remaining space after placing a piece
calculate_remaining_space(Puzzle, Piece, RemainingSpace) :-
    st_difference(Puzzle, Piece, RemainingSpace).

% Try to place a piece with rotation
try_place_piece_with_rotation(Puzzle, Piece, Rotation, Dx, Dy) :-
    write('Trying rotation '), write(Rotation), write(' degrees...'), nl,
    st_rotate(Piece, Rotation, RotatedPiece),
    write('Rotated piece: '), write(RotatedPiece), nl,
    try_place_piece(Puzzle, RotatedPiece, Dx, Dy).

% Test placing piece with rotation
test_place_piece :-
    write('Testing piece placement with rotation...'), nl,
    get_puzzle('Board1', Puzzle),
    write('Got puzzle: '), write(Puzzle), nl,
    db_import('SELECT ST_AsText(geom) as geom FROM tetrominoes WHERE name = \'I\'', [], [row(IGeom)]),
    write('Got piece I: '), write(IGeom), nl,
    try_place_piece_with_rotation(Puzzle, IGeom, 90, 1, 1),
    st_rotate(IGeom, 90, RotatedPiece),
    st_translate(RotatedPiece, 1, 1, TranslatedPiece),
    calculate_remaining_space(Puzzle, TranslatedPiece, RemainingSpace),
    write('Remaining space: '), write(RemainingSpace), nl. 