:- module(tetris_solver, [test_db/0, test_geometries/0, test_yap_translate/0, test_yap_rotate/0, test_yap_difference/0, test_yap_fit/0]).
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