:- consult('yap2pgsql.pl').

% Function to validate shape after transformation
validate_shape(Original, Transformed, Operation) :-
    write('Validating shape after '), write(Operation), write(':'), nl,
    write('Original shape: '), write(Original), nl,
    write('Transformed shape: '), write(Transformed), nl,
    % Check if number of vertices is the same
    count_vertices(Original, OriginalVertices),
    count_vertices(Transformed, TransformedVertices),
    write('Original vertices count: '), write(OriginalVertices), nl,
    write('Transformed vertices count: '), write(TransformedVertices), nl,
    (OriginalVertices =:= TransformedVertices ->
        write('✓ Vertex count maintained'), nl
    ;   write('✗ Vertex count different!'), nl
    ),
    % Check if area is maintained (for rotation and translation)
    (Operation = rotate ; Operation = translate),
    calculate_area(Original, OriginalArea),
    calculate_area(Transformed, TransformedArea),
    write('Original area: '), write(OriginalArea), nl,
    write('Transformed area: '), write(TransformedArea), nl,
    atom_number(OriginalArea, OriginalAreaNum),
    atom_number(TransformedArea, TransformedAreaNum),
    (abs(OriginalAreaNum - TransformedAreaNum) < 0.01 ->
        write('✓ Area maintained'), nl
    ;   write('✗ Area different!'), nl
    ).

% Function to count vertices by counting coordinate pairs
count_vertices(Polygon, Count) :-
    atom_codes(Polygon, Codes),
    count_coordinate_pairs(Codes, 0, Count).

count_coordinate_pairs([], Count, Count).
count_coordinate_pairs([C1,C2|Rest], Acc, Count) :-
    is_digit(C1),
    (C2 = 32 ; C2 = 44), % space or comma
    NewAcc is Acc + 1,
    count_coordinate_pairs(Rest, NewAcc, Count).
count_coordinate_pairs([_|Rest], Acc, Count) :-
    count_coordinate_pairs(Rest, Acc, Count).

is_digit(C) :-
    C >= 48, C =< 57.

% Function to calculate area
calculate_area(Polygon, Area) :-
    db_open('localhost', 5432, 'postgres', 'tvmarcon', '1234'),
    db_import('SELECT ST_Area(ST_GeomFromText($1))', [Polygon], [row(Area)]),
    db_close.

% Test database connection
test_connection :-
    write('Testing database connection...'), nl,
    db_open('localhost', 5432, 'postgres', 'tvmarcon', '1234'),
    write('Connected to database'), nl,
    db_import('SELECT name, type FROM tetrominoes', [], Result),
    write('Tetrominoes: '), write(Result), nl.

% Test rotation of all tetrominos
test_rotate_all :-
    write('Testing rotation of all tetrominos...'), nl, nl,
    
    % Test I tetromino
    write('Testing I tetromino:'), nl,
    OriginalI = 'POLYGON((0 0, 4 0, 4 1, 0 1, 0 0))',
    write('Original: '), write(OriginalI), nl,
    st_rotate(OriginalI, 90, ResultI),
    write('Rotated 90°: '), write(ResultI), nl, nl,
    
    % Test O tetromino
    write('Testing O tetromino:'), nl,
    OriginalO = 'POLYGON((0 0, 2 0, 2 2, 0 2, 0 0))',
    write('Original: '), write(OriginalO), nl,
    st_rotate(OriginalO, 90, ResultO),
    write('Rotated 90°: '), write(ResultO), nl, nl,
    
    % Test T tetromino
    write('Testing T tetromino:'), nl,
    OriginalT = 'POLYGON((1 0, 2 0, 2 1, 3 1, 3 2, 0 2, 0 1, 1 1, 1 0))',
    write('Original: '), write(OriginalT), nl,
    st_rotate(OriginalT, 90, ResultT),
    write('Rotated 90°: '), write(ResultT), nl, nl,
    
    % Test J tetromino
    write('Testing J tetromino:'), nl,
    OriginalJ = 'POLYGON((0 0, 2 0, 2 3, 1 3, 1 1, 0 1, 0 0))',
    write('Original: '), write(OriginalJ), nl,
    st_rotate(OriginalJ, 90, ResultJ),
    write('Rotated 90°: '), write(ResultJ), nl, nl,
    
    % Test L tetromino
    write('Testing L tetromino:'), nl,
    OriginalL = 'POLYGON((0 0, 2 0, 2 1, 1 1, 1 3, 0 3, 0 0))',
    write('Original: '), write(OriginalL), nl,
    st_rotate(OriginalL, 90, ResultL),
    write('Rotated 90°: '), write(ResultL), nl, nl,
    
    % Test S tetromino
    write('Testing S tetromino:'), nl,
    OriginalS = 'POLYGON((0 0, 2 0, 2 1, 3 1, 3 2, 1 2, 1 1, 0 1, 0 0))',
    write('Original: '), write(OriginalS), nl,
    st_rotate(OriginalS, 90, ResultS),
    write('Rotated 90°: '), write(ResultS), nl, nl,
    
    % Test Z tetromino
    write('Testing Z tetromino:'), nl,
    OriginalZ = 'POLYGON((1 0, 3 0, 3 1, 2 1, 2 2, 0 2, 0 1, 1 1, 1 0))',
    write('Original: '), write(OriginalZ), nl,
    st_rotate(OriginalZ, 90, ResultZ),
    write('Rotated 90°: '), write(ResultZ), nl.

% Test translation of all tetrominos
test_translate_all :-
    write('Testing translation of all tetrominos...'), nl, nl,
    
    % Test I tetromino
    write('Testing I tetromino:'), nl,
    OriginalI = 'POLYGON((0 0, 4 0, 4 1, 0 1, 0 0))',
    write('Original: '), write(OriginalI), nl,
    st_translate(OriginalI, 2, 3, ResultI),
    write('Translated (2,3): '), write(ResultI), nl, nl,
    
    % Test O tetromino
    write('Testing O tetromino:'), nl,
    OriginalO = 'POLYGON((0 0, 2 0, 2 2, 0 2, 0 0))',
    write('Original: '), write(OriginalO), nl,
    st_translate(OriginalO, 2, 3, ResultO),
    write('Translated (2,3): '), write(ResultO), nl.

% Test difference between tetrominos
test_difference :-
    write('Testing difference between tetrominos...'), nl, nl,
    
    % Test I and O
    write('Testing difference between I and O:'), nl,
    OriginalI = 'POLYGON((0 0, 4 0, 4 1, 0 1, 0 0))',
    OriginalO = 'POLYGON((0 0, 2 0, 2 2, 0 2, 0 0))',
    write('I: '), write(OriginalI), nl,
    write('O: '), write(OriginalO), nl,
    st_difference(OriginalI, OriginalO, Result),
    write('Difference: '), write(Result), nl.

% Run all tests
run_all_tests :-
    write('Running all tests...'), nl, nl,
    test_connection, nl,
    test_rotate_all, nl,
    test_translate_all, nl,
    test_difference, nl,
    db_close,
    write('All tests completed.'), nl.

% To run all tests, use:
% ?- run_all_tests.
%
% To run specific tests, use:
% ?- test_connection.
% ?- test_rotate_all.
% ?- test_translate_all.
% ?- test_difference. 