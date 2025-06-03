:- load_foreign_files(['yap2pgsql'], [], init_predicates).

% Database Connection Predicates

db_open(Host, Port, DBName, User, Pass) :-
    db_connect(Host, Port, DBName, User, Pass).

db_close :-
    db_disconnect.

% Database Query Predicates

db_import(Query, Params, Result) :-
    db_query(Query, Params, Result).

% Spatial Operation Predicates

st_difference(Geom1, Geom2, Result) :-
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

st_translate(Geom, X, Y, Result) :-
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

st_rotate(Geom, Angle, Result) :-
    atomic_concat('\'', Geom, G1),
    atomic_concat(G1, '\'', G1Quote),
    atomic_concat('SELECT ST_AsText(ST_Rotate(ST_GeomFromText(', G1Quote, Q1),
    atomic_concat(Q1, '), radians(', Q2),
    number_atom(Angle, AngleAtom),
    atomic_concat(Q2, AngleAtom, Q3),
    atomic_concat(Q3, '), ST_MakePoint(0,0)))', Query),
    write('Debug - Query: '), write(Query), nl,
    db_import(Query, [], [row(Result)]).