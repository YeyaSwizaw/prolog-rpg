% Lists
list(nil).
list(cons(_, T)) :- list(T).

% For Each
foreach(nil, _).
foreach(cons(H, T), F) :-
    list(T),
    call(F, H),
    foreach(T, F).

% Map
map(nil, _, nil).
map(cons(H, T), F, cons(X, Y)) :-
    list(T),
    map(T, F, Y),
    call(F, H, X).

% Filter
filter(nil, _, nil).
filter(cons(H, T), P, cons(H, Y)) :-
    list(T),
    filter(T, P, Y),
    call(P, H).
filter(cons(H, T), P, Y) :-
    list(T),
    filter(T, P, Y),
    \+ call(P, H).

% Fold
fold(nil, _, A, A).
fold(cons(H, T), F, A, X) :-
    list(T),
    call(F, A, H, B),
    fold(T, F, B, X).

% Times
times(0, _, nil) :- !.
times(N, F, cons(H, T)) :-
    M is N - 1,
    times(M, F, T),
    call(F, H).

% Print List
writelist(X) :-
    foreach(X, write),
    nl.

/*
% Generate Cell
gencell_impl(0, ' ').
gencell_impl(1, '#').

gencell(X) :-
    random(0, 2, N),
    gencell_impl(N, X).

% Generate Row
genrow(N, X) :- times(N, gencell, X).

% Generate Room
genroom(W, H, X) :- times(H, genrow(W), X).
genroom(W, H, Seed, X) :-
    set_random(seed(Seed)),
    genroom(W, H, X).

% Print Random Room
room(W, H) :- 
    genroom(W, H, X),
    foreach(X, writelist).
room(W, H, Seed) :-
    genroom(W, H, Seed, X),
    foreach(X, writelist).
*/

wall('#').
floor(' ').

empty(W, H, X) :-
    times(H, times(W, wall), X).

placeroomrow(X, cons(Tile, Empty), RoomX1, RoomX2, cons(Tile, Placed)) :-
    X < RoomX1,
    N is X + 1,
    placeroomrow(N, Empty, RoomX1, RoomX2, Placed).

placeroomrow(X, Empty, _, RoomX2, Empty) :-
    X > RoomX2.

placeroomrow(X, cons(_, Empty), RoomX1, RoomX2, cons(PlacedTile, Placed)) :-
    N is X + 1,
    floor(PlacedTile),
    placeroomrow(N, Empty, RoomX1, RoomX2, Placed).

placeroom(Y, cons(Row, Empty), RoomX1, RoomY1, RoomX2, RoomY2, cons(Row, Placed)) :-
    Y < RoomY1,
    N is Y + 1,
    placeroom(N, Empty, RoomX1, RoomY1, RoomX2, RoomY2, Placed).

placeroom(Y, Empty, _, _, _, RoomY2, Empty) :-
    Y > RoomY2.

placeroom(Y, cons(Row, Empty), RoomX1, RoomY1, RoomX2, RoomY2, cons(PlacedRow, Placed)) :-
    N is Y + 1,
    placeroomrow(0, Row, RoomX1, RoomX2, PlacedRow),
    placeroom(N, Empty, RoomX1, RoomY1, RoomX2, RoomY2, Placed).

placerooms(0, _, _, Roomed, Roomed) :- !.
placerooms(Rooms, W, H, Empty, Roomed) :-
    N is Rooms - 1,
    MinW is round(W / 7),
    MinH is round(H / 7),
    MaxW is round(W / 3),
    MaxH is round(H / 3),
    random(MinW, MaxW, RoomW),
    random(MinH, MaxH, RoomH),
    MaxX is (W - RoomW) - 1,
    MaxY is (H - RoomH) - 1,
    random(1, MaxX, RoomX1),
    random(1, MaxY, RoomY1),
    RoomX2 is RoomX1 + RoomW,
    RoomY2 is RoomY1 + RoomH,
    placeroom(0, Empty, RoomX1, RoomY1, RoomX2, RoomY2, Placed),
    placerooms(N, W, H, Placed, Roomed).

dungeon(Width, Height, Rooms) :-
    empty(Width, Height, Empty),
    placerooms(Rooms, Width, Height, Empty, Roomed),
    foreach(Roomed, writelist).

