:- module(kbst).

% Facts
action(picking_up).
object(box).
object(drawer).

name(picking_up, 'PickingUp').
name(box, 'white round box').
name(drawer, 'drawer').

property(box, color, white).
property(box, shape, round).

located_at(box, drawer).

involves(picking_up, box).

%
