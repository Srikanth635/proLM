% Facts
action(picking_up).
object(cup).
object(table).

name(picking_up, 'PickingUp').
name(cup, 'clear glass cup').
name(table, 'table').

property(cup, color, clear).
property(cup, material, glass).
property(cup, shape, cylindrical).
property(cup, size, medium).
property(cup, transparency, transparent).
property(table, position, on).
property(table, surface, flat).

located_at(cup, table).

involves(picking_up, cup).

%
