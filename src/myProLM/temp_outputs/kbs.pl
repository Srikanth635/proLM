% Facts
action(act_picking_up).
object(obj_cup).

name(act_picking_up, 'PickingUp').
name(obj_cup, 'clear glass cup').

involves(act_picking_up, obj_cup).

property(obj_cup, shape, cylindrical).
property(obj_cup, material, glass).
property(obj_cup, color, clear).
property(obj_cup, transparency, transparent).

%
designator("(an action (type PickingUp) (an object (type cup) (shape cylindrical) (material glass) (transparency transparent) (color clear)))").
designator("(an action (type PickingUp) (an object (type cup) (shape cylindrical) (material glass) (transparency transparent) (color clear) (size medium) (texture smooth)) (source (a location (on table) (position on) (surface flat))))").
designator("(an action (type PickingUp) (an object (type apple) (size small) (shape round) (color red) (texture smooth)) (source (a location (on table) (position on))))").
