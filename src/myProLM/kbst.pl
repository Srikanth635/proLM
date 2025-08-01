% Facts
action(act_picking_up).
object(obj_water_glass).
object(loc_table).

name(act_picking_up,    'PickingUp').
name(obj_water_glass,   'water glass').
name(loc_table,         'table').

property(obj_water_glass, material,     glass).
property(obj_water_glass, shape,        cylindrical).
property(obj_water_glass, transparency, transparent).
property(obj_water_glass, reflectance,  glossy).

located_at(obj_water_glass, loc_table).

involves(act_picking_up, obj_water_glass).

%
