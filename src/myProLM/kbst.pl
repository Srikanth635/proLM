:- module(kbst).

action(act_pouring).  
object(obj_mug).  
object(obj_water).  
object(obj_pot).  
involves(obj_mug, act_pouring).  
involves(obj_water, act_pouring).  
involves(obj_pot, act_pouring).  
property(obj_mug, material, ceramic).  
property(obj_mug, shape, cylindrical).  
property(obj_mug, position, on).  
property(obj_water, material, liquid).  
property(obj_water, color, blue).  
property(obj_pot, material, ceramic).  
property(obj_pot, shape, cylindrical).  
property(obj_pot, position, on).  
name(obj_mug, 'ceramic mug').  
name(obj_water, 'water').  
name(obj_pot, 'ceramic pot').
