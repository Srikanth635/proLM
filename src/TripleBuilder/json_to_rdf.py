from ontology_helper import get_or_create_class, create_individual

def process_action(onto, action_json):
    action_type = action_json["type"]
    ActionClass = get_or_create_class(onto, action_type)
    action_instance = create_individual(ActionClass, f"action_{action_type}")

    # Process object
    obj = action_json.get("object")
    if obj:
        ObjClass = get_or_create_class(onto, obj["type"])
        obj_instance = create_individual(ObjClass, f"object_{obj['type']}")
        # Set attributes as annotations (or create new data properties)
        for key, value in obj.items():
            if key != "type":
                setattr(obj_instance, key, value)
        action_instance.hasObject = [obj_instance]  # Custom relation, can define if needed

    # Process source
    src = action_json.get("source")
    if src:
        SrcClass = get_or_create_class(onto, src.get("type", "Location"))
        src_instance = create_individual(SrcClass, src["name"])
        if "position" in src:
            src_instance.position = src["position"]  # Again, custom property

        action_instance.hasSource = [src_instance]

    return action_instance
