from owlready2 import Thing, Ontology, types

from owlready2 import Thing, Ontology, types


def get_or_create_class(onto: Ontology, class_name: str):
    """
    Returns an existing class or creates a new one
    """
    class_name = class_name.replace(" ", "_")
    existing_class = onto.search_one(iri="*" + class_name)

    if existing_class:
        return existing_class
    else:
        with onto:
            try:
                NewClass = types.new_class(class_name, (Thing,))
                return NewClass
            except Exception as e:
                print(f"Failed to create class '{class_name}': {e}")
                return None  # Only return None if class creation truly fails


def create_individual(cls, name: str):
    """
    Creates an individual instance of a class
    """
    if cls is None:
        raise ValueError("Cannot create individual: class is None")

    instance_name = name.replace(" ", "_")
    return cls(instance_name)
