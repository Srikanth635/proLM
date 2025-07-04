from owlready2 import get_ontology

def load_ontology(path: str):
    """
    Loads the OWL ontology using owlready2
    """
    onto = get_ontology(path).load()
    return onto
