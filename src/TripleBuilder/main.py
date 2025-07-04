from ontology_loader import load_ontology
from json_to_rdf import process_action

# Load ontology
onto = load_ontology("../ontology/SOMA.owl")

# JSON from LLM
json_data = {
  "action": {
    "type": "grab-object",
    "object": {
      "type": "box of Cheez-It crackers",
      "material": "cardboard",
      "texture": "smooth"
    },
    "source": {
      "type": "location",
      "name": "on the table",
      "position": "on"
    }
  }
}

# Process and create OWL individuals
process_action(onto, json_data["action"])

# Save to OWL
onto.save(file="output.owl", format="rdfxml")
