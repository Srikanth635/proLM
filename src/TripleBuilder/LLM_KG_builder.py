import json
from typing import Dict, List, Any, Optional
from dataclasses import dataclass
import uuid
from datetime import datetime


# ============================================================================
# KNOWLEDGE GRAPH FOUNDATION
# ============================================================================

@dataclass
class Triple:
    """Basic RDF-like triple structure"""
    subject: str
    predicate: str
    object: str

    def __str__(self):
        return f"<{self.subject}> <{self.predicate}> <{self.object}>"


class SOMAKnowledgeGraph:
    """Knowledge Graph based on SOMA ontology concepts"""

    def __init__(self):
        self.triples: List[Triple] = []
        self.namespaces = {
            'soma': 'http://www.ease-crc.org/ont/SOMA.owl#',
            'dul': 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#',
            'instance': 'http://example.org/instances#',
            'xsd': 'http://www.w3.org/2001/XMLSchema#',
            'rdf': 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
            'rdfs': 'http://www.w3.org/2000/01/rdf-schema#'
        }
        self.instances = {}

    def add_namespace(self, prefix: str, uri: str):
        """Add a new namespace"""
        self.namespaces[prefix] = uri

    def uri(self, prefix: str, local_name: str) -> str:
        """Create full URI from prefix and local name"""
        return f"{self.namespaces[prefix]}{local_name}"

    def add_triple(self, subject: str, predicate: str, obj: str):
        """Add a triple to the knowledge graph"""
        triple = Triple(subject, predicate, obj)
        self.triples.append(triple)
        return triple

    def add_instance(self, instance_id: str, class_type: str, properties: Dict[str, Any] = None):
        """Add an instance of a SOMA class"""
        # Type assertion
        self.add_triple(
            self.uri('instance', instance_id),
            self.uri('rdf', 'type'),
            self.uri('soma', class_type)
        )

        # Store instance for reference
        self.instances[instance_id] = {
            'type': class_type,
            'properties': properties or {}
        }

        # Add properties if provided
        if properties:
            for prop, value in properties.items():
                self.add_property(instance_id, prop, value)

    def add_property(self, instance_id: str, property_name: str, value: Any):
        """Add a property to an instance"""
        subject = self.uri('instance', instance_id)

        if isinstance(value, str) and not value.startswith('http'):
            # String literal
            obj = f'"{value}"'
        elif isinstance(value, (int, float)):
            # Numeric literal
            obj = f'"{value}"^^{self.uri("xsd", "double" if isinstance(value, float) else "integer")}'
        elif isinstance(value, bool):
            # Boolean literal
            obj = f'"{str(value).lower()}"^^{self.uri("xsd", "boolean")}'
        elif isinstance(value, list):
            # Handle lists by creating multiple triples
            for item in value:
                self.add_property(instance_id, property_name, item)
            return
        else:
            # Assume it's a reference to another instance
            obj = self.uri('instance', str(value))

        # Determine predicate URI
        if property_name.startswith('soma:'):
            predicate = self.uri('soma', property_name[5:])
        elif property_name.startswith('has'):
            predicate = self.uri('soma', property_name)
        else:
            predicate = self.uri('soma', f'has{property_name.capitalize()}')

        self.add_triple(subject, predicate, obj)

    def add_relation(self, subject_id: str, relation: str, object_id: str):
        """Add a relation between two instances"""
        self.add_triple(
            self.uri('instance', subject_id),
            self.uri('soma', relation),
            self.uri('instance', object_id)
        )

    def export_turtle(self) -> str:
        """Export knowledge graph as Turtle format"""
        turtle = ""

        # Add namespace prefixes
        for prefix, uri in self.namespaces.items():
            turtle += f"@prefix {prefix}: <{uri}> .\n"
        turtle += "\n"

        # Add triples
        for triple in self.triples:
            turtle += f"{triple} .\n"

        return turtle

    def export_json_ld(self) -> Dict:
        """Export knowledge graph as JSON-LD"""
        context = {f"@{k}": v for k, v in self.namespaces.items()}

        graph = []
        for triple in self.triples:
            graph.append({
                "@subject": triple.subject,
                "@predicate": triple.predicate,
                "@object": triple.object
            })

        return {
            "@context": context,
            "@graph": graph
        }

    def query_instances_of_type(self, class_type: str) -> List[str]:
        """Query all instances of a specific type"""
        instances = []
        type_uri = self.uri('soma', class_type)
        rdf_type = self.uri('rdf', 'type')

        for triple in self.triples:
            if triple.predicate == rdf_type and triple.object == type_uri:
                instances.append(triple.subject)

        return instances

    def get_instance_properties(self, instance_id: str) -> Dict[str, List[str]]:
        """Get all properties of an instance"""
        instance_uri = self.uri('instance', instance_id)
        properties = {}

        for triple in self.triples:
            if triple.subject == instance_uri:
                if triple.predicate not in properties:
                    properties[triple.predicate] = []
                properties[triple.predicate].append(triple.object)

        return properties


# ============================================================================
# APPLE INSTANCE BUILDER
# ============================================================================

def create_apple_knowledge_graph():
    """Create knowledge graph with apple instance based on SOMA ontology"""

    kg = SOMAKnowledgeGraph()

    # Generate unique instance ID
    apple_id = "apple_001"
    table_id = "table_001"

    # ====== APPLE INSTANCE ======

    # 1. Basic classification
    kg.add_instance(apple_id, "Item", {
        "name": "Red Apple",
        "description": "A round, edible fruit with smooth skin and crisp flesh"
    })

    # 2. Physical qualities
    # Mass attribute
    mass_id = f"{apple_id}_mass"
    kg.add_instance(mass_id, "MassAttribute", {"hasMassValue": 0.18})
    kg.add_relation(apple_id, "hasMassAttribute", mass_id)

    # Color
    color_id = f"{apple_id}_color"
    kg.add_instance(color_id, "RedColor", {
        "hasRGBValue": "220,20,60",
        "hasHSVValue": "348,91,86"
    })
    kg.add_relation(apple_id, "hasColor", color_id)

    # Shape
    shape_id = f"{apple_id}_shape"
    kg.add_instance(shape_id, "SphereShape", {
        "hasRadius": 0.04,
        "hasWidth": 0.08,
        "hasHeight": 0.085
    })
    kg.add_relation(apple_id, "hasShape", shape_id)

    # Temperature
    temp_id = f"{apple_id}_temperature"
    kg.add_instance(temp_id, "Temperature")
    kg.add_property(temp_id, "hasTemperatureValue", 20.0)
    kg.add_relation(apple_id, "hasTemperature", temp_id)

    # 3. Capabilities and Affordances
    # Graspability
    grasp_id = f"{apple_id}_graspability"
    kg.add_instance(grasp_id, "Graspability")
    kg.add_property(grasp_id, "hasGraspabilityValue", 0.9)
    kg.add_relation(apple_id, "hasCapability", grasp_id)

    # 4. Material
    material_id = f"{apple_id}_material"
    kg.add_instance(material_id, "Material")
    kg.add_property(material_id, "materialType", "organic")
    kg.add_relation(apple_id, "hasMaterial", material_id)

    # 5. State properties
    # Cleanliness
    clean_id = f"{apple_id}_cleanliness"
    kg.add_instance(clean_id, "Clean")
    kg.add_relation(apple_id, "hasQuality", clean_id)

    # ====== TABLE INSTANCE ======

    kg.add_instance(table_id, "DesignedFurniture", {
        "name": "Kitchen Table",
        "description": "A flat surface for placing objects"
    })

    # Table shape
    table_shape_id = f"{table_id}_shape"
    kg.add_instance(table_shape_id, "BoxShape", {
        "hasWidth": 1.2,
        "hasHeight": 0.03,
        "hasLength": 0.8
    })
    kg.add_relation(table_id, "hasShape", table_shape_id)

    # ====== SPATIAL RELATIONS ======

    # Apple is supported by table
    kg.add_relation(apple_id, "isSupportedBy", table_id)
    kg.add_relation(table_id, "supports", apple_id)

    # ====== AFFORDANCES ======

    # Define pickup affordance
    pickup_affordance_id = f"{apple_id}_pickup_affordance"
    kg.add_instance(pickup_affordance_id, "Affordance")
    kg.add_property(pickup_affordance_id, "affordanceType", "pickup")

    # Connect affordance to apple
    kg.add_relation(pickup_affordance_id, "affordsBearer", apple_id)
    kg.add_relation(pickup_affordance_id, "affordsTask", "PickupTask")

    # ====== ROLES IN INSTRUCTION CONTEXT ======

    # Apple plays Patient role in pickup action
    patient_role_id = f"{apple_id}_patient_role"
    kg.add_instance(patient_role_id, "Patient")
    kg.add_relation(apple_id, "playsRole", patient_role_id)

    # ====== LOCALIZATION ======

    # Apple's position relative to table
    localization_id = f"{apple_id}_localization"
    kg.add_instance(localization_id, "Localization")
    kg.add_property(localization_id, "hasPositionData", "0.0,0.0,0.042")
    kg.add_property(localization_id, "hasReferenceFrame", "table_surface")
    kg.add_relation(apple_id, "hasLocalization", localization_id)

    return kg


# ============================================================================
# DEMONSTRATION AND ANALYSIS
# ============================================================================

def demonstrate_knowledge_graph():
    """Demonstrate the knowledge graph creation and querying"""

    print("=" * 70)
    print("SOMA KNOWLEDGE GRAPH - APPLE INSTANCE DEMONSTRATION")
    print("=" * 70)
    print()

    # Create the knowledge graph
    kg = create_apple_knowledge_graph()

    print(f"📊 KNOWLEDGE GRAPH STATISTICS:")
    print(f"   Total triples: {len(kg.triples)}")
    print(f"   Total instances: {len(kg.instances)}")
    print(f"   Namespaces: {len(kg.namespaces)}")
    print()

    # Query examples
    print("🔍 QUERY EXAMPLES:")
    print()

    # Find all items
    items = kg.query_instances_of_type("Item")
    print(f"1. All Items: {[i.split('#')[-1] for i in items]}")

    # Find all designed furniture
    furniture = kg.query_instances_of_type("DesignedFurniture")
    print(f"2. All Designed Furniture: {[f.split('#')[-1] for f in furniture]}")

    # Find all shapes
    shapes = kg.query_instances_of_type("SphereShape")
    print(f"3. All Sphere Shapes: {[s.split('#')[-1] for s in shapes]}")

    print()

    # Get properties of apple
    apple_props = kg.get_instance_properties("apple_001")
    print("🍎 APPLE INSTANCE PROPERTIES:")
    for prop, values in apple_props.items():
        prop_name = prop.split('#')[-1] if '#' in prop else prop
        clean_values = [v.replace('"', '').split('^^')[0] for v in values]
        print(f"   {prop_name}: {clean_values}")

    print()

    # Show some key triples
    print("🔗 SAMPLE KNOWLEDGE GRAPH TRIPLES:")
    key_triples = [
        "apple_001 rdf:type soma:Item",
        "apple_001 soma:isSupportedBy table_001",
        "apple_001 soma:hasColor apple_001_color",
        "apple_001_color rdf:type soma:RedColor",
        "apple_001_graspability soma:hasGraspabilityValue 0.9"
    ]

    for i, triple_desc in enumerate(key_triples, 1):
        print(f"   {i}. {triple_desc}")

    print()

    return kg


# ============================================================================
# EXPORT FUNCTIONS
# ============================================================================

def export_knowledge_graph_formats(kg: SOMAKnowledgeGraph):
    """Export knowledge graph in different formats"""

    print("📤 EXPORT FORMATS:")
    print()

    # Turtle format
    print("1. TURTLE/RDF FORMAT:")
    print("-" * 50)
    turtle_output = kg.export_turtle()
    print(turtle_output[:500] + "..." if len(turtle_output) > 500 else turtle_output)
    print()

    # JSON-LD format
    print("2. JSON-LD FORMAT (sample):")
    print("-" * 50)
    jsonld_output = kg.export_json_ld()
    print(json.dumps(jsonld_output, indent=2)[:500] + "..." if len(str(jsonld_output)) > 500 else json.dumps(
        jsonld_output, indent=2))
    print()

    # SPARQL query example
    print("3. SAMPLE SPARQL QUERIES:")
    print("-" * 50)
    sparql_queries = [
        """
# Find all items that can be grasped
SELECT ?item ?graspability WHERE {
    ?item rdf:type soma:Item .
    ?item soma:hasCapability ?grasp .
    ?grasp rdf:type soma:Graspability .
    ?grasp soma:hasGraspabilityValue ?graspability .
}""",
        """
# Find all support relationships  
SELECT ?supporter ?supported WHERE {
    ?supporter soma:supports ?supported .
}""",
        """
# Find all red colored objects
SELECT ?object WHERE {
    ?object soma:hasColor ?color .
    ?color rdf:type soma:RedColor .
}"""
    ]

    for i, query in enumerate(sparql_queries, 1):
        print(f"Query {i}:{query}")
        print()


# ============================================================================
# EXTENSIBILITY DEMONSTRATION
# ============================================================================

def demonstrate_extensibility(kg: SOMAKnowledgeGraph):
    """Show how to easily add more objects and relations"""

    print("🔧 EXTENSIBILITY DEMONSTRATION:")
    print("-" * 50)

    # Add a knife on the table
    apple_id = "apple_001"
    knife_id = "knife_001"
    kg.add_instance(knife_id, "DesignedTool", {
        "name": "Kitchen Knife",
        "description": "Sharp cutting tool"
    })

    # Knife capabilities
    cutting_id = f"{knife_id}_cutting"
    kg.add_instance(cutting_id, "CanCut")
    kg.add_relation(knife_id, "hasCapability", cutting_id)

    # Knife also on table
    kg.add_relation(knife_id, "isSupportedBy", "table_001")
    kg.add_relation("table_001", "supports", knife_id)

    # Define cutting affordance between knife and apple
    cut_affordance_id = "apple_knife_cut_affordance"
    kg.add_instance(cut_affordance_id, "Affordance")
    kg.add_relation(cut_affordance_id, "affordsBearer", knife_id)
    kg.add_relation(cut_affordance_id, "affordsTask", "CuttingTask")
    kg.add_relation(cut_affordance_id, "affordsPatient", apple_id)

    print("✅ Added knife instance with cutting capabilities")
    print("✅ Created cutting affordance relationship")
    print(f"✅ Total triples now: {len(kg.triples)}")
    print()

    # Query the extended graph
    tools = kg.query_instances_of_type("DesignedTool")
    print(f"🔧 All Tools: {[t.split('#')[-1] for t in tools]}")

    return kg


# ============================================================================
# MAIN EXECUTION
# ============================================================================

if __name__ == "__main__":
    # Create and demonstrate the knowledge graph
    kg = demonstrate_knowledge_graph()

    print()
    export_knowledge_graph_formats(kg)

    print()
    kg_extended = demonstrate_extensibility(kg)

    print()
    print("=" * 70)
    print("🎯 CONCLUSION:")
    print("=" * 70)
    print("""
This demonstrates how your SOMA ontological concepts serve as the perfect
foundation for building rich, queryable knowledge graphs from LLM-extracted
object information.

Key Benefits:
✅ Ontology-grounded instances ensure semantic consistency
✅ Rich relational structure enables complex reasoning
✅ Standard formats (RDF/Turtle/JSON-LD) ensure interoperability
✅ SPARQL querying enables powerful information retrieval
✅ Easy extensibility for adding new objects and relations
✅ Direct mapping from Pydantic models to KG instances

Next Steps:
- Integrate with graph databases (Neo4j, Apache Jena, etc.)
- Add reasoning engines for inference
- Build automated instance creation from LLM outputs
- Implement graph-based query interfaces
    """)