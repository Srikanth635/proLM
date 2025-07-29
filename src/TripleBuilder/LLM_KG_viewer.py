import json
import csv
import networkx as nx
from typing import Dict, List, Tuple
import xml.etree.ElementTree as ET
from xml.dom import minidom
import os
from datetime import datetime


# Extending the previous SOMAKnowledgeGraph class
class VisualizableKnowledgeGraph:
    """Extended knowledge graph with visualization capabilities"""

    def __init__(self):
        self.triples = []
        self.namespaces = {
            'soma': 'http://www.ease-crc.org/ont/SOMA.owl#',
            'dul': 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#',
            'instance': 'http://example.org/instances#',
            'xsd': 'http://www.w3.org/2001/XMLSchema#',
            'rdf': 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
            'rdfs': 'http://www.w3.org/2000/01/rdf-schema#'
        }
        self.instances = {}

    def add_triple(self, subject: str, predicate: str, obj: str):
        """Add a triple to the knowledge graph"""
        from dataclasses import dataclass

        @dataclass
        class Triple:
            subject: str
            predicate: str
            object: str

            def __str__(self):
                return f"<{self.subject}> <{self.predicate}> <{self.object}>"

        triple = Triple(subject, predicate, obj)
        self.triples.append(triple)
        return triple

    def uri(self, prefix: str, local_name: str) -> str:
        """Create full URI from prefix and local name"""
        return f"{self.namespaces[prefix]}{local_name}"

    def add_instance(self, instance_id: str, class_type: str, properties: Dict = None):
        """Add an instance of a SOMA class"""
        self.add_triple(
            self.uri('instance', instance_id),
            self.uri('rdf', 'type'),
            self.uri('soma', class_type)
        )
        self.instances[instance_id] = {
            'type': class_type,
            'properties': properties or {}
        }

    def add_relation(self, subject_id: str, relation: str, object_id: str):
        """Add a relation between two instances"""
        self.add_triple(
            self.uri('instance', subject_id),
            self.uri('soma', relation),
            self.uri('instance', object_id)
        )

    # ========================================================================
    # EXPORT FORMATS
    # ========================================================================

    def export_turtle(self, filename: str = None) -> str:
        """Export as Turtle/TTL format"""
        turtle = ""

        # Namespace prefixes
        for prefix, uri in self.namespaces.items():
            turtle += f"@prefix {prefix}: <{uri}> .\n"
        turtle += "\n"

        # Triples
        for triple in self.triples:
            turtle += f"{self._format_uri_for_turtle(triple.subject)} {self._format_uri_for_turtle(triple.predicate)} {self._format_uri_for_turtle(triple.object)} .\n"

        if filename:
            with open(filename, 'w', encoding='utf-8') as f:
                f.write(turtle)
            print(f"✅ Turtle file saved: {filename}")

        return turtle

    def export_rdf_xml(self, filename: str = None) -> str:
        """Export as RDF/XML format"""
        root = ET.Element("rdf:RDF")

        # Add namespace declarations
        for prefix, uri in self.namespaces.items():
            if prefix != 'rdf':
                root.set(f"xmlns:{prefix}", uri)
        root.set("xmlns:rdf", self.namespaces['rdf'])

        # Group triples by subject
        subjects = {}
        for triple in self.triples:
            if triple.subject not in subjects:
                subjects[triple.subject] = []
            subjects[triple.subject].append((triple.predicate, triple.object))

        # Create RDF descriptions
        for subject, predicates in subjects.items():
            desc = ET.SubElement(root, "rdf:Description")
            desc.set("rdf:about", subject)

            for predicate, obj in predicates:
                pred_elem = ET.SubElement(desc, self._uri_to_qname(predicate))
                if obj.startswith('"'):
                    # Literal value
                    pred_elem.text = obj.strip('"').split('^^')[0]
                else:
                    # Resource reference
                    pred_elem.set("rdf:resource", obj)

        # Pretty print XML
        rough_string = ET.tostring(root, 'unicode')
        reparsed = minidom.parseString(rough_string)
        pretty_xml = reparsed.toprettyxml(indent="  ")

        if filename:
            with open(filename, 'w', encoding='utf-8') as f:
                f.write(pretty_xml)
            print(f"✅ RDF/XML file saved: {filename}")

        return pretty_xml

    def export_json_ld(self, filename: str = None) -> Dict:
        """Export as JSON-LD format"""
        context = {}
        for prefix, uri in self.namespaces.items():
            context[prefix] = uri

        graph = []

        # Group by subject for cleaner JSON-LD
        subjects = {}
        for triple in self.triples:
            if triple.subject not in subjects:
                subjects[triple.subject] = {"@id": triple.subject}

            pred = self._uri_to_compact(triple.predicate)
            obj = triple.object

            # Handle object formatting
            if obj.startswith('"'):
                # Literal
                if '^^' in obj:
                    value, datatype = obj.strip('"').split('^^')
                    obj = {"@value": value, "@type": datatype}
                else:
                    obj = obj.strip('"')
            else:
                # Resource reference
                obj = {"@id": obj}

            if pred in subjects[triple.subject]:
                # Multiple values - convert to array
                if not isinstance(subjects[triple.subject][pred], list):
                    subjects[triple.subject][pred] = [subjects[triple.subject][pred]]
                subjects[triple.subject][pred].append(obj)
            else:
                subjects[triple.subject][pred] = obj

        graph = list(subjects.values())

        result = {
            "@context": context,
            "@graph": graph
        }

        if filename:
            with open(filename, 'w', encoding='utf-8') as f:
                json.dump(result, f, indent=2, ensure_ascii=False)
            print(f"✅ JSON-LD file saved: {filename}")

        return result

    def export_csv(self, filename: str = None) -> str:
        """Export as CSV format (simple triple format)"""
        csv_content = []
        csv_content.append(["Subject", "Predicate", "Object"])

        for triple in self.triples:
            csv_content.append([
                self._clean_uri(triple.subject),
                self._clean_uri(triple.predicate),
                self._clean_uri(triple.object)
            ])

        if filename:
            with open(filename, 'w', newline='', encoding='utf-8') as f:
                writer = csv.writer(f)
                writer.writerows(csv_content)
            print(f"✅ CSV file saved: {filename}")

        # Return as string
        import io
        output = io.StringIO()
        writer = csv.writer(output)
        writer.writerows(csv_content)
        return output.getvalue()

    def export_graphml(self, filename: str = None) -> str:
        """Export as GraphML format (for tools like yEd, Gephi)"""
        try:
            G = self._to_networkx()

            if filename:
                # Try the standard NetworkX method first
                try:
                    nx.write_graphml(G, filename, encoding='utf-8', prettyprint=True)
                    print(f"✅ GraphML file saved: {filename}")
                except (TypeError, AttributeError) as e:
                    # Fallback: create GraphML manually
                    print(f"⚠️  NetworkX GraphML export failed ({str(e)}), using manual export...")
                    self._manual_graphml_export(G, filename)
                    print(f"✅ GraphML file saved (manual): {filename}")

            return f"GraphML export completed for {len(G.nodes)} nodes and {len(G.edges)} edges"

        except Exception as e:
            print(f"❌ GraphML export failed: {str(e)}")
            return f"GraphML export failed: {str(e)}"

    def export_gexf(self, filename: str = None) -> str:
        """Export as GEXF format (for Gephi)"""
        try:
            G = self._to_networkx()

            if filename:
                # Try NetworkX GEXF export
                try:
                    nx.write_gexf(G, filename)
                    print(f"✅ GEXF file saved: {filename}")
                except Exception as e:
                    # Fallback: create GEXF manually
                    print(f"⚠️  NetworkX GEXF export failed ({str(e)}), using manual export...")
                    self._manual_gexf_export(G, filename)
                    print(f"✅ GEXF file saved (manual): {filename}")

            return f"GEXF export completed for {len(G.nodes)} nodes and {len(G.edges)} edges"

        except Exception as e:
            print(f"❌ GEXF export failed: {str(e)}")
            return f"GEXF export failed: {str(e)}"

    def export_dot(self, filename: str = None) -> str:
        """Export as DOT format (for Graphviz)"""
        G = self._to_networkx()

        dot_content = "digraph KnowledgeGraph {\n"
        dot_content += "  rankdir=LR;\n"
        dot_content += "  node [shape=ellipse, style=filled];\n\n"

        # Add nodes with labels
        for node in G.nodes():
            label = self._clean_uri(node)
            node_type = self._get_node_type(node)
            color = self._get_node_color(node_type)
            dot_content += f'  "{node}" [label="{label}", fillcolor="{color}"];\n'

        dot_content += "\n"

        # Add edges
        for source, target, data in G.edges(data=True):
            label = data.get('label', '')
            dot_content += f'  "{source}" -> "{target}" [label="{label}"];\n'

        dot_content += "}\n"

        if filename:
            with open(filename, 'w', encoding='utf-8') as f:
                f.write(dot_content)
            print(f"✅ DOT file saved: {filename}")

        return dot_content

    # ========================================================================
    # VISUALIZATION METHODS
    # ========================================================================

    def visualize_matplotlib(self, filename: str = None, figsize: Tuple[int, int] = (12, 8)):
        """Create visualization using matplotlib/networkx"""
        try:
            import matplotlib.pyplot as plt
            import matplotlib
            matplotlib.use('Agg')  # Use non-GUI backend

            G = self._to_networkx()

            plt.figure(figsize=figsize)

            # Use spring layout for better visualization
            pos = nx.spring_layout(G, k=3, iterations=50)

            # Draw nodes with different colors based on type
            node_colors = []
            for node in G.nodes():
                node_type = self._get_node_type(node)
                node_colors.append(self._get_node_color_hex(node_type))

            # Draw the graph
            nx.draw_networkx_nodes(G, pos, node_color=node_colors,
                                   node_size=1000, alpha=0.8)
            nx.draw_networkx_edges(G, pos, edge_color='gray',
                                   arrows=True, arrowsize=20, alpha=0.6)

            # Add labels
            labels = {node: self._clean_uri(node) for node in G.nodes()}
            nx.draw_networkx_labels(G, pos, labels, font_size=8)

            # Add edge labels
            edge_labels = nx.get_edge_attributes(G, 'label')
            edge_labels = {k: self._clean_uri(v) for k, v in edge_labels.items()}
            nx.draw_networkx_edge_labels(G, pos, edge_labels, font_size=6)

            plt.title("SOMA Knowledge Graph Visualization", size=16)
            plt.axis('off')
            plt.tight_layout()

            if filename:
                plt.savefig(filename, dpi=300, bbox_inches='tight')
                print(f"✅ Matplotlib visualization saved: {filename}")

            plt.close()  # Close to free memory

        except ImportError:
            print("❌ Matplotlib not available. Skipping visualization.")
        except Exception as e:
            print(f"❌ Matplotlib visualization failed: {str(e)}")

    def _manual_graphml_export(self, G, filename: str):
        """Manual GraphML export as fallback"""
        graphml_content = '''<?xml version="1.0" encoding="UTF-8"?>
<graphml xmlns="http://graphml.graphdrawing.org/xmlns"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns
         http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">

  <key id="label" for="node" attr.name="label" attr.type="string"/>
  <key id="type" for="node" attr.name="type" attr.type="string"/>
  <key id="label" for="edge" attr.name="label" attr.type="string"/>

  <graph id="KnowledgeGraph" edgedefault="directed">
'''

        # Add nodes
        for node in G.nodes():
            node_id = str(hash(node))  # Create safe ID
            label = self._clean_uri(node)
            node_type = self._get_node_type(node)
            graphml_content += f'''    <node id="{node_id}">
      <data key="label">{label}</data>
      <data key="type">{node_type}</data>
    </node>
'''

        # Add edges
        for source, target, data in G.edges(data=True):
            source_id = str(hash(source))
            target_id = str(hash(target))
            label = self._clean_uri(data.get('label', ''))
            graphml_content += f'''    <edge source="{source_id}" target="{target_id}">
      <data key="label">{label}</data>
    </edge>
'''

        graphml_content += '''  </graph>
</graphml>'''

        with open(filename, 'w', encoding='utf-8') as f:
            f.write(graphml_content)

    def _manual_gexf_export(self, G, filename: str):
        """Manual GEXF export as fallback"""
        gexf_content = '''<?xml version="1.0" encoding="UTF-8"?>
<gexf xmlns="http://www.gexf.net/1.2draft" version="1.2">
  <meta lastmodifieddate="2024-01-01">
    <creator>SOMA Knowledge Graph Exporter</creator>
    <description>Knowledge Graph Export</description>
  </meta>
  <graph mode="static" defaultedgetype="directed">

    <attributes class="node">
      <attribute id="0" title="type" type="string"/>
    </attributes>

    <nodes>
'''

        # Add nodes
        node_id_map = {}
        for i, node in enumerate(G.nodes()):
            node_id_map[node] = str(i)
            label = self._clean_uri(node)
            node_type = self._get_node_type(node)
            color = self._get_node_color_hex(node_type).replace('#', '')

            gexf_content += f'''      <node id="{i}" label="{label}">
        <attvalues>
          <attvalue for="0" value="{node_type}"/>
        </attvalues>
        <viz:color r="{int(color[1:3], 16)}" g="{int(color[3:5], 16)}" b="{int(color[5:7], 16)}"/>
      </node>
'''

        gexf_content += '''    </nodes>

    <edges>
'''

        # Add edges
        for i, (source, target, data) in enumerate(G.edges(data=True)):
            source_id = node_id_map[source]
            target_id = node_id_map[target]
            label = self._clean_uri(data.get('label', ''))

            gexf_content += f'''      <edge id="{i}" source="{source_id}" target="{target_id}" label="{label}"/>
'''

        gexf_content += '''    </edges>
  </graph>
</gexf>'''

        with open(filename, 'w', encoding='utf-8') as f:
            f.write(gexf_content)

    def export_cytoscape_json(self, filename: str = None) -> Dict:
        """Export for Cytoscape visualization"""
        elements = []

        # Add nodes
        nodes = set()
        for triple in self.triples:
            nodes.add(triple.subject)
            nodes.add(triple.object)

        for node in nodes:
            if not node.startswith('"'):  # Skip literals
                elements.append({
                    "data": {
                        "id": node,
                        "label": self._clean_uri(node),
                        "type": self._get_node_type(node)
                    }
                })

        # Add edges
        for i, triple in enumerate(self.triples):
            if not triple.object.startswith('"'):  # Skip literal objects
                elements.append({
                    "data": {
                        "id": f"edge_{i}",
                        "source": triple.subject,
                        "target": triple.object,
                        "label": self._clean_uri(triple.predicate)
                    }
                })

        cytoscape_data = {
            "elements": elements,
            "style": [
                {
                    "selector": "node",
                    "style": {
                        "label": "data(label)",
                        "text-valign": "center",
                        "color": "black",
                        "background-color": "#3498db"
                    }
                },
                {
                    "selector": "edge",
                    "style": {
                        "label": "data(label)",
                        "curve-style": "bezier",
                        "target-arrow-shape": "triangle"
                    }
                }
            ]
        }

        if filename:
            with open(filename, 'w', encoding='utf-8') as f:
                json.dump(cytoscape_data, f, indent=2)
            print(f"✅ Cytoscape JSON saved: {filename}")

        return cytoscape_data

    # ========================================================================
    # WEB VISUALIZATION
    # ========================================================================

    def create_html_visualization(self, filename: str = "knowledge_graph.html"):
        """Create interactive HTML visualization using vis.js"""
        G = self._to_networkx()

        # Prepare data for vis.js
        nodes = []
        edges = []

        for node in G.nodes():
            nodes.append({
                "id": node,
                "label": self._clean_uri(node),
                "color": self._get_node_color_hex(self._get_node_type(node)),
                "title": f"Type: {self._get_node_type(node)}"
            })

        for source, target, data in G.edges(data=True):
            edges.append({
                "from": source,
                "to": target,
                "label": self._clean_uri(data.get('label', '')),
                "arrows": "to"
            })

        html_template = f"""
<!DOCTYPE html>
<html>
<head>
    <title>SOMA Knowledge Graph Visualization</title>
    <script type="text/javascript" src="https://unpkg.com/vis-network/standalone/umd/vis-network.min.js"></script>
    <style type="text/css">
        #knowledge-graph {{
            width: 100%;
            height: 600px;
            border: 1px solid lightgray;
        }}
        .controls {{
            margin: 10px 0;
        }}
        .info {{
            background: #f9f9f9;
            padding: 10px;
            margin: 10px 0;
            border-radius: 5px;
        }}
    </style>
</head>
<body>
    <h1>SOMA Knowledge Graph Visualization</h1>

    <div class="info">
        <strong>Graph Statistics:</strong>
        Nodes: {len(nodes)} | Edges: {len(edges)} | Triples: {len(self.triples)}
    </div>

    <div class="controls">
        <button onclick="network.fit()">Fit to Screen</button>
        <button onclick="togglePhysics()">Toggle Physics</button>
        <button onclick="exportImage()">Export Image</button>
    </div>

    <div id="knowledge-graph"></div>

    <div class="info">
        <strong>Legend:</strong><br>
        <span style="color: #3498db;">●</span> Instances |
        <span style="color: #e74c3c;">●</span> Classes |
        <span style="color: #2ecc71;">●</span> Properties |
        <span style="color: #f39c12;">●</span> Literals
    </div>

    <script type="text/javascript">
        var nodes = new vis.DataSet({json.dumps(nodes)});
        var edges = new vis.DataSet({json.dumps(edges)});
        var container = document.getElementById('knowledge-graph');
        var data = {{
            nodes: nodes,
            edges: edges
        }};
        var options = {{
            nodes: {{
                shape: 'dot',
                size: 16,
                font: {{
                    size: 12,
                    color: '#000000'
                }},
                borderWidth: 2
            }},
            edges: {{
                width: 2,
                color: {{inherit: 'from'}},
                smooth: {{
                    type: 'continuous'
                }},
                font: {{
                    size: 10,
                    align: 'middle'
                }}
            }},
            physics: {{
                stabilization: false,
                barnesHut: {{
                    gravitationalConstant: -80000,
                    springConstant: 0.001,
                    springLength: 200
                }}
            }},
            interaction: {{
                tooltipDelay: 200,
                hover: true
            }}
        }};
        var network = new vis.Network(container, data, options);

        var physicsEnabled = true;
        function togglePhysics() {{
            physicsEnabled = !physicsEnabled;
            network.setOptions({{physics: physicsEnabled}});
        }}

        function exportImage() {{
            var canvas = network.body.container.getElementsByTagName('canvas')[0];
            var dataURL = canvas.toDataURL();
            var link = document.createElement('a');
            link.download = 'knowledge_graph.png';
            link.href = dataURL;
            link.click();
        }}

        // Event listeners
        network.on("click", function (params) {{
            if (params.nodes.length > 0) {{
                var nodeId = params.nodes[0];
                console.log('Clicked node:', nodeId);
            }}
        }});

        network.on("stabilizationIterationsDone", function () {{
            network.setOptions({{physics: false}});
        }});
    </script>
</body>
</html>
        """

        with open(filename, 'w', encoding='utf-8') as f:
            f.write(html_template)

        print(f"✅ Interactive HTML visualization created: {filename}")
        print(f"   Open {filename} in your browser to view the interactive graph!")

        return filename

    # ========================================================================
    # HELPER METHODS
    # ========================================================================

    def _to_networkx(self):
        """Convert to NetworkX graph"""
        G = nx.DiGraph()

        for triple in self.triples:
            if not triple.object.startswith('"'):  # Skip literals
                G.add_edge(triple.subject, triple.object,
                           label=triple.predicate)

        return G

    def _clean_uri(self, uri: str) -> str:
        """Clean URI for display"""
        if '#' in uri:
            return uri.split('#')[-1]
        elif '/' in uri:
            return uri.split('/')[-1]
        return uri.strip('"').split('^^')[0]

    def _format_uri_for_turtle(self, uri: str) -> str:
        """Format URI for Turtle export"""
        if uri.startswith('"'):
            return uri
        for prefix, namespace in self.namespaces.items():
            if uri.startswith(namespace):
                return f"{prefix}:{uri[len(namespace):]}"
        return f"<{uri}>"

    def _uri_to_qname(self, uri: str) -> str:
        """Convert URI to QName"""
        for prefix, namespace in self.namespaces.items():
            if uri.startswith(namespace):
                return f"{prefix}:{uri[len(namespace):]}"
        return uri

    def _uri_to_compact(self, uri: str) -> str:
        """Convert URI to compact form"""
        for prefix, namespace in self.namespaces.items():
            if uri.startswith(namespace):
                return f"{prefix}:{uri[len(namespace):]}"
        return uri

    def _get_node_type(self, node: str) -> str:
        """Determine node type for visualization"""
        if 'instance' in node:
            return 'instance'
        elif 'soma' in node:
            return 'class'
        elif node.startswith('"'):
            return 'literal'
        else:
            return 'property'

    def _get_node_color(self, node_type: str) -> str:
        """Get color name for node type"""
        colors = {
            'instance': 'lightblue',
            'class': 'lightcoral',
            'property': 'lightgreen',
            'literal': 'lightyellow'
        }
        return colors.get(node_type, 'lightgray')

    def _get_node_color_hex(self, node_type: str) -> str:
        """Get hex color for node type"""
        colors = {
            'instance': '#3498db',
            'class': '#e74c3c',
            'property': '#2ecc71',
            'literal': '#f39c12'
        }
        return colors.get(node_type, '#95a5a6')


# ============================================================================
# DEMONSTRATION WITH APPLE EXAMPLE
# ============================================================================

def create_sample_knowledge_graph():
    """Create sample KG with apple example"""
    kg = VisualizableKnowledgeGraph()

    # Add apple instance
    kg.add_instance("apple_001", "Item")
    kg.add_instance("table_001", "DesignedFurniture")
    kg.add_instance("apple_001_color", "RedColor")
    kg.add_instance("apple_001_shape", "SphereShape")

    # Add relations
    kg.add_relation("apple_001", "hasColor", "apple_001_color")
    kg.add_relation("apple_001", "hasShape", "apple_001_shape")
    kg.add_relation("apple_001", "isSupportedBy", "table_001")
    kg.add_relation("table_001", "supports", "apple_001")

    return kg


def demonstrate_all_exports():
    """Demonstrate all export formats with error handling"""
    kg = create_sample_knowledge_graph()

    print("🎯 KNOWLEDGE GRAPH EXPORT DEMONSTRATION")
    print("=" * 60)

    # Create output directory
    output_dir = "kg_exports"
    os.makedirs(output_dir, exist_ok=True)

    print(f"📁 Creating exports in directory: {output_dir}/")
    print()

    # Export in different formats with error handling
    export_functions = [
        ("Turtle/TTL", lambda: kg.export_turtle(f"{output_dir}/knowledge_graph.ttl")),
        ("RDF/XML", lambda: kg.export_rdf_xml(f"{output_dir}/knowledge_graph.rdf")),
        ("JSON-LD", lambda: kg.export_json_ld(f"{output_dir}/knowledge_graph.jsonld")),
        ("CSV", lambda: kg.export_csv(f"{output_dir}/knowledge_graph.csv")),
        ("GraphML", lambda: kg.export_graphml(f"{output_dir}/knowledge_graph.graphml")),
        ("GEXF", lambda: kg.export_gexf(f"{output_dir}/knowledge_graph.gexf")),
        ("DOT", lambda: kg.export_dot(f"{output_dir}/knowledge_graph.dot")),
        ("Cytoscape JSON", lambda: kg.export_cytoscape_json(f"{output_dir}/knowledge_graph_cytoscape.json")),
        ("Interactive HTML", lambda: kg.create_html_visualization(f"{output_dir}/interactive_graph.html")),
        ("Matplotlib Plot", lambda: kg.visualize_matplotlib(f"{output_dir}/graph_plot.png"))
    ]

    successful_exports = []
    failed_exports = []

    for name, export_func in export_functions:
        try:
            export_func()
            successful_exports.append(name)
        except Exception as e:
            print(f"❌ {name} export failed: {str(e)}")
            failed_exports.append((name, str(e)))

    print()
    print("📊 EXPORT SUMMARY:")
    print("-" * 40)

    print(f"✅ Successful exports ({len(successful_exports)}):")
    for export in successful_exports:
        print(f"   • {export}")

    if failed_exports:
        print(f"\n❌ Failed exports ({len(failed_exports)}):")
        for export, error in failed_exports:
            print(f"   • {export}: {error[:50]}...")

    print()
    print("🔍 RECOMMENDED VIEWING OPTIONS:")
    print("-" * 40)
    viewing_options = [
        ("🌐 Interactive HTML", f"Open {output_dir}/interactive_graph.html in browser"),
        ("📊 Gephi", f"Import {output_dir}/knowledge_graph.gexf into Gephi"),
        ("🎨 yEd", f"Import {output_dir}/knowledge_graph.graphml into yEd"),
        ("📈 Online DOT Viewer", f"Upload {output_dir}/knowledge_graph.dot to GraphvizOnline"),
        ("📋 Spreadsheet", f"Open {output_dir}/knowledge_graph.csv in Excel/Sheets")
    ]

    for option, instruction in viewing_options:
        file_exists = any(file in instruction for file in os.listdir(output_dir)
                          if instruction.split('/')[-1].split(' ')[0] in file)
        status = "✅" if file_exists else "❌"
        print(f"{status} {option}: {instruction}")

    # Create a simple README file
    readme_content = f"""# Knowledge Graph Exports

Generated on: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

## Files in this directory:

### 🌐 Interactive Visualization
- `interactive_graph.html` - Open in any web browser for interactive exploration

### 📊 Network Analysis Tools  
- `knowledge_graph.gexf` - Import into Gephi for network analysis
- `knowledge_graph.graphml` - Import into yEd Graph Editor or Cytoscape

### 🔍 Text-based Formats
- `knowledge_graph.ttl` - Turtle/RDF format (human-readable)
- `knowledge_graph.rdf` - RDF/XML format (standard semantic web)
- `knowledge_graph.jsonld` - JSON-LD format (web-friendly)

### 📋 Data Formats
- `knowledge_graph.csv` - Simple table format for Excel/Sheets
- `knowledge_graph.dot` - GraphViz format for publication diagrams
- `knowledge_graph_cytoscape.json` - Cytoscape web format

### 🖼️ Images
- `graph_plot.png` - Static matplotlib visualization

## Quick Start:
1. Double-click `interactive_graph.html` for immediate viewing
2. For advanced analysis, install Gephi and open the .gexf file
3. For clean diagrams, install yEd and open the .graphml file

## Online Viewers (no installation needed):
- DOT files: https://dreampuf.github.io/GraphvizOnline/
- RDF/Turtle: http://www.ldf.fi/service/rdf-grapher
- JSON-LD: https://json-ld.org/playground/
"""

    with open(f"{output_dir}/README.md", 'w') as f:
        f.write(readme_content)

    print(f"\n📝 Created README.md with viewing instructions")
    print(f"\n🎉 Export complete! Check the '{output_dir}' directory")

    return output_dir


if __name__ == "__main__":
    demonstrate_all_exports()