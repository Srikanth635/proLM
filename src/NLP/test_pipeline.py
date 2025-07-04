# test_pipeline.py

from input_handler import clean_input
from preprocessing import preprocess
from pos_lemma_stem import pos_lemma_stem
from ner_module import extract_named_entities
from dependency_parser import parse_dependencies
from coref_resolution import resolve_coreferences
from intent_detection import detect_intent
from entity_extraction import extract_entities
from output_builder import build_output

# Sample input
input_text = "Book a flight to New York tomorrow. Barack Obama was the president. He lives in Washington."

# 1. Clean input
cleaned = clean_input(input_text)

# 2. Preprocessing
preprocessed = preprocess(cleaned)

# 3. POS tagging, lemmatization, stemming
pos_lemmas = pos_lemma_stem(preprocessed['tokens'])

# 4. Named Entity Recognition
entities = extract_named_entities(cleaned)

# 5. Dependency Parsing
dependencies = parse_dependencies(cleaned)

# 6. Coreference Resolution
coreferences = resolve_coreferences(cleaned)

# 7. Intent Detection
intent = detect_intent(cleaned)

# 8. Semantic Entity Extraction
semantic_entities = extract_entities(cleaned)

# 9. Build final output
final_output = build_output(
    input_text,
    preprocessed,
    pos_lemmas,
    entities,
    dependencies,
    coreferences,
    intent,
    semantic_entities
)

# Print results
import json
print(json.dumps(final_output, indent=2))
