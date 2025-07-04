
def extract_named_entities(text):
    import spacy
    nlp = spacy.load('en_core_web_sm')
    doc = nlp(text)
    return [{'text': ent.text, 'label': ent.label_} for ent in doc.ents]
