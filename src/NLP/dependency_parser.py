
def parse_dependencies(text):
    import spacy
    nlp = spacy.load('en_core_web_sm')
    doc = nlp(text)
    return [{'head': token.head.text, 'dependent': token.text, 'relation': token.dep_} for token in doc]
