
def build_output(input_text, preprocessed, pos_lemma_stem, ner, dep, coref, intent, sem_entities):
    return {
        'input': input_text,
        'sentences': preprocessed['sentences'],
        'tokens': preprocessed['tokens'],
        'pos_lemma_stem': pos_lemma_stem,
        'entities': ner,
        'dependencies': dep,
        'coreferences': coref,
        'intent': intent,
        'semantic_entities': sem_entities
    }
