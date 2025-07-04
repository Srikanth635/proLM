
def resolve_coreferences(text):
    try:
        import spacy
        import coreferee
        nlp = spacy.load('en_core_web_sm')
        nlp.add_pipe('coreferee')
        doc = nlp(text)
        clusters = []
        for cluster in doc._.coref_chains:
            main = cluster.main.text
            mentions = [mention.text for mention in cluster]
            clusters.append({'main': main, 'mentions': mentions})
        return clusters
    except:
        return 'Coreference resolution module requires coreferee and spaCy compatible setup.'
