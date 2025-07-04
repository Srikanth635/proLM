
def pos_lemma_stem(tokens):
    import nltk
    from nltk.stem import PorterStemmer, WordNetLemmatizer
    from nltk.corpus import wordnet
    nltk.download('averaged_perceptron_tagger', quiet=True)
    nltk.download('wordnet', quiet=True)
    nltk.download('omw-1.4', quiet=True)

    ps = PorterStemmer()
    lemmatizer = WordNetLemmatizer()

    pos_tags = nltk.pos_tag(tokens)

    def get_wordnet_pos(tag):
        if tag.startswith('J'):
            return wordnet.ADJ
        elif tag.startswith('V'):
            return wordnet.VERB
        elif tag.startswith('N'):
            return wordnet.NOUN
        elif tag.startswith('R'):
            return wordnet.ADV
        else:
            return wordnet.NOUN

    result = []
    for word, tag in pos_tags:
        lemma = lemmatizer.lemmatize(word, get_wordnet_pos(tag))
        stem = ps.stem(word)
        result.append({'text': word, 'pos': tag, 'lemma': lemma, 'stem': stem})
    return result
