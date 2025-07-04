
def preprocess(text: str):
    import nltk
    nltk.download('punkt', quiet=True)
    from nltk.tokenize import sent_tokenize, word_tokenize

    sentences = sent_tokenize(text)
    tokens = word_tokenize(text)
    return {'sentences': sentences, 'tokens': tokens}
