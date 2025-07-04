import nltk
from nltk.stem import WordNetLemmatizer, PorterStemmer
from nltk.tokenize import word_tokenize

# --- NLTK Data Download (Run this once if you don't have them) ---
# It's good practice to ensure these are downloaded.
# You might need to run these lines in your environment if you encounter errors
# like "Resource 'corpora/wordnet' not found." or "Resource 'tokenizers/punkt' not found."
try:
    nltk.data.find('corpora/wordnet')
except LookupError:
    print("Downloading 'wordnet' NLTK data...")
    nltk.download('wordnet')
try:
    nltk.data.find('tokenizers/punkt')
except LookupError:
    print("Downloading 'punkt' NLTK data...")
    nltk.download('punkt')
try:
    # word_tokenize sometimes implicitly requires 'punkt_tab'
    nltk.data.find('tokenizers/punkt_tab')
except LookupError:
    print("Downloading 'punkt_tab' NLTK data...")
    nltk.download('punkt_tab')


def lemmatize_dict_values(input_dict: dict) -> dict:
    """
    Lemmatizes the string values in a dictionary using NLTK's WordNetLemmatizer.

    Args:
        input_dict (dict): A dictionary where values are strings to be lemmatized.

    Returns:
        dict: A new dictionary with lemmatized string values.
              Non-string values are returned as is.
    """
    lemmatizer = WordNetLemmatizer()
    lemmatized_dict = {}
    for key, value in input_dict.items():
        if isinstance(value, str):
            # Tokenize the string into words
            words = word_tokenize(value)
            # Lemmatize each word
            lemmatized_words = [lemmatizer.lemmatize(word) for word in words]
            # Join the lemmatized words back into a string
            lemmatized_dict[key] = " ".join(lemmatized_words)
        else:
            # Keep non-string values as they are
            lemmatized_dict[key] = value
    return lemmatized_dict

def stem_dict_values(input_dict: dict) -> dict:
    """
    Stems the string values in a dictionary using NLTK's PorterStemmer.

    Args:
        input_dict (dict): A dictionary where values are strings to be stemmed.

    Returns:
        dict: A new dictionary with stemmed string values.
              Non-string values are returned as is.
    """
    stemmer = PorterStemmer()
    stemmed_dict = {}
    for key, value in input_dict.items():
        if isinstance(value, str):
            # Tokenize the string into words
            words = word_tokenize(value)
            # Stem each word
            stemmed_words = [stemmer.stem(word) for word in words]
            # Join the stemmed words back into a string
            stemmed_dict[key] = " ".join(stemmed_words)
        else:
            # Keep non-string values as they are
            stemmed_dict[key] = value
    return stemmed_dict

# --- Example Usage ---
if __name__ == "__main__":
    my_data = {
        "sentence1": "The quick brown foxes are jumping over the lazy dogs.",
        "sentence2": "Running and playing in the beautiful gardens.",
        "item_count": 123,
        "list_of_words": ["apples", "oranges"] # This will not be processed directly as it's not a string
    }

    print("Original Dictionary:")
    print(my_data)
    print("-" * 30)

    # Lemmatize example
    lemmatized_data = lemmatize_dict_values(my_data)
    print("\nLemmatized Dictionary:")
    print(lemmatized_data)
    print("-" * 30)

    # Stemming example
    stemmed_data = stem_dict_values(my_data)
    print("\nStemmed Dictionary:")
    print(stemmed_data)
    print("-" * 30)

    # Example with a value that is not a string
    data_with_non_string = {
        "text": "This is a test sentence.",
        "number": 123,
        "boolean": True
    }
    print("\nOriginal Dictionary (with non-string values):")
    print(data_with_non_string)
    print("-" * 30)

    lemmatized_non_string = lemmatize_dict_values(data_with_non_string)
    print("\nLemmatized Dictionary (with non-string values):")
    print(lemmatized_non_string)
    print("-" * 30)

    stemmed_non_string = stem_dict_values(data_with_non_string)
    print("\nStemmed Dictionary (with non-string values):")
    print(stemmed_non_string)
    print("-" * 30)
