
def clean_input(text: str) -> str:
    import re
    text = text.strip().lower()
    text = re.sub(r'\s+', ' ', text)
    return text
