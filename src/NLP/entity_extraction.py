
def extract_entities(text):
    # Very basic entity extraction via regex (can be extended)
    import re
    date_pattern = r'\b(?:today|tomorrow|\d{1,2}/\d{1,2}/\d{2,4})\b'
    location_pattern = r'\b(?:New York|Paris|London|Tokyo)\b'
    return {
        'dates': re.findall(date_pattern, text),
        'locations': re.findall(location_pattern, text)
    }
