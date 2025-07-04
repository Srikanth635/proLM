
def detect_intent(text):
    # Dummy rule-based intent detector
    if 'book' in text and 'flight' in text:
        return 'book_flight'
    elif 'weather' in text:
        return 'check_weather'
    else:
        return 'unknown'
