import re
import json
import subprocess
import tempfile
import os
from typing import Dict, List, Tuple, Any
from dataclasses import dataclass
from dotenv import load_dotenv

load_dotenv()

try:
    import openai
    OPENAI_AVAILABLE = True
except ImportError:
    OPENAI_AVAILABLE = False
    print("Warning: OpenAI library not installed. Install with: pip install openai")

@dataclass
class ReasoningResult:
    success: bool
    prolog_facts: List[str]
    prolog_results: List[Dict]
    llm_suggestions: List[str]
    final_answer: str
    confidence: float

class PrologInterface:
    def __init__(self, kb_file_path: str = "cricket_knowledge_base.pl"):
        self.kb_file = kb_file_path
        print(f"📂 Using Prolog KB file: {self.kb_file}")

    def add_fact(self, fact: str):
        fact = fact.strip()
        if not fact.endswith('.'):
            fact += '.'
        with open(self.kb_file, 'a') as f:
            f.write(f"{fact}\n")
        print(f"✅ Fact added to KB: {fact}")

    def query(self, query_str: str) -> List[Dict]:
        try:
            print(f"🔎 Running Prolog query: {query_str}")
            with open(self.kb_file) as kb:
                print("🧾 Current KB contents:")
                print(kb.read())

            var_match = re.search(r'\(([^)]+)\)', query_str)
            var_part = var_match.group(1) if var_match else ""
            variables = [v.strip() for v in var_part.split(',') if v.strip().isupper()]
            target_var = variables[0] if variables else "X"

            query_file = tempfile.mktemp(suffix="_query.pl")
            with open(query_file, 'w') as f:
                f.write(f":- consult('{self.kb_file}').\n")
                f.write(f":- findall({target_var}, ({query_str}), Results), writeln(Results), halt.\n")

            result = subprocess.run(
                ['swipl', '-q', '-t', 'halt(1)', '-s', query_file],
                capture_output=True,
                text=True,
                timeout=10
            )

            print("🔍 Raw Prolog output:", result.stdout)

            if result.returncode == 0 and result.stdout:
                output = result.stdout.strip()
                if output.startswith("[") and output.endswith("]"):
                    content = output.strip("[]")
                    if not content:
                        return []
                    items = [i.strip() for i in content.split(',')]
                    return [{target_var: i} for i in items]
            return []
        except Exception as e:
            print(f"❌ Prolog query failed: {e}")
            return []

class FormalSoftReasoner:
    def __init__(self, openai_api_key: str = None):
        self.prolog = PrologInterface()
        self.openai_client = None
        if openai_api_key and OPENAI_AVAILABLE:
            try:
                openai.api_key = openai_api_key
                self.openai_client = openai
                print("✓ OpenAI client initialized successfully")
            except Exception as e:
                print(f"✗ Failed to initialize OpenAI client: {e}")
        print("✓ Using SWI-Prolog")

    def _llm_preprocess(self, user_input: str) -> Dict[str, Any]:
        prompt = f"""
        You are an assistant converting natural language cricket queries into Prolog.

        Query: "{user_input}"

        Return JSON with:
        - entities: list of players, teams, etc.
        - query_type: score, captain, batsman, etc.
        - prolog_queries: best guesses for Prolog queries
        - reasoning_needed: formal / soft / both
        - missing_info: list of gaps if any
        """

        try:
            response = self.openai_client.ChatCompletion.create(
                model="gpt-4",
                messages=[{"role": "user", "content": prompt}],
                temperature=0.2
            )
            content = response['choices'][0]['message']['content']
            print("🧠 LLM raw output:", content)
            json_match = re.search(r'{.*}', content, re.DOTALL)
            if json_match:
                parsed = json.loads(json_match.group())
                parsed["prolog_queries"] = [self._normalize_prolog_query(q) for q in parsed.get("prolog_queries", [])]
                return parsed
        except Exception as e:
            print(f"❌ LLM preprocessing failed: {e}")
        return self._simple_preprocess(user_input)

    def _normalize_prolog_query(self, query: str) -> str:
        tokens = re.split(r'([(),.])', query)
        normalized = []
        for token in tokens:
            if token.isidentifier() and not token[0].isupper():
                normalized.append(token.lower())
            else:
                normalized.append(token)
        fixed = ''.join(normalized)
        print(f"🔧 Normalized Prolog query: {query} -> {fixed}")
        return fixed

    def _normalize_entity_string(self, text: str) -> str:
        keywords = ["india", "australia", "england", "pakistan"]
        matches = []
        for keyword in keywords:
            if keyword in text.lower():
                matches.append(keyword.replace(" ", "_").lower())
        caps = re.findall(r'\b[A-Z][a-z]+\b', text)
        if caps:
            matches.append("_".join(caps).lower())
        print(f"🔤 Normalized entity: '{text}' -> '{' | '.join(matches)}'")
        return matches[0] if matches else ""

    def _simple_preprocess(self, user_input: str) -> Dict[str, Any]:
        print(f"💬 Preprocessing user input: '{user_input}'")
        entity = self._normalize_entity_string(user_input)
        query_type = "unknown"

        if any(word in user_input.lower() for word in ["score", "run"]):
            query_type = "score"
        elif any(word in user_input.lower() for word in ["captain", "leader"]):
            query_type = "captain"
        elif "batsman" in user_input.lower():
            query_type = "batsman"
        elif "bowler" in user_input.lower():
            query_type = "bowler"
        elif "win" in user_input.lower() or "result" in user_input.lower():
            query_type = "match_result"

        prolog_queries = []
        if entity:
            if query_type == "score":
                prolog_queries.append(f"score({entity}, X)")
            elif query_type == "captain":
                prolog_queries.append(f"captain({entity}, X)")
                prolog_queries.append(f"captain(X, {entity})")
            elif query_type == "batsman":
                prolog_queries.append(f"batsman({entity})")
            elif query_type == "bowler":
                prolog_queries.append(f"bowler({entity})")
            elif query_type == "match_result":
                prolog_queries.extend([
                    f"match_result({entity}, X, Y)",
                    f"match_result(X, {entity}, Y)"
                ])

        print(f"🧠 Query type: {query_type}")
        print(f"🛠️  Generated Prolog queries: {prolog_queries}")

        return {
            "entities": [entity],
            "query_type": query_type,
            "prolog_queries": prolog_queries,
            "reasoning_needed": "formal",
            "missing_info": [],
            "original_input": user_input
        }

    def reason(self, user_input: str) -> ReasoningResult:
        pre = self._llm_preprocess(user_input) if self.openai_client else self._simple_preprocess(user_input)
        results = []
        failed_queries = []
        for query in pre.get("prolog_queries", []):
            query_results = self.prolog.query(query)
            if query_results:
                results.append({"query": query, "results": query_results})
            else:
                failed_queries.append(query)

        interpretation = self._generate_interpretation(results)
        suggestions = self._generate_suggestions(failed_queries, user_input)
        final_answer = self._generate_final_answer(results, user_input, interpretation)

        return ReasoningResult(
            success=bool(results or suggestions),
            prolog_facts=pre.get("entities", []),
            prolog_results=results,
            llm_suggestions=suggestions,
            final_answer=final_answer,
            confidence=0.8 if results else 0.4
        )

    def _generate_interpretation(self, results):
        if not results:
            return "No direct relationships found in the knowledge base."
        parts = []
        for r in results:
            query = r['query']
            for res in r['results']:
                if 'X' in res:
                    parts.append(f"{query.split('(')[0]} -> {res['X']}")
        return "; ".join(parts)

    def _generate_suggestions(self, failed_queries, user_input):
        suggestions = []
        if failed_queries:
            suggestions.append("Some queries could not be answered — consider expanding the knowledge base.")
        if any(word in user_input.lower() for word in ["might", "probably", "best"]):
            suggestions.append("This query may require soft reasoning or value judgment.")
        return suggestions

    def _generate_final_answer(self, results, query, interpretation):
        base = f"Based on the knowledge base: {interpretation}"
        if "best" in query.lower():
            base += " Additional criteria may be needed for evaluative questions."
        return base

if __name__ == "__main__":
    reasoner = FormalSoftReasoner()

    print("\n🎮 INTERACTIVE MODE — Ask cricket questions")
    print("Type 'quit' to exit or 'add fact(...)' to insert new facts\n")

    while True:
        query = input("❓ Your question: ").strip()
        if query.lower() in ["quit", "exit"]:
            print("👋 Goodbye!")
            break
        if query.lower().startswith("add "):
            fact = query[4:].strip()
            reasoner.prolog.add_fact(fact)
            continue

        result = reasoner.reason(query)
        print(f"\n📊 Success: {result.success}")
        print(f"🧠 Confidence: {result.confidence:.2f}")
        print(f"🎯 Answer: {result.final_answer}")
        if result.llm_suggestions:
            print("💡 Suggestions:")
            for s in result.llm_suggestions:
                print(f"  • {s}")
        print("\n" + "─" * 50)
