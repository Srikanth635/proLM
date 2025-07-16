# llm_bridge.py
import sys
from langchain_ollama import ChatOllama
from langchain_core.messages import HumanMessage
import json
import re

# Instantiate the Ollama LLM
ollama_llm = ChatOllama(model="qwen3:8b", temperature=0.4)

def think_remover(res : str):
    if re.search(r"<think>.*?</think>", res, flags=re.DOTALL):
        cleaned_res = re.sub(r"<think>.*?</think>", "", res, flags=re.DOTALL).strip()
    else:
        cleaned_res = res.strip()

    return cleaned_res

def query_llm(prompt):
    """Send a prompt to the LLM and return cleaned answer."""
    # Wrap the prompt in a HumanMessage
    response = ollama_llm.invoke([HumanMessage(content=prompt)])
    return think_remover(response.content.strip())

if __name__ == "__main__":
    # prompt = sys.argv[1]
    prompt = sys.stdin.read().strip()
    if not prompt:
        print(json.dumps({"answer": "No prompt provided"}))
        sys.exit(0)
    # Wrap prompt to instruct LLM to return JSON
    wrapped_prompt = f"Respond in JSON format: {{\"answer\": <your answer>}}.\n{prompt}"
    result = query_llm(wrapped_prompt)
    # Print as JSON
    print(json.dumps({"answer": result}, ensure_ascii=False))