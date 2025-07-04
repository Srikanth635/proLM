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
    # Wrap the prompt in a HumanMessage
    response = ollama_llm.invoke([HumanMessage(content=prompt)])
    return think_remover(response.content.strip())

if __name__ == "__main__":
    prompt = sys.argv[1]
    result = query_llm(f"Respond in JSON format: {{\"answer\": <your answer>}}.\n{prompt}")
    print(json.dumps({"answer": result}))