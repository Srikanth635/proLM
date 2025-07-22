import asyncio
import backoff
import ollama
from typing import Any
import re

# -----------------------------
# Backoff-wrapped single calls
# -----------------------------

def think_remover(res: str):
    if re.search(r"<think>.*?</think>", res, flags=re.DOTALL):
        cleaned_res = re.sub(r"<think>.*?</think>", "", res, flags=re.DOTALL).strip()
    else:
        cleaned_res = res.strip()

    return cleaned_res

@backoff.on_exception(backoff.expo, Exception, max_tries=5)
def ollama_generate_with_backoff(**kwargs):
    """
    Wrapper for ollama.generate with backoff retries.
    """
    return ollama.generate(**kwargs)

@backoff.on_exception(backoff.expo, Exception, max_tries=5)
def ollama_chat_with_backoff(**kwargs):
    """
    Wrapper for ollama.chat with backoff retries.
    """
    return ollama.chat(**kwargs)

# -----------------------------
# Async batch dispatchers
# -----------------------------

async def dispatch_ollama_chat_requests(
    messages_list: list[list[dict[str, Any]]],
    model: str,
    temperature: float,
    max_tokens: int,
    stop_words: list[str]
) -> list[str]:
    """
    Dispatches multiple chat requests to Ollama concurrently.
    """
    async def _call(messages):
        resp = await asyncio.to_thread(
            ollama_chat_with_backoff,
            model=model,
            messages=messages,
            options={
                "temperature": temperature,
                # "num_predict": max_tokens
            }
        )
        content = resp['message']['content'].strip()
        content = think_remover(content)
        # for stop in stop_words:
        #     idx = content.find(stop)
        #     if idx != -1:
        #         content = content[:idx]
        #         break
        return content

    return await asyncio.gather(*[_call(m) for m in messages_list])


async def dispatch_ollama_prompt_requests(
    prompt_list: list[str],
    model: str,
    temperature: float,
    max_tokens: int,
    stop_words: list[str]
) -> list[str]:
    """
    Dispatches multiple prompt requests to Ollama concurrently.
    """
    async def _call(prompt):
        resp = await asyncio.to_thread(
            ollama_generate_with_backoff,
            model=model,
            prompt=prompt,
            options={
                "temperature": temperature,
                # "num_predict": max_tokens
            }
        )
        content = resp['response'].strip()
        content = think_remover(content)
        # for stop in stop_words:
        #     idx = content.find(stop)
        #     if idx != -1:
        #         content = content[:idx]
        #         break
        return content

    return await asyncio.gather(*[_call(p) for p in prompt_list])


# -----------------------------
# Main class
# -----------------------------

class OllamaModel:
    def __init__(self, model_name: str, stop_words=None, max_new_tokens: int = 256):
        """
        Initialize the OllamaModel.

        :param model_name: Name of the model to use in ollama (e.g. 'llama3', 'mistral', etc.)
        :param stop_words: List of stop words/tokens.
        :param max_new_tokens: Maximum tokens to generate.
        """
        self.model_name = model_name
        self.stop_words = stop_words or []
        self.max_new_tokens = max_new_tokens

    def _apply_stop_words(self, text: str) -> str:
        if not self.stop_words:
            return text
        for stop in self.stop_words:
            idx = text.find(stop)
            if idx != -1:
                return text[:idx]
        return text

    def chat_generate(self, input_string: str, temperature: float = 0.0) -> str:
        resp = ollama_chat_with_backoff(
            model=self.model_name,
            messages=[{"role": "user", "content": input_string}],
            options={
                "temperature": temperature,
                # "num_predict": self.max_new_tokens
            }
        )
        # return self._apply_stop_words(resp['message']['content'].strip())
        return think_remover(resp['message']['content'].strip())

    def prompt_generate(self, input_string: str, temperature: float = 0.0) -> str:
        resp = ollama_generate_with_backoff(
            model=self.model_name,
            prompt=input_string,
            options={
                "temperature": temperature,
                # "num_predict": self.max_new_tokens
            }
        )
        # return self._apply_stop_words(resp['response'].strip())
        return think_remover(resp['response'].strip())

    def generate(self, input_string: str, temperature: float = 0.0) -> str:
        # Default to chat interface
        return self.chat_generate(input_string, temperature)

    def batch_chat_generate(self, messages_list: list[str], temperature: float = 0.0):
        open_ai_messages_list = [
            [{"role": "user", "content": m}] for m in messages_list
        ]
        return asyncio.run(
            dispatch_ollama_chat_requests(
                open_ai_messages_list,
                self.model_name,
                temperature,
                self.max_new_tokens,
                self.stop_words
            )
        )

    def batch_prompt_generate(self, prompt_list: list[str], temperature: float = 0.0):
        return asyncio.run(
            dispatch_ollama_prompt_requests(
                prompt_list,
                self.model_name,
                temperature,
                self.max_new_tokens,
                self.stop_words
            )
        )

    def batch_generate(self, messages_list: list[str], temperature: float = 0.0):
        # Default to chat interface
        return self.batch_chat_generate(messages_list, temperature)

    def generate_insertion(self, input_string: str, suffix: str, temperature: float = 0.0) -> str:
        # No direct insertion API; we append suffix and use prompt_generate
        combined_prompt = f"{input_string}\n{suffix}"
        return self.prompt_generate(combined_prompt, temperature)


if __name__ == "__main__":
    model = OllamaModel(model_name="qwen3:4b", stop_words=["\n"], max_new_tokens=128)

    # print(model.generate("Tell me a fun fact."))

    batch = model.batch_generate([
                "Write a haiku about autumn.",
                "Summarize the plot of Macbeth."
            ])

    print(len(batch))
    print(batch)