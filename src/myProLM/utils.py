from __future__ import annotations
import backoff  # for exponential backoff
from openai import OpenAI, AsyncOpenAI
from openai import RateLimitError, APIError
import os
import asyncio
from typing import Any

# Global clients - will be initialized when OpenAIModel is created
client = None
async_client = None


@backoff.on_exception(backoff.expo, RateLimitError)
def completions_with_backoff(**kwargs):
    return client.completions.create(**kwargs)


@backoff.on_exception(backoff.expo, RateLimitError)
def chat_completions_with_backoff(**kwargs):
    return client.chat.completions.create(**kwargs)


async def dispatch_openai_chat_requests(
        messages_list: list[list[dict[str, Any]]],
        model: str,
        temperature: float,
        max_tokens: int,
        top_p: float,
        stop_words: list[str]
) -> list[str]:
    """Dispatches requests to OpenAI API asynchronously.

    Args:
        messages_list: List of messages to be sent to OpenAI ChatCompletion API.
        model: OpenAI model to use.
        temperature: Temperature to use for the model.
        max_tokens: Maximum number of tokens to generate.
        top_p: Top p to use for the model.
        stop_words: List of words to stop the model from generating.
    Returns:
        List of responses from OpenAI API.
    """

    async def make_request(messages):
        return await async_client.chat.completions.create(
            model=model,
            messages=messages,
            temperature=temperature,
            max_tokens=max_tokens,
            top_p=top_p,
            stop=stop_words
        )

    tasks = [make_request(x) for x in messages_list]
    return await asyncio.gather(*tasks)


async def dispatch_openai_prompt_requests(
        messages_list: list[list[dict[str, Any]]],
        model: str,
        temperature: float,
        max_tokens: int,
        top_p: float,
        stop_words: list[str]
) -> list[str]:
    async def make_request(prompt):
        return await async_client.completions.create(
            model=model,
            prompt=prompt,
            temperature=temperature,
            max_tokens=max_tokens,
            top_p=top_p,
            frequency_penalty=0.0,
            presence_penalty=0.0,
            stop=stop_words
        )

    tasks = [make_request(x) for x in messages_list]
    return await asyncio.gather(*tasks)


class OpenAIModel:
    def __init__(self, API_KEY, model_name, stop_words, max_new_tokens) -> None:
        global client, async_client

        # Initialize clients with API key
        client = OpenAI(api_key=API_KEY)
        async_client = AsyncOpenAI(api_key=API_KEY)

        self.model_name = model_name
        self.max_new_tokens = max_new_tokens
        self.stop_words = stop_words

    # used for chat-gpt and gpt-4
    def chat_generate(self, input_string, temperature=0.0):
        response = chat_completions_with_backoff(
            model=self.model_name,
            messages=[
                {"role": "user", "content": input_string}
            ],
            max_tokens=self.max_new_tokens,
            temperature=temperature,
            top_p=1.0,
            stop=self.stop_words
        )
        generated_text = response.choices[0].message.content.strip()
        return generated_text

    # used for text/code-davinci
    def prompt_generate(self, input_string, temperature=0.0):
        response = completions_with_backoff(
            model=self.model_name,
            prompt=input_string,
            max_tokens=self.max_new_tokens,
            temperature=temperature,
            top_p=1.0,
            frequency_penalty=0.0,
            presence_penalty=0.0,
            stop=self.stop_words
        )
        generated_text = response.choices[0].text.strip()
        return generated_text

    def generate(self, input_string, temperature=0.0):
        # Updated model names for current OpenAI models
        if self.model_name in ['text-davinci-002', 'code-davinci-002', 'text-davinci-003', 'gpt-3.5-turbo-instruct']:
            return self.prompt_generate(input_string, temperature)
        elif self.model_name in ['gpt-4', 'gpt-3.5-turbo', 'gpt-4-turbo', 'gpt-4o', 'gpt-4o-mini']:
            return self.chat_generate(input_string, temperature)
        else:
            raise Exception(f"Model name '{self.model_name}' not recognized")

    def batch_chat_generate(self, messages_list, temperature=0.0):
        open_ai_messages_list = []
        for message in messages_list:
            open_ai_messages_list.append(
                [{"role": "user", "content": message}]
            )
        predictions = asyncio.run(
            dispatch_openai_chat_requests(
                open_ai_messages_list, self.model_name, temperature, self.max_new_tokens, 1.0, self.stop_words
            )
        )
        return [x.choices[0].message.content.strip() for x in predictions]

    def batch_prompt_generate(self, prompt_list, temperature=0.0):
        predictions = asyncio.run(
            dispatch_openai_prompt_requests(
                prompt_list, self.model_name, temperature, self.max_new_tokens, 1.0, self.stop_words
            )
        )
        return [x.choices[0].text.strip() for x in predictions]

    def batch_generate(self, messages_list, temperature=0.0):
        if self.model_name in ['text-davinci-002', 'code-davinci-002', 'text-davinci-003', 'gpt-3.5-turbo-instruct']:
            return self.batch_prompt_generate(messages_list, temperature)
        elif self.model_name in ['gpt-4', 'gpt-3.5-turbo', 'gpt-4-turbo', 'gpt-4o', 'gpt-4o-mini']:
            return self.batch_chat_generate(messages_list, temperature)
        else:
            raise Exception(f"Model name '{self.model_name}' not recognized")

    def generate_insertion(self, input_string, suffix, temperature=0.0):
        # Note: Text insertion is only supported by certain models
        response = completions_with_backoff(
            model=self.model_name,
            prompt=input_string,
            suffix=suffix,
            temperature=temperature,
            max_tokens=self.max_new_tokens,
            top_p=1.0,
            frequency_penalty=0.0,
            presence_penalty=0.0
        )
        generated_text = response.choices[0].text.strip()
        return generated_text