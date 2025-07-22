import json
import sys
import re
import os
from tqdm import tqdm
from collections import OrderedDict
from typing import Dict, List, Tuple
from utils import OpenAIModel
from ollama_utils import OllamaModel
import argparse
import requests
import janus_swi as janus
from pathlib import Path
from typing import Union
from dotenv import load_dotenv
load_dotenv(verbose=True)

api_key = os.environ['OPENAI_API_KEY']

model_name = "gpt-4"
max_new_tokens = 2048
kb_file_path = "/home/malineni/PycharmProjects/ProLLM/src/myProLM/kbst.pl"


def display(json_data:dict):
    try:
        display_server_url = 'http://127.0.0.1:8082/display'
        server_b_response = requests.post(display_server_url, json=json_data)
        # Check if Server B responded successfully (e.g., status code 200 OK)
        server_b_response.raise_for_status()
        print("Displayed results")
    except Exception as e:
        print(f"Error displaying results: {e}")

class ContactLLM:
    def __init__(self, api_key, model_name, max_new_tokens, stop_words="------"):
        self.openai_api = OpenAIModel(api_key, model_name, stop_words, max_new_tokens)
        self.ollama = OllamaModel(model_name="qwen3:14b", stop_words=["\n"], max_new_tokens=10000)
        # self.gen_endpoint = "http://127.0.0.1:8081/generate"
        # self.build_endpoint = "http://127.0.0.1:8081/build"
        # self.reason_endpoint = "http://127.0.0.1:8081/reason"

    def query_llm(self, input_text: str) -> dict:
        try:
            headers = {
                "Content-Type": "application/json"
            }
            payload = {
                "instruction": input_text
            }
            response = requests.post("http://127.0.0.1:8081/generate", headers=headers, json=payload)
            response.raise_for_status()  # Raise an exception for bad status codes
            # print(f"flask response type is {type(response.json())}")
            # print(f"flask response text type is {type(response.text)}")

            self.first_response = response.json()

            full_prompt = f"""
            "Generate a concise, factual description of a robotic manipulation activity and its involved entities solely from the provided JSON data.

            ** Description should include: **

            - Activity Performed: State the core action ('action core').

            - Action Sequence: Detail the steps in the 'cram plan' chronologically.

            - Entity Details: For each entity involved, list its specific properties (e.g., color, shape, material, texture, source/position) based on 'enriched information'.

            *** Strictly avoid any external information, assumptions, or narrative phrasing. ***

            ---

            The JSON information about the action performed is given as, \n

            {self.first_response}
            """

            # second_response = self.openai_api.generate(full_prompt)
            second_response = self.ollama.generate(full_prompt)

            self.first_response['task_description'] = second_response

            payload2 = {
                "instruction": self.first_response['instruction'],
                "action_core": self.first_response['action_core'],
                "cram_plan_response": self.first_response['cram_plan_response'],
                "enriched_action_core_attributes": self.first_response['enriched_action_core_attributes']
            }

            # response2 = requests.post("http://127.0.0.1:8081/build", headers=headers, json=payload2)
            #
            # self.first_response['models_data'] = response2.json()

            # with open("/home/malineni/PycharmProjects/ProLLM/src/logicLM/Logic-LLM/data/AD/llm_first_response.json", 'w') as f:
            #     json.dump(self.first_response, f, indent=2, ensure_ascii=False)
            with open("./data/AD/llm_first_response_ollama.json", 'w') as f:
                json.dump(self.first_response, f, indent=2, ensure_ascii=False)

            # return response.json()
            return self.first_response

        except Exception as e:
            print("LLM query failed: %s" % str(e))
            return None


class LogicProgramGenerator:
    def __init__(self, dataset_name, api_key, model_name, split='dev', data_path='./data',
                 save_path='./outputs/logic_programs', stop_words='------', max_new_tokens=1024):
        # self.args = args
        self.data_path = data_path
        self.dataset_name = dataset_name
        self.split = split
        self.model_name = model_name
        self.save_path = save_path

        self.openai_api = OpenAIModel(api_key, model_name, stop_words, max_new_tokens)
        self.ollama_api = OllamaModel(model_name="qwen3:14b", stop_words=["\n"], max_new_tokens=10000)
        self.prompt_creator = {'FOLIO': self.prompt_folio,
                               'AD': self.prompt_action_designator}
        self.load_prompt_templates()

    def load_prompt_templates(self):
        prompt_file = f'./models/prompts/{self.dataset_name}.txt'
        prompt_file2 = ""
        if self.dataset_name == 'AR-LSAT' and self.model_name == 'gpt-4':
            prompt_file = f'./models/prompts/{self.dataset_name}-long.txt'
        elif self.dataset_name == 'AD':
            prompt_file = "./prompts/AD.txt"
            prompt_file2 = "./prompts/AD_models.txt"
        with open(prompt_file, 'r') as f:
            self.prompt_template = f.read()

        if prompt_file2:
            with open(prompt_file2, 'r') as f:
                self.prompt_template2 = f.read()

    def prompt_action_designator(self, test_data):
        task_description = test_data['task_description'].strip()
        # question = test_data['question'].strip()
        full_prompt = self.prompt_template.replace('[[TASK_DESCRIPTION]]', task_description)
        return full_prompt

    def prompt_action_designator_models(self, test_data):
        models_data = test_data['models_data']
        flanagan_data = models_data['flanagan']
        # question = test_data['question'].strip()
        full_prompt = self.prompt_template2.replace('[[FLANAGAN_MODEL_JSON]]', str(flanagan_data))
        return full_prompt

    def prompt_folio(self, test_data):
        problem = test_data['context']
        question = test_data['question'].strip()
        full_prompt = self.prompt_template.replace('[[PROBLEM]]', problem).replace('[[QUESTION]]', question)
        return full_prompt

    def load_raw_dataset(self, split):
        try:
            with open("/home/malineni/PycharmProjects/ProLLM/src/logicLM/Logic-LLM/data/AD/llm_first_response.json",'r') as f:
                raw_dataset = [json.load(f)]
        except:
            print("File not read/found")
        # with open(os.path.join(self.data_path, self.dataset_name, f'{split}.json')) as f:
        #     raw_dataset = json.load(f)
        return raw_dataset

    def ad_logic_program_generation(self, full=False):
        # load raw dataset
        raw_dataset = self.load_raw_dataset(self.split)
        # print(f"Loaded {len(raw_dataset)} examples from {self.split} split.")

        outputs = []
        for example in tqdm(raw_dataset):
            # create prompt
            try:
                full_prompt = self.prompt_creator[self.dataset_name](example)
                output = self.openai_api.generate(full_prompt)
                # print(full_prompt)
                programs = [output]

                # create output
                example['raw_logic_programs'] = programs

                # output = {'id': example['id'],
                #           'context': example['context'],
                #           'question': example['question'],
                #           'answer': example['answer'],
                #           'options': example['options'],
                #           'raw_logic_programs': programs}
                display({'raw_logic_programs': programs})  # To display on server 8082
                outputs.append(example)
            except:
                print('Error in generating logic programs for example action ')

            if full:
                try:
                    full_prompt = self.prompt_action_designator_models(example)
                    output = self.openai_api.generate(full_prompt)
                    # print(full_prompt)
                    programs = [output]

                    # create output
                    example['raw_logic_programs_models'] = programs

                    outputs.append(example)
                except:
                    print('Error in generating logic programs for example models ')

        # save outputs
        # with open(os.path.join(self.save_path, f'{self.dataset_name}_{self.split}_{self.model_name}.json'), 'w') as f:
        #     json.dump(outputs, f, indent=2, ensure_ascii=False)

        with open(
                f"/home/malineni/PycharmProjects/ProLLM/src/logicLM/Logic-LLM/outputs/AD_logic_programs/AD_{self.model_name}.json",
                'w') as f:
            json.dump(outputs, f, indent=2, ensure_ascii=False)

    def prolog_kb_generation(self):
        # load raw dataset
        with open(
                f"/home/malineni/PycharmProjects/ProLLM/src/logicLM/Logic-LLM/outputs/AD_logic_programs/AD_{self.model_name}.json",
                'r') as f:
            FOL_dataset = json.load(f)

        outputs = []

        with open(
                "/home/malineni/PycharmProjects/ProLLM/src/logicLM/Logic-LLM/models/prompts/AD_FOL_to_Prolog_template.txt",
                'r') as f:
            FOL_Prolog_prompt_template = f.read()

        for example in tqdm(FOL_dataset):
            # create prompt
            try:
                raw_logic_programs = example['raw_logic_programs'][0]
                full_prompt = FOL_Prolog_prompt_template.replace('[[FOL_block]]', raw_logic_programs)
                # full_prompt = self.prompt_creator[self.dataset_name](example)
                output = self.openai_api.generate(full_prompt)
                # print(full_prompt)
                uncleaned_prologs = output
                prologs = ""
                if uncleaned_prologs.startswith('```prolog'):
                    lines = uncleaned_prologs.splitlines()
                    prologs = '\n'.join(lines[1:]).lstrip()
                # create output
                example['prologs'] = prologs
                # output = {'id': example['id'],
                #           'context': example['context'],
                #           'question': example['question'],
                #           'answer': example['answer'],
                #           'options': example['options'],
                #           'raw_logic_programs': example['raw_logic_programs'],
                #           'prologs': prologs}
                display({'prologs': prologs})  # To display on server 8082
                outputs.append(example)
            except:
                print('Error in generating logic programs for example: ', example['id'])

        with open(
                f"/home/malineni/PycharmProjects/ProLLM/src/logicLM/Logic-LLM/outputs/AD_logic_programs/AD_{self.model_name}_facts.json",
                'w') as f:
            json.dump(outputs, f, indent=2, ensure_ascii=False)

        with open(
                f"/home/malineni/PycharmProjects/ProLLM/src/logicLM/Logic-LLM/outputs/AD_logic_programs/AD_{self.model_name}_kb.pl",
                'w') as f:
            for out in outputs:
                f.writelines(out['prologs'] + '\n')

    def knowledge_generator(self, basic_designator_json):
        try:
            task_description = basic_designator_json['task_description'].strip()
            full_prompt = self.prompt_template.replace('[[TASK_DESCRIPTION]]', task_description)
            # output = self.openai_api.generate(full_prompt)
            output = self.ollama_api.generate(full_prompt + " \nothink")
            programs = output
            basic_designator_json['raw_logic_programs'] = programs

            with open("./prompts/AD_FOL_to_Prolog_template.txt", 'r') as f:
                FOL_Prolog_prompt_template = f.read()

            try:
                raw_logic_programs = basic_designator_json['raw_logic_programs']
                full_prompt = FOL_Prolog_prompt_template.replace('[[FOL_block]]', raw_logic_programs)
                # output = self.openai_api.generate(full_prompt)
                output = self.ollama_api.generate(full_prompt + " \nothink")
                uncleaned_prologs = output
                prologs = re.sub(r"```[a-zA-Z0-9]*\s*", "", uncleaned_prologs).strip()
                # if uncleaned_prologs.startswith('```prolog'):
                #     lines = uncleaned_prologs.splitlines()
                #     prologs = '\n'.join(lines[1:]).lstrip()
                # create output
                basic_designator_json['prologs'] = prologs

                # file_path = "./temp_outputs/kbst.pl"
                if os.path.exists(kb_file_path):
                    os.remove(kb_file_path)

                # with open(f"./temp_outputs/kbst.pl", 'w') as f:
                with open(kb_file_path, 'w') as f:
                    f.write(':- module(kbst).\n\n')
                    f.writelines(basic_designator_json['prologs'] + '\n')

            except:
                print('Error in generating logic programs for example: (prologs) ')

        except:
            print('Error in generating logic programs for example action (task_description)')


class LogicProgramInference:
    def __init__(self, kb_file_path, api_key, model_name, stop_words='------', max_new_tokens=2048):
        self.kb_file_path = kb_file_path
        self.openai_api = OpenAIModel(api_key, model_name, stop_words=stop_words, max_new_tokens=max_new_tokens)
        self.ollama_api = OllamaModel(model_name="qwen3:8b", stop_words=["\n"], max_new_tokens=10000)

    def load_kb(self):
        facts = []
        try:
            with open(self.kb_file_path, "r") as f:
                for line in f:
                    line = line.strip()
                    if line and not line.startswith('%'):  # ignore comments
                        facts.append(line)
        except FileNotFoundError:
            # If KB doesn't exist, return empty
            pass
        return facts

    def handle_enquire(self, designator: str, prompt: str):
        # Load knowledge base (for now, just a list of facts)
        kb_facts = self.load_kb()
        kb_facts_str = '\n'.join(kb_facts)
        # Here you'd do something meaningful with designator/prompt/kb_facts
        # with open("/home/malineni/PycharmProjects/ProLLM/src/logicLM/Logic-LLM/models/prompts/AD_Inference.txt",'r') as f:
        with open("./prompts/AD_Inference_query_output.txt",'r') as f:
            Prolog_LLM_Inference = f.read()

        full_prompt = (Prolog_LLM_Inference.replace('[[FACTS]]', kb_facts_str)
                                            .replace('[[DESIGNATOR]]',designator)
                                            .replace('[[QUESTION]]', prompt))
        # output = self.openai_api.generate(full_prompt)
        output = self.ollama_api.generate(full_prompt + " \nothink")

        # For now, just return a dummy answer showing you received them
        # answer = f"Received designator: {designator}, prompt: {prompt}, kb_facts_count: {len(kb_facts)}"
        return {"answer": output}

def gen_action_designator(prompt):
    """Function to be called from Prolog via janus"""
    if not prompt:
        return {"error": "No prompt provided"}

    llm = ContactLLM(api_key=api_key, model_name=model_name, max_new_tokens=max_new_tokens)
    try:
        res = llm.query_llm(prompt)
        # res is a dictionary with 'cram_plan_response'
        if isinstance(res, dict) and "cram_plan_response" in res:
            try:
                logic_program_generator = LogicProgramGenerator(api_key=api_key,
                                                                model_name=model_name,
                                                                max_new_tokens=max_new_tokens,
                                                                dataset_name='AD')
                logic_program_generator.knowledge_generator(res)
            except Exception as e:
                return {"error": str(e)}


            # Saving Query results in JSON file (remove this later)
            # save = {"designator": res["cram_plan_response"]}
            # with open("/home/malineni/PycharmProjects/ProLLM/src/logicLM/Logic-LLM/models/temp_outputs/query_outputs.json","w") as f:
            #     json.dump(save, f, indent=2, ensure_ascii=False)

            # Return the dictionary directly (janus handles JSON conversion)
            output = {"cram_plan_response": res["cram_plan_response"],
                      "kb_file": kb_file_path}

            return output
        else:
            return {"error": "cram_plan_response not found"}
    except Exception as e:
        return {"error": str(e)}


def enquire_designator(data):
    """Function to be called from Prolog via janus for enquire_designator"""
    designator = data.get("designator", "")
    prompt = data.get("prompt", "")

    # instantiate your inference class with the KB file you generated
    # kb_file = "/home/malineni/PycharmProjects/ProLLM/src/logicLM/Logic-LLM/models/temp_outputs/kbst.pl"
    inferencer = LogicProgramInference(kb_file_path, api_key=api_key, model_name=model_name)

    try:
        result = inferencer.handle_enquire(designator, prompt)
        pquery = re.sub(r'^\?-\s*', '', result['answer'])
        # presult = run_prolog_query(kb_file_path, pquery)
        # return {"answer": presult, "prolog_query": pquery}
        return {'prolog_query':pquery}
    except Exception as e:
        return {"error": str(e)}



def run_prolog_query(kb_path: Union[str, Path], query: str):
    try:
        janus.consult("kbst.pl")
        result = janus.query_once(query)
        return {'answer':result}
    except Exception as e:
        return {"error": str(e)}


# Keep the original main for backward compatibility or direct script execution
if __name__ == '__main__':

    print(run_prolog_query(kb_path=kb_file_path, query="property(obj_bottle, color, X)."))
#     # ... your existing main code remains the same ...
#     args = sys.argv[1:]
#
#     if len(args) > 0 and args[0] == "gen_action_designator":
#         prompt = sys.stdin.read().strip()
#         result = gen_action_designator(prompt)
#         print(json.dumps(result, ensure_ascii=False))
#         sys.exit(0)
#
#     elif len(args) > 0 and args[0] == "enquire_designator":
#         raw = sys.stdin.read().strip()
#         if not raw:
#             print(json.dumps({"error": "No input provided"}))
#             sys.exit(1)
#
#         try:
#             data = json.loads(raw)
#         except json.JSONDecodeError as e:
#             print(json.dumps({"error": f"Invalid JSON: {str(e)}"}))
#             sys.exit(1)
#
#         result = enquire_designator(data)
#         print(json.dumps(result, ensure_ascii=False))
#         sys.exit(0)
#
#     else:
# # ... your existing mode 2 code ...
