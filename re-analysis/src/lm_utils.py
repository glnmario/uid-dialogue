import logging
import torch
from torch.utils.data import DataLoader
from tqdm import tqdm

logger = logging.getLogger(__name__)


def pad(tokenizer, batch, attention_mask=True):
    max_len = max([x['input_ids'].shape[1] for x in batch])
    if tokenizer.pad_token_id:
        pad_id = tokenizer.pad_token_id
    else:
        pad_id = tokenizer.eos_token_id

    padded_input_ids = []
    if attention_mask:
        padded_attention_masks = []

    for x in batch:
        x_len = x['input_ids'].shape[1]
        difference = max_len - x_len
        padded_input_ids.append(torch.cat((
            x['input_ids'],
            torch.tensor(difference * [pad_id], dtype=torch.long).unsqueeze(0)),
            dim=1))
        if attention_mask:
            padded_attention_masks.append(torch.tensor(x_len * [1] + difference * [0], dtype=torch.long).unsqueeze(0))

    if attention_mask:
        return {'input_ids': torch.cat(padded_input_ids, dim=0),
                'attention_mask': torch.cat(padded_attention_masks, dim=0)}
    else:
        return {'input_ids': torch.cat(padded_input_ids, dim=0)}


class DecontextualisedDataset(torch.utils.data.Dataset):

    def __init__(self, dataframe, tokenizer, max_seq_len):
        super(DecontextualisedDataset).__init__()
        self.data = []
        self.tokenizer = tokenizer

        logger.warning('Tokenize...')
        for idx, row in tqdm(dataframe.iterrows(), total=len(dataframe)):

            try:
                sentence = self.tokenizer.eos_token + row['text']
            except TypeError:
                sentence = self.tokenizer.eos_token

            inputs = self.tokenizer(
                sentence,
                return_tensors='pt',
                truncation=True,
                add_special_tokens=False,
                max_length=max_seq_len
            )
            self.data.append((idx, inputs))

    def __len__(self):
        return len(self.data)

    def __getitem__(self, index):
        return self.data[index]


class ContextualisedDataset(torch.utils.data.Dataset):

    def __init__(self, dataframe, tokenizer, max_seq_len, context_field='id', add_special_tokens=True):
        super(ContextualisedDataset).__init__()
        self.data = []
        self.tokenizer = tokenizer
        self.add_special_tokens = add_special_tokens
        self.max_seq_len = max_seq_len
        self.current_document = None

        logger.warning('Tokenize...')

        self.reset_current_document_context()
        current_context = ''

        for idx, row in tqdm(dataframe.iterrows(), total=len(dataframe)):
            try:
                sentence = row['text'] + self.tokenizer.eos_token
            except TypeError:
                logger.warning('Empty sentence?  {}'.format(row['text']))
                sentence = self.tokenizer.eos_token

            inputs = self.tokenizer(
                sentence,
                return_tensors='pt',
                truncation=True,
                add_special_tokens=False,
                max_length=max_seq_len
            )

            if row[context_field] != current_context:
                self.reset_current_document_context()
                current_context = row[context_field]

            contextualised_inputs = {
                'input_ids': torch.cat((self.current_document['input_ids'], inputs['input_ids']), 1),
                'attention_mask': torch.cat((self.current_document['attention_mask'], inputs['attention_mask']), 1),
            }

            if contextualised_inputs['input_ids'].shape[1] <= self.max_seq_len:
                start_index = self.current_document['prev_end_index']
                end_index = self.current_document['prev_end_index'] + inputs['input_ids'].shape[1]

            # cut off context if longer than max_seq_len
            else:
                contextualised_inputs = {
                    'input_ids': contextualised_inputs['input_ids'][:, -self.max_seq_len:],
                    'attention_mask': contextualised_inputs['attention_mask'][:, -self.max_seq_len:]
                }
                start_index = self.max_seq_len - inputs['input_ids'].shape[1]
                end_index = self.max_seq_len

            self.data.append((
                idx,
                contextualised_inputs.copy(),
                start_index
            ))
            self.current_document = {
                'input_ids': contextualised_inputs['input_ids'],
                'attention_mask': contextualised_inputs['attention_mask'],
                'prev_end_index': end_index
            }

    def reset_current_document_context(self):
        self.current_document = self.tokenizer(
            self.tokenizer.eos_token, add_special_tokens=False, return_tensors='pt')
        self.current_document['prev_end_index'] = 0

    def __len__(self):
        return len(self.data)

    def __getitem__(self, index):
        return self.data[index]
