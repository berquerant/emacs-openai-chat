# emacs-openai-chat

Chat completion with emacs.

# Usage

Select region
```
Hello!
```
and `openai-chat-start-region` then you got
```
user>
Hello!
---
assistant>
Hello there! How can I assist you today?
```
in `*openai-chat*` buffer.
Append to the buffer:
```
user>
Hello!
---
assistant>
Hello there! How can I assist you today?
---
user>
Do you know ELIZA?
```
and `mark-whole-buffer`, `openai-chat-start-region` then

```
user>
Hello!
---
assistant>
Hello there! How can I assist you today?
---
user>
Do you know ELIZA?
---
assistant>
Yes, ELIZA is a natural language processing program created in the 1960s by MIT professor Joseph Weizenbaum. It was one of the first attempts at creating a chatbot, and it simulated a Rogerian psychotherapist to engage in conversations with people. ELIZA was designed to use pattern matching and scripted responses to simulate a conversation with a human. It was groundbreaking in its time and paved the way for the development of more advanced chatbot technology, which we see today.
```
in the buffer.

# Requirements

## `OPENAI_API_KEY`

The environment variable that contains the api key.

# Customize

## `openai-chat-chat-completion-timeout`

Increase this value if frequent API timeouts occur.

## Others

See `customize`.
