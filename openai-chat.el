;;; openai-chat.el --- Chat with openai -*- lexical-binding: t -*-

;; Author: berquerant
;; Maintainer: berquerant
;; Package-Requires: ((request "0.3.2") (s "1.13.0"))
;; Created: 8 Apr 2023
;; Version: 0.1.0
;; Keywords: openai
;; URL: https://github.com/berquerant/emacs-openai-chat

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'request) ; https://github.com/tkf/emacs-request
(require 's)       ; https://github.com/magnars/s.el

;;
;; variables
;;

(defgroup openai-chat nil
  "OpenAI chat."
  :prefix "openai-chat-"
  :group 'openai-chat)

(defcustom openai-chat-history-file nil
  "File to save the chat history.
If nil, not saved."
  :type 'file)

(defcustom openai-chat-buffer-name "*openai-chat*"
  "Buffer to save the chat thread history."
  :type 'string)

(defcustom openai-chat-history-buffer-name "*openai-chat-history*"
  "Buffer to save the chat history."
  :type 'string)

(defcustom openai-chat-debug-buffer-name "*openai-chat-debug*"
  "Buffer for debug log."
  :type 'string)

(defcustom openai-chat-model "gpt-3.5-turbo"
  "ID of the model to use."
  :type 'string)

(defcustom openai-chat-temperature 1
  "What sampling temperature to use."
  :type 'number)

(defcustom openai-chat-default-user-role "user"
  "Default chat user role."
  :type 'string)

(defcustom openai-chat-message-role-separator ">"
  "Separate role and content."
  :type 'string)

(defcustom openai-chat-message-separator "\n---\n"
  "Separate messages."
  :type 'string)

(defcustom openai-chat-api-key-env "OPENAI_API_KEY"
  "Name of the environment variable that contains the api key."
  :type 'string)

(defcustom openai-chat-chat-completion-endpoint "https://api.openai.com/v1/chat/completions"
  "https://platform.openai.com/docs/api-reference/chat/create"
  :type 'string)

(defcustom openai-chat-chat-completion-timeout 30
  "HTTP timeout (second)."
  :type 'number)

;;
;; utilities
;;

(defun openai-chat--timestamp ()
  "Return a timestamp of the current time (second)."
  (let ((ts (current-time)))
    (+ (* (car ts) (expt 2 16)) (cadr ts))))

(defun openai-chat--datetime (&optional time zone)
  "Convert TIME (timestamp) in ZONE (timezone) into string.
default TIME is now, ZONE is here."
  (format-time-string "%F %T"
                      (or time (current-time))
                      (or zone (current-time-zone))))

(defun openai-chat--jsonify (alist-or-hash)
  (json-encode alist-or-hash))

(defun openai-chat--split-string (string separator)
  "Split STRING by SEPARATOR."
  (s-split (regexp-quote separator) string))

(defun openai-chat--split-string-1 (string separator)
  "Split STRING by SEPARATOR at most once."
  (s-split-up-to (regexp-quote separator) string 1 t))

;;
;; output functions
;;

(defun openai-chat--overwrite-buffer (input buffer)
  "Truncate BUFFER and write INPUT into BUFFER."
  (with-current-buffer buffer
    (erase-buffer)
    (insert input)))

(defun openai-chat--append-buffer (input buffer)
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert input)))

(defun openai-chat--append-file (input filename)
  (when filename
    (write-region input nil openai-chat-history-file t 'silent)))

(defun openai-chat--overwrite-chat-buffer (input)
  (openai-chat--overwrite-buffer input
                                 (get-buffer-create openai-chat-buffer-name)))

(defun openai-chat--append-to-history-file (input)
  (when openai-chat-history-file
    (openai-chat--append-file input
                              openai-chat-history-file)))

(defun openai-chat--append-to-history-buffer (input)
  (openai-chat--append-buffer input
                              (get-buffer-create openai-chat-history-buffer-name)))

(defun openai-chat--append-to-debug-buffer (input)
  (openai-chat--append-buffer input
                              (get-buffer-create openai-chat-debug-buffer-name)))

;;
;; structures
;;

(cl-defstruct openai-chat--chat-message
  "Role and Content pair."
  (role
   nil
   :read-only t
   :type string)
  (content
   nil
   :read-only t
   :type string))

(defun openai-chat--chat-message-from-string (string separator default-role)
  (if (s-contains? separator string)
      (let ((ss (openai-chat--split-string-1 string separator)))
        (if (= (length ss) 2)
            (make-openai-chat--chat-message :role (nth 0 ss)
                                       :content (nth 1 ss))
          (error "Incomplete message %s, no content?" string)))
    (make-openai-chat--chat-message :role default-role
                                    :content string)))

(defun openai-chat--chat-message-into-alist (message)
  (unless (openai-chat--chat-message-p message)
    (error "Cannot convert %S into alist, message is not openai-chat--chat-message" message))
  `(("role" . ,(openai-chat--chat-message-role message))
    ("content" . ,(openai-chat--chat-message-content message))))

(defun openai-chat--chat-message-into-string (message separator)
  (unless (openai-chat--chat-message-p message)
    (error "Cannot convert %S into string, message is not openai-chat--chat-message" message))
  (s-join separator (list (openai-chat--chat-message-role message)
                          (openai-chat--chat-message-content message))))

(cl-defstruct openai-chat--chat-request
  "Request body."
  (messages
   nil
   :documentation "List of `openai-chat--chat-message'."
   :read-only t
   :type list)
  (model
   nil
   :documentation "ID of the model to use."
   :read-only t
   :type string)
  (temperature
   nil
   :documentation "What sampling temperature to use, between 0 and 2."
   :read-only t
   :type number))

(defun openai-chat--chat-messages-from-string (string message-separator role-separator default-role)
  (cl-loop for s in (openai-chat--split-string
                     string
                     message-separator)
           collect (openai-chat--chat-message-from-string
                    s
                    role-separator
                    default-role)))

(defun openai-chat--chat-request-from-string
    (string
     message-separator
     role-separator
     model
     temperature
     default-role)
  (let ((messages (openai-chat--chat-messages-from-string string message-separator role-separator default-role)))
    (make-openai-chat--chat-request :model model
                                    :temperature temperature
                                    :messages messages)))

(defun openai-chat--chat-messages-into-alist (messages)
  (cl-loop for msg in messages
           collect (openai-chat--chat-message-into-alist msg)))

(defun openai-chat--chat-messages-into-string (messages message-separator role-separator)
  (s-join message-separator (cl-loop for msg in messages
                                     collect (openai-chat--chat-message-into-string msg role-separator))))

(defun openai-chat--chat-request-into-alist (req)
  (unless (openai-chat--chat-request-p req)
    (error "Cannot convert %S into alist, req is not openai-chat--chat-request" req))
  `(("model" . ,(openai-chat--chat-request-model req))
    ("temperature" . ,(openai-chat--chat-request-temperature req))
    ("messages" . ,(openai-chat--chat-messages-into-alist (openai-chat--chat-request-messages req)) )))

(cl-defstruct openai-chat--chat-client
  "API Client."
  (endpoint
   nil
   :documentation "Chat Completion API endpoint."
   :type string)
  (timeout
   nil
   :documentation "HTTP timeout (second)."
   :type number)
  (api-key
   nil
   :documentation "OpenAI API Key."
   :type string))

(defun openai-chat--advice-around-ignore-return-value (orig-func &rest args)
  (apply orig-func args)
  nil)

(defun openai-chat--chat-client-send-request (client req message-separator role-separator)
  "Send API request REQ (`openai-chat--chat-request') by CLIENT (`openai-chat--chat-client').
Return `openai-chat--chat-response'."
  (unless (openai-chat--chat-client-p client)
    (error "Cannot send request by %S, client is not openai-chat--chat-client" client))
  (unless (openai-chat--chat-request-p req)
    (error "Cannot send request with %S, req is not openai-chat--chat-request" req))
  (let ((req-time (openai-chat--timestamp)))
    (advice-add 'request :around 'openai-chat--advice-around-ignore-return-value) ; turn off noisy logs on *Messages*
    (request
      (openai-chat--chat-client-endpoint client)
      :timeout (openai-chat--chat-client-timeout client)
      :type "POST"
      :headers `(("Content-Type" . "application/json")
                 ("Authorization" . ,(format "Bearer %s" (openai-chat--chat-client-api-key client))))
      :parser 'json-read
      :data (openai-chat--jsonify (openai-chat--chat-request-into-alist req))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (openai-chat--chat-client-send-request-callback data
                                                                  req
                                                                  req-time
                                                                  (openai-chat--timestamp)
                                                                  message-separator
                                                                  role-separator))))
    (advice-remove 'request 'openai-chat--advice-around-ignore-return-value))) ; restore request function

(defun openai-chat--chat-client-send-request-callback (data req req-time res-time message-separator role-separator)
  (openai-chat--append-to-debug-buffer (openai-chat--jsonify data))
  (openai-chat--append-to-debug-buffer "\n")
  (let ((response (make-openai-chat--chat-response :request req
                                                   :message (openai-chat--chat-client-get-message-from-raw data)
                                                   :request-timestamp req-time
                                                   :response-timestamp res-time)))
    (openai-chat--chat-response-write-to-history-file response)
    (openai-chat--chat-response-write-to-history-buffer response)
    (openai-chat--chat-response-write-to-chat-buffer response message-separator role-separator)))

(defun openai-chat--chat-client-get-message-from-raw (data)
  "Get reply message from raw response body DATA."
  (let* ((message (cdr (assoc 'message (aref (cdr (assoc 'choices data)) 0))))
         (role (cdr (assoc 'role message)))
         (content (cdr (assoc 'content message))))
    (make-openai-chat--chat-message :role role
                                    :content content)))

(cl-defstruct openai-chat--chat-response
  "Response body."
  (request
    nil
    :documentation "Request body of type `openai-chat--chat-request'."
    :read-only t)
  (request-timestamp
   nil
   :documentation "Timestamp at the start of the request."
   :read-only t
   :type integer)
  (response-timestamp
   nil
   :documentation "Timestamp at the end of the request."
   :read-only t
   :type integer)
  (message
   nil
   :documentation "Reply of type `openci-chat--chat-message'."
   :read-only t))

(defun openai-chat--chat-response-into-thread (response message-separator role-separator)
  (unless (openai-chat--chat-response-p response)
    (error "Cannot convert %S into thread, response is not openai-chat--chat-response" response))
  (let* ((req (openai-chat--chat-response-request response))
         (req-messages (openai-chat--chat-request-messages req))
         (res-message (openai-chat--chat-response-message response))
         (messages (nconc req-messages (list res-message))))
    (openai-chat--chat-messages-into-string messages message-separator role-separator)))

(defun openai-chat--chat-response-into-history (response)
  (unless (openai-chat--chat-response-p response)
    (error "Cannot convert %S into history, response is not openai-chat--chat-response" response))
  (let* ((req (openai-chat--chat-request-into-alist (openai-chat--chat-response-request response)))
         (req-ts (openai-chat--chat-response-request-timestamp response))
         (res-ts (openai-chat--chat-response-response-timestamp response))
         (duration (- res-ts req-ts))
         (req-time (openai-chat--datetime req-ts))
         (res-time (openai-chat--datetime res-ts))
         (msg (openai-chat--chat-message-into-alist (openai-chat--chat-response-message response))))
    (openai-chat--jsonify
     `(("request" . ,req)
       ("request-ts" . ,req-ts)
       ("response-ts" . ,res-ts)
       ("duration" . ,duration)
       ("request-time" . ,req-time)
       ("response-time" . ,res-time)
       ("message" . ,msg)))))

(defun openai-chat--chat-response-write-to-chat-buffer (response message-separator role-separator)
  (openai-chat--overwrite-chat-buffer (openai-chat--chat-response-into-thread response message-separator role-separator)))

(defun openai-chat--chat-response-write-to-history-file (response)
  (openai-chat--append-to-history-file (openai-chat--chat-response-into-history response))
  (openai-chat--append-to-history-file "\n"))

(defun openai-chat--chat-response-write-to-history-buffer (response)
  (openai-chat--append-to-history-buffer (openai-chat--chat-response-into-history response))
  (openai-chat--append-to-history-buffer "\n"))

;;
;; interfaces
;;

(defun openai-chat--api-key-from-env ()
  (getenv openai-chat-api-key-env))

(defun openai-chat-default-client ()
  (make-openai-chat--chat-client :endpoint openai-chat-chat-completion-endpoint
                                 :timeout openai-chat-chat-completion-timeout
                                 :api-key (openai-chat--api-key-from-env)))

(defun openai-chat-default-request (input)
  (make-openai-chat--chat-request :messages (openai-chat--chat-messages-from-string input
                                                                                    openai-chat-message-separator
                                                                                    openai-chat-message-role-separator
                                                                                    openai-chat-default-user-role)
                                  :model openai-chat-model
                                  :temperature openai-chat-temperature))

(defun openai-chat--start (input)
  (openai-chat--chat-client-send-request (openai-chat-default-client)
                                         (openai-chat-default-request input)
                                         openai-chat-message-separator
                                         openai-chat-message-role-separator))

;;
;; interactive functions
;;

;;;###autoload
(defun openai-chat-start-region (start end)
  "Send region INPUT to chat API.

Messages are separated by `openai-chat-message-separator',
and role and content are separated by `openai-chat-message-role-separator'.
Reply will be sent to `openai-chat-buffer-name' buffer.

e.g.

  user>Hello

is equal to

  [
    {\"role\": \"user\", \"content\": \"Hello\"}
  ]

  user>Hello
  ---
  assistant>
  Hello there, how may I assist you today?
  ---
  user>I have a headache

is equal to

  [
    {\"role\": \"user\", \"content\": \"Hello\"},
    {\"role\": \"assistant\", \"content\": \"\nHello there, how may I assist you today?\"},
    {\"role\": \"user\", \"content\": \"I have a headache\"}
  ]

  Hello

is equal to

  [
    {\"role\": \"user\", \"content\": \"Hello\"}
  ]

when `openai-chat-default-user-role' is \"user\"."
  (interactive "r")
  (openai-chat--start (buffer-substring start end)))

(provide 'openai-chat)
;;; openai-chat.el ends here
