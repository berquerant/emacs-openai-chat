;;; openai-chat-test.el --- unit test -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'openai-chat)

(defmacro test-chat-response-into-thread
    (name want response message-separator role-separator)
  (let ((testname (format "test-chat-response-into-thread-%s" name)))
    `(progn
       (ert-deftest ,(read testname)
           ()
         (should (equal ,want
                        (openai-chat--chat-response-into-thread ,response
                                                                ,message-separator
                                                                ,role-separator)))))))

(test-chat-response-into-thread "reply"
                                "user>hello
system>Hello there, how may I assist you today?"
                                (make-openai-chat--chat-response :request
                                                                 (make-openai-chat--chat-request :messages
                                                                                                 (list (make-openai-chat--chat-message :role
                                                                                                                                       "user"
                                                                                                                                       :content
                                                                                                                                       "hello")))
                                                                 :message
                                                                 (make-openai-chat--chat-message :role
                                                                                                 "system"
                                                                                                 :content
                                                                                                 "Hello there, how may I assist you today?"))
                                "\n"
                                ">")

(test-chat-response-into-thread "reply2"
                                "user>hello
system>Hello there, how may I assist you today?
user>I have a headache
system>Why do you say you have a headache?"
                                (make-openai-chat--chat-response :request
                                                                 (make-openai-chat--chat-request :messages
                                                                                                 (list (make-openai-chat--chat-message :role
                                                                                                                                       "user"
                                                                                                                                       :content
                                                                                                                                       "hello")
                                                                                                       (make-openai-chat--chat-message :role
                                                                                                                                       "system"
                                                                                                                                       :content
                                                                                                                                       "Hello there, how may I assist you today?")
                                                                                                       (make-openai-chat--chat-message :role
                                                                                                                                       "user"
                                                                                                                                       :content
                                                                                                                                       "I have a headache")))
                                                                 :message
                                                                 (make-openai-chat--chat-message :role
                                                                                                 "system"
                                                                                                 :content
                                                                                                 "Why do you say you have a headache?"))
                                "\n"
                                ">")

(defmacro test-chat-client-get-message-from-raw
    (name want data)
  (let ((testname (format "test-chat-client-get-message-from-raw-%s" name)))
    `(progn
       (ert-deftest ,(read testname)
           ()
         (should (equal ,want
                        (openai-chat--chat-client-get-message-from-raw ,data)))))))

(test-chat-client-get-message-from-raw "sample"
                                       (make-openai-chat--chat-message :role "assistant"
                                                                       :content "

Hello there, how may I assist you today?")
                                       '((id . "chatcmpl-123") (object . "chat.completion") (created . 1677652288) (choices . [((index . 0) (message (role . "assistant") (content . "

Hello there, how may I assist you today?")) (finish_reason . "stop"))]) (usage (prompt_tokens . 9) (completion_tokens . 12) (total_tokens . 21))))

(defmacro test-chat-messages-from-string
    (name want string message-separator role-separator default-role)
  (let ((testname (format "test-chat-messages-from-string-%s" name)))
    `(progn
       (ert-deftest ,(read testname)
           ()
         (should (equal ,want
                        (openai-chat--chat-messages-from-string ,string
                                                                ,message-separator
                                                                ,role-separator
                                                                ,default-role)))))))

(test-chat-messages-from-string "a-message"
                                (list (make-openai-chat--chat-message :role "r"
                                                                      :content "c"))
                                "r>c"
                                ">>"
                                ">"
                                "DEF")
(test-chat-messages-from-string "a-message-without-role"
                                (list (make-openai-chat--chat-message :role "DEF"
                                                                      :content "c"))
                                "c"
                                ">>"
                                ">"
                                "DEF")
(test-chat-messages-from-string "2-messages"
                                (list (make-openai-chat--chat-message :role "r1"
                                                                      :content "c1")
                                      (make-openai-chat--chat-message :role "r2"
                                                                      :content "c2"))
                                "r1>c1>>r2>c2"
                                ">>"
                                ">"
                                "DEF")
(test-chat-messages-from-string "3-messages"
                                (list (make-openai-chat--chat-message :role "r1"
                                                                      :content "c1")
                                      (make-openai-chat--chat-message :role "r2"
                                                                      :content "c2")
                                      (make-openai-chat--chat-message :role "r1"
                                                                      :content "c3"))
                                "r1>c1>>r2>c2>>r1>c3"
                                ">>"
                                ">"
                                "DEF")
(test-chat-messages-from-string "3-messages-with-default-role"
                                (list (make-openai-chat--chat-message :role "r1"
                                                                      :content "c1")
                                      (make-openai-chat--chat-message :role "DEF"
                                                                      :content "c2")
                                      (make-openai-chat--chat-message :role "r1"
                                                                      :content "c3"))
                                "r1>c1>>c2>>r1>c3"
                                ">>"
                                ">"
                                "DEF")

(defmacro test-chat-message-into-string
    (name want message separator)
  (let ((testname (format "test-chat-message-into-string-%s" name)))
    `(progn
       (ert-deftest ,(read testname)
           ()
         (should (equal ,want
                        (openai-chat--chat-message-into-string
                         ,message
                         ,separator)))))))

(test-chat-message-into-string "run"
                               "user>content"
                               (make-openai-chat--chat-message :role "user"
                                                               :content "content")
                               ">")

(defmacro test-chat-message-into-alist
    (name want message)
  (let ((testname (format "test-chat-message-into-alist-%s" name)))
    `(progn
       (ert-deftest ,(read testname)
           ()
         (should (equal ,want
                        (openai-chat--chat-message-into-alist
                         ,message)))))))

(test-chat-message-into-alist "run"
                              '(("role" . "user")
                                ("content" . "into-alist"))
                              (make-openai-chat--chat-message :role "user"
                                                              :content "into-alist"))

(defmacro test-chat-message-from-string
    (name want string separator default-role)
  (let ((testname (format "test-chat-message-from-string-%s" name)))
    `(progn
       (ert-deftest ,(read testname)
           ()
         (should (equal ,want
                        (openai-chat--chat-message-from-string
                         ,string
                         ,separator
                         ,default-role)))))))

(test-chat-message-from-string "role-and-content"
                               (make-openai-chat--chat-message :role "user"
                                                               :content "msg")
                               "user|msg"
                               "|"
                               "x")
(test-chat-message-from-string "default-role-and-content"
                               (make-openai-chat--chat-message :role "x"
                                                               :content "msg")
                               "msg"
                               "|"
                               "x")
(test-chat-message-from-string "only-one-split"
                               (make-openai-chat--chat-message :role "user"
                                                               :content "msg|msg2")
                               "user|msg|msg2"
                               "|"
                               "x")

(provide 'openai-chat-test)
;;; openai-chat-test.el ends here
