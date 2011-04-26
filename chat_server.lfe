;; Copyright (c) 2011 Robert Virding. All rights reserved.
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

(defmodule chat_server
  (export (start_link 1) (stop 0)))

;; Start misultin http server.
(defun start_link (port)
  (: misultin start_link
    (list (tuple 'port port)
	  (tuple 'loop (lambda (req) (handle-http req port)))
	  (tuple 'ws_loop (lambda (ws) (handle-websocket ws)))
	  (tuple 'ws_autoexit 'false))))

;; Stop misultin.
(defun stop ()
  (: misultin stop))

;; Callback on request received.
(defun handle-http (req port)
  (: lfe_io fwrite '"hh: ~p\n" (list (tuple (self) req port)))
  ;; Output
  (case (tuple (call req 'get 'method)
	       (call req 'resource '(lowercase urldecode)))
    ((tuple 'GET (list '"chat"))	;Our chat program
     (let* (((tuple 'ok file) (: file read_file '"./chat.html"))
	    ((list bef aft) (: binary split file #b("%%HOST:PORT%%")))
	    (host (: proplists get_value 'Host (call req 'get 'headers))))
       (call req 'ok (list bef host aft))))
    ((tuple 'GET (list file))
     (if (: filelib is_regular file)
       (progn (: lfe_io fwrite '"hh: sending ~p\n" (list file))
	      (call req 'file file))
       (progn (: lfe_io fwrite '"hh: no file ~p\n" (list file))
	      (call req 'respond 404 () (list '"no file: " file)))))
    (_ (: lfe_io fwrite '"hh: ignoring\n"))))

(defun handle-websocket (ws)
  (: chat_backend new_user)		;Create user
  (ws-loop ws))

(defun ws-loop (ws)
  (receive
    ((tuple 'browser data)
     (case data
       ((++ '"msg ! " msg)
        (: chat_backend send_message msg))
       ((++ '"nick ! " nick)
        (: chat_backend set_nick nick))
       (_				;Unrecognised data
	(call ws 'send (list '"status ! received'" data '"'"))))
     (ws-loop ws))
    ('closed
     ;; Must ensure this process terminates.
     (: lfe_io fwrite '"The WebSocket was CLOSED!\n" ())
     'closed)
    ((tuple 'chat_server (tuple 'message msg))
     ;; Send message to output.
     (call ws 'send (list '"output ! " msg))
     (ws-loop ws))
    (_ (ws-loop ws))			;Ignore 
    (after 10000
      (call ws 'send (++ '"clock ! tick "
			 (: io_lib fwrite '"~p" (list (time)))))
      (ws-loop ws))))
