;; Copyright (c) 2011-2014 Robert Virding. All rights reserved.
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

(defmodule chat_ws_handler
  (export (init 2) (websocket_handle 3) (websocket_info 3)))

;; Internal state.
(defrecord state (opts ()))

(defun init (req opts)
  (chat_server:new_user)		;A new user
  `#(cowboy_websocket ,req ,(make-state opts opts)))

;; Callback on received websocket data.

(defun websocket_handle
  ([`#(text ,data) req st]
   (lfe_io:format "ws text: ~p\n" (list data))
   (case data
     ((binary "msg ! " (msg binary))
      (chat_server:send_message msg)
      `#(ok ,req ,st))
     ((binary "nick ! " (nick binary))
      (chat_server:set_nick nick)
      `#(ok ,req ,st))
     (_
      `#(reply #(text ("status ! received '" ,data "'")) ,req ,st))))
  ([other req st]
   (lfe_io:format "ws other: ~p\n" (list other))
   `#(ok ,req ,st)))

;; Callback on message from erlang.

(defun websocket_info
  ([(= `#(text #(message ,msg)) data) req st]
   (lfe_io:format "cs text: ~p\n" (list data))
   `#(reply #(text ("output ! " ,msg)) ,req ,st))
  ([other req st]
   (lfe_io:format "cs other: ~p\n" (list other))
   `#(ok ,req ,st)))
