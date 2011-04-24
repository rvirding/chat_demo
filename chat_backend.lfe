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

(defmodule chat_backend
  (export (start_link 0) (start 0) (stop 0) (debug 0))
  (export (new_user 0) (set_nick 1) (send_message 1))
  (import (from lists (reverse 1) (foreach 2))))

(defrecord st (buf (new-buf)) (users ())) ;Server state
(defrecord user pid (nick ()))		  ;User info

(defun start_link ()
  (: proc_lib spawn_link (lambda () (init))))

(defun start ()
  (: proc_lib spawn (lambda () (init))))

(defun stop ()
  (! 'chat_backend 'stop))

(defun debug ()
  (! 'chat_backend (tuple (self) 'debug))
  (receive
    ((tuple 'chat_backend (tuple 'debug_info dbg)) dbg)))

;; User API

(defun new_user () (! 'chat_backend (tuple (self) 'new_user)))
(defun set_nick (nick) (! 'chat_backend (tuple (self) (tuple 'set_nick nick))))
(defun send_message (msg) (! 'chat_backend (tuple (self) (tuple 'message msg))))

(defun init ()
  (register 'chat_backend (self))
  (process_flag 'trap_exit 'true)
  (loop (make-st buf (new-buf) users ())))

(defun loop (st0)
  (receive
    ((tuple pid 'new_user)		;Add a new user
     (link pid)				;We link to the user
     (let ((us (add-user pid (st-users st0))))
       (send-msgs pid (st-buf st0))
       (loop (set-st-users st0 us))))
    ((tuple pid (tuple 'set_nick nick))	;Set the nick
     (let ((us (set-nick nick pid (st-users st0))))
       (loop (set-st-users st0 us))))
    ((tuple pid (tuple 'message text))	;Send a message
     (let ((msg (build-msg pid text (st-users st0))))
       (broadcast-msg msg (st-users st0))
       (let ((buf (buffer-msg msg (st-buf st0))))
	 (loop (set-st-buf st0 buf)))))
    ((tuple pid 'debug)			;Return debug info
     (! pid (tuple 'chat_backend (tuple 'debug_info st0)))
     (loop st0))
    ((tuple 'EXIT pid _)
     (let ((us (del-user pid (st-users st0))))
       (loop (set-st-users st0 us))))
    ('stop 'ok)))			;We're done

;; new-buf() -> Buffer.
;; get-msgs(Buffer) -> [Msg].
;; buffer-msg(Message, Buffer) -> Buffer.
;; The message is a queue structure with a count. We save max 20
;; messages in buffer.

(defun new-buf () (tuple 0 () ()))

(defun get-msgs ([(tuple _ b f)] (++ f (reverse b))))

(defun buffer-msg
  ([msg (tuple c b f)] (when (< c 20))
   (tuple (+ c 1) (cons msg b) f))
  ([msg (tuple 20 b (cons _ f))] (tuple 20 (cons msg b) f))
  ([msg (tuple 20 b ())]
   (buffer-msg msg (tuple 20 () (reverse b)))))

;; broadcast-msg(Msg, [Users]) -> ok.
;; send-msgs(Pid, Buf) -> ok.

(defun broadcast-msg (msg us)
  (foreach (lambda (u) (send-msg (user-pid u) msg)) us))

(defun send-msgs (pid buf)
  (foreach (lambda (m) (send-msg pid m)) (get-msgs buf)))

(defun send-msg (pid msg) (! pid (tuple 'chat_server (tuple 'message msg))))

(defun build-msg (pid text users) (list (get-nick pid users) '": " text))

;; add-user(Pid, Users) -> Users.
;; del-user(Pid, Users) -> Users.
;; set-nick(Nick, Pid, Users) -> Users.
;; get-nick(Pid, Users) -> Nick.
;; Work with user info.

(defun add-user (pid us) (cons (make-user pid pid nick '"Anon") us))

(defun del-user
  ([pid (cons (match-user pid p) us)] (when (=:= pid p)) us)
  ([pid (cons u us)] (cons u (del-user pid us)))
  ([_ ()] ()))

(defun set-nick (nick pid us0)
  (case us0
    ((cons (= (match-user pid pid) u) us) (cons (set-user-nick u nick) us))
    ((cons u us) (cons u (set-nick nick pid us)))
    (() ())))				;Be kind

(defun get-nick
  ([pid (cons (match-user pid p nick nick) _)] (when (=:= pid p)) nick)
  ([pid (cons _ us)] '"Unknown"))	;Be kind
