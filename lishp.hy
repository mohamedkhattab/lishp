#! /usr/bin/env hy
(import os sys re)
(import [signal :as sign])

;; shell builtins

(defn echo [args]
  (print (remove-quotes args)))

(defn clear [args]
  (print "\033c"))

(defn pwd [args]
  (print (.getcwd os)))

(defn cd [args]
  (.chdir os args))

;; globals

(setv *sys-path* (.split (.getenv os "PATH") ":"))
(setv *username* (.getenv os "LOGNAME"))
(setv *nodename* (get (.uname os) 1))
(setv *prompt* (+ *username* "@" *nodename* "% "))
(setv *msg* (+ "Lishp shell v0.0.1 running on "
               (get (.uname os) 0) " "
               (get (.uname os) 4) "\n"))
(setv *cmds*
      {"echo" echo
       "clear" clear
       "pwd" pwd
       "cd" cd})

;; helper functions

(defn lower [str]
  (.lower str))

(defn strip [str]
  (.strip str))

(defn remove-quotes [str]
  (-> str
      (.strip "\"")
      (.strip "\'")
      (.replace "\"" " ")
      (.replace "\'"  " ")))

(defn exists-in-path? [cmd]
  (setv result False)
  (for [path *sys-path*]
    (if (.isfile (. os path) (+ path "/" cmd))
      (setv result True)
      False))
  result)

(defn get-absolute-path [cmd]
  (setv abs-path "")
  (for [path *sys-path*]
    (if (.isfile (. os path) (+ path "/" cmd))
      (setv abs-path (+ path "/" cmd))
      False))
  abs-path)

(defn split-arg-list [args]
  (if args
    (->> (.split args " ")
         (filter (fn [arg] (not (= arg ""))))
         (list))
    [""]))

(defn arg-list-not-empty? [args]
  (if (get args 0)
    True
    False))

(defn get-final-args [cmd args]
  (setv arg-list (split-arg-list args))
  (if (arg-list-not-empty? arg-list)
    (+ [cmd] () arg-list)
    [cmd]))

;; handle interrupt signals
(defn sig-handler [sig frame]
  (cond
    [(= sig (. sign *sigint*)) (print "^C")]
    [(= sig (. sign *sigtstp*)) (print "^Z")]))

;; Interrupt signal
((. sign signal) (. sign *sigint*) sig-handler)
((. sign signal) (. sign *sigtstp*) sig-handler)

;; process input

(defn tokenize-arg [arg]
  (setv patt (.compile re "(\w+)(.*)"))
  (-> (.findall patt arg)
      (get 0)
      (list)))

;; TODO: only lower cmd not args
(defn lower-list [tokens]
  (list (map lower tokens)))

(defn strip-list [tokens]
  (list (map strip tokens)))

(defn exec [cmd args]
  (setv pid (.fork os))
  (if (= pid 0)
    (.execvp os
             cmd
             (get-final-args cmd args))
    (.waitpid os pid 0)))

(defn exec-cmd [tokens]
  (setv cmd (get tokens 0))
  (setv args (get tokens 1))
  (cond [(in cmd *cmds*) ((get *cmds* cmd) args)]
        [(exists-in-path? cmd) (exec cmd args)]
        [(= 1 1) (print (+ "lishp: command '" cmd "' not found."))]))

(defn process-arg [arg]
  (cond [(or (= arg "e") (= arg "exit"))
         (.exit sys 0)]
        [(not (strip arg)) (+ 1 1)]
        [(= 1 1) (-> arg
                     (tokenize-arg)
                     (lower-list)
                     (strip-list)
                     (exec-cmd))]))

;; start lishp

(print *msg*)

(defn main []
  (setv arg (input *prompt*))
  (process-arg arg))

(while True
  (main))
