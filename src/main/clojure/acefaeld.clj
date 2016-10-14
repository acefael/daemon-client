;;;
;;; ese@acefael.es
;;;
;;; Copyright (c) 2016
;;;

(ns acefaeld
  (:use [clojure.core.async :only [chan >!! <!!]]
        [bytes])
  (:import [java.net Socket InetSocketAddress]

(def sock

  "the socket we use to talk"

  (doto (Socket.)
    (.setReuseAddress true)))

(def c

  "this is our means of synchronising access to the single-threaded
  MSF.  channel capacity is 1.  in the worker thread one message is
  read and then accessed.  the worker will not read a further message
  until the previous one is processed and a response sent."

  (chan))

(def t

  "the worker thread"

  (ref nil))

(defn connected?

  "tells if the socket isConneted"

  []
  (.isConnected sock))

(defn- work

  "the function running in the worker thread"

  []
  (let [in (.getInputStream sock)
        out (.getOutputStream sock)]
    (while true
      (let [{command :message
             rv      :response} (<!! c)]
        (.write out command)
        (let [nbytes (get-uint in)]
          (.println System/out (str "reading " nbytes " response bytes"))
          (deliver rv (get-bytes in nbytes)))))))

(defn connect

  "opens the socket connection, if not yet done"

  [host port]
  (when (not (connected?))
    (.connect sock (InetSocketAddress. host port))
    (when-not
        @t
      (dosync (ref-set t (.start (Thread. work)))))))

(defn disconnect

  "closes the socket, if connected"

  []
  (when (connected?)
    (.close sock)))

(defn call

  "invokes the MSF

  yields a byte array to read response (bytes) from.  nil if
  not connected."

  ([bytes]
   (when (connected?)
     (let [rv (promise)]
                                        ; send over the actuall
                                        ; function invocation and a
                                        ; promise, on which we then
                                        ; wait for a response
       (>!! c {:message bytes
               :response rv})
       (deref rv))))
  ([service parameters]
   (when (connected?)
     (call (byte-array (concat (str-to-bytes service) parameters))))))

