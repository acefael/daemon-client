;;;
;;; ese@acefael.es
;;;
;;; Copyright (c) 2016
;;;

(ns bytes

  "byte-manipulation and io functions"
  
  (:import [java.nio.charset Charset]
           [java.io DataInputStream]))

(def default-charset

  "this is the charset where characters are represented as 16
  bit (little endian) shorts.  by using this charset java does not
  prepend a BOM to the character sequence, which would get in the way
  in the MSF."
  
  "UTF-16LE")

(defn bytes-to-int
  
  "kindly taken from https://gist.github.com/pingles/1235344 and
   adapted for little-endian."
  
  ([bytes]
   (bytes-to-int bytes 0))
  ([bytes offset]
   (reduce + 0
           (map (fn [i]
                  (let [shift (* i 8)]
                    (bit-shift-left (bit-and (nth bytes (+ i offset))
                                             0x000000FF)
                                    shift)))
                (range 0 4)))))

(defn int-to-bytes
  
  "turn an int into four bytes, little-endian.

  the bytes are suitable for the .get_unsigned_int() method on
  external_binary_input_stream in the MSF."
  
  [number]
  (into []
        (map (fn [i]
               (let [shift (* i 8)]
                 (bit-shift-right
                  (bit-and number
                           (bit-shift-left 0x000000ff
                                           shift))
                  shift)))
             (range 0 4))))

(defn str-to-bytes

  "turns a string into a byte array. default charset is
  UTF-16LE (little endian).  this is suitable for
  .get_unsigned_short() and character.from_value() in the MSF.

  always prepends the number of characters in the string as
  little-endian 4 byte unsigned int (for msf)."
  
  ([the-string]
   (str-to-bytes the-string default-charset))
  ([the-string charset-name]
   (let [the-bytes (.getBytes the-string (Charset/forName charset-name))]
     (byte-array (concat (int-to-bytes (count the-string)) the-bytes)))))

(defn bytes-to-str
  
  "turns a byte array into a string. default charset is
  UTF-16LE (little endian)."
  
  ([the-bytes]
   (bytes-to-str the-bytes default-charset))
  ([the-bytes charset-name]
   (String. the-bytes charset-name)))

(defn get-bytes
  
  "reads nbytes from istr and returns them as byte array. blocks until
  the bytes are actually available, which is helpful for socket
  streams."
  
  [istr nbytes]
  (let [buf (byte-array nbytes)
        is (DataInputStream. istr)]
    (.readFully is buf 0 nbytes)
    buf))

(defn get-uint

  "reads four bytes from istr and makes an unsigned integer from them."
  
  [istr]
  (bytes-to-int (get-bytes istr 4)))
