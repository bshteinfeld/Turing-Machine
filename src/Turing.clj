(ns Turing)

;; Turing Machine
;; Ben Shteinfeld
;; D Block

(defn string-to-char
  "Convert a string into a character by creating a seq from the string and
 taking the first element"
  [s]
  (->> (str s)
    (seq)
    (first))

(defn vector-to-tuple
  "Function used to set up the map of tuples. The map of tupels is set up
with a vector key [state input] and vector value [state input newState output
direction]. Take care of all appropriate conversions (state and newState are
integers, input output and direction are characters)."
  [map [s i ns o d]]
  (assoc map [(read-string s) (string-to-char i)]
         [(read-string s) (string-to-char i) (read-string ns) 
          (string-to-char o) (string-to-char d)]))

(defn tuple-to-string 
  "Takes a vector value (found as values in the tuples map) and returns a
string representation of that tuple - used for printing the trace."
  [[s i ns o d]]
  (str "S: " s " I: " i " NS: " ns " O: " o " D: " d))

(defn read-tuples-from-file 
  "Takes a filepath and returns a map of tuples contained in the file."
  [filename]
  (->> (slurp "swap.txt")
        (clojure.string/split-lines)
        (map #(re-find #"\s*-?\d+\s+.\s+-?\d+\s+.\s+[r|l]\s*" %))
        (remove nil?)
        (map #(clojure.string/split % #"\s+"))
        (reduce vector-to-tuple {})))

(defn execute-once 
  "Given the state, position, tape, and tuples will return a vector in this
format - [newState newPosition newTape]"
  [[[s p t] tuples]]
  ; The action to be taken next depends on the state and the input (function of
  ; the tape and position). These two values in the vector are a key for the
  ; corresponding value (action to be taken) in the tuples map.
  ; The stringbuilder is used to change values of the tape.
  (let [action (tuples [s (.charAt t p)])
        string-builder (StringBuilder. t)]
    (if (nil? action) nil ; if no tuple was found, then execution is over
    [(action 2)
     (+ p (if (= (action 4) \r) 1 -1))
     (do (.setCharAt string-builder p (action 3))
       (.toString string-builder))
     ])))

; Definition of new predicate to check if value is not-nil
(def not-nil? (complement nil?))

(defn execute-main-loop 
  "Create a lazyseq of vectors that correspond to the actions taken
 with the format [state position tape]"
  [s p t tup trace]
  (map #(first %)
       (take-while #(not-nil? (first %))
                   (iterate (fn [[[s p t] tup]]
                              [(execute-once [[s p t] tup]) tup])
                            [[s p t] tup]))))

(defn insert-brackets 
  "Takes a string and a position, returns a string with brackets around the
character at that position, using a stringbuilder."
  [text index]
  (let [string-builder (StringBuilder. text)]
    (.insert string-builder (inc index) " ]")
    (.insert string-builder index "[ ")
    (.toString string-builder)))

(defn trace-string 
  "Takes an array of actions-taken and returns a string representation of the
trace by parsing the actions and finding the corresponding tuples used and
putting into string format."
  [actions tuples]
  (map #(str "\n" (insert-brackets (last %) (% 1)) " "
             (tuple-to-string (tuples [(first %) (.charAt (last %) (% 1))])) )
       actions))

(defn run-turing 
  "Run the Turing Machine. Must be given an initial state, position, tape,
filepath to a file containing the tuples, and whether or not trace is needed."
  [state position tape filename trace]
  (let [tuples (read2 filename)
        actions-taken (execute-main-loop state position tape tuples trace)]
    (print (str "\n__________________________________\n\nBegin Reading Tuples:\n"
                (clojure.string/join "\n" (map tuple-to-string (vals tuples)))))
    
    (print (str "\n__________________________________\n\nBegin Execution With:\n"
              "Initial State: " state "\nInitial Position: " position
              "\nInitial Tape: " tape "\nTrace: " trace))

    (if trace (do (print "\n")
                (print (clojure.string/join 
                         (into [] (butlast 
                                    (trace-string actions-taken tuples)))))))
    
    (print (str "\n\nEnd Execution With Final Tape: " (last (last actions-taken))
                      "\nExecution took " (- (count actions-taken) 1) " steps"))))