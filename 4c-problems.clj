;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 15. Double down
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(#(* 2 %) 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 16. Hello World
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= (#(str "Hello, " % "!") "Dave") "Hello, Dave!")

;;Test change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 19. Last element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(#(first (reverse %)) [1 2 3 4 5])

(__ [1 2 3 4 5])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 20. Penultimate element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((comp second reverse) (list 1 2 3 4 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 21. Nth element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(#(drop (%2) %1) [:a :b :c] 0 )
(#(first (drop %2 %1)) [:a :b :c] 0 )
(__ [:a :b :c] 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 22. Count a sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(last (map-indexed (fn [idx itm] (inc idx)) '(1 2 3 3 1)))

(#(last (map-indexed (fn [idx itm] (inc idx)) %)) '(1 2 3 3 1) )
(__ '(1 2 3 3 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 25. Find the odd numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(filter odd? [1 2 3 4 5])
(#(filter odd? %) [1 2 3 4 5])
(__ #{1 2 3 4 5})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 51. Advanced destructuring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] __] [a b c d]))

[1 2 3 4 5]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 62. Re- iterate!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn p62 [f x] (lazy-seq (f (f x))))

(take 5 (p62 #(* 2 %) 1) )


(fn it [f x]
  (lazy-seq (cons x (it f (f x)))))

(= (take 5 (repeat #(* 2 %) 1)) [1 2 4 8 16])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 63. Group a seq
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= (__ #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})

(map  #(> % 5) [1 3 6 8])

(defn p63 [f d]
  (interleave (map f d) d))


(defn p63 [f d]
  (apply merge-with concat (for [d ds] {(f d) [d]})))

(p63 #(> % 5) [1 3 6 8])

(for [f d] {(f d) d})


((fn [f xs] (apply merge-with concat (for [x xs] {(f x) [x]}))) #(> % 5) [1 3 6 8] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 66. Greatest common divisor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(first (filter #(mod ) ) )

((__ 2 4) 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 81. Set intersection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn p81 [s1 s2]
   (set (filter #(s2 %) s1)))

(sort (p81 #{0 1 2 3} #{2 3 4 5} )

(filter #(#{2 3 4 5} %) #{0 1 2 3}))

(p81 #{0 1 2 3} #{2 3 4 5})

#{0 1 2 3} #{2 3 4 5}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 83. Map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn p83 [k v]
  (into {} (map vector k v)))

(p83 [:a :b :c] [1 2 3])

(into {} (map vector [:a :b :c] [1 2 3]))

(apply hash-map (interleave [:a :b :c] [1 2 3] ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 83. Half truth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(some true? [false true])
(some false? [false])

(defn half-truth [x]
  (true? (and (some true? x) (some false? x))))

(half-truth [true false])


(half-truth [false false])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 88. Set - symmetric difference
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(clojure.set/intersection #{1 2 3 4 5 6} #{1 3 5 7})
(clojure.set/difference #{1 2 3 4 5 6} #{1 3 5 7})

(#(clojure.set/difference
   (clojure.set/union %1 %2)
   (clojure.set/intersection %1 %2))
 #{1 2 3 4 5 6} #{1 3 5 7} )

#{1 2 3 4 5 6} #{1 3 5 7}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 95. To tree or not to tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;symmetry?
(= (__ '(:a (:b nil nil) nil))
   true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 96. Beauty is symmetry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(#(= % (reverse %)) '(:a (:b nil nil) (:b nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 97. Pascal's Triangle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn p97 [x]
  (if (= x 1)
    [1]
    (p97 (dec x)))
  )

(defn p97 [x]
  (if (= x 1)
    [1]
    (do
      ;;(println (dec x))
      (println (p97 (dec x)))
      (concat '(1)
              (#(map + % (drop 1 %)) (p97 (dec x)))
              '(1))
      )))

(p97 4)

(fn p97 [x]
  (if (= x 1)
    [1]
    (concat '(1)
            (#(map + % (drop 1 %)) (p97 (dec x)))
            '(1))
    ))


(concat '(1) '(3) '(4))
(map + [1 2 1] (drop 1 [1 2 1]) )


(#(map + % (drop 1 %)) [1 1])
(#(map + % (drop 1 %)) [1 3 3 1])

;;- The first row is 1.
;;- Each successive row is computed by adding together adjacent numbers in the row above, and adding a 1 to the beginning and end of the row.


(= (map __ (range 1 6))
   [     [1]
    [1 1]
    [1 2 1]
    [1 3 3 1]
    [1 4 6 4 1]
    [1 5 10 10 5 1]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 99. Multiply and split
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#(map read-string (map str %))

(map str (str (#(* %1 %2)) 8 4))

(map read-string (map str (str (#(* %1 %2) 8 4))))

(#(map read-string (map str (str ( (* %1 %2))))) 4 6)

(defn p99 [d1 d2]
(map read-string (map str (str (* d1 d2)))) )

(p99 8 4)

(#(->> %&
       (apply *)
       str
       (map str)
       (map read-string)
       ) 8 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 107. Lexical closures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make_power [n]
 #(reduce * (repeat n %))
)

((make_power 3) 5)

(= [1 8 27 64] (map (make_power 3) [1 2 3 4]))

(map (make_power 3) [1 2 3 4])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 118. Re-implement map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(loop [f [x & remain]]
  (f x))

(defn p118 [f x]
  (loop [[el & remain] x
         result []]
    ;;(println "bye")
    (if (nil? el)
      result
      (recur remain (conj result (f el))))))

(p118 inc [2 3 4 5 6])

(= [3 4 5 6 7]
   (__ inc [2 3 4 5 6]))

(conj [] (inc 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 120. Sum of square of digits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(map #(str %) (str 10))

(Integer/valueOf "1")

(map #(Integer/valueOf (str %)) (String/valueOf 10))

(String/valueOf 10)
(= 8 (__ (range 10)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 122. Read a binary number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(Integer/toBinaryString (Integer. "7"))
(#(Integer/toBinaryString (Integer. %)) "0")

(reduce + (seq "111"))
(seq "111")

(take 5 (iterate #(* % 2) 1))

((fn [x]
   (take (count x)
         (iterate #(* % 2) 1)))
 "111" )

(__ "111")
(__ "1000")
(count "111")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 134. A nil key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(__ :a {:a nil :b 2})

(#(if (contains? %2 %1)
    (nil? (%2 %1) )
    false) :c {:a nil :b 2})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 135. Infix calculator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn infix [f s t]
  (s f t))

((fn p11 [& x]
   (let [result (apply infix (take 3 x))]
     (if (< 3 (count x))
       (do (println result)
           (println (cons result (drop 3 x)))
           (apply p11
                  (cons result (drop 3 x))))
       (do (println result)
           result)
       ))) 1 + 2 / 6 * 8)

((fn p11 [& x]
   (let [result (apply infix (take 3 x))]
     (if (< 3 (count x))
       (apply p11
              (cons result (drop 3 x)))
       result
       ))) 2 + 5)


((fn p11 [& x]
   (let [infixit #(%2 %1 %3)
         result (apply infixit (take 3 x))]
     (if (< 3 (count x))
       (apply p11
              (cons result (drop 3 x)))
       result
       ))) 2 + 5)

(#(%2 %1 %3) 2 + 5)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 143. Dot product
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reduce + (map * [0 1 0] [1 0 0]))
(#(reduce + (map * %1 %2)) [0 1 0] [1 0 0])
(__ [0 1 0] [1 0 0])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 147. Pascal's trapezoid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;lazy seq

#_(defn p147 [x]
    (let [nxt-fcn #(map +' % (drop 1 %))]
      (lazy-seq (concat
                 (list (first x))
                 (nxt-fcn x)
                 (list (last x))))))

#_(defn p147 [x]
    (let [nxt-fcn #(map +' % (drop 1 %))]
      (lazy-seq (concat
                 (list (first x))
                 (nxt-fcn x)
                 (list (last x))))))

(defn p147 [s]
  (let [add-last-layer #(map +' % (drop 1 %))
        make-next-layer #(lazy-seq (concat
                                    (list (first %))
                                    (add-last-layer %)
                                    (list (last %))))]
    (iterate make-next-layer s)))

(take 5 (p147 [1]))
(take 5 (p147 [2 3 2]))
(take 2 (p147 [3 1 2]))


(#(map + % (drop 1 %)) [2 3 2])

(= (second (__ [2 3 2])) [2 5 5 2])

;; anjensan's solution --- beautiful!!!
;;iterate #(map + `(0 ~@% 0) `(~@% 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 156. Map defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(#(apply hash-map (interleave %2 (repeat %1))) "x" [1 2 3])
(interleave (repeat (count [1 2 3]) "x") [1 2 3])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 157. Indexing sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(map-indexed (comp
              #(into [] %)
              reverse
              vector) [[:foo]
                       {:bar :baz}])
[:a :b :c]

((fn [x]
   (map-indexed
    (comp
     #(into [] %)
     reverse
     vector) x)) [:a :b :c])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 166. GT LT EQ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn p166 [op x y]
  (if (op x y)
    ))
(= :gt (__ < 5 1))

(= :eq (__ (fn [x y] (< (count x) (count y))) "pear" "plum"))






