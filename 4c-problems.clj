
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
;; 43. Reverse interleave
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(map-indexed #(list %1 %2) [1 2 3 4 5 6])

(defn p43 [x n]
  (map-indexed #(list (mod %1 n) %2) x))
(defn get-group [n]
  (println n)
  (fn [x] (map second (filter #(= n (first %1)) x))))


((get-group 1) (p43 [1 2 3 4 5 6] 2))
((second  (map get-group (range 2))) (p43 [1 2 3 4 5 6] 2))
((apply juxt (map get-group (range 2))) (p43 [1 2 3 4 5 6] 2))
((juxt (get-group 0) (get-group 1)) (p43 [1 2 3 4 5 6] 2))


(filter #(= 1 (first %1)) (p43 [1 2 3 4 5 6] 2))


;; TRY FOR REAL
(defn p43 [x n]
  (let [re-index (fn [x n]
                   (map-indexed #(list (mod %1 n) %2) x))
        re-group (fn [n]
                   (fn [x]
                     (map second
                          (filter #(= n (first %1)) x))))]

    ((apply juxt (map re-group (range n))) (re-index x n))))

(p43 [1 2 3 4 5 6] 2 )

(#(partition %2 %1) [1 2 3 4 5 6] 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 44. Rotate sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(take -2 [1 2 3 4 5])

(defn p44 [r x]
  (let [n (mod r (count x))]
    (if (> 0 n)
      (concat
       (drop (+ (count x) n) x)
       (take (+ (count x) n) x))
      (concat
       (drop n x)
       (take n x)))))

(p44 2 [1 2 3 4 5])

((fn [n x]
   (take n (cycle x))) 10 [1 2 3 4 5])


(= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))

(= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 46. Flipping out
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn [x] #(x %2 %1))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 50. Split by type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set (map second (group-by type [1 :a 2 :b 3 :c])))

(type :a)
(= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 51. Advanced destructuring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] __] [a b c d]))

[1 2 3 4 5]



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 55. Count occurences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reduce-kv
 (fn [x y z] (into x (hash-map y (count z)))) {}
 (group-by identity [1 1 2 3 2 1 1]))


(#(reduce-kv
   (fn [x y z] (into x (hash-map y (count z)))) {}
   (group-by identity %)) [1 1 2 3 2 1 1])

(merge-with count [1 1 2 3 2 1 1])

(= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 56. Find distinct
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map first (group-by identity [1 2 1 3 1 2 4]))

(#(map first (group-by identity %)) (range 50))

(= (__ [1 2 1 3 1 2 4]) [1 2 3 4])

(sort-by #(.indexOf (range 50) %) (group-by identity (range 50)))


(defn p56 [s] (sort-by #(.indexOf s %) (map #(first %) (group-by identity s))))

(p56 [1 2 1 3 1 2 4])

(p56 (range 50))

;; Anjesan's solution -- elegant!
;; reduce #(if ((set %) %2) % (conj % %2)) []

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 54. Partition Seq
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn p54 [n x]
  (filter #(= n (count %))
          (map
           #(map second (val %))
           (->> x
                (map-indexed #(vector (Math/floor (/ %1 n)) %2))
                (group-by first)))))

(p54 3 (range 8))

(= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 58. Function Composition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn p58 [& fns]
  #(->> %
        reverse
        rest))

(reduce #(vector (apply %1 %2)) [1 2 3 4] [reverse rest])


(reduce #(%2 %1) [1 2 3] [min])


(apply [sum diff])
((p58 nil) [1 2 3 4])


(defn p58a [& fns]
  (fn [& args]
    (loop [[f & remain] (reverse fns)
           result args]
      (if (nil? f)
        (first result)
        (do
          (println f result)
          (recur remain [(apply f result)]))))))



((p58a rest reverse) [1 2 3 4])

((p58a zero? #(mod % 8) +) 3 5 7 9)
(= true )

(= [3 2 1] )

(= [3 2 1] (#(->> %
                  reverse
                  rest) [1 2 3 4]))


((fn comb [& funcs]
   (println (reverse funcs ))) reverse rest)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 59. Juxtaposition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn p59 [& fns]
  (fn [& args]
    (loop [[f & remain] fns
           result []]
      (if (nil? f)
        result
        (recur
         remain
         (conj result (apply f args)))))))

(defn p59 [& fns]
  (fn [& args]
    (for [f fns]
      (apply f args))))

((p59 + max min) 2 3 5 1 6 4)

(= [21 6 1] ((__ + max min) 2 3 5 1 6 4))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 60. Sequence reductions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn my-reduce

  ([op input] (my-reduce op (first input) (rest input)))

  ([op result input]

   (lazy-seq
    (if (empty? input) (list result)
        (cons result
              (my-reduce op
                         (op result (first input))
                         (rest input)))))))


(defn my-reduce

  ([op input] (my-reduce op (first input) (rest input)))

  ([op result input]

   (lazy-seq
    (if (empty? input) (list result)
        (cons result
              (my-reduce op
                         (op result (first input))
                         (rest input)))))))

()




(take 5 (my-reduce + (range)))

(loop [f +
       s nil
       x range
       r]
  (recur ))


(rdxns + (range))


(reduce {})
(= (take 5 (__ + (range))) [0 1 3 6 10])


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

((fn [f xs]
   (for [x xs] {(f x) [x]})) #(> % 5) [1 3 6 8])

(map #(> % 5) [1 3 6 8])

(defn pxx [f x]
  (apply merge-with concat
         (map hash-map (map f x) (map vector x))))

(pxx #(> % 5) [1 3 6 8])
(merge-with )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 65. Black box testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= :map (__ {:a 1, :b 2}))

(sequential? {:a 1, :b 2})
(sequential? #{1 2 3})
(sequential? '(1 2 3))
(sequential? [1 2 3])

(keys '(1 2 3))
(vals [1 2 3 4])
(map?  {:a 3})

(#(condp = (first (str %))
    \# (println "set")
    \{ (println "map")
    \( (println "list")
    \[ (println "vec")) #{1 2 3})



(first (str #{3 4 5 6}))

#({{} :map #{} :set} (empty %) (if (reversible? %) :vector :list))

(= :map )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 66. Greatest common divisor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(first (filter #(mod ) ) )

((__ 2 4) 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 67. Prime numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(= (__ 2) [2 3])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 70. Word Sorting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sort-by
 #(clojure.string/lower-case %)
 (clojure.string/split "Have a nice day." #" "))

(defn p70 [x]
  (-> x
      #_println
      #_(partial clojure.string/split % #" ")
      (partial re-seq #"[a-zA-Z]")
      println
      #_(partial sort-by clojure.string/lower-case)))

(defn p70 [x]
  (->> x
       (re-seq #"[a-zA-Z ]")
       (apply str)
       (#(clojure.string/split % #" "))
       (sort-by #(clojure.string/lower-case %))))

(re-seq #"\w+" "Have a nice Day!")


(p70 "Have a nice day!")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 74. Filter perfect squares
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map #(Integer. %) (re-seq #"\w+" "2,3,4,5,6"))

(= (__ "4,5,6,7,8,9") "4,9")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 77. Anagram finder
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(apply set )

(clojure.string/split "meat" #"")

(= #{"m" "e" "a" "t"} (set (clojure.string/split "meat" #"")))

(sort "meat")

(set (filter #(> (count %) 1)
             (map #(set (map :name (second %)))
                  (group-by #(sort (:name %))
                            (map-indexed
                             #(hash-map :index %1 :name %2)
                             ["meat" "mat" "team" "mate" "eat"])))))


(defn p77 [s]
  (set
   (filter #(> (count %) 1)
           (map #(set (map :name (second %)))
                (group-by #(sort (:name %))
                          (map-indexed #(hash-map :index %1 :name %2) s))))))

(p77 ["meat" "mat" "team" "mate" "eat"])

(get-in {:level1 {:level2 "hi"}} [:level1])

(= (group-by sort ["meat" "mat" "team" "mate" "eat"])
   #{#{"meat" "team" "mate"}})


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
;; 90. Cartesian product
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(for [e1 [:a :b :c]
      e2 '(1 2 3)]
  (list e1 e2))

(#(set (for [s1 %1
             s2 %2]
         (list s1 s2)))  #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})

(= (__ #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
   #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
     ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
     ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 95. To tree or not to tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn p95 [x]
  (let [valid-entry? (fn [x]
                       (if (nil? x)
                         true
                         (if (sequential? x)
                           (p95 x)
                           false)))]
    (and
     (every? true? (flatten (map valid-entry? (rest x))))
     (= (count (rest x)) 2))))

(p95 '(:b nil (1 nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 96. Beauty is symmetry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; split into left and right halves
;; reverse one half
;; check if they're equal

(second '(:a (:b nil nil) (:b nil nil)))


(defn p96 [y]
  (let
      [reverse-branch (fn [[f s t]]
                        (let [next-step (fn [x]
                                          (if (sequential? x)
                                            (reverse-branch x)
                                            x))]
                          (list f (next-step t) (next-step s))))]
    (= (nth y 2)
       (reverse-branch (second y)))))

(= (nth '(:a (:b nil nil) (:b nil nil)) 2)
   (reverse-branch (second '(:a (:b nil nil) (:b nil nil)))))


(p96 '(:a (:b nil nil) (:b nil nil)))

(reverse-branch (second '(:a (:b nil nil) (:b nil nil))))

(first '(:a (:b nil nil) (:b nil nil)))



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
;; 100. Least common multiple
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-lcm [n1 n2 & args]
  (let [finder #(loop [x %1 y %2]
                  (if (= 0 (mod x y))
                    x
                    (recur (+ x %1) y)))]
    (if (nil? args)
      (finder n1 n2)
      (apply find-lcm (list* (finder n1 n2) args)))))

(find-lcm 3/4 1/6)

(find-lcm 5 3 7)

(__ 2 3)

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
(#(Integer/toBinaryString (Integer. %)) "313")

(seq "111")

;; Need anonymous function because it's a Java method
(map #(Integer/parseInt %) (map str (seq "111")))


(Integer/valueOf "111")

(#(Integer/valueOf % 2) "111")

(__ "111")
(__ "1000")
(count "111")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 128. Recognize playing cards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn p128 [text-in]
  (let [num->val {:2 0
                  :3 1
                  :4 2
                  :5 3
                  :6 4
                  :7 5
                  :8 6
                  :9 7
                  :T 8
                  :J 9
                  :Q 10
                  :K 11
                  :A 12}
        chr->suit {:C :club
                   :D :diamond
                   :H :heart
                   :S :spade}
        [s n] (map keyword (map str (seq text-in)))]
    (println {:suit (s chr->suit) :rank (n num->val)})
    {:suit (s chr->suit) :rank (n num->val)}))

convert-num (fn [x] (#(second %) x))
(println (map [num->val chr->suit] card-symbols))


(p128 "DQ")

(#(first %) (map str (seq "DQ")))

(= {:suit :diamond :rank 10} (__ "DQ"))


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
;; 146. Trees into tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Deep recursion into trees
(defn find-tree [y]
  (for [[k v] y]
    (if (not (map? v))
      (concat k v)
      (concat k (find-tree v)))))

(defn find-tree [y & pth]
  (flatten
   (for [[k v] y]
     (if (not (map? v))
       (do
         (println pth)
         (list (into [] (rest (concat pth (list k)))) v))
       (do
         (println pth k)
         (find-tree v pth k))))))

;; Only need to flatten two levels!!!!!!!!!!!
((fn find-tree [y]
   (into {} (for [[k v] y
                  [kk vv] v]
              [[k kk] vv]))) '{a {p 1, q 2}
                               b {m 3, n 4}})



(﻿(fn [mp]
   (into {}
         (for [[k v] mp
               [vk vv] v]
           (do
             (println k v vk vv)
             (vec [[k vk] vv]))))) '{a {p 1, q 2}
                                     b {m 3, n 4}})


(#(into {}
        (for [[a h] %
              [b v] h]
          (do
            (println a h b v)
            [[a b] v]))) '{a {p 1, q 2}
                           b {m 3, n 4}})




(find-tree '{[1] {a b c d}
             [2] {q r s t u v w x}})


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
;; 153. Pairwise disjoint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn p153t [bigset]
  (println "Iteration---")
  (println (seq bigset))
  (let [[s1 s2 & s] (if (sequential? bigset)
                      bigset
                      (seq bigset))]
    (println s1)
    (println s2)
    (println s)
    (println "Intersection set:: " (clojure.set/intersection s1 s2))
    (println "Unioned set:: "(clojure.set/union s1 s2))
    (println "List::" (list* (clojure.set/union s1 s2) s))
    (if (not (empty? (clojure.set/intersection s1 s2)))
      false
      (if (nil? s2)
        true
        (p153t (list* (clojure.set/union s1 s2) s))))))

(defn p153t [bigset]
  (let [[s1 s2 & s] (if (sequential? bigset)
                      bigset
                      (seq bigset))]
    (if (not (empty? (clojure.set/intersection s1 s2)))
      false
      (if (nil? s2)
        true
        (p153t (list* (clojure.set/union s1 s2) s))))))

(p153t #{#{:a :b :c :d :e}
         #{:a :b :c :d}
         #{:a :b :c}
         #{:a :b}
         #{:a}})

(p153t #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})



(apply clojure.set/intersection (seq #{#{:a :b :c :d :e}
                                       #{:a :b :c :d}
                                       #{:a :b :c}
                                       #{:a :b}
                                       #{:a}}))

;; Yet again, Anjesan is amazing
#(let [s (apply concat %)] (= (count s) (count (set s))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 173. Intro to destructuring 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(= 3
   (let [[x y] [+ (range 3)]] (apply __))
   (let [[[x y] b] [[+ 1] 2]] (__ b))
   (let [[x y] [inc 2]] (__)))


;; Hello, Sahil!




(= (__ 3) '(1 1 2))

(last (take 3 (iterate #(flatten (concat '(1) (map + % (drop 1 %)) '(1))) nil)))

(last (take 4 (iterate #(conj % (reduce + (take 2 (reverse %)))) [1 1])))

((fn [n] (last
          (take (- n 1) (iterate #(conj % (reduce + (take 2 (reverse %)))) [1 1])))) 3
 )
