(ns haar.core)

(def sig [4 6 10 12 8 6 5 5])

(def sqrt2 (java.lang.Math/sqrt 2))

(defn haar-transform [signal]
  (loop [signal signal
         [trends flucts :as transform] [[] []]]
    (if (seq signal)
      (let [sample (take 2 signal)
            trend (* sqrt2 (/ (apply + sample) 2))
            fluct (* sqrt2 (/ (apply - sample) 2))]
        (recur (drop 2 signal)
               [(conj trends trend)
                (conj flucts fluct)]))
      transform)))

(defn inverse-haar [transform]
  (loop [signal []
         [trends flucts :as transform] transform]
    (if (and (seq trends)
             (seq flucts))
      (let [f1 (/ (+ (first trends)
                     (first flucts))
                  sqrt2)
            f2 (/ (- (first trends)
                     (first flucts))
                  sqrt2)]
        (recur (conj signal f1 f2)
               [(rest trends)
                (rest flucts)]))
      signal)))