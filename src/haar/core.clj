(ns haar.core)

(def sig [4 6 10 12 8 6 5 5])

(defn haar-transform [signal]
  (loop [signal signal
         [trends flucts :as transform] [[] []]]
    (if (seq signal)
      (let [sample (take 2 signal)
            trend (/ (apply + sample) 2)
            fluct (/ (apply - sample) 2)]
        (recur (drop 2 signal)
               [(conj trends trend)
                (conj flucts fluct)]))
      transform)))