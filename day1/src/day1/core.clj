(ns day1.core
  (:gen-class)
  (:import [java.lang Character]))

(defn index-of [item collection] 
	(count (take-while (partial not= item) collection)))

(defn first-and-last-occurrences [haystack needle]
	(let [
		  firstIndex (.indexOf haystack needle)
		  lastIndex (.lastIndexOf haystack needle)
		  ]
	[needle firstIndex lastIndex]))

(defn is-match [[_ firstIndex _]]
	(> firstIndex -1))

(defn min-and-max-by [f collection] 
	(let [
		  sorted-collection (sort-by f collection)
		  a (first sorted-collection)
		  b (last sorted-collection)
		  ]
	(vector a b)))

(defonce word-numbers ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"])
(defn word-to-digit [word]
	(+ 1 (index-of word word-numbers)))

(defn is-digit [s]
	(Character/isDigit s))

(defn numeric-digit-finder [input_line] 
	(let [
		  digits (->> input_line
				  (filter is-digit)
				  (map #(Character/getNumericValue %)))
		  first-digit (first digits)
		  last-digit (last digits)
		  ]
	(vector first-digit last-digit)))

(defn word-or-numeric-digit-finder [input_line]
	(let [
		  word_occurrences (->> word-numbers
							   (map #(first-and-last-occurrences input_line %))
							   (filter is-match)
							   (map (fn [[word first-idx last-idx]] (vector (word-to-digit word) first-idx last-idx))))
		  digit_occurrences (->> (range 0 9)
								 (map str)
								 (map #(first-and-last-occurrences input_line %))
								 (filter is-match))
		  all_occurrences (concat word_occurrences digit_occurrences)
		  ]
	(map first (min-and-max-by #(nth % 1) all_occurrences))))

(defn line-to-number [digit-finder input_line] 
	(let [
		  [first-number last-number] (digit-finder input_line)
		  ] 
	(+ (* 10 first-number) last-number)))

(defn part1 [input_lines]
	(->> input_lines
		 .lines
		 .iterator
		 iterator-seq
		 (map #(line-to-number numeric-digit-finder %))
		 (reduce +)))

(defn part2 [input_lines]
	(->> input_lines
		 .lines
		 .iterator
		 iterator-seq
		 (map #(line-to-number word-or-numeric-digit-finder %))
		 (reduce +)))

(defn -main []
	(let [input (slurp *in*)]
	  (printf "Part 1: %d" (part1 input))
	  (printf "Part 2: %d" (part2 input))))
