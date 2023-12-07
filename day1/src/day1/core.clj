(ns day1.core
  (:gen-class))

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

(defonce word_numbers ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"])
(defn word_to_digit [word]
	(+ 1 (index-of word word_numbers)))


(defn numeric-digit-finder [input_line] 
	(filter #(.isDigit %) input_line))

(defn word-or-numeric-digit-finder [input_line]
	(let [
		  word_occurrences (->> word_numbers
							   (map #(first-and-last-occurrences input_line %))
							   ((filter is-match)))
		  digit_occurrences (->> (range 0 9)
								(map str)
							   (map #(first-and-last-occurrences input_line %))
								(filter is-match))
		  all_occurrences (concat word_occurrences digit_occurrences)
		  ]
	(min-and-max-by #(nth % 1) all_occurrences)))

(defn line-to-number [digit-finder input_line]
	(let [
		  [first-number last-number] (digit-finder input_line)
		  ]
		((+ (* 10 first-number) last-number))))

(defn part1 [input_lines]
	(+ (->> input_lines
		 .lines
		 .iterator
		 (map #(line-to-number numeric-digit-finder %))
	)))

(defn part2 [input_lines]
	(+ (->> input_lines
		 .lines
		 .iterator
		 (map #(line-to-number word-or-numeric-digit-finder %))
	)))

(defn -main []
	(let [input (slurp *in*)]
	  (printf "Part 1: %s" (part1 input))
	  (printf "Part 2: %s" (part2 input))))
