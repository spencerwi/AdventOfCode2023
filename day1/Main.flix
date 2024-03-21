mod Calibration {
    pub def numericDigitFinder(input : String) : Option[(Int32, Int32)] =
        forM (
            firstDigitIndex <- String.findIndexOfLeft(Char.isDigit, input);
            lastDigitIndex <- String.findIndexOfRight(Char.isDigit, input);
            firstDigit <- 
                input
                |> String.charAt(firstDigitIndex)
                |> Char.digit(radix = 10);
            lastDigit <-
                input
                |> String.charAt(lastDigitIndex)
                |> Char.digit(radix = 10)
        ) yield {
            (firstDigit, lastDigit)
        }

    pub def wordOrNumericDigitFinder(input : String) : Option[(Int32, Int32)] =
        let digitNames = List#{"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};
        def wordToNumber(word : String) : Option[Int32] =
            forM (
                zeroIndex <- List.indexOf(word, digitNames)
            ) yield {
                zeroIndex + 1
            }
        ;

        let digitChars : List[Char] = 
            List.range(1, 10)
            |> List.map(Char.forDigit(radix = 10))
            |> List.flatMap(Option.toList);
        let digitOccurrences: List[(Int32, Int32, Int32)] = 
            digitChars
            |> List.flatMap(digitChar -> 
                let maybeIndices : Option[(Int32, Int32, Int32)] = forM (
                    firstIndex <- String.findIndexOfLeft(c -> c == digitChar, input);
                    lastIndex <- String.findIndexOfRight(c -> c == digitChar, input);
                    value <- Char.digit(radix = 10, digitChar)
                ) yield { 
                    (value, firstIndex, lastIndex)
                };
                Option.toList(maybeIndices)
            );

        let wordOccurrences = 
            digitNames
            |> List.flatMap(digitName ->
                let maybeIndices = forM (
                    firstIndex <- String.indexOfLeft(substr = digitName, input);
                    lastIndex <- String.indexOfRight(substr = digitName, input);
                    value <- wordToNumber(digitName)
                ) yield {
                    (value, firstIndex, lastIndex)
                };
                Option.toList(maybeIndices)
            );

        let allOccurrences = List.append(digitOccurrences, wordOccurrences);
        forM (
            (firstDigit, _, _) <- 
                allOccurrences
                |> List.sortBy(x -> 
                    let (_, firstIndex, _) = x;
                    firstIndex
                )
                |> List.head;
            (lastDigit, _, _) <-
                allOccurrences
                |> List.sortBy(x -> 
                    let (_, _, lastIndex) = x;
                    lastIndex
                )
                |> List.last
        ) yield  {
            (firstDigit, lastDigit)
        }



    pub def lineToNumber(digitFinder : String -> Option[(Int32, Int32)], line : String) : Option[Int64] = 
        forM (
            (firstDigit, lastDigit) <- digitFinder(line)
        ) yield {
            Int32.toInt64((firstDigit * 10) + lastDigit)
        }
}

def part1(line : String) : Option[Int64] =
    line
    |> Calibration.lineToNumber(Calibration.numericDigitFinder)

def part2(line : String) : Option[Int64] =
    line
    |> Calibration.lineToNumber(Calibration.wordOrNumericDigitFinder)

def solve(input : List[String]): (Int64, Int64) =
    input
    |> List.filter(line -> not String.isEmpty(line))
    |> List.flatMap(line -> 
        let maybeAnswers = forM (
            answer1 <- part1(line);
            answer2 <- part2(line)
        ) yield {
            (answer1, answer2)
        };
        Option.toList(maybeAnswers)
    )
    |> List.foldLeft(
        (each, acc) -> 
            let (a1, a2) = each;
            let (b1, b2) = acc;
            (a1+b1, a2+b2),
        (0i64, 0i64)
    )


def main(): Unit \ IO = 
    let lines = region rc {
        let lines = Iterator.toList(System.StdIn.readLines(rc));
        lines
    };
    let (part1, part2) = solve(lines);
    println("Part 1: ${part1}");
    println("Part 2: ${part2}")