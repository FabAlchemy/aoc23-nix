{ input }:
let
  inherit (builtins)
    add
    concatStringsSep
    elemAt
    filter
    foldl'
    fromJSON
    genList
    isList
    isString
    length
    match
    readFile
    replaceStrings
    split
    stringLength
    substring
    ;

  # Returns true if a value is a non-empty string
  isNonEmptyString = s: isString s && stringLength s != 0;

  # Converts a string to a list of one-character strings
  stringToCharacters = s: genList (p: substring p 1 s) (stringLength s);

  # Returns true if a string contains a single digit
  isDigit = c: isList (match "([[:digit:]])" c);

  # Retrieve the first element of a list
  first = l: elemAt l 0;
  # Retrieve the last element of a list
  last = l: elemAt l (length l - 1);

  # Extract the "calibration value" by concatenating the first and last digit
  firstLast = l: [ (first l) (last l) ];

  lines = filter isNonEmptyString (split "\n" (readFile input));

  # Compute the calibration value for a given input
  computeValue = lines:
    let
      listsOfDigits = map (l: filter isDigit l) (map stringToCharacters lines);
      firstAndLastDigits = map firstLast listsOfDigits;
      calibrationValues = map fromJSON (map (concatStringsSep "") firstAndLastDigits);
    in
    foldl' add 0 calibrationValues;

  partOne = computeValue lines;

  spelledDigits = [ "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" ];
  stringDigits = [ "1" "2" "3" "4" "5" "6" "7" "8" "9" ];

  # The input might contain these tricky cases. Replacing the first occurence
  # "erases" the second one, the this works around this problem.
  # An improved version would scan the string and store the digits as it goes,
  # but I've found it to be too much work for the limited cases we have here.
  additionalSpelledDigits = [ "oneight" "twone" "threeight" "fiveight" "sevenine" "eightwo" "eighthree" ];
  additionalStringDigits = [ "18" "21" "38" "58" "79" "82" "83" ];

  # Replace the spelled-out digits with the digit representation.
  replaceSpelledDigits = s: replaceStrings
    (additionalSpelledDigits ++ spelledDigits)
    (additionalStringDigits ++ stringDigits)
    s;

  partTwo = computeValue (map replaceSpelledDigits lines);
in
{ inherit partOne partTwo; }
