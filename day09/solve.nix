{ input }:
let
  inherit (builtins)
    add
    any
    elemAt
    filter
    foldl'
    fromJSON
    genList
    head
    isString
    length
    readFile
    split
    stringLength
    ;

  # Returns true if a value is a non-empty string
  isNonEmptyString = s: isString s && stringLength s != 0;

  # Split a string into a list of strings where the pattern is found
  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  lines = splitString "\n" (readFile input);
  histories = map (l: map fromJSON (splitString " " l)) lines;

  # Compute the list of difference for a given list
  listDiff = list:
    genList
      (n: (elemAt list (n + 1)) - (elemAt list n))
      (length list - 1);

  # Recursively predict the next value of `history`
  predict = history:
    let
      last = elemAt history (length history - 1);
      diff = listDiff history;
    in
    if any (e: e != 0) diff
    then last + (predict diff)
    else last;

  predictions = map predict histories;

  # Compute the sum of the elements of a list
  sum = foldl' add 0;

  partOne = sum predictions;

  # Recursively predict the previous value of `history`
  predictPrevious = history:
    let
      first = head history;
      diff = listDiff history;
    in
    if any (e: e != 0) diff
    then first - (predictPrevious diff)
    else first;

  partTwo = sum (map predictPrevious histories);

in
{ inherit partOne partTwo; }
