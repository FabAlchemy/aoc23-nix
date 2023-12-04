{ input }:
let
  inherit (builtins)
  add
  any
  concatLists
  elem
  elemAt
  filter
  foldl'
  fromJSON
  genList
  isString
  length
  lessThan
  readFile
  split
  stringLength
  substring
  ;

  # Returns true if a value is a non-empty string
  isNonEmptyString = s: isString s && stringLength s != 0;

  # Split a string into a list of strings where the pattern is found
  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  # Enumerate the elements of a list
  # (int -> a -> b) -> [a] -> [b]
  enumerate = f: list: genList (n: f n (elemAt list n)) (length list);

  # Sum up a list of integers
  sum = l: foldl' add 0 l;

  # Multiply a list of integers
  mul = l: foldl' builtins.mul 1 l;

  # Return the maximum value between x and y
  max = x: y: if lessThan x y then y else x;

  # Return the absolute value of x
  abs = x: max x (-x);

  # Parse the "items" of a string.
  # An item is described by its predicate.
  # ([char] -> bool) -> str -> [{start, end, value}]
  findItems = itemPred: string:
    # finalize an occurence and add it to the accumulator
    let
      # we add a space character to simplify termination
      characters = splitString "" string ++ [ " " ];
      init = { acc = [ ]; index = 0; start = null; };

      # drive the scan forward, taking action to update `acc`.
      updateAcc = { acc, index, start }: char:
        if start == null
        # we were not looking at a number
        then
          if itemPred char
          # found the beginning of a number
          then { inherit acc; start = index; index = index + 1; }
          else { inherit acc start; index = index + 1; }

        # we were looking at a number
        else
          if !itemPred char
          # this is the end of a number
          then
            let newAcc = acc ++ [{
              inherit start;
              end = index - 1;
              value = (substring start (index - start) string);
            }]; in
            { acc = newAcc; start = null; index = index + 1; }
          # found another digit
          else
            { inherit acc start; index = index + 1; }
      ;
    in
    (foldl' updateAcc init characters).acc;

  # The same as `findItems` but for a list of items.
  # It also adds the line number to the resulting attrsets.
  findItemsWithLine = itemPred: lines:
    let
      items = map (findItems itemPred) lines;
    in
    concatLists (
      enumerate
        (i: line: map (item: item // { line = i; }) line)
        items
    );

  digits = splitString "" "0123456789";
  isDigit = char: elem char digits;
  isSymbol = c: !isDigit c && c != ".";

  lines = splitString "\n" (readFile input);
  inputNumbers = findItemsWithLine isDigit lines;
  inputSymbols = findItemsWithLine isSymbol lines;

  # Return true if the parts described by `a` and `b` are adjacent.
  isAdjacent = a: b:
    lessThan (abs (a.line - b.line)) 2
    && (lessThan (abs (a.start - b.start)) 2 || lessThan (abs (a.end - b.end)) 2);

  adjacents = filter (part: any (isAdjacent part) inputSymbols) inputNumbers;
  partOne = sum (map (part: fromJSON part.value) adjacents);

  gears = filter (sym: sym.value == "*") inputSymbols;
  partsForGears =
    filter (l: (length l) == 2)
      (map (gear: filter (isAdjacent gear) inputNumbers) gears);

  partTwo = sum (map mul (map (parts: map (part: fromJSON part.value) parts) partsForGears));
in
{ inherit partOne partTwo; }
