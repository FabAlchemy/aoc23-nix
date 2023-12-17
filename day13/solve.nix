{ input }:
let
  inherit (builtins)
    elem
    elemAt
    filter
    foldl'
    genList
    hasAttr
    head
    isString
    length
    lessThan
    readFile
    split
    stringLength
    ;

  # Returns true if a value is a non-empty string
  isNonEmptyString = s: isString s && stringLength s != 0;

  # Split a string into a list of strings where the pattern is found
  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  patterns = map (p: map (splitString "") (splitString "\n" p)) (splitString "\n\n" (readFile input));

  min = x: y: if lessThan x y then x else y;

  # Indicate whether `row` is a line of reflection in `pattern`
  # i.e. pattern[row - i] == pattern[row + i + 1]
  # until either is out of bounds.
  isLineOfReflection = pattern: row:
    foldl'
      (res: idx:
        let
          a = elemAt pattern ((row - idx));
          b = elemAt pattern (row + idx + 1);
        in
        res && (a == b))
      true
      # add an extra `0` so that checking the first row still does something
      ([ 0 ] ++ (genList (n: n) ((min row (length pattern - row - 2)) + 1)));

  # Find the horizontal reflections in `pattern`.
  findHorizontalReflections = pattern:
    map
      (n: n + 1) # offset the result because indices are 1-based in the exercise
      (filter (n: isLineOfReflection (pattern) n) (genList (n: n) (length pattern - 1)));

  # transpose a list of lists
  transpose = pattern:
    genList
      (colIndex: genList
        (rowIndex: elemAt (elemAt pattern rowIndex) colIndex)
        (length pattern))
      (length (head pattern));

  # Find the vertical reflections in `pattern`.
  findVerticalReflection = pattern: findHorizontalReflections (transpose pattern);

  horizontalReflections = map (pattern: findHorizontalReflections pattern) patterns;
  verticalReflections = map (pattern: findVerticalReflection pattern) patterns;

  flatten = list: foldl' (res: l: res ++ l) [ ] list;
  sum = list: foldl' (res: x: res + x) 0 list;
  partOne = sum (map (n: n * 100) (flatten horizontalReflections)) + sum (flatten verticalReflections);

  swapElement = element: if element == "." then "#" else ".";

  # Swap the "smudge" at the given coordinates.
  swapSmudge = pattern: smudgeRow: smudgeCol:
    genList
      (rowIndex:
        let row = elemAt pattern rowIndex; in
        genList
          (colIndex:
            let element = elemAt row colIndex;
            in
            if rowIndex == smudgeRow && colIndex == smudgeCol
            then swapElement element else element)
          (length row)
      )
      (length pattern);

  # Indicate whether a list is empty or not.
  isEmpty = list: length list == 0;

  # Remove from A the elements in B
  filterList = listA: listB: filter (e: !elem e listB) listA;

  # Recursively find the smudge in `pattern`, with the additional
  # information of the initial reflexion in `symRow` and `symCol`
  # [[char]] -> number -> number -> [number] -> number
  findSmudgeInner = pattern: row: col: symRow: symCol:
    let
      swappedPattern = swapSmudge pattern (row) (col);
      swappedSymRows = filterList (findHorizontalReflections swappedPattern) symRow;
      swappedSymCols = filterList (findVerticalReflection swappedPattern) symCol;
    in
    if !isEmpty swappedSymRows then { row = head swappedSymRows; }
    else if !isEmpty swappedSymCols then { col = head swappedSymCols; }
    else
      if row == (length pattern)
      then { couldNotFind = true; }
      else
        if col == (length (head pattern))
        then findSmudgeInner pattern (row + 1) 0 symRow symCol
        else findSmudgeInner pattern row (col + 1) symRow symCol
  ;

  # Recursively find the smudge in `pattern`.
  findSmudge = pattern:
    let
      symRow = findHorizontalReflections pattern;
      symCol = findVerticalReflection pattern;
    in
    findSmudgeInner pattern 0 0 symRow symCol;

  partTwo = foldl'
    (res: value:
      # compute the "summarization"
      if hasAttr "row" value
      then res + value.row * 100
      else res + value.col
    ) 0
    (map findSmudge patterns);

in
{ inherit partOne partTwo; }
