{ input }:
let
  inherit (builtins)
    add
    all
    elemAt
    filter
    foldl'
    genList
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

  # The map of the universe, a list of lists of "." or "#".
  universe = map (splitString "") (splitString "\n" (readFile input));
  height = length universe;
  width = length (head universe);

  # Retrieve an element of the `universe` at (`row`, `col`).
  getElement = universe: row: col: elemAt (elemAt universe row) col;

  # a % b
  mod = a: b: a - b * (a / b);

  # Flatten a list of lists
  flatten = list: foldl' (res: l: res ++ l) [ ] list;

  # Compute the list of galaxies
  galaxies = flatten
    (genList
      (n:
        let
          row = (n / (height));
          col = mod n width;
        in
        if getElement universe row col == "#"
        then [{ inherit row col; }]
        else [ ]
      )
      (height * width)
    )
  ;

  max = a: b: if lessThan a b then b else a;

  # Expand the universe (each empty row moves the subsequent galaxies by `shift`).
  #
  # This function works on rows and columns at the same time (`n`):
  # if a column or a row is empty, then all the galaxies with a greater column or row are updated.
  expandUniverse = shift:
    foldl'
      (prev: n:
        let
          emptyRow = all (galaxy: galaxy.row != prev.row) prev.galaxies;
          emptyCol = all (galaxy: galaxy.col != prev.col) prev.galaxies;
          galaxies = map
            ({ row, col }:
              let
                newRow = if emptyRow && lessThan prev.row row then row + shift - 1 else row;
                newCol = if emptyCol && lessThan prev.col col then col + shift - 1 else col;
              in
              { row = newRow; col = newCol; })
            prev.galaxies;
        in
        {
          inherit galaxies;
          row = if emptyRow then prev.row + shift else prev.row + 1;
          col = if emptyCol then prev.col + shift else prev.col + 1;
        }
      )
      {
        inherit galaxies;
        # Track the current shifted indices.
        row = 0;
        col = 0;
      }
      # We do a little bit too much work when the universe is not square
      (genList (n: n) (max width height));

  # Compute the 2-combinations of elements from `list`.
  combinations = list:
    flatten
      (genList
        (x:
          genList
            (b: { a = elemAt list x; b = elemAt list b; })
            x)
        (length list));

  abs = x: if lessThan x 0 then -x else x;
  sum = foldl' add 0;

  pairDistance = { a, b }: abs (b.row - a.row) + abs (b.col - a.col);

  answer = shift: sum (map pairDistance (combinations (expandUniverse shift).galaxies));
  partOne = answer 2;
  partTwo = answer 1000000;

in
{ inherit partOne partTwo; }
