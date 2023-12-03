{ input }:
let
  inherit (builtins)
    add
    any
    filter
    foldl'
    fromJSON
    hasAttr
    head
    isString
    lessThan
    readFile
    split
    stringLength
    tail
    ;

  # Returns true if a value is a non-empty string
  isNonEmptyString = s: isString s && stringLength s != 0;

  # Split a string into a list of strings where the pattern is found
  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  # Functional equivalent to the merge operator
  mergeAttrs = x: y: x // y;

  # Sum up a list of integers
  sum = l: foldl' add 0 l;

  # Return true if x is greater than y
  greaterThan = x: y: lessThan y x;

  # Return the maximum value between x and y
  max = x: y: if lessThan x y then y else x;

  # Parse a value description like `3 blue` into the related attrset `{blue = 3;}`
  parseValue = v:
    let
      elements = splitString " " v;
      quantity = fromJSON (head elements);
      color = head (tail elements);
    in
    {
      "${color}" = quantity;
    }
  ;

  # Parse a game description
  parseGame = l:
    let
      game_info = builtins.match "Game ([[:digit:]]+): (.*)" l;
      id = fromJSON (head game_info);
      setsValues = splitString "; " (head (tail game_info));
      sets = map (set: (foldl' mergeAttrs { } (map parseValue (splitString ", " set)))) setsValues;
    in
    {
      inherit id sets;
    }
  ;

  lines = splitString "\n" (readFile input);
  games = map parseGame lines;

  # A set is invalid if it revealed more cubes of a given color than possible.
  isSetInvalid = set:
    (hasAttr "red" set && greaterThan set.red 12) ||
    (hasAttr "green" set && greaterThan set.green 13) ||
    (hasAttr "blue" set && greaterThan set.blue 14);

  isGameInvalid = game: any isSetInvalid game.sets;
  isGameValid = game: !(isGameInvalid game);
  validGames = map (game: game.id) (filter isGameValid games);
  partOne = sum validGames;

  # Merge two sets, keeping the maximum value for each color
  minimumSet = s1: s2: {
    red = max (s1.red or 0) (s2.red or 0);
    green = max (s1.green or 0) (s2.green or 0);
    blue = max (s1.blue or 0) (s2.blue or 0);
  };

  # Compute the power of a set (product of the value for each color)
  setPower = set: set.red * set.green * set.blue;

  powers = map (game: setPower (foldl' minimumSet { } game.sets)) games;
  partTwo = sum powers;
in
{ inherit partOne partTwo; }
