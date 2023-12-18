{ input }:
let
  inherit (builtins)
    elemAt
    filter
    foldl'
    genList
    hasAttr
    head
    isString
    length
    readFile
    replaceStrings
    split
    stringLength
    ;

  # Returns true if a value is a non-empty string
  isNonEmptyString = s: isString s && stringLength s != 0;

  # Split a string into a list of strings where the pattern is found
  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  # The platform, oriented with the north on the left.
  platform = transpose (map (splitString "") (splitString "\n" (readFile input)));

  # Transpose a list of lists.
  transpose = pattern:
    genList
      (colIndex: genList
        (rowIndex: elemAt (elemAt pattern rowIndex) colIndex)
        (length pattern))
      (length (head pattern));

  # Join a `list` of strings, separating them with `separator`.
  join = separator: list: foldl' (finalPlatform: e: finalPlatform + separator + e) "" list;

  # Shift a list of rocks to the left.
  shiftRocksList = line:
    splitString "" (shiftRocksString (join "" line));

  # Shift a the string representation of a list of rocks to the left.
  shiftRocksString = string:
    let updated = replaceStrings [ ".O" ] [ "O." ] string;
    in
    if updated == string then string else shiftRocksString updated;

  reverse = list:
    let len = length list;
    in
    genList (n: elemAt list (len - n - 1)) len;

  enumerate = list:
    genList
      (n: {
        inherit n; value = elemAt list n;
      })
      (length list);

  flatten = list: foldl' (finalPlatform: l: finalPlatform ++ l) [ ] list;

  # Compute the load on a (north-to-the-left) oriented platform.
  computeLoad = platform:
    let elements = flatten (map (l: enumerate (reverse l)) platform);
    in
    foldl'
      (finalPlatform: e: if e.value == "O" then finalPlatform + (e.n + 1) else finalPlatform)
      0
      elements;

  tiltedPlatform = map shiftRocksList platform;
  partOne = computeLoad tiltedPlatform;

  # Perfom a single tilt cycle.
  cycle = platform:
    let
      north = map shiftRocksList platform;
      west = map (shiftRocksList) (transpose north);
      south = map shiftRocksList (map reverse (transpose west));
      east = map shiftRocksList (map reverse (transpose (map reverse south)));
    in
    transpose (map reverse east);

  # Perform tilt cycles and find the first state cycle.
  # After each cycle, we store the platform state in memory along with its rank.
  # When we find the same state later, we have found the end of the state cycle.
  performCycles = platform: idx: memory:
    let
      finalPlatform = cycle platform;
      key = builtins.toJSON finalPlatform;
    in
    if hasAttr key memory
    then {
      inherit memory;
      start = memory."${key}";
      period = idx - memory."${key}";
    }
    else performCycles finalPlatform (idx + 1) (memory // { "${key}" = idx; });


  mod = x: y: x - (x / y) * y;

  cycles = (performCycles platform 0 { });

  N = 1000000000;
  # Index of the state of the N-th iteration in the state cycle.
  idx = cycles.start + (mod (N - cycles.start) cycles.period);

  # Find the platform state for the N-th iteration.
  finalPlatform = head (filter (r: cycles.memory."${r}" == idx - 1) (builtins.attrNames cycles.memory));
  partTwo = computeLoad (builtins.fromJSON finalPlatform);

in
{ inherit partOne partTwo; }
