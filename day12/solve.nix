{ input }:
let
  inherit (builtins)
    all
    any
    elemAt
    filter
    foldl'
    fromJSON
    genList
    head
    isString
    length
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

  rawRecords = splitString "\n" (readFile input);

  # [{ springs = [char]; broken = [number]; }]
  records = map
    (record:
      let values = splitString " " record;
      in
      {
        springs = splitString "" (head values);
        broken = map fromJSON (splitString "," (elemAt values 1));
      })
    rawRecords;

  isEmpty = list: length list == 0;

  lessThanOrEqual = x: y: lessThan x y || x == y;

  # Create a sublist of `len` elements from `list` starting at `start`.
  subList = list: start: len:
    if lessThan len 0 then
      [ ]
    else
      genList
        (n: elemAt list (start + n))
        len;

  # Recursively compute the possible number of arrangements for the given
  # spring states and broken groups.
  # This function uses memoization to avoid redundant computations on the same input.
  arrangements = springs: broken: cache:
    let
      # the memoization key
      key = builtins.toJSON [ springs broken ];
      # helper to generate an answer that does not update the cache
      mkAnswer = result: { inherit cache result; };
    in
    if builtins.hasAttr key cache
    then mkAnswer cache."${key}"
    else
      let
        answer =
          if isEmpty springs then
          # no more spring to check
          # the solution is valid solution if we've used up all the broken groups
            if isEmpty broken then mkAnswer 1 else mkAnswer 0
          else if isEmpty broken then
          # no more broken group
          # the solution is valid if there aren't any broken spring left
            if any (s: s == "#") springs then mkAnswer 0 else mkAnswer 1
          else
            let
              spring = head springs;
              rest_of_springs = tail springs;
            in
            if spring == "."
            # the current spring is functional, let's check the rest of the input
            then
              arrangements rest_of_springs broken cache
            else if spring == "#"
            # the current spring is broken, let's see if we can form the first broken group
            then
              let
                group = head broken;
              in
              if # there are enough springs to make a group
                lessThanOrEqual group (length springs)
                # the group does not contain any functional spring
                && all (c: c != ".") (subList springs 0 group)
                # there aren't any spring left or the next is not broken too
                && (length springs == group || elemAt springs group != "#")
              then
                arrangements (subList springs (group + 1) (length springs - group - 1)) (tail broken) cache
              else mkAnswer 0
            else # if spring == "?"
              let
                with_broken = (arrangements ([ "#" ] ++ rest_of_springs) broken cache);
                # the memoization only works for this specific recursive call
                with_functional = (arrangements ([ "." ] ++ rest_of_springs) broken with_broken.cache);
              in
              {
                cache = with_functional.cache;
                result = with_broken.result + with_functional.result;
              }
        ;
      in
      { cache = answer.cache // { "${key}" = answer.result; }; result = answer.result; };

  sum = l: foldl' (res: x: res + x) 0 l;

  partOne = sum (map (r: (arrangements r.springs r.broken { }).result) records);

  repeat = list: x: intersperse:
    if x == 1 then list
    else list ++ intersperse ++ repeat list (x - 1) intersperse;

  arrangementsUnfold = r: arrangements (repeat r.springs 5 [ "?" ]) (repeat r.broken 5 [ ]) { };

  partTwo = sum (map (r: (arrangementsUnfold r).result) records);
in
{ inherit partOne partTwo; }
