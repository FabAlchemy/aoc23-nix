{ input }:
let
  inherit (builtins)
    concatLists
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

  parseNumberList = s: map fromJSON (splitString " " s);

  chunks = splitString "\n\n" (readFile input);
  seeds = parseNumberList (elemAt (splitString ": " (head chunks)) 1);

  splittedMaps = map (c: tail (splitString "\n" c)) (tail chunks);

  # Convert a range map to its descriptive attrset
  parseMap = s:
    let
      numbers = parseNumberList s;
    in
    {
      destination = elemAt numbers 0;
      source = elemAt numbers 1;
      length = elemAt numbers 2;
    };

  # The groups of range maps.
  # [[rangeMap]]
  rangeMaps = map (map parseMap) splittedMaps;

  lessThanOrEqual = x: y: lessThan x y || x == y;

  # Apply a range map to a value, shifting it if necessary.
  applyRangeMapToValue = rangeMap: x:
    if lessThanOrEqual rangeMap.source x && lessThanOrEqual x (rangeMap.source + rangeMap.length)
    then (rangeMap.destination + (x - rangeMap.source))
    else x
  ;

  # Apply a group of range maps to a list of values.
  applyRangeMapsToValues = value: maps: map
    (source:
      foldl'
        (current: rangeMap:
          if current == source
          then applyRangeMapToValue rangeMap current
          else current)
        source
        maps)
    value;

  # Apply all the range map groups to the seed values.
  updatedSeeds = foldl'
    applyRangeMapsToValues
    seeds
    rangeMaps;

  # Return the smallest element of a list
  listMin = l: foldl' (x: y: if lessThan x y then x else y) (head l) l;

  partOne = listMin updatedSeeds;

  seedRanges = genList
    (n: {
      start = (elemAt seeds (2 * n));
      length = (elemAt seeds (2 * n + 1));
    })
    ((length seeds) / 2);

  min = (x: y: if lessThan x y then x else y);
  max = (x: y: if lessThan x y then y else x);

  # Truncate a `range` so that it is contained in `initial`
  truncateRange = initial: range:
    let
      start = max range.start initial.start;
      end = min (range.start + range.length) (initial.start + initial.length);
      length = end - start;
    in
    {
      inherit start length;
    };

  # Shift a `range` by `delta`
  shiftRange = range: delta: {
    inherit (range) length;
    start = range.start + delta;
  };

  # Apply a range map to a range.
  # This results in a list of `splitted` ranges as well as a `shifted` range.
  # These ranges might be empty (length <= 0).
  # {start, length} -> {source, length, destination} -> [{shifted = range, splitted = [range]}]
  splitRangeForMap = rangeMap: range:
    let
      truncate = truncateRange range;
      rangeMapEnd = rangeMap.source + rangeMap.length;
      rangeEnd = range.start + range.length;
    in
    {
      splitted = [
        (truncate { start = range.start; length = (rangeMap.source - range.start); })
        (truncate { start = rangeMapEnd; length = (rangeEnd - rangeMapEnd); })
      ];
      shifted = (shiftRange (truncate { start = rangeMap.source; length = rangeMap.length; }) (rangeMap.destination - rangeMap.source));
    }
  ;

  # Remove the empty ranges from a list of ranges.
  # [range] -> [range]
  filterOutEmptyRanges = ranges: filter (r: !lessThan r.length 1) ranges;

  # Apply a list of range maps to a list of ranges.
  # [rangeMap] -> [range] -> [range]
  splitRangesForMaps = rangeMaps: initRanges:
    foldl'
      (res: rangeMap:
        let
          split = map (splitRangeForMap rangeMap) res.splitted;
        in
        {
          splitted = filterOutEmptyRanges (concatLists (map (s: s.splitted) split));
          shifted = res.shifted ++ filterOutEmptyRanges (map (s: s.shifted) split);
        })
      { splitted = initRanges; shifted = [ ]; }
      rangeMaps;

  # Apply the groups of range maps to the ranges.
  updatedRanges = foldl'
    (ranges: rangeMap:
      let
        res = splitRangesForMaps rangeMap ranges;
      in
      res.splitted ++ res.shifted)
    seedRanges
    rangeMaps;

  # The result is the minimum value described by a range, that is the minimum start value.
  partTwo = listMin (map (v: v.start) updatedRanges);
in
{ inherit partOne partTwo; }
