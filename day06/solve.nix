{ input }:
let
  inherit (builtins)
    ceil
    elemAt
    filter
    floor
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

  lines = splitString "\n" (readFile input);
  extractNumbers = s: map fromJSON (tail (splitString " " s));

  raceDurations = extractNumbers (head lines);
  raceDistances = extractNumbers (elemAt lines 1);

  races = genList (n: { duration = elemAt raceDurations n; distance = elemAt raceDistances n; }) (length raceDurations);

  # distance traveled after pressing the button for `t`:
  # d(t) = (T - t) * v(t)
  # where:
  # - v(t) = t is the speed of the boat after pressing the button for `t`
  # - T is the race duration

  # we win the race if d(t) > D where D is the race length

  # The distance traveled by the boat after pressing the button for `t`.
  computeDistance = T: t: (T - t) * t;

  # Return the list of winning times for a given `race`.
  raceWinningTimes = race: filter (t: lessThan race.distance (computeDistance race.duration t)) (genList (x: x) (race.duration + 1));

  count = list: foldl' (x: y: x + 1) 0 list;
  mul = l: foldl' builtins.mul 1 l;

  partOne = mul (map count (map raceWinningTimes races));

  concatStrings = l: foldl' (res: s: res + s) "" l;
  extractNumbersIgnoreWhitespace = s: fromJSON (concatStrings (tail (splitString " " s)));

  raceDuration = extractNumbersIgnoreWhitespace (head lines);
  raceDistance = extractNumbersIgnoreWhitespace (elemAt lines 1);

  # we win the race if d(t) > D where D is the race length
  # this gives the following quadratic equation
  # -t^2 + T * t - D > 0
  #
  # the solution is:
  # t = (T ± sqrt(T^2 - 4D)) / 2

  # Return the maximum value between x and y
  max = x: y: if lessThan x y then y else x;

  # Return the absolute value of `x`
  abs = x: max x (-x);

  # Compute the square root of `n` using Newton's method
  sqrt = n:
    let
      epsilon = 0.7; # smaller values cause stack overflows with the real input
      improve = x: (x + n / x) / 2;
      isGoodEnough = x: lessThan (abs (x * x - n)) epsilon;
      iterate = x: if isGoodEnough x then x else iterate (improve x);
    in
    iterate 1.0;

  minTime = ceil ((raceDuration - (sqrt (raceDuration * raceDuration - 4 * raceDistance))) / 2);
  maxTime = floor ((raceDuration + (sqrt (raceDuration * raceDuration - 4 * raceDistance))) / 2);

  partTwo = maxTime - minTime + 1; # add 1 because the last time is valid!
in
{ inherit partOne partTwo; }
