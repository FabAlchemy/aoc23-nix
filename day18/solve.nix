{ input }:
let
  inherit (builtins)
    elemAt
    filter
    foldl'
    fromJSON
    genList
    head
    isString
    length
    match
    readFile
    replaceStrings
    split
    stringLength
    ;

  # Returns true if a value is a non-empty string
  isNonEmptyString = s: isString s && stringLength s != 0;

  # Split a string into a list of strings where the pattern is found
  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  steps = splitString "\n" (readFile input);

  parseStepPartOne = step:
    let
      splitStep = splitString " " step;
      direction = head splitStep;
      moves = fromJSON (elemAt splitStep 1);
    in
    { inherit direction moves; };

  isDigit = s: (match "([0-9])" s) != null;

  reverse = list:
    let len = length list;
    in
    genList (n: elemAt list (len - n - 1)) len;

  pow = x: n: if n == 0 then 1 else x * (pow x (n - 1));

  hexStringToNumber = hex:
    (foldl'
      (state: digit:
        let
          value =
            if isDigit digit then fromJSON digit
            else if digit == "a" then 10
            else if digit == "b" then 11
            else if digit == "c" then 12
            else if digit == "d" then 13
            else if digit == "e" then 14
            else 15;
        in
        { value = state.value + value * (pow 16.0 state.i); i = state.i + 1; })
      { value = 0.0; i = 0; }
      (reverse hex)).value
  ;

  parseStepPartTwo = step:
    let
      splitStep = splitString " " step;
      instruction = replaceStrings [ "(#" ")" ] [ "" "" ] (elemAt splitStep 2);
      moves = hexStringToNumber (splitString "" (head (match "([0-9a-f]{5}).*" instruction)));
      intDirection = head (match ".*([0-3])" instruction);

      # 0 means R, 1 means D, 2 means L, and 3 means U
      direction =
        if intDirection == "0" then "R"
        else if intDirection == "1" then "D"
        else if intDirection == "2" then "L"
        else # if intDirection == "3" then 
          "U";
    in
    { inherit direction moves; };


  findContour = stepParser: steps: foldl'
    (state: step:
      let
        point = head state;
        parsedStep = stepParser step;
        x =
          if parsedStep.direction == "U" then point.x - parsedStep.moves
          else if parsedStep.direction == "D" then point.x + parsedStep.moves
          else point.x;
        y =
          if parsedStep.direction == "L" then point.y - parsedStep.moves
          else if parsedStep.direction == "R" then point.y + parsedStep.moves
          else point.y;
        moves = point.moves + (parsedStep.moves);

      in
      [{ inherit x y moves; }] ++ state
    )
    [{ x = 0.0; y = 0.0; moves = 0.0; }]
    steps;


  # Compute the area of the polygon described by the loop
  # using the Shoelace formula.
  shoelace = tiles:
    foldl'
      (
        area: i:
          let
            a = (elemAt tiles i);
            b = elemAt tiles (i + 1);
          in
          area + (((a.y + b.y) * (a.x - b.x) / 2.0))
      )
      0.0
      (genList (n: n) ((length tiles) - 1));

  area = stepParser: steps:
    let
      contour = (findContour stepParser steps);
      contourLength = (head contour).moves;
    in
    # using Pick's theorem, JSON to get all the digits
    builtins.toJSON ((shoelace contour) + (contourLength / 2.0) + 1.0);

in
{
  partOne = area parseStepPartOne steps;
  partTwo = area parseStepPartTwo steps;
}
