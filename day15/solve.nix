{ input }:
let
  inherit (builtins)
    any
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
    split
    stringLength
    substring
    ;

  # Returns true if a value is a non-empty string
  isNonEmptyString = s: isString s && stringLength s != 0;

  # Split a string into a list of strings where the pattern is found
  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  ASCII_CHARACTERS = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
  ASCII_MAP = foldl' (finalBoxes: n: finalBoxes // { "${substring n 1 ASCII_CHARACTERS}" = n + 32; }) { } (genList (n: n) (stringLength ASCII_CHARACTERS));

  # Retrieve the ASCII code of a character.
  char_to_ascii = char: ASCII_MAP."${char}";

  # x % y
  mod = x: y: x - (x / y) * y;

  steps = (splitString "," (head (splitString "\n" (readFile input))));

  hash = value:
    foldl'
      (curr: char: mod ((curr + char_to_ascii char) * 17) 256)
      0
      (splitString "" value);

  sum = foldl' (finalBoxes: x: finalBoxes + x) 0;
  partOne = sum (map hash steps);

  parseStep = step:
    let
      parts = match "([a-z]+)([-=])([0-9]*)" step;
      label = head parts;
      action = elemAt parts 1;
      focalLength = if action == "=" then fromJSON (elemAt parts 2) else 0;
    in
    { inherit label action focalLength; };

  makeBoxes = genList (n: [ ]) 256;

  executeStep = boxes: step:
    if step.action == "-"
    then removeLens boxes step.label
    else # step.action == "="
      placeLens boxes step.label step.focalLength;

  # (number -> a -> b) -> [a] -> [b]
  mapEnumerate = op: list:
    genList (n: op n (elemAt list n)) (length list);

  # Remove the lens with `label` from its associated (via hash) box.
  removeLens = boxes: label:
    let boxIndex = hash label;
    in
    mapEnumerate
      (n: box:
        if n == boxIndex
        then filter (lens: lens.label != label) box
        else box
      )
      boxes
  ;

  # Place the lens with `label` in its associated box,
  # replacing it if a lens with the same label is already present.
  placeLens = boxes: label: focalLength:
    let boxIndex = hash label;
    in
    mapEnumerate
      (n: box:
        if n == boxIndex
        then
          if any (lens: lens.label == label) box
          then map (lens: if lens.label == label then { inherit label focalLength; } else lens) box
          else box ++ [{ inherit label focalLength; }]
        else box
      )
      boxes;

  finalBoxes = foldl'
    executeStep
    makeBoxes
    (map parseStep steps);

  partTwo = sum (mapEnumerate
    (n: box:
      (n + 1) * sum (mapEnumerate (slot: lens: (slot + 1) * lens.focalLength) box))
    finalBoxes);

in
{ inherit partOne partTwo; }
