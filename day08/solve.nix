{ input }:
let
  inherit (builtins)
    attrNames
    elemAt
    filter
    foldl'
    head
    isString
    length
    match
    readFile
    split
    stringLength
    tail
    ;

  # Returns true if a value is a non-empty string
  isNonEmptyString = s: isString s && stringLength s != 0;

  # Split a string into a list of strings where the pattern is found
  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  instructionsAndNodes = splitString "\n\n" (readFile input);

  instructions = splitString "" (head instructionsAndNodes);

  # Parse the nodes as an attribute set { node: { "L": node; "R": node; } }
  nodes =
    let
      list = splitString "\n" (head (tail instructionsAndNodes));
      parseNode = node:
        let matches = match "([A-Z0-9]+) = \\(([A-Z0-9]+), ([A-Z0-9]+)\\)" node; in
        {
          "${head matches}" = {
            "L" = elemAt matches 1;
            "R" = elemAt matches 2;
          };
        };
    in
    foldl' (res: node: res // parseNode node) { } list;

  # Compute x % y
  mod = x: y: x - (x / y) * y;

  # Return `true` if `node` ends with `letter`.
  nodeEndsWith = letter: node:
    let
      letters = splitString "" node;
    in
    (elemAt letters 2) == letter;

  # Follow the `instructions` for a given `node` in `nodes`,
  # returning the number of steps taken to reach the end 
  followInstructions = instructions: nodes: node: steps:
    let
      nextStep = steps + 1;
      instruction = elemAt instructions (mod steps (length instructions));
      nextNode = nodes.${node}.${instruction};
    in
    if nodeEndsWith "Z" nextNode then nextStep
    else followInstructions instructions nodes nextNode nextStep;

  partOne = followInstructions instructions nodes "AAA" 0;

  startNodes = filter
    (nodeEndsWith "A")
    (attrNames nodes);

  # Compute the Greatest Common Denominator of a and b
  gcd = a: b: if b == 0 then a else gcd b (mod a b);

  # Compute the Least Common Multiple of a and b
  lcm = a: b: a / (gcd a b) * b;

  # Compute the LCM of a list
  lcmList = list: foldl' (res: x: lcm res x) (head list) (tail list);

  # This works because the provided input "conveniently" loops on each
  # path with the same period, but it's not really a general solution.
  partTwo = lcmList (map
    (node: followInstructions instructions nodes node 0)
    startNodes);
in
{ inherit partOne partTwo; }
