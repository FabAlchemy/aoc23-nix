{ input }:
let
  inherit (builtins)
    attrNames
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
    toJSON
    ;

  # Returns true if a value is a non-empty string
  isNonEmptyString = s: isString s && stringLength s != 0;

  # Split a string into a list of strings where the pattern is found
  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  # The parsed contraption layout.
  contraption = map (splitString "") (splitString "\n" (readFile input));
  height = length contraption;
  width = length (head contraption);

  # Compact representation of a light ray.
  hashRay = { row, col, direction }: "${toJSON row}-${toJSON col}--${direction}";

  # Retrieve the symbol of the tile at {x, y}.
  getTile = { col, row }: elemAt (elemAt contraption row) col;

  # Find out how a ray is impacted by the contraption at its position.
  propagateRay = { row, col, direction }:
    let
      tile = getTile { inherit row col; };
      makeRays = map (d: { inherit row col; direction = d; });
    in
    if (direction == "u" || direction == "d") then
      if tile == "-"
      then makeRays [ "l" "r" ]
      else if tile == "/" then
        if direction == "u"
        then makeRays [ "r" ]
        else # direction == "d"
          makeRays [ "l" ]
      else if tile == "\\" then
        if direction == "u"
        then makeRays [ "l" ]
        else # direction == "d"
          makeRays [ "r" ]
      else # if tile == "." || tile == "|"
        makeRays [ direction ]
    else # if (direction == "l" || direction == "r") then
      if tile == "|"
      then makeRays [ "u" "d" ]
      else if tile == "/" then
        if direction == "l"
        then makeRays [ "d" ]
        else # direction == "r"
          makeRays [ "u" ]
      else if tile == "\\" then
        if direction == "l"
        then makeRays [ "u" ]
        else # direction == "r"
          makeRays [ "d" ]
      else # if tile == "." || tile == "-"
        makeRays [ direction ];

  # Compute how a ray will propagate for one step.
  nextRays = { col, row, direction }:
    if direction == "u" && (row != 0) then
      propagateRay { inherit direction col; row = row - 1; }
    else if direction == "d" && (row != height - 1) then
      propagateRay { inherit direction col; row = row + 1; }
    else if direction == "r" && (col != width - 1) then
      propagateRay { inherit direction row; col = col + 1; }
    else if direction == "l" && (col != 0) then
      propagateRay { inherit direction row; col = col - 1; }
    else [ ];

  # Propagate a single ray on the contraption.
  propagate =
    ray@{ row, col, direction }:
    visited:

    let
      key = hashRay ray;
    in
    if hasAttr key visited then { }
    else
      foldl'
        (res: ray: res // (propagate ray res))
        (visited // { "${key}" = true; })
        (nextRays ray);

  # Count the energized tiles after ray propagation
  # (strip the direction part of the visited hashset keys).
  countEnergized = ray:
    let
      start = head (propagateRay ray);
      visited = propagate start { };
      nodes = foldl'
        (res: s: res // { "${head (splitString "--" s)}" = 0; })
        { }
        (attrNames visited);
    in
    length (attrNames nodes);

  partOne = countEnergized { row = 0; col = 0; direction = "r"; };

  flatten = list: foldl' (res: l: res ++ l) [ ] list;
  starts = flatten
    (genList
      (n: [
        { row = n; col = 0; direction = "r"; }
        { row = 0; col = n; direction = "d"; }
        { row = height - 1; col = n; direction = "u"; }
        { row = n; col = width - 1; direction = "l"; }
      ])
      (height)
    );
  max = a: b: if lessThan a b then b else a;
  maxList = list: foldl' max (head list) list;
  partTwo = maxList (map (start: countEnergized start) starts);

in
{ inherit partOne partTwo; }
