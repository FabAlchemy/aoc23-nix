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
    ;

  # Returns true if a value is a non-empty string
  isNonEmptyString = s: isString s && stringLength s != 0;

  # Split a string into a list of strings where the pattern is found
  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  stringToList = string:
    builtins.genList
      (n: builtins.substring n 1 string)
      (builtins.stringLength string)
  ;

  world = map stringToList (splitString "\n" (readFile input));
  height = length world;
  width = length (head world);

  # Retrieve the symbol of the tile at {x, y}.
  getTile = { x, y, ... }: elemAt (elemAt world y) x;

  # Find the tile with `symbol`, starting at (x, y).
  findTile = symbol:
    let
      findTileInner = { x, y }:
        let
          row = elemAt world y;
          value = elemAt row x;
        in
        if value == symbol
        then { inherit x y; }
        else
          if (x + 1) == width
          then
            findTileInner { x = 0; y = y + 1; }
          else
            findTileInner { x = x + 1; y = y; };
    in
    findTileInner { x = 0; y = 0; };

  start = findTile "S";

  # Helper that returns an "optional" list containing the tile coordinates
  # if the boolean is true and the tile describes a connected pipe.
  getConnectedTile = bool: node:
    let tile = getTile node; in
    if bool && (tile == "." || tile == "S") then [ node ] else [ ];

  # Retrieve a list of the connected tiles.
  # x for the column, y for the row
  # {x, y} -> [{x, y}]
  getConnectedTiles = { x, y, distance }:
    let
      left = getConnectedTile (x != 0) { x = x - 1; y = y; };
      right = getConnectedTile (x != width - 1) { x = x + 1; y = y; };
      top = getConnectedTile (y != 0) { x = x; y = y - 1; };
      bottom = getConnectedTile (y != height - 1) { x = x; y = y + 1; };
    in
    map (n: n // { distance = distance + 1; }) (left ++ bottom ++ right ++ top);

  nodeTag = node: "${builtins.toJSON node.x + "-" + builtins.toJSON node.y}";
  mod = x: y: x - (x / y) * y;
  greaterThanOrEqual = x: y: lessThan y x;

  depthFirstSearch = maxDistance: node@{ x, y, distance }: visited:
    let
      tag = nodeTag node;
      newVisited = visited // { ${tag} = distance; };
      nextNodes = (getConnectedTiles node);
    in
    if
      greaterThanOrEqual distance maxDistance || (hasAttr tag visited) && !lessThan distance visited.${tag}
    then visited
    else
      foldl' (visited: node: depthFirstSearch maxDistance node visited) newVisited nextNodes;

  walk = start: maxDistance:
    let
      visited = (depthFirstSearch maxDistance (start // { distance = 0; }) { });
    in
    # only the visited nodes with the correct parity
      # (because you spend the last move going backwards, recursively)
    length (filter (node: (mod visited.${node} 2) == (mod maxDistance 2)) (attrNames visited));

  partOne = walk start 64;

  # Not a general solution, but the personalized input have additional properties
  # that make a tracatable solution possible.
  # This is based of explanations found on the Reddit mega-thread.
  partTwo =
    let
      squareA = walk start 129;
      squareB = walk start 130;
      smallTriangleA = walk { x = 0; y = 0; } 64;
      smallTriangleB = walk { x = 0; y = 130; } 64;
      smallTriangleC = walk { x = 130; y = 0; } 64;
      smallTriangleD = walk { x = 130; y = 130; } 64;
      bigTriangleA = walk { x = 0; y = 0; } 195;
      bigTriangleB = walk { x = 0; y = 130; } 195;
      bigTriangleC = walk { x = 130; y = 0; } 195;
      bigTriangleD = walk { x = 130; y = 130; } 195;
      tailA = walk { x = 0; y = 65; } 130;
      tailB = walk { x = 65; y = 0; } 130;
      tailC = walk { x = 65; y = 130; } 130;
      tailD = walk { x = 130; y = 65; } 130;

      calculateNumberOfSquares = branche: parity:
        let
          calculateContribution = { n, amount }:
            if (mod n 2 == parity) then amount else 0;
        in
        foldl' (res: x: res + x) 0
          (map (n: calculateContribution { n = n; amount = n * 4; }) (genList (x: x) branche));

      steps = 26501365;
      dimension = 131;
      branching = steps / dimension;
      numberOfASquares = 1 + calculateNumberOfSquares branching 0;
      numberOfBSquares = calculateNumberOfSquares branching 1;

      rectangles = numberOfASquares * squareA + numberOfBSquares * squareB;
      bigTriangles = bigTriangleA + bigTriangleB + bigTriangleC + bigTriangleD;
      smallTriangles = smallTriangleA + smallTriangleB + smallTriangleC + smallTriangleD;
      tails = tailA + tailB + tailC + tailD;
    in
    rectangles + (branching - 1) * bigTriangles + branching * smallTriangles + tails;

in
{ inherit partOne partTwo; }
