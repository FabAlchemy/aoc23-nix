{ input }:
let
  inherit (builtins)
    elem
    elemAt
    filter
    foldl'
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

  pipes = map (splitString "") (splitString "\n" (readFile input));

  height = length pipes;
  width = length (head pipes);

  # Retrieve the symbol of the tile at {x, y}.
  getTile = { x, y }: elemAt (elemAt pipes y) x;

  # Find the tile with `symbol`, starting at (x, y).
  findTile = symbol: { x, y }:
    let
      row = elemAt pipes y;
      value = elemAt row x;
    in
    if value == symbol
    then { inherit x y; }
    else
      if (x + 1) == width
      then
        findTile symbol { x = 0; y = y + 1; }
      else
        findTile symbol { x = x + 1; y = y; };

  # Helper that returns an "optional" list containing the tile coordinates
  # if the boolean is true and the tile describes a connected pipe.
  getConnectedTile = bool: coords: validTiles:
    if bool && (elem (getTile coords) validTiles) then [ coords ] else [ ];

  # Retrieve a list of the connected tiles.
  # x for the column, y for the row
  # {x, y} -> [{x, y}]
  getConnectedTiles = { x, y }:
    let
      tile = getTile { inherit x y; };
      left = getConnectedTile
        ((x != 0) && elem tile [ "S" "-" "J" "7" ])
        { x = x - 1; y = y; } [ "-" "F" "L" "S" ];
      right = getConnectedTile
        ((x != width - 1) && elem tile [ "S" "L" "F" "-" ])
        { x = x + 1; y = y; }
        [ "-" "J" "7" "S" ];
      top = getConnectedTile
        ((y != 0) && elem tile [ "S" "L" "J" "|" ])
        { x = x; y = y - 1; }
        [ "|" "F" "7" "S" ];
      bottom = getConnectedTile
        ((y != height - 1) && elem tile [ "S" "F" "7" "|" ])
        { x = x; y = y + 1; }
        [ "|" "J" "L" "S" ];
    in
    left ++ bottom ++ right ++ top;

  # Indicate if a list is empty.
  isEmpty = list: lessThan (length list) 1;

  # Recursively compute the route from `origin` to `destination`.
  # You might need to increase the stack limit (`ulimit -s`).
  # {x, y} -> {x, y} -> [{x, y}] -> [{x, y}]
  depthFirstSearch = origin: destination: visited:
    if (getTile origin) == (getTile destination)
    then [ destination ]
    else
      let
        newVisited = visited ++ [ origin ];
        connected = filter (tile: !elem tile newVisited) (getConnectedTiles origin);
        route = depthFirstSearchMulti connected destination newVisited;
      in
      if isEmpty route
      then route
      else [ origin ] ++ route;

  # Recursively compute the path from each origin to `destination`.
  # (if the first origin does not work, try the remaining ones)
  # [{x, y}] -> {x, y} -> [{x, y}] -> [{x, y}]
  depthFirstSearchMulti = origins: destination: visited:
    if isEmpty origins
    then origins
    else
      let
        route = depthFirstSearch (head origins) destination visited;
      in
      if isEmpty route
      then depthFirstSearchMulti (tail origins) destination visited
      else route;

  start = findTile "S" { x = 0; y = 0; };

  # The large, continuous loop we're looking for.
  loop = [ start ] ++ depthFirstSearchMulti (getConnectedTiles start) start [ ];
  partOne = length loop / 2;

  # Compute the area of the polygon described by the loop
  # using the Shoelace formula.
  shoelace = tiles:
    foldl'
      (
        res: i:
          let
            a = elemAt tiles i;
            b = elemAt tiles (i + 1);
          in
          res + (a.y + b.y) * (a.x - b.x)
      )
      0
      (genList (n: n) ((length tiles) - 1));

  # Pick's theorem to compute the number of interior points.
  partTwo = ((shoelace loop) / 2) - ((length loop) / 2) + 1;

in
{ inherit partOne partTwo; }
