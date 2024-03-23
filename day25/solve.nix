{ input }:
let
  inherit (builtins)
    attrNames
    elem
    elemAt
    filter
    foldl'
    genList
    hashString
    head
    isString
    length
    lessThan
    mapAttrs
    readFile
    replaceStrings
    split
    stringLength
    substring
    ;

  isNonEmptyString = s: isString s && stringLength s != 0;
  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  lines = (splitString "\n" (readFile input));

  parseNode = str:
    let
      nameAndOthers = splitString ": " str;
      name = head nameAndOthers;
      others = splitString " " (elemAt nameAndOthers 1);
    in
    { inherit name others; };

  connections = map parseNode lines;

  graphRaw = foldl'
    (graph: connection:

      foldl' (graph: name: graph // { ${name} = (graph.${name} or [ ]) ++ [ connection.name ]; }) graph connection.others
      // { ${connection.name} = (graph.${connection.name} or [ ]) ++ connection.others; }
    )
    { }
    connections;

  listToSet = foldl'
    (set: item: set // { ${item} = true; })
    { };

  graph = mapAttrs (node: neighbors: (attrNames (listToSet neighbors))) graphRaw;

  add = x: y: x + y;
  sum = foldl' add 0;

  argMax = list: f:
    if length list == 0
    then builtins.throw "empty list"
    else
      (foldl'
        ({ arg, value }@state: x:
          if lessThan value (f x)
          then { arg = x; value = (f x); }
          else state)
        { arg = (head list); value = (f (head list)); }
        list).arg;


  mod = a: b: a - b * (a / b);

  seed = replaceStrings [ "-" "\n" ] [ "" "" ] (readFile /proc/sys/kernel/random/uuid);

  # Reworked from https://github.com/figsoda/rand-nix
  rng = seed: rec {
    int = (fromTOML "x=0x${substring 28 8 (hash seed)}").x;
    hash = hashString "sha256";
    intBetween = x: y: x + mod int (y - x);
    next = rng (hash seed);
  };

  shuffleList = rng: list:
    foldl'
      ({ rng, list, res }: _item:
        let
          index = rng.intBetween 0 (length list);
          element = elemAt list index;
        in
        {
          rng = rng.next;
          list = filter (x: x != element) list;
          res = res ++ [ element ];
        })
      { inherit rng list; res = [ ]; }
      list
  ;


  solve = component: rng:
    let
      shuffleResult = shuffleList rng component;
      nextRng = shuffleResult.rng.next;
      nodes = shuffleResult.res;

      countForeignNeighbors = node: length (filter (n: !elem n nodes) (graph.${node}));
      foreignNeighborCounts = map countForeignNeighbors nodes;
      mostConnectedIndex = argMax (genList (i: i) (length foreignNeighborCounts)) (elemAt foreignNeighborCounts);
      mostConnected = elemAt nodes mostConnectedIndex;

      newComponent = (filter (n: n != mostConnected) component);

      inComponentCount = length nodes;
      outComponentCount = length (attrNames graph) - inComponentCount;
    in
    if sum (foreignNeighborCounts) == 3
    then inComponentCount * outComponentCount
    else solve newComponent nextRng;
in
{
  partOne = solve (attrNames graph) (rng seed);
}
