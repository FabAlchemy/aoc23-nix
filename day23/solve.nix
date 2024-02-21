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
    toJSON
    ;

  isNonEmptyString = s: isString s && stringLength s != 0;

  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  stringToList = string:
    builtins.genList
      (n: builtins.substring n 1 string)
      (builtins.stringLength string)
  ;

  world = map stringToList (splitString "\n" (readFile input));
  height = length world;
  width = length (head world);

  start = { x = 1; y = 0; };
  end = { x = width - 2; y = height - 1; };

  getTile = { x, y, ... }: elemAt (elemAt world y) x;

  lessThanOrEqual = x: y: x == y || lessThan x y;

  isInGrid = { x, y }:
    lessThanOrEqual 0 x && lessThan x width
    && lessThanOrEqual 0 y && lessThan y height;

  isNotWall = tile@{ x, y }: getTile tile != "#";

  getNeighbors = isSlippy: tile@{ x, y }:
    let
      right = { x = x + 1; inherit y; };
      left = { x = x - 1; inherit y; };
      down = { y = y + 1; inherit x; };
      up = { y = y - 1; inherit x; };

      nextMap = {
        ">" = [ right ];
        "<" = [ left ];
        "v" = [ down ];
        "^" = [ up ];
        "." = [ right left down up ];
      };
      char = getTile tile;
      next = if isSlippy then nextMap.${char} else [ right left down up ];
    in
    filter (neighbor: isInGrid neighbor && isNotWall neighbor) next;

  # Build a sublist of `count` element from `list`
  # lisŧ[start..(start + count)]
  sublist =
    list:
    start:
    count:
    let
      len = length list;
    in
    genList
      (n: elemAt list (n + start))
      (
        if lessThanOrEqual len start then 0 else
        if lessThan len (start + count) then len - start
        else count
      );

  # Find the first element in `list` for which `predicate` returns `true`.
  findFirstIndex = list: predicate:
    (foldl'
      (state: value:
        if state.answer == null && predicate value
        then { answer = state.index; index = state.index + 1; }
        else { inherit (state) answer; index = state.index + 1; }
      )
      {
        answer = null;
        index = 0;
      }
      list).answer;

  # Priority queue
  # The queue is a constantly ordered list of {element, prio} values.
  # Using a proper heap might be faster.

  q_is_empty = q: length q == 0;
  q_extract_min = q: { min = head q; q = tail q; };

  q_add_with_priority = q: element: prio:
    let
      insertPosition = findFirstIndex q (e: !lessThan e.prio prio);
      left = sublist q 0 insertPosition;
      right = sublist q insertPosition (length q - insertPosition);
    in
    if insertPosition == null
    then q ++ [{ inherit element prio; }]
    else
      left ++ [{ inherit element prio; }] ++ right;

  isIntersection = isSlippy: tile:
    tile == start || tile == end
    || lessThan 2 (length (getNeighbors isSlippy tile));

  findNextJunctions = isSlippy: visited: queue:
    if q_is_empty queue then [ ]
    else
      let
        queueAndMin = q_extract_min queue;
        queueRest = queueAndMin.q;
        head = queueAndMin.min;
        node = head.element;
        prio = head.prio;
      in
      if lessThan 0 prio && isIntersection isSlippy node then [ head ] ++ findNextJunctions isSlippy visited queueRest
      else
        let
          neighbors = filter (neighbor: !elem neighbor visited) (getNeighbors isSlippy node);
          visited' = visited ++ neighbors;
          queue' = foldl'
            (queue: neighbor: q_add_with_priority queue neighbor (prio + 1))
            queueRest
            neighbors;
        in
        findNextJunctions isSlippy visited' queue';

  nodeToJSON = node: "(${toJSON node.x}, ${toJSON node.y})";

  treatIntersection = isSlippy: visited: graph: queue:
    if q_is_empty queue then graph
    else
      let
        node = head queue;
        queueRest = tail queue;
        nextIntersections = findNextJunctions isSlippy [ node ] (q_add_with_priority [ ] node 0);
        unseenIntersections = filter (inter: !elem inter visited) (map (i: i.element) nextIntersections);
        graph' = graph // { ${nodeToJSON node} = nextIntersections; };
        visited' = visited ++ unseenIntersections;
        queue' = queueRest ++ unseenIntersections;
      in
      treatIntersection isSlippy visited' graph' queue';

  makeGraph = isSlippy: treatIntersection isSlippy [ start ] { } [ start ];

  max = x: y: if lessThan x y then y else x;
  maxList = list: foldl' max (head list) list;

  findLongestPath = isSlippy:
    let
      graph = makeGraph isSlippy;
      go = visited: len: current:
        if current == end then len
        else
          let
            visited' = (visited) ++ [ current ];
            neighbors = (filter (node: !elem node.element visited') graph.${nodeToJSON current});
            best = maxList ((map ({ element, prio }: go visited' (len + prio) element) neighbors) ++ [ 0 ]);
          in
          best;
    in
    go [ start ] 0 start;

in
{
  partOne = findLongestPath true;
  partTwo = findLongestPath false;
}
