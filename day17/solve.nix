{ input }:
let
  inherit (builtins)
    elemAt
    filter
    foldl'
    fromJSON
    genList
    hasAttr
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

  # Returns true if a value is a non-empty string
  isNonEmptyString = s: isString s && stringLength s != 0;

  # Split a string into a list of strings where the pattern is found
  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  # The parsed heat loss city maps.
  city = map (line: map fromJSON (splitString "" line)) (splitString "\n" (readFile input));
  height = length city;
  width = length (head city);

  getHeatLoss = { row, col, ... }: elemAt (elemAt city row) col;

  start = { row = 0; col = 0; };
  end = { row = height - 1; col = width - 1; };

  lessThanOrEqual = x: y: lessThan x y || x == y;

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

  makeKey = { row, col, dir, ... }: "${toJSON row}-${toJSON col}-${dir}";

  # Dijkstra with a priority queue
  # Our approach is to generate all possible moves from a node
  # at each step so that we needn't bother with counting moves
  # and direction bookeeping.
  # Also, sorry for the concise attribute names but we need to save memory.
  dijkstra = range:
    let
      q = [
        { element = (start // { dir = "h"; dis = 0; }); prio = 0; }
        { element = (start // { dir = "v"; dis = 0; }); prio = 0; }
      ];
      dist = { };
    in
    dijkstra'' range { inherit dist q; n = 0; done = false; };

  # Tiny hack to "break" the recursion and manage the stack size.
  dijkstra'' = range: state@{ q, dist, n, done }:
    let res = dijkstra' range (state // { n = 0; done = false; });
    in
    if res.done == true
    then res.res
    else
      dijkstra'' range res;

  dijkstra' = range: state@{ q, dist, n, done }:
    let
      splitq = q_extract_min q;
      q' = (splitq.q);
      node = (splitq.min.element);

      state' = foldl'
        (state: neighbor:
          let
            neighborKey = makeKey neighbor;
          in
          if (!hasAttr neighborKey state.dist) || lessThan neighbor.dis state.dist."${neighborKey}"
          then {
            dist = state.dist // { "${neighborKey}" = neighbor.dis; };
            q = q_add_with_priority state.q neighbor neighbor.dis;
            inherit n done;
          }
          else state
        )
        { inherit dist n done; q = q'; }
        (findNeighbors range node);
    in
    if q_is_empty q then builtins.throw "too late"
    else if (node.row == end.row && node.col == end.col) then state // { done = true; res = node.dis; }
    else if lessThan 100 n then state'
    else
      dijkstra' range (state' // { n = state.n + 1; done = false; });

  # Indicate whether a neighbor is out of bounds.
  isInvalidNeighbor = { row, col, ... }:
    (lessThan row 0 || lessThanOrEqual height row || lessThan col 0 || lessThanOrEqual width col);

  # Compute the distance between two nodes.
  neighborDistance = node@{ row, col, ... }: neigh:
    if node.row == neigh.row && node.col == neigh.col
    then node.dis
    else (getHeatLoss neigh) + (neighborDistance node {
      row = if lessThan row neigh.row then neigh.row - 1 else if row == neigh.row then row else neigh.row + 1;
      col = if lessThan col neigh.col then neigh.col - 1 else if col == neigh.col then col else neigh.col + 1;
    });

  optional = cond: value: if cond then value else [ ];

  # List the orthogonal neighbors of `node` for distances described by the `range`.
  allNeighbors = range@{ start, end }: node@{ row, col, dir, ... }:
    if lessThan end start then [ ]
    else
      allNeighbors { start = range.start; end = range.end - 1; } node
      ++ (optional (dir == "v") [
        { inherit col; row = row - end; dir = "h"; }
        { inherit col; row = row + end; dir = "h"; }
      ])
      ++
      (optional (dir == "h") [
        { inherit row; col = col - end; dir = "v"; }
        { inherit row; col = col + end; dir = "v"; }
      ]);

  compareDistances = a: b: lessThan a.dis b.dis;

  # List the valid orthogonal neighbors of `node` for distances described by `range`,
  # and sort them by distance.
  findNeighbors = range: node@{ row, col, dir, ... }:
    builtins.sort compareDistances
      (map (neigh: neigh // { dis = neighborDistance node neigh; })
        (filter (n: !isInvalidNeighbor n) (allNeighbors range node)));
in
{
  partOne = dijkstra { start = 1; end = 3; };
  partTwo = dijkstra { start = 4; end = 10; };
}
