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
    lessThan
    match
    readFile
    split
    stringLength
    ;

  isNonEmptyString = s: isString s && stringLength s != 0;
  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  lines = (splitString "\n" (readFile input));

  parseNumber = str: fromJSON (head (match "(-?[0-9]*)" str)) + 0.0;
  parseTriplet = str:
    let parsed = map parseNumber (splitString ",[[:space:]]*" str);
    in { x = (elemAt parsed 0); y = (elemAt parsed 1); z = (elemAt parsed 2); };

  parseLine = str:
    let
      parsed = map parseTriplet (splitString "[[:space:]]*@[[:space:]]*" str);
    in
    { p = elemAt parsed 0; v = elemAt parsed 1; };

  stones = map parseLine lines;

  # x1 = at + b
  # y1 = ct + d
  # 
  # x2 = et + f
  # y2 = gt + h
  # 
  # at + b = et' + f
  # ct + d = gt' + h
  # 
  # t' = (at + b - f) / e
  # t = (gt' + h - d)/c
  #   = g(at + b - f) / ce + h/c - d/c
  #   = gat/ce + gb/ce - gf/ce + h/c - d/c
  #   = (gb - gf)/ce + (h-d)/c) / (1 - ga/ce)
  intersect = stoneA: stoneB:
    let
      a = stoneA.v.x;
      b = stoneA.p.x;
      c = stoneA.v.y;
      d = stoneA.p.y;

      e = stoneB.v.x;
      f = stoneB.p.x;
      g = stoneB.v.y;
      h = stoneB.p.y;

      t = ((g * b - g * f) / (c * e) + ((h - d) / c)) / (1 - ((g * a) / (c * e)));
      t' = (a * t + b - f) / e;
    in
    if c == 0 || e == 0 || (1 - ((g * a) / (c * e))) == 0 || lessThan t 0 || lessThan t' 0 then null
    else { x = a * t + b; y = c * t + d; };

  lessThanOrEqual = x: y: x == y || lessThan x y;

  isInBounds = { x, y }: { min, max }:
    lessThanOrEqual min x && lessThanOrEqual x max
    && lessThanOrEqual min y && lessThanOrEqual y max;

  intersectInBounds = stoneA: stoneB: bounds:
    let
      intersection = (intersect stoneA stoneB);
    in
    if intersection == null then false
    else isInBounds intersection bounds;

  countIntersections = bounds:
    foldl'
      (state:
        stoneA:
        foldl'
          (state: stoneB: state + (if stoneA == stoneB
          then 0 else
            if (intersectInBounds stoneA stoneB bounds) then 1 else 0))
          state
          stones)
      0
      stones;

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

  get = matrix: row: col:
    elemAt (elemAt matrix row) col;

  set = matrix: targetRow: targetCol: newValue:
    genList
      (rowIndex:
        let
          row = elemAt matrix rowIndex;
        in
        genList
          (colIndex:
            let currentValue = elemAt row colIndex;
            in
            if rowIndex == targetRow && colIndex == targetCol
            then newValue
            else currentValue
          )
          (length row)
      )
      (length matrix);

  swap_rows = matrix: i: j:
    let
      a = elemAt matrix i;
      b = elemAt matrix j;
    in
    genList
      (rowIndex:
        let row = elemAt matrix rowIndex; in
        if rowIndex == i then b
        else if rowIndex == j then a
        else row
      )
      (length matrix);

  # Gaussian elimination
  elim = matrix: elimInner matrix 0 0;

  elimInner = matrix: pivotRow: pivotCol:
    let
      colCount = length (head matrix);
      rowCount = length matrix;
    in
    if lessThan pivotRow rowCount && lessThan pivotCol colCount
    then

      let
        pivot = argMax (genRange pivotRow rowCount) (i: get matrix i pivotCol);
      in
      if (get matrix pivot pivotCol) == 0
      then elimInner matrix pivotRow (pivotCol + 1)
      else
        let
          matrixSwapped = swap_rows matrix pivotRow pivot;
          matrixUpdated = performElimination matrixSwapped pivotRow pivotCol;
        in
        elimInner matrixUpdated (pivotRow + 1) (pivotCol + 1)
    else matrix
  ;

  genRange = start: end:
    genList (x: x + start) (end - start);

  performElimination = matrix: pivotRow: pivotCol:
    let
      colCount = length (head matrix);
      rowCount = length matrix;
    in
    foldl'
      (matrix: i:
        let
          f = (get matrix i pivotCol) / (get matrix pivotRow pivotCol);
          matrix' = set matrix i pivotCol 0.0;
          matrix'' = foldl'
            (matrix: j:
              let
                value = (get matrix i j) - (get matrix pivotRow j) * f;
              in
              set matrix i j value
            )
            matrix'
            (genRange (pivotCol + 1) colCount);
        in
        matrix''
      )
      matrix
      (genRange (pivotRow + 1) (rowCount))
  ;

  reverse = list:
    let len = length list;
    in
    genList (n: elemAt list (len - n - 1)) len;

  backSubstitution = matrix:
    let
      rowCount = length matrix;
      colCount = length (head matrix);
      values = foldl'
        (state: i:
          let
            rhs = get matrix i (colCount - 1);
            lhs = foldl'
              (total: j: total + (elemAt state (rowCount - j - 1)) * (get matrix i j))
              0
              (genRange (i + 1) (colCount - 1));
          in
          state ++ [ ((rhs - lhs) / (get matrix i i)) ])
        [ ]
        (genList (n: rowCount - n - 1) rowCount)
      ;
    in
    reverse values
  ;

  # Notations
  # - p_i: initial position of hailstone i (x_i, y_i, z_i)
  # - v_i: speed of hailstone i (x'_i, y'_i, z'_i)
  # - t_i: time of the collision between hailstone i and 0
  #
  # For each hailstone, we have p_0 + t * v_0 = p_i + t * v_i
  # This is equivalent to (p_i - p_0) + t * (v_i - v_0) = 0
  # This mean that both vectors are parallel so we can solve
  # to have their cross product equal to zero:
  # (p_i - p_0) * (v_i - v_0) = 0
  # (p_i * v_i) - (p_0 * v_0) - (p_0 * v_i) + (p_0 * v_0) = 0
  # (p_0 * v_0) = (p_i * v_i) + (p_0 * v_0) + (p_0 * v_i)
  #
  # Finally, we can replace the left-hand side with the values of
  # any other hailstone j and we end up with this equation:
  # p_0 * v_j - p_0 * v_i + p_j * v_0 - p_i * v_0 = p_j * v_j - p_i * v_i
  # p_0 * (v_j - v_i) + (p_j - p_i) * v_0 = p_j * v_j - p_i * v_i
  #
  # We express this as 3 linear equations
  # (order of the variables p_0_{x,y,z} v_0_{x,y,z})
  matrixProblem = i: j:
    [
      [ 0 (j.v.z - i.v.z) (i.v.y - j.v.y) 0 (i.p.z - j.p.z) (j.p.y - i.p.y) (j.p.y * j.v.z - j.p.z * j.v.y - i.p.y * i.v.z + i.p.z * i.v.y) ]
      [ (i.v.z - j.v.z) 0 (j.v.x - i.v.x) (j.p.z - i.p.z) 0 (i.p.x - j.p.x) (j.p.z * j.v.x - j.p.x * j.v.z - i.p.z * i.v.x + i.p.x * i.v.z) ]
      [ (j.v.y - i.v.y) (i.v.x - j.v.x) 0 (i.p.y - j.p.y) (j.p.x - i.p.x) 0 (j.p.x * j.v.y - j.p.y * j.v.x - i.p.x * i.v.y + i.p.y * i.v.x) ]
    ];

  matrix = (matrixProblem (elemAt stones 0) (elemAt stones 2)) ++ (matrixProblem (elemAt stones 4) (elemAt stones 5));
  hailstone0 = backSubstitution (elim matrix);
  partTwo = builtins.toJSON ((elemAt hailstone0 0) + (elemAt hailstone0 1) + (elemAt hailstone0 2));
in
{
  # partOne = countIntersections { min = 7; max = 27; };
  partOne = countIntersections { min = 200000000000000; max = 400000000000000; } / 2;
  inherit partTwo;
}

