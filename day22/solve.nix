{ input }:
let
  inherit (builtins)
    elemAt
    filter
    foldl'
    fromJSON
    genList
    isString
    length
    lessThan
    match
    readFile
    sort
    split
    stringLength
    toJSON
    ;

  isNonEmptyString = s: isString s && stringLength s != 0;

  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  rawBricks = splitString "\n" (readFile input);

  parseCoordinates = str:
    let
      parsed = map fromJSON (match "([0-9]*),([0-9]*),([0-9]*)" str);
    in
    {
      x = elemAt parsed 0;
      y = elemAt parsed 1;
      z = elemAt parsed 2;
    };

  parseBrick = str:
    let
      points = match "(.*)~(.*)" str;
    in
    {
      start = parseCoordinates (elemAt points 0);
      end = parseCoordinates (elemAt points 1);
    };

  bricks = map parseBrick rawBricks;

  min = x: y: if lessThan x y then x else y;

  getBrickHeight = brick: min brick.start.z brick.end.z;

  sortBricks = bricks:
    sort (b1: b2: lessThan (getBrickHeight b1) (getBrickHeight b2)) bricks;

  mapEnumerate = op: list:
    genList (n: op n (elemAt list n)) (length list);

  sortedBricks = mapEnumerate (idx: brick: brick // { idx = idx; }) (sortBricks bricks);

  genRange = start: end: genList (n: n + start) (end - start + 1);

  getHeight = heightmap: x: y: (heightmap."${toJSON x}-${toJSON y}" or 0);

  findHighest = heightmap: brick:
    foldl'
      (
        max: x:
          foldl'
            (max: y:
              let
                curr = getHeight heightmap x y;
              in
              if lessThan curr max
              then max
              else curr
            )
            max
            (genRange brick.start.y brick.end.y)
      )
      0
      (genRange brick.start.x brick.end.x);

  updateHeightmap = heightmap: brick:
    foldl'
      (
        state: x:
          foldl'
            (heightmap: y:
              heightmap // { "${toJSON x}-${toJSON y}" = brick.end.z; }
            )
            state
            (genRange brick.start.y brick.end.y)
      )
      heightmap
      (genRange brick.start.x brick.end.x);


  lowerBrick = brick: zDelta:
    {
      idx = brick.idx;
      start = brick.start // { z = brick.start.z - zDelta; };
      end = brick.end // { z = brick.end.z - zDelta; };
    };

  simulate = bricks: foldl'
    (state: brick:
      let
        supportHeight = (findHighest state.heightmap brick);
        rawDelta = (brick.start.z - supportHeight) - 1;
        delta = if lessThan rawDelta 0 then 0 else rawDelta;
        updatedBrick = lowerBrick brick delta;
        updatedHeightmap = (updateHeightmap state.heightmap updatedBrick);
      in
      { bricks = state.bricks ++ [ updatedBrick ]; heightmap = updatedHeightmap; }
    )
    { bricks = [ ]; heightmap = { }; }
    bricks;

  settled = (simulate sortedBricks).bricks;

  partOne =
    foldl'
      (total: brick:
        let
          others = (filter (b: b != brick) settled);
          res = if ((simulate others).bricks) == others then 1 else 0;
        in
        (total + res)
      )
      0
      settled;

  brickToStr = brick:
    "${toJSON brick.idx}:${toJSON brick.start.x}.${toJSON brick.start.y}.${toJSON brick.start.z}~${toJSON brick.end.x}.${toJSON brick.end.y}.${toJSON brick.end.z}";

  bricksToSet = bricks:
    foldl' (res: brick: res // { ${brickToStr brick} = true; }) { } bricks;

  countDifferences = bricksA: bricksB:
    let
      a = bricksToSet bricksA;
      b = bricksToSet bricksB;
    in
    length (builtins.attrNames a) - length (builtins.attrNames (builtins.intersectAttrs a b));

  partTwo = foldl'
    (total: brick:
      let
        others = filter (b: b != brick) settled;
        res = countDifferences others (simulate others).bricks;
      in
      (total + res)
    )
    0
    settled;
in
{ inherit partOne partTwo; }
