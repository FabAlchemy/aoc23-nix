{ input }:
let
  inherit (builtins)
    attrNames
    elemAt
    filter
    foldl'
    fromJSON
    hasAttr
    head
    isString
    length
    lessThan
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

  workflowsAndParts = splitString "\n\n" (readFile input);
  rawWorkflows = splitString "\n" (head workflowsAndParts);
  rawParts = splitString "\n" (head (tail workflowsAndParts));
  # "{x=787,m=2655,a=1222,s=2876}"
  parsePart = part:
    let
      ratings = splitString "," (head (match "\\{(.*)}" part));
    in
    foldl'
      (part: rating:
        let
          parsedRating = splitString "=" rating;
        in
        part // { ${head parsedRating} = fromJSON "${elemAt parsedRating 1}"; })
      { }
      ratings
  ;

  parts = (map parsePart rawParts);

  # "px{a<2006:qkq,m>2090:A,rfg}"
  parseWorkflow = workflow:
    let
      tagAndBody = match "(.*)\\{(.*)}" workflow;
      tag = head tagAndBody;
      body = elemAt tagAndBody 1;
      rules = map parseRule (splitString "," body);
    in
    { ${tag} = rules; };

  greaterThan = x: y: !lessThan x y && x != y;

  # "a<2006:qkq"
  parseRule = rule:
    let
      content = match "(.*)([<>])(.*):(.*)" rule;
    in
    if content == null # the last rule is simply a destination
    then { destination = rule; }
    else
      {
        rating = elemAt content 0;
        comparator = elemAt content 1;
        value = fromJSON (elemAt content 2);
        destination = elemAt content 3;
      };

  workflows = foldl'
    (res: rawWorkflow:
      res // (parseWorkflow rawWorkflow)
    )
    { }
    rawWorkflows;

  # Returns `true` if the `workflows` lead the `part` to be accepted.
  filterPart = part: tag:
    if tag == "A" then true
    else if tag == "R" then false
    else
      let
        rules = workflows.${tag};
        destination = applyRulesToPart rules part;
      in
      filterPart part destination;

  applyRulesToPart = rules: part:
    foldl'
      (res: rule:
        if res != "" then res
        else
          if hasAttr "rating" rule
          then
            let comparator = if rule.comparator == "<" then lessThan else greaterThan;
            in
            if comparator part.${rule.rating} rule.value then rule.destination else ""
          else rule.destination
      )
      ""
      rules;

  acceptedParts = filter (part: filterPart part "in") parts;

  partOne =
    foldl'
      (total: part: total + part.a + part.m + part.s + part.x)
      0
      acceptedParts;

  # For part two, we are using a fix point technique.
  # The state is an attribute set from "rule tag" to "abstract part", an object
  # that describes a set of parts (each attribute is a range of values for the corresponding rating).

  wholeRange = [ 1 4000 ];
  wholeAbstractPart = { a = wholeRange; m = wholeRange; s = wholeRange; x = wholeRange; };

  isEmpty = l: length l == 0;

  # Remove the attributes of `set` that map to an empty list.
  filterEmptyAttrs = set:
    let nullAttrs = filter (attr: isEmpty set.${attr}) (attrNames set);
    in
    builtins.removeAttrs set nullAttrs;

  # Split an abstract part into two abstracts parts:
  # one that respects the rule, and one that do not.
  applyRuleToAbstractPart = rule: part:
    if !hasAttr "rating" rule
    # this is a terminal rule, forward the part to the destination
    then { ${rule.destination} = [ part ]; }
    else
      let
        range = part.${rule.rating};
        a = head range;
        b = head (tail range);
        x = rule.value;
        comparator = rule.comparator;

        # splitted range according the the rule
        splitted =
          if comparator == ">"
          then
            if lessThan x a then [ [ ] [ a b ] ]
            else if lessThan b x then [ [ a b ] [ ] ]
            else [ [ a x ] [ (x + 1) b ] ]
          else # if comparator == "<"
            if lessThan x a then [ [ a b ] [ ] ]
            else if lessThan b x then [ [ ] [ a b ] ]
            else [ [ x b ] [ a (x - 1) ] ];

        makeNewPart = range:
          if isEmpty (range) then null else
          part // { ${rule.rating} = range; };

        invalidPart = makeNewPart (elemAt splitted 0);
        validPart = makeNewPart (elemAt splitted 1);
      in
      filterEmptyAttrs {
        ${rule.destination} = [ validPart ];
        rest = [ invalidPart ];
      }
  ;

  flatten = list:
    foldl' (res: item: res ++ item) [ ] list;

  # Merge two attribute lists (attributes sets that have lists as values).
  # { tag = [x]; } -> { tag = [y]; } -> { tag = [x y]; }
  mergeAttrLists = a: b:
    let
      duplicateAttrs = attrNames (builtins.intersectAttrs a b);
      merged = foldl'
        (res: tag: res // { ${tag} = flatten (builtins.catAttrs tag [ a b ]); })
        { }
        duplicateAttrs;
    in
    (a // b // merged);

  # Apply a workflow (described by its `tag`) to a list of abstract `parts`.
  applyWorkflowToGenericParts = tag: parts:
    let
      rules = workflows.${tag};
    in
    foldl'
      (state: rule:
        (foldl'
          (state: newState: mergeAttrLists (builtins.removeAttrs state [ "rest" ]) newState)
          (state)
          (map (applyRuleToAbstractPart rule) (state.rest or [ ])))
      )
      { rest = parts; }
      rules
  ;

  # Apply the workflows to the parts until we're done
  fixPointApplyWorkflows = state:
    let
      tags = builtins.sort lessThan (attrNames state);
      activeTags = filter (t: t != "A" && t != "R") tags;
    in
    if tags == [ "A" "R" ] then state
    else
      fixPointApplyWorkflows
        (foldl'
          (currentState: tag: mergeAttrLists currentState (applyWorkflowToGenericParts tag state.${tag}))
          (builtins.removeAttrs state activeTags) # keep A and R
          activeTags
        );

  # Count the numbers of parts described by an abstract part.
  countCombinationsForGenericPart = part:
    foldl'
      (res: tag: res * ((elemAt part.${tag} 1) - (elemAt part.${tag} 0) + 1))
      1
      [ "x" "m" "a" "s" ];

  # Sum up the elements of a list.
  sum = list: foldl' (res: item: res + item) 0 list;

  partTwoState = (fixPointApplyWorkflows { "in" = [ wholeAbstractPart ]; });
  partTwo = sum (map countCombinationsForGenericPart partTwoState.A);
in
{ inherit partOne partTwo; }
