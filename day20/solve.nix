{ input }:
let
  inherit (builtins)
    all
    attrNames
    elemAt
    filter
    foldl'
    hasAttr
    head
    isString
    length
    match
    readFile
    split
    stringLength
    tail
    throw
    ;

  # Returns true if a value is a non-empty string
  isNonEmptyString = s: isString s && stringLength s != 0;

  # Split a string into a list of strings where the pattern is found
  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  lines = splitString "\n" (readFile input);

  parseLine = line:
    let
      sourceAndDestinations = match "(.*) -> (.*)" line;
      source = head sourceAndDestinations;
      destinations = splitString ", " (elemAt sourceAndDestinations 1);

      maybeType = match "([&%])(.*)" source;
      type = if maybeType == null then source else head maybeType;
      name = if maybeType == null then source else elemAt maybeType 1;
    in
    { ${name} = { inherit type destinations; }; };

  parsedConfiguration = foldl' (res: item: res // item) { } (map parseLine lines);
  configuration = parsedConfiguration // {
    output = { type = "output"; destinations = [ ]; };
  };

  # Flip-flop module (%)
  flip = state@{ on, tag, ... }: { kind, ... }:
    if kind == "high"
    then {
      inherit state;
      output = null;
    }
    else if kind == "low"
    then
      let
        newKind = if state.on then "low" else "high";
      in
      {
        state = state // { on = !state.on; };
        output = { kind = newKind; inherit tag; };
      }
    else throw "Invalid pulse kind ${kind}";

  isAllHigh = inputs:
    let tags = attrNames inputs;
    in all (tag: inputs.${tag} == "high") tags;

  # Conjunction module (%)
  conjuction = state@{ tag, inputs, triggeredHigh }: pulse@{ kind, ... }:
    let
      updatedInputs = (inputs // { ${pulse.tag} = pulse.kind; });
      newKind = if isAllHigh updatedInputs then "low" else "high";
    in
    {
      state = state // { inputs = updatedInputs; triggeredHigh = if newKind == "high" then true else state.triggeredHigh; };
      output = { kind = newKind; inherit tag; };
    };

  # Broadcaster node (broadcaster)
  broadcast = state: pulse: { inherit state; output = pulse // { tag = "broadcaster"; }; };

  # Output node
  output = state: pulse: { inherit state; output = null; };

  applyPulse = states: pulse:
    if !hasAttr pulse.target configuration
    then { inherit states; pulses = [ ]; }
    else
      let
        target = configuration.${pulse.target};
        fn =
          if target.type == "%" then flip else
          if target.type == "&" then conjuction else
          if target.type == "broadcaster" then broadcast else
          if target.type == "output" then output
          else throw "Unimplemented type ${target.type}";

        newStateAndPulse = fn states.${pulse.target} pulse;
      in
      {
        states = states // { ${pulse.target} = newStateAndPulse.state; };
        pulses = if newStateAndPulse.output == null then [ ] else
        map
          (destination: newStateAndPulse.output // { target = "${destination}"; })
          target.destinations;
      }
  ;

  listContainsElem = list: elem:
    if length list == 0 then false
    else elem == head list || listContainsElem (tail list) elem;

  findInputsForModule = name:
    let names = attrNames configuration;
    in filter (n: listContainsElem configuration.${n}.destinations name) names;

  makeConjuctionInitialInputs = name:
    foldl' (res: item: res // { ${item} = "low"; }) { } (findInputsForModule name);

  initialStates =
    let
      names = attrNames configuration;
    in
    foldl'
      (res: name:
        let
          state =
            if configuration.${name}.type == "%" then { on = false; } else
            if configuration.${name}.type == "&" then { inputs = makeConjuctionInitialInputs name; triggeredHigh = false; }
            else { };
        in
        res // { ${name} = state // { tag = "${name}"; }; })
      { }
      names
  ;

  simulateStep = currentStates: pulses:
    foldl'
      (res: pulse:
        let updatedStatesAndNewPulses = applyPulse res.states pulse;
        in
        {
          states = updatedStatesAndNewPulses.states;
          pulses = res.pulses ++ updatedStatesAndNewPulses.pulses;
        }
      )
      { states = currentStates; pulses = [ ]; }
      pulses
  ;

  countPulses = pulses:
    foldl'
      (res: pulse: res // { ${pulse.kind} = res.${pulse.kind} + 1; })
      { high = 0; low = 0; }
      pulses;

  combineLowAndHighCounts = a: b:
    { high = a.high + b.high; low = a.low + b.low; };

  simulate = states: pulses:
    let
      step = simulateStep states pulses;
      next = simulate step.states step.pulses;
    in
    if length step.pulses == 0
    then
      { states = step.states; res = countPulses pulses; }
    else
    #  if builtins.any (pulse: listContainsElem [ "ln" "vn" "zx" "dr" ] pulse.tag && pulse.kind == "high") pulses then throw "${builtins.toJSON pulses}" else
      {
        states = next.states;
        res = combineLowAndHighCounts (countPulses pulses) next.res;
      };

  simulateManyTimes = n: states:
    if n == 0 then { high = 0; low = 0; }
    else
      let
        step = (simulate states initialPulses);
        next = (simulateManyTimes (n - 1) step.states);
      in
      combineLowAndHighCounts step.res next
  ;

  initialPulses = [{ target = "broadcaster"; kind = "low"; tag = "button"; }];

  simulationPartOne = simulateManyTimes 1000 initialStates;
  partOne = simulationPartOne.high * simulationPartOne.low;

  # find the number of buttons presses to get a "high" pulse from `target`.
  findCycleLength = target: n: N: states:
    if n == N then { high = 0; low = 0; }
    else
      let
        step = (simulate states initialPulses);
      in
      if states.${target}.triggeredHigh then n
      else findCycleLength target (n + 1) N step.states
  ;

  # Compute x % y
  mod = x: y: x - (x / y) * y;

  # Compute the Greatest Common Denominator of a and b
  gcd = a: b: if b == 0 then a else gcd b (mod a b);

  # Compute the Least Common Multiple of a and b
  lcm = a: b: a / (gcd a b) * b;

  # Compute the LCM of a list
  lcmList = list: foldl' (res: x: lcm res x) (head list) (tail list);

  partTwo = lcmList (map (tag: findCycleLength tag 0 5000 initialStates) [ "ln" "vn" "zx" "dr" ]);
in
{ inherit partOne partTwo; }

