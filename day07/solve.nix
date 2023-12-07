{ input }:
let
  inherit (builtins)
    attrValues
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
    removeAttrs
    sort
    split
    stringLength
    tail
    ;

  # Returns true if a value is a non-empty string
  isNonEmptyString = s: isString s && stringLength s != 0;

  # Split a string into a list of strings where the pattern is found
  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  lines = splitString "\n" (readFile input);

  # Parse the bids from the input
  # { hand = string; bid = number; }
  bids = map
    (s:
      let
        hand = splitString " " s;
      in
      { hand = head hand; bid = fromJSON (elemAt hand 1); })
    lines;

  # Convert a hand to an attribute set describing it
  # string -> { card = count; }
  handAsSet = hand: foldl'
    (
      cards: card:
        let
          count = if hasAttr card cards then cards.${card} + 1 else 1;
        in
        cards // { "${card}" = count; }
    )
    { }
    (splitString "" hand);

  # Return the type of the hand described by its set
  # The returned values are number whose ordering is equivalent to the type strength.
  setToType = set:
    let
      counts = sort (x: y: !(lessThan x y)) (attrValues set);
      first = head counts;
      second = if lessThan (length counts) 2 then 0 else (elemAt counts 1);
    in
    if first == 5 then 6 # five of a kind
    else if first == 4 then 5 # four of a kind
    else if first == 3 && second == 2 then 4 # full house
    else if first == 3 then 3 # three of a kind
    else if first == 2 && second == 2 then 2 # two pairs
    else if first == 2 then 1 # one pair
    else 0; # high card

  # All possible cards sorted by strength (from stronger to weaker).
  CARD_ENUM = [ "A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2" ];

  # Map from card to rank
  # [string] -> { card = number; }
  computeCardRanks = cardsEnum: foldl'
    (ranks: n:
      let card = elemAt cardsEnum n; in ranks // { ${card} = n; })
    { }
    (genList (n: n) (length cardsEnum));

  CARD_RANKS = computeCardRanks CARD_ENUM;

  # Compare two cards using `ranks`, returns true if `cardA` is weaker than `cardB`
  compareCards = ranks: cardA: cardB: lessThan ranks.${cardB} ranks.${cardA};

  # Recursively compare the strength of two hands.
  # [card] -> [card] -> [bool]
  compareHandStrengthInner = ranks: cardsA: cardsB:
    let
      # will abort if there's no card!
      a = head cardsA;
      b = head cardsB;
    in
    if a == b
    then compareHandStrengthInner ranks (tail cardsA) (tail cardsB)
    else compareCards ranks a b;

  # Compute the strength of two hands.
  compareHandStrength = ranks: handA: handB:
    let
      cardsA = (splitString "" handA);
      cardsB = (splitString "" handB);
    in
    compareHandStrengthInner ranks cardsA cardsB;

  # Compare two hands (first by their type, then by card).
  compareHands = setToType: ranks: handA: handB:
    let
      typeA = setToType (handAsSet handA);
      typeB = setToType (handAsSet handB);
    in
    if typeA == typeB
    then compareHandStrength ranks handA handB
    else lessThan typeA typeB;

  sortedBids = sort
    (bid1: bid2: compareHands setToType CARD_RANKS bid1.hand bid2.hand)
    bids;

  partOne = foldl'
    (total: n: total + (n + 1) * (elemAt sortedBids n).bid)
    0
    (genList (n: n) (length sortedBids));

  # Return the type of the hand described by its set, considering jokers
  setToTypeJoker = set:
    let
      jokerCount = set."J" or 0;
      counts = sort (x: y: !(lessThan x y)) (attrValues (removeAttrs set [ "J" ]));

      first = if counts == [ ] then 0 else head counts;
      second = if lessThan (length counts) 2 then 0 else (elemAt counts 1);
    in
    if first + jokerCount == 5 then 6 # five of a kind
    else if first + jokerCount == 4 then 5 # four of a kind
    else if first + second + jokerCount == 5 then 4 # full house
    else if first + jokerCount == 3 then 3 # three of a kind
    else if first + second + jokerCount == 4 then 2 # two pairs
    else if first + jokerCount == 2 then 1 # one pair
    else 0; # high card

  # All possible cards sorted by strength (from stronger to weaker).
  CARD_ENUM_JOKER = [ "A" "K" "Q" "T" "9" "8" "7" "6" "5" "4" "3" "2" "J" ];

  # Map from card to rank
  CARD_RANKS_JOKER = computeCardRanks CARD_ENUM_JOKER;

  sortedBidsJoker = sort
    (bid1: bid2: compareHands setToTypeJoker CARD_RANKS_JOKER bid1.hand bid2.hand)
    bids;

  partTwo = foldl'
    (total: n: total + (n + 1) * (elemAt sortedBidsJoker n).bid)
    0
    (genList (n: n) (length sortedBidsJoker));

in
{ inherit partOne partTwo; }
