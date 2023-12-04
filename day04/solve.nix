{ input }:
let
  inherit (builtins)
    add
    elem
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
    tail
    ;

  # Returns true if a value is a non-empty string
  isNonEmptyString = s: isString s && stringLength s != 0;

  # Split a string into a list of strings where the pattern is found
  splitString = pattern: string: filter isNonEmptyString (split pattern string);

  # Parse a list of numbers from a list of strings
  parseNumbers = s: map fromJSON (splitString " " s);

  # Parse a card from its description string.
  parseCard = line:
    let
      cardInfo = match "Card.*([[:digit:]]+): (.*)" line;
      id = fromJSON (head cardInfo);
      allNumbers = splitString " \\| " (head (tail cardInfo));

      winningNumbers = parseNumbers (head allNumbers);
      selectedNumbers = parseNumbers (head (tail allNumbers));
    in
    { inherit id winningNumbers selectedNumbers; };

  # Count the number of items in a list
  count = list: foldl' (x: y: x + 1) 0 list;

  # Compute x^y
  pow = x: y: if y == 0 then 1 else x * (pow x (y - 1));

  # Sum up a list of integers
  sum = l: foldl' add 0 l;

  # Compute how many numbers on the card match the winning numbers.
  getCardValidNumbers = card:
    let
      isWinningNumber = x: elem x card.winningNumbers;
      validNumbers = filter isWinningNumber card.selectedNumbers;
    in
    count validNumbers;

  # Compute the score of the card.
  getCardScore = card:
    let
      validNumbersCount = getCardValidNumbers card;
    in
    if lessThan validNumbersCount 2 then validNumbersCount else pow 2 (validNumbersCount - 1);

  lines = splitString "\n" (readFile input);
  cards = map parseCard lines;
  scores = map getCardScore cards;
  partOne = sum scores;

  # Return a sublist of `list` from `start`, trying to grab `count` elements.
  # This works by generating a new list of the correct length that will grab its elements from `list`.
  sublist = start: count: list:
    let len = length list; in
    genList
      (n: elemAt list (n + start))
      (if lessThan len start || len == start
      then 0
      else
        if lessThan len (start + count)
        then len - start
        else count
      );

  # Increment the `count` first elements of `list` by `value`.
  incrementCardCounts = count: value: list: (map (x: x + value) (sublist 0 count list)) ++ (sublist count (length list) list);

  # Play a card: update the following card counts according to the number of valid numbers
  playCard = ({ card_counts, total }: card:
    let matching = getCardValidNumbers card; in
    {
      card_counts = incrementCardCounts matching (head card_counts) (tail card_counts);
      total = total + (head card_counts);
    });

  # Compute the total number of cards according to the new behavior.
  partTwo =
    let
      gameState = foldl'
        playCard
        { card_counts = genList (n: 1) (length cards); total = 0; }
        cards;
    in
    gameState.total;
in
{
  inherit partOne partTwo;
}
