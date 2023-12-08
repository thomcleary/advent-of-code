import { getPuzzleInput, logChallenge, toLines } from "../utils.js";

type HandType =
  | "High Card"
  | "One Pair"
  | "Two Pair"
  | "Three of a Kind"
  | "Full House"
  | "Four of a Kind"
  | "Five of a Kind";
type CamelCard = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | "T" | "J" | "Q" | "K" | "A";
type Hand = [CamelCard, CamelCard, CamelCard, CamelCard, CamelCard];

const handPriority = {
  "High Card": 1,
  "One Pair": 2,
  "Two Pair": 3,
  "Three of a Kind": 4,
  "Full House": 5,
  "Four of a Kind": 6,
  "Five of a Kind": 7,
} satisfies Record<HandType, Number>;

const camelCardPriority = {
  1: 1,
  2: 2,
  3: 3,
  4: 4,
  5: 5,
  6: 6,
  7: 7,
  8: 8,
  9: 9,
  T: 10,
  J: 11,
  Q: 12,
  K: 13,
  A: 14,
} satisfies Record<CamelCard, number>;

const getHandType = (hand: Hand): HandType => {
  const uniqueCards = [...new Set(hand).values()];

  switch (uniqueCards.length) {
    case 1:
      return "Five of a Kind";
    case 2:
      const firstCardCount = hand.filter((card) => card === hand[0]).length;
      return firstCardCount === 1 || firstCardCount === 4 ? "Four of a Kind" : "Full House";
    case 3:
      const hasThree = (camelCard: CamelCard) => hand.filter((card) => card === camelCard).length === 3;
      const threeOfAKind = uniqueCards.map(hasThree).reduce((prev, curr) => prev || curr, false);
      return threeOfAKind ? "Three of a Kind" : "Two Pair";
    case 4:
      return "One Pair";
    default:
      return "High Card";
  }
};

const hands = toLines(getPuzzleInput(import.meta.url))
  .map((line) => line.split(" "))
  .map(([hand, bid]) => ({ hand: hand!.split("") as Hand, bid: Number.parseInt(bid!) }))
  .map((hand) => ({ ...hand, type: getHandType(hand.hand) }));

const part1 = () =>
  hands
    .sort((a, b) => {
      const aHandPriority = handPriority[a.type];
      const bHandPriority = handPriority[b.type];

      if (aHandPriority > bHandPriority) {
        return 1;
      } else if (bHandPriority > aHandPriority) {
        return -1;
      } else {
        let currentCard = 0;
        let aCardPriority = camelCardPriority[a.hand[currentCard]!];
        let bCardPriority = camelCardPriority[b.hand[currentCard]!];

        while (aCardPriority === bCardPriority) {
          currentCard += 1;
          aCardPriority = camelCardPriority[a.hand[currentCard]!];
          bCardPriority = camelCardPriority[b.hand[currentCard]!];
        }

        return aCardPriority > bCardPriority ? 1 : -1;
      }
    })
    .reduce((prev, curr, index) => prev + curr.bid * (index + 1), 0);

const part2 = () => {};

const camelCards = () =>
  logChallenge({
    name: "Day 7: Camel Cards",
    part1: { run: part1, expected: 250474325 },
    part2: { run: part2, expected: undefined },
  });

camelCards();
