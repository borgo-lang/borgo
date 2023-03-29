# Examples

---

Basics

```rust
fn borgo_main() {
    // Int
    (1 + 1).inspect();

    // String
    "Hello world".inspect();

    // Bool
    (true || false).inspect();

    // Char
    'a'.inspect();

    // List
    [1, 2, 3].inspect();

    // Sequence
    [1, 2, 3].seq().map(|x| x * 3).inspect();

    // Map
    Map::new()
        .insert("foo", 1)
        .insert("bar", 2)
        .insert("baz", 3)
        .inspect();

    // Set
    Set::new()
        .insert(1)
        .insert(1)
        .inspect();

    // Option
    [1, 2, 3].seq().first().inspect();

    // Result
    None.ok_or("that's an error").inspect();

    // Tuples
    (1, "a", true).inspect();

    ()
}
```

Custom types

```rust
enum User {
    Verified(String),
    NotVerified,
}

fn is_verified(u: User) -> Bool {
    match u {
        User::Verified(_) => true,
        User::NotVerified => false,
    }
}

struct Point {
    x: Int,
    y: Float,
}

fn borgo_main() {
    let alice = User::Verified("alice");
    let a_point = Point { x: 1, y: 5.2 };

    Debug::assert_eq(is_verified(alice), true);

    // equivalent to above
    alice.is_verified().assert_eq(true);

    Debug::inspect(alice);
    a_point.inspect();

    ()
}
```

Collections

```rust
// All collections are immutable

fn borgo_main() {
  // List
  let xs = [7,8,9];

  xs.get(0).assert_eq(Some(7));
  xs.get(1).assert_eq(Some(8));
  xs.get(4).assert_eq(None);

  "New list:".inspect();
  xs.push(10).inspect();
  "Original:".inspect();
  xs.inspect();

  // Map
  let m = Map::new()
    .insert("b", 1)
    .insert("c", 2);

  m.get("b").assert_eq(Some(1));
  m.get("missing").assert_eq(None);

  "New map:".inspect();
  m.insert("d", 3).inspect();
  "Original:".inspect();
  m.inspect();

  // Set
  "Set:".inspect();
  Set::new()
    .insert(55)
    .insert(22)
    .insert(55)
    .inspect();

  ()
}
```

Sequences

```rust
// `Seq<T>` represents a lazy sequence.

fn borgo_main() {
  let xs = [1,2,3,4,5];

  let s = xs
  .seq()
  .map(|n| n + 1)
  .inspect();

  // Turn a Seq back into a list
  let xs2 = s.to_list();


  ()
}
```

AoC 2022 #1

```rust
// Advent of Code 2022 - Day 01

const input: String = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";

fn borgo_main() {
    let solution = input
    .split("\n\n")
    .map(|group| group
          .split("\n")
          .map(|x| String::parse_int(x).unwrap())
          .sum()
          )
    .max_by(Int::cmp)
    .unwrap();

    solution.inspect();
    solution.assert_eq(24000);
}
```

AoC 2022 #2

```rust
// Advent of Code 2022 - Day 02

const input: String = "A Y
B X
C Z";

enum Hand {
    Rock,
    Paper,
    Scissors,
}

enum GameResult {
    Win,
    Lose,
    Draw,
}

impl Hand {
    fn parse(s: String) -> Option<Hand> {
        match s {
            "A" => Some(Rock),
            "X" => Some(Rock),
            "B" => Some(Paper),
            "Y" => Some(Paper),
            "C" => Some(Scissors),
            "Z" => Some(Scissors),
            _ => None,
        }
    }

    fn play_against(self, other: Hand) -> GameResult {
        match (self, other) {
            (Rock, Scissors) => Win,
            (Rock, Paper) => Lose,
            (Scissors, Paper) => Win,
            (Scissors, Rock) => Lose,
            (Paper, Rock) => Win,
            (Paper, Scissors) => Lose,
            _ => Draw,
        }
    }

    fn value(self) -> Int {
        match self {
            Rock => 1,
            Paper => 2,
            Scissors => 3,
        }
    }

    fn score_against(self, other: Hand) -> Int {
        let score = match self.play_against(other) {
            Win => 6,
            Draw => 3,
            Lose => 0,
        };

        self.value() + score
    }
}

fn borgo_main() {
    let total = input
        .split("\n")
        .map(|turn| {
            let hands = turn.split(" ");
            let a = Hand::parse(hands.get(0).unwrap()).unwrap();
            let b = Hand::parse(hands.get(1).unwrap()).unwrap();

            a.score_against(b)
        })
        .sum();

    total.inspect().assert_eq(15);
}
```

AoC 2022 #3

```rust
// Advent of Code 2022 - Day 03

const input: String = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw";

fn borgo_main() {
    let a = 'a'.to_int() - 1;
    let z = 'z'.to_int();
    let big_a = 'A'.to_int() - 1;

    let result = input
        .split("\n")
        .map(|line| {
            let items = line.chars();
            let (fst_half, snd_half) = items.split_at(items.len() / 2);
            let items_in_rucksack = fst_half.to_set();

            let common = snd_half
                .find_map(|c| {
                    if items_in_rucksack.contains(c) {
                        return Some(c.to_int());
                    }

                    None
                })
                .unwrap();

            if common >= a && common <= z {
                common - a
            } else {
                common - big_a + 26
            }
        })
        .sum();

    result.inspect().assert_eq(157);
    ()
}
```

AoC 2022 #4

```rust
// Advent of Code 2022 - Day 04

const input: String = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8";

struct Part {
    start: Int,
    end: Int,
}

impl Part {
    fn parse(s: String) -> Part {
        let parts = s.split("-");

        Part {
            start: parts.get(0).and_then(String::parse_int).unwrap(),
            end: parts.get(1).and_then(String::parse_int).unwrap(),
        }
    }

    fn contains(self, other: Part) -> Bool {
        self.start <= other.start && self.end >= other.end
    }
}

fn borgo_main() {
    let result = input
        .split("\n")
        .map(|line| {
            let parts = line.split(",");
            let first = Part::parse(parts.get(0).unwrap());
            let second = Part::parse(parts.get(1).unwrap());
            (first, second)
        })
        .reduce(0, |acc, (first, second)| {
            if first.contains(second) || second.contains(first) {
                acc + 1
            } else {
                acc
            }
        });

    result.inspect().assert_eq(2);
    ()
}
```

AoC 2022 #5

```rust
// Advent of Code 2022 - Day 05

const input: String = "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2";

struct Cmd {
    count: Int,
    from: Int,
    to: Int,
}

fn parse_stacks(s: String) -> Map<Int, List<Char>> {
    let rows = s.split("\n");
    let rows = rows.take(rows.len() - 1);

    rows.map(|row| {
        row.chars()
            .chunks(4)
            .filter_map(|chars| chars.filter(|c| c != '[' && c != ']').first())
    })
    .reverse()
    .reduce(Map::new(), |m, row| {
        row.enumerate().reduce(m, |acc, (index, c)| {
            acc.update(index + 1, [], |stack| {
                if c != ' ' {
                    stack.push(c)
                } else {
                    stack
                }
            })
        })
    })
}

fn parse_commands(s: String) -> Seq<Cmd> {
    s.split("\n").map(|row| {
        let parts = row.split(" ");

        fn parse(index: Int) -> Int {
            parts.get(index).and_then(String::parse_int).unwrap()
        }

        let count = parse(1);
        let from = parse(3);
        let to = parse(5);
        Cmd { count, from, to }
    })
}

fn run_commands(stacks: Map<Int, List<Char>>, cmds: Seq<Cmd>) -> Map<Int, List<Char>> {
    cmds.reduce(stacks, |acc, cmd| {
        Loop::start((acc, cmd.count), |(new_stacks, remaining)| {
            if remaining == 0 {
                return Loop::Done(new_stacks);
            }

            let from = new_stacks.get(cmd.from).unwrap();
            let to = new_stacks.get(cmd.to).unwrap();
            let item = from.seq().last().unwrap();

            let new_stacks = new_stacks
                .insert(cmd.from, from.pop())
                .insert(cmd.to, to.push(item));

            Loop::Recur((new_stacks, remaining - 1))
        })
    })
}

fn borgo_main() {
    let parts = input.split("\n\n").to_list();

    let stacks = parse_stacks(parts.get(0).unwrap());
    let commands = parse_commands(parts.get(1).unwrap());
    let result = run_commands(stacks, commands)
        .seq_values()
        .map(|stack| stack.seq().last().unwrap())
        .to_list();

    result.inspect().assert_eq(['C', 'M', 'Z']);
}
```

AoC 2022 #6

```rust
// Advent of Code 2022 - Day 06

fn solve(input: String, n_different: Int) -> Int {
    let target = input
        .chars()
        .map(|c| c.to_unquoted_string())
        .windows(n_different)
        .filter(find_different)
        .first()
        .unwrap();

    input.index_of(target.join("")).unwrap() + n_different
}

fn find_different(chunk: Seq<String>) -> Bool {
    let unique = chunk.to_set();
    unique.len() == chunk.len()
}

fn borgo_main() {
    solve("bvwbjplbgvbhsrlpgdmjqwftvncz", 4).inspect().assert_eq(5);

    solve("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4).inspect().assert_eq(10);
}
```
