#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define DIGITS 10
#define DISPLAY_DIGITS 4
#define SEGMENTS 7
#define VALUES 10 // should be max(DIGITS, SEGMENTS)

#define SEGMENTS_MASK ((1 << (SEGMENTS + 1)) - 1)

typedef uint16_t BitSet;
typedef int8_t Value;

// A bit set where the rightmost bit corresponds to
// signal a being activated, the next to b being activated, etc.
typedef BitSet Pattern;

// A bit set where the rightmost bit represents whether
// a pattern could be a 0, the next whether it could be a 1, etc
typedef BitSet CandidateSet;

// A signal index (0 -> 'a', 1 -> 'b', ...)
typedef Value SignalIndex;

// An actual digit.
typedef Value Digit;

// An input line corresponding to a 4-digit display wired in
// a certain random way.
struct Line {
  Pattern digitPatterns[DIGITS];
  Pattern outputPatterns[DISPLAY_DIGITS];
};

struct Mappings {
  // Maps each (randomly) wired signal to possible
  // segment indices.
  Pattern wiringToSegments[SEGMENTS];
};

struct OptionalMappings {
  struct Mappings mappings;
  bool present;
};

Pattern correctWirings[] = {
  0b1110111, // 0
  0b0100100, // 1
  0b1011101, // 2
  0b1101101, // 3
  0b0101110, // 4
  0b1101011, // 5
  0b1111011, // 6
  0b0100101, // 7
  0b1111111, // 8
  0b1101111  // 9
};

void printSignalIndex(SignalIndex i) {
  printf("%c", 'a' + i);
}

void printPattern(Pattern p) {
  for (SignalIndex i = 0; i < SEGMENTS; i++) {
    if ((p >> i) & 1) {
      printSignalIndex(i);
    }
  }
}

void printMappings(struct Mappings mappings) {
  for (SignalIndex i = 0; i < SEGMENTS; i++) {
    printf("  ");
    printSignalIndex(i);
    printf(" -> one of ");
    printPattern(mappings.wiringToSegments[i]);
    printf("\n");
  }
}

SignalIndex signalIndex(char c) {
  return (SignalIndex) (c - 'a');
}

Value single(BitSet x) {
  Value value;
  int count = 0;
  for (int i = 0; i < VALUES; i++) {
    int bit = (x >> i) & 1;
    count += bit;
    if (bit) {
      value = i;
    }
  }
  return count == 1 ? value : -1;
}

int size(BitSet x) {
  int count = 0;
  for (int i = 0; i < VALUES; i++) {
    count += (x >> i) & 1;
  }
  return count;
}

Pattern translate(Pattern pattern, struct Mappings mappings) {
  Pattern result = 0;
  for (SignalIndex i = 0; i < SEGMENTS; i++) {
    if ((pattern >> i) & 1) {
      Pattern segments = mappings.wiringToSegments[i];
      SignalIndex segment = single(segments);
      if (segment >= 0) {
        result |= 1 << segment;
      }
    }
  }
  return result;
}

Digit translateToDigit(Pattern pattern, struct Mappings mappings) {
  Pattern translated = translate(pattern, mappings);
  if (translated == 0) {
    return -1;
  }
  for (Digit i = 0; i < DIGITS; i++) {
    if (correctWirings[i] == translated) {
      return i;
    }
  }
  return -1;
}

bool isCandidate(Digit digit, Pattern pattern, struct Mappings mappings) {
  if (size(correctWirings[digit]) != size(pattern)) {
    return false;
  }
  Pattern translated = translate(pattern, mappings);
  // Check if translation is subset of correct wiring
  return (translated & correctWirings[digit]) == translated;
}

CandidateSet computeCandidateSet(Pattern pattern, struct Mappings mappings) {
  CandidateSet candidates = 0;
  for (Digit i = 0; i < DIGITS; i++) {
    if (isCandidate(i, pattern, mappings)) {
      candidates |= 1 << i;
    }
  }
  return candidates;
}

void updateMappings(Pattern pattern, Digit digit, struct Mappings *mappings) {
  for (SignalIndex i = 0; i < SEGMENTS; i++) {
    if ((pattern >> i) & 1) {
      mappings->wiringToSegments[i] &= correctWirings[digit];
    }
  }
}

bool areValidMappings(struct Mappings mappings, struct Line line) {
  bool found[DIGITS] = { false };
  for (Digit i = 0; i < DIGITS; i++) {
    Digit j = translateToDigit(line.digitPatterns[i], mappings);
    if (j == -1 || found[j]) {
      return false;
    }
    found[j] = true;
  }
  return true;
}

struct OptionalMappings solveMappings(struct Mappings mappings, struct Line line) {
  // Find most unambiguous unsolved mapping
  SignalIndex next = -1;
  Pattern nextPattern = 0;
  int nextChoices = 10000; // arbitrary large number
  for (SignalIndex i = 0; i < SEGMENTS; i++) {
    Pattern pattern = mappings.wiringToSegments[i];
    int choices = size(pattern);
    if (choices == 0) {
      // If there are no choices, we must have made some incorrect choice earlier
      return (struct OptionalMappings) { .present = false };
    }
    if (choices > 1 && choices < nextChoices) {
      next = i;
      nextPattern = pattern;
      nextChoices = choices;
    }
  }

  // If there is no such mapping, we might be done.
  if (next == -1) {
    return (struct OptionalMappings) { .mappings = mappings, .present = areValidMappings(mappings, line) };
  }

  // Otherwise try all choices
  for (SignalIndex i = 0; i < SEGMENTS; i++) {
    if ((nextPattern >> i) & 1) {
      // Choose next -> i
      struct Mappings newMappings = mappings;
      newMappings.wiringToSegments[next] = 1 << i;
      for (SignalIndex j = 0; j < SEGMENTS; j++) {
        if (j != next) {
          newMappings.wiringToSegments[j] &= (1 << i) ^ SEGMENTS_MASK;
        }
      }

      // Recurse
      struct OptionalMappings afterChoice = solveMappings(newMappings, line);

      if (afterChoice.present) {
        // Solved it!
        return afterChoice;
      }
    }
  }
  
  return (struct OptionalMappings) { .present = false };
}

struct Mappings computeMappings(struct Line line) {
  struct Mappings mappings;
  CandidateSet candidateSets[DIGITS] = { 0 };
  bool completedCandidateSets[DIGITS] = { false };
  Pattern completedDigitPatterns[DIGITS] = { 0 };

  // Initialize mappings
  for (SignalIndex i = 0; i < SEGMENTS; i++) {
    mappings.wiringToSegments[i] = SEGMENTS_MASK;
  }

  // Find the unambiguous mappings first (1, 4, 7, 8)
  bool foundNewMappings;
  do {
    foundNewMappings = false;
    for (Digit i = 0; i < DIGITS; i++) {
      if (!completedCandidateSets[i]) {
        Pattern pattern = line.digitPatterns[i];
        CandidateSet set = computeCandidateSet(pattern, mappings);
        candidateSets[i] = set;
        Digit candidate = single(set);
        if (candidate >= 0) {
          // We found a new pattern -> digit mapping
          foundNewMappings = true;
          completedDigitPatterns[candidate] = pattern;
          completedCandidateSets[i] = true;
          // Update mappings for the wirings
          for (Digit j = 0; j < DIGITS; j++) {
            if (completedCandidateSets[j]) {
              updateMappings(completedDigitPatterns[j], j, &mappings);
            }
          }
        }
      }
    }
  } while (foundNewMappings);

  // Solve the ambiguous mappings
  struct OptionalMappings solvedMappings = solveMappings(mappings, line);
  if (!solvedMappings.present) {
    printf("Could not solve mappings!\n");
    exit(1);
  }

  return solvedMappings.mappings;
}

int computeDisplay(struct Line line) {
  struct Mappings mappings = computeMappings(line);
  int display = 0;
  int n = 1;

  for (int i = DISPLAY_DIGITS - 1; i >= 0; i--) {
    display += n * translateToDigit(line.outputPatterns[i], mappings);
    n *= 10;
  }

  return display;
}

bool isWhitespace(char c) {
  return c == ' ' || c == '\n';
}

void skipWhitespace(char **raw) {
  while (isWhitespace(**raw)) {
    ++*raw;
  }
}

Pattern parsePattern(char **raw) {
  Pattern pattern = 0;
  while (!isWhitespace(**raw)) {
    pattern |= 1 << signalIndex(**raw);
    ++*raw;
  }
  return pattern;
}

struct Line parseLine(char **raw) {
  struct Line line;
  // Parse digit patterns
  int i = 0;
  while (**raw != '|') {
    line.digitPatterns[i++] = parsePattern(raw);
    skipWhitespace(raw);
  }
  // Skip separator
  while (**raw == '|') {
    ++*raw;
  }
  skipWhitespace(raw);
  // Parse output patterns
  int j = 0;
  while (**raw != '\0') {
    line.outputPatterns[j++] = parsePattern(raw);
    skipWhitespace(raw);
  }
  return line;
}

int main(void) {
  FILE *f = fopen("resources/mini-demo.txt", "r");
  if (f == NULL) {
    printf("Could not find input file.\n");
    return EXIT_FAILURE;
  }

  char raw[1024] = { '\0' };
  while (fgets(raw, sizeof(raw) / sizeof(char), f)) {
    char *parsePtr = raw;
    struct Line line = parseLine(&parsePtr);
    int display = computeDisplay(line);
    printf("%d\n", display);
  }

  fclose(f);
  return EXIT_SUCCESS;
}
