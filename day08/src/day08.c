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

// The digits displayed by a 4-digit display.
struct Display {
  Digit outputDigits[DISPLAY_DIGITS];
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
    Pattern segments = mappings.wiringToSegments[i];
    SignalIndex segment = single(segments);
    if (segment >= 0) {
      result |= 1 << segment;
    }
  }
  return result;
}

Digit translateToDigit(Pattern pattern, struct Mappings mappings) {
  Pattern translated = translate(pattern, mappings);
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
      if (i == 8) {
        printf("%d is candidate for ", i);
        printPattern(pattern);
        printf("\n");
      }
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

struct OptionalMappings solveMappings(struct Mappings mappings) {
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

  // If there is no such mapping, we've already solved everything!
  if (next == -1) {
    return (struct OptionalMappings) { .mappings = mappings, .present = true };
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
      struct OptionalMappings afterChoice = solveMappings(newMappings);

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
          printf("Updating mappings, we know ");
          printPattern(pattern);
          printf(" -> %d (correct: ", candidate);
          printPattern(correctWirings[candidate]);
          printf(")\n");
          for (Digit j = 0; j < DIGITS; j++) {
            if (completedCandidateSets[j]) {
              updateMappings(completedDigitPatterns[j], j, &mappings);
            }
          }
          printMappings(mappings);
        }
      }
    }
  } while (foundNewMappings);

  // Solve the ambiguous mappings
  struct OptionalMappings solvedMappings = solveMappings(mappings);
  if (!solvedMappings.present) {
    printf("Could not solve mappings!\n");
    exit(1);
  } else {
    printf("We solved 'em!\n");
  }

  return solvedMappings.mappings;
}

struct Display computeDisplay(struct Line line) {
  struct Mappings mappings = computeMappings(line);
  printf("Final mappings:\n");
  printMappings(mappings);
  struct Display display;

  for (int i = 0; i < DISPLAY_DIGITS; i++) {
    display.outputDigits[i] = translateToDigit(line.outputPatterns[i], mappings);
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
    struct Display display = computeDisplay(line);
    printf("%d, %d, %d, %d\n", display.outputDigits[0], display.outputDigits[1], display.outputDigits[2], display.outputDigits[3]);
  }

  fclose(f);
  return EXIT_SUCCESS;
}
