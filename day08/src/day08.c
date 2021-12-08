#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define DIGITS 10
#define DISPLAY_DIGITS 4
#define SEGMENTS 7
#define VALUES 10 // should be max(DIGITS, SEGMENTS)

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

Pattern correctWirings[] = {
  0b1110111, // 0
  0b0010010, // 1
  0b1011101, // 2
  0b1011011, // 3
  0b0111010, // 4
  0b1101011, // 5
  0b1101111, // 6
  0b1010010, // 7
  0b1111111, // 8
  0b1111011  // 9
};

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
      candidates |= 1 << i;
    }
  }
  return candidates;
}

void updateMappings(Pattern pattern, Digit digit, struct Mappings *mappings) {
  for (SignalIndex i = 0; i < SEGMENTS; i++) {
    if ((pattern >> i) & 1) {
      mappings->wiringToSegments[i] &= correctWirings[i];
    }
  }
}

struct Mappings computeMappings(struct Line line) {
  struct Mappings mappings = { .wiringToSegments = { 0 } };
  CandidateSet candidateSets[DIGITS] = { 0 };
  bool completedCandidateSets[DIGITS] = { false };
  bool hasAmbiguousCandidateSets = true;

  // Search until the candidate set for every digit has size 1 (i.e. are unambiguous)
  while (hasAmbiguousCandidateSets) {
    hasAmbiguousCandidateSets = false;
    for (Digit i = 0; i < DIGITS; i++) {
      if (!completedCandidateSets[i]) {
        hasAmbiguousCandidateSets = true;
        CandidateSet set = computeCandidateSet(line.digitPatterns[i], mappings);
        candidateSets[i] = set;
        Digit candidate = single(set);
        if (candidate >= 0) {
          updateMappings(line.digitPatterns[i], candidate, &mappings);
          completedCandidateSets[i] = true;
        }
      }
    }
  }

  return mappings;
}

struct Display computeDisplay(struct Line line) {
  struct Mappings mappings = computeMappings(line);
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
  FILE *f = fopen("resources/demo.txt", "r");
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
