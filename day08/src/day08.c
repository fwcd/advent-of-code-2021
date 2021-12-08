#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

// A bit set where the rightmost bit corresponds to
// signal a being activated, the next to b being activated, etc.
typedef uint16_t Pattern;

// A bit set where the rightmost bit represents whether
// a pattern could be a 0, the next whether it could be a 1, etc
typedef uint16_t CandidateSet;

// A signal index (0 -> 'a', 1 -> 'b', ...)
typedef uint8_t SignalIndex;

// An input line corresponding to a 4-digit display wired in
// a certain random way.
struct Line {
  Pattern digitPatterns[10];
  Pattern outputPatterns[4];
};

// The digits displayed by a 4-digit display.
struct Display {
  int outputDigits[4];
};

struct Mappings {
  // Maps the (randomly) wired signal to the correct
  // segment index. -1 denotes an unknown mapping.
  SignalIndex wiringToSegment[10];
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
  return (int) (c - 'a');
}

int candidateCount(CandidateSet x) {
  int count = 0;
  for (int i = 0; i < 10; i++) {
    count += (x >> i) & 1;
  }
  return count;
}

int signalCount(Pattern x) {
  int count = 0;
  for (int i = 0; i < 10; i++) {
    count += (x >> i) & 1;
  }
  return count;
}

Pattern translate(Pattern pattern, struct Mappings mappings) {
  Pattern result = 0;
  for (int i = 0; i < 10; i++) {
    SignalIndex mapping = mappings.wiringToSegment[i];
    if (mapping >= 0) {
      result |= 1 << mapping;
    }
  }
  return result;
}

int translateToDigit(Pattern pattern, struct Mappings mappings) {
  Pattern translated = translate(pattern, mappings);
  for (int i = 0; i < 10; i++) {
    if (correctWirings[i] == translated) {
      return i;
    }
  }
  return -1;
}

bool isCandidate(int digit, Pattern pattern, struct Mappings mappings) {
  if (signalCount(correctWirings[digit]) != signalCount(pattern)) {
    return false;
  }
  Pattern translated = translate(pattern, mappings);
  // Check if translation is subset of correct wiring
  return (translated & correctWirings[digit]) == translated;
}

CandidateSet computeCandidateSet(Pattern pattern, struct Mappings mappings) {
  CandidateSet candidates = 0;
  for (int i = 0; i < 10; i++) {
    if (isCandidate(i, pattern, mappings)) {
      candidates |= 1 << i;
    }
  }
  return candidates;
}

struct Mappings computeMappings(struct Line line) {
  struct Mappings mappings;

  // Initialize all mappings to be empty
  for (int i = 0; i < sizeof(mappings.wiringToSegment) / sizeof(int); i++) {
    mappings.wiringToSegment[i] = -1;
  }

  CandidateSet candidateSets[10] = { 0 };
  bool hasAmbiguousCandidateSets = true;

  // Search until the candidate set for every digit has size 1 (i.e. are unambiguous)
  while (hasAmbiguousCandidateSets) {
    hasAmbiguousCandidateSets = true;
    for (int i = 0; i < 10; i++) {
      CandidateSet set = computeCandidateSet(line.digitPatterns[i], mappings);
      candidateSets[i] = set;
      int count = candidateCount(set);
      assert(count != 0);
      hasAmbiguousCandidateSets |= count != 1;
    }
  }

  return mappings;
}

struct Display computeDisplay(struct Line line) {
  struct Mappings mappings = computeMappings(line);
  struct Display display;

  for (int i = 0; i < 4; i++) {
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
