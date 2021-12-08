#include <stdio.h>

const char *correctWirings[] = {
  "abcefg",  // 0
  "cf",      // 1
  "acdeg",   // 2
  "acdfg",   // 3
  "bcdf",    // 4
  "abdfg",   // 5
  "abdefg",  // 6
  "acf",     // 7
  "abcdefg", // 8
  "abcdfg"   // 9
};

struct Pattern {
  char signals[9];
};

struct Line {
  struct Pattern digitPatterns[10];
  struct Pattern outputPatterns[4];
};

struct Pattern parsePattern(char **raw) {
  int i = 0;
  struct Pattern pattern;
  while (**raw != ' ' && **raw != '\0') {
    pattern.signals[i++] = **raw;
    ++*raw;
  }
  pattern.signals[i] = '\0';
  return pattern;
}

struct Line parseLine(char **raw) {
  struct Line line;
  // Parse digit patterns
  int i = 0;
  while (**raw != '|') {
    line.digitPatterns[i++] = parsePattern(raw);
  }
  // Skip separator
  while (**raw == '|' || **raw == ' ') {
    ++*raw;
  }
  // Parse output patterns
  int j = 0;
  while (*raw != '\0') {
    line.outputPatterns[j++] = parsePattern(raw);
  }
  return line;
}

int main(void) {
  printf("Hello world\n");
  return 0;
}
