#include <stdio.h>

struct Pattern {
  char signals[9];
};

struct Line {
  struct Pattern digitPatterns[10];
  struct Pattern outputPatterns[4];
};

struct Pattern correctWirings[] = {
  { .signals = "abcefg" },  // 0
  { .signals = "cf" },      // 1
  { .signals = "acdeg" },   // 2
  { .signals = "acdfg" },   // 3
  { .signals = "bcdf" },    // 4
  { .signals = "abdfg" },   // 5
  { .signals = "abdefg" },  // 6
  { .signals = "acf" },     // 7
  { .signals = "abcdefg" }, // 8
  { .signals = "abcdfg" }   // 9
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
