#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

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

bool isWhitespace(char c) {
  return c == ' ' || c == '\n';
}

void skipWhitespace(char **raw) {
  while (isWhitespace(**raw)) {
    ++*raw;
  }
}

struct Pattern parsePattern(char **raw) {
  int i = 0;
  struct Pattern pattern;
  while (!isWhitespace(**raw)) {
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
    printf("Got line\n");
  }

  fclose(f);
  return EXIT_SUCCESS;
}
