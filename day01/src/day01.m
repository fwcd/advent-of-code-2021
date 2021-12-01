#import <Foundation/Foundation.h>

NSArray *readInput() {
  NSString *input = [NSString stringWithContentsOfFile:@"resources/input.txt" encoding:NSUTF8StringEncoding error:nil];
  NSArray *lines = [input componentsSeparatedByString:@"\n"];
  return lines;
}

int computeIncreaseSteps(NSArray *input, int windowSize) {
  int window = [input[0] intValue];
  int steps = 0;
  for (int i = 1; i < [input count]; i++) {
    int last = window;
    window += [input[i] intValue];
    if (i >= windowSize) {
      window -= [input[i - windowSize] intValue];
      if (window > last) {
        steps++;
      }
    }
  }
  return steps;
}

int main(void) {
  @autoreleasepool {
    NSArray *input = readInput();
    int part1 = computeIncreaseSteps(input, 1);
    NSLog(@"Part 1: %d", part1);
    int part2 = computeIncreaseSteps(input, 3);
    NSLog(@"Part 2: %d", part2);
    return 0;
  }
}
