#import <Foundation/Foundation.h>

NSArray *readInput() {
  NSString *input = [NSString stringWithContentsOfFile:@"resources/input.txt" encoding:NSUTF8StringEncoding error:nil];
  NSArray *lines = [input componentsSeparatedByString:@"\n"];
  return lines;
}

int computeIncreaseSteps(NSArray *input) {
  int last = [input[0] intValue];
  int steps = 0;
  for (int i = 1; i < [input count]; i++) {
    int current = [input[i] intValue];
    if (current > last) {
      steps++;
    }
    last = current;
  }
  return steps;
}

int main(void) {
  @autoreleasepool {
    NSArray *input = readInput();
    int part1 = computeIncreaseSteps(input);
    NSLog(@"Part 1: %d", part1);
    return 0;
  }
}
