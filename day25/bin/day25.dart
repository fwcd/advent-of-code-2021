import 'dart:io';

typedef Grid = List<List<String>>;

extension GridMapping on Grid {
  int get height => length;
  int get width => this[0].length;

  Grid mapGrid(void Function(Grid, Grid, int, int) action) {
    List<List<String>> result = List.generate(height, (y) => List.generate(width, (x) => this[y][x]));
    for (var y = 0; y < height; y++) {
      for (var x = 0; x < width; x++) {
        action(this, result, y, x);
      }
    }
    return result;
  }

  bool deepEquals(Grid other) {
    for (var y = 0; y < height; y++) {
      for (var x = 0; x < width; x++) {
        if (this[y][x] != other[y][x]) {
          return false;
        }
      }
    }
    return true;
  }
}

Grid step(Grid grid) {
  return grid
    .mapGrid((g1, g2, y, x) {
      final nx = (x + 1) % g1.width;
      if (g1[y][x] == '>' && g1[y][nx] == '.') {
        g2[y][nx] = '>';
        g2[y][x] = '.';
      }
    })
    .mapGrid((g1, g2, y, x) {
      final ny = (y + 1) % g1.height;
      if (g1[y][x] == 'v' && g1[ny][x] == '.') {
        g2[ny][x] = 'v';
        g2[y][x] = '.';
      }
    });
}

Future<void> main(List<String> arguments) async {
  final input = await File('resources/input.txt').readAsString();
  Grid grid = input.split('\n')
    .where((l) => l.isNotEmpty)
    .map((l) => l.split('').toList())
    .toList();
  Grid last;
  int steps = 0;

  do {
    last = grid;
    grid = step(grid);
    steps++;
  } while (!last.deepEquals(grid));

  print('Part 1: $steps');
}
