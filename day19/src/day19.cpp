#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <functional>
#include <vector>

struct Point {
  int x;
  int y;
  int z;

  Point(int x, int y, int z) : x(x), y(y), z(z) {}

  Point operator+(Point rhs) const {
    return Point(x + rhs.x, y + rhs.y, z + rhs.z);
  }

  Point operator-(Point rhs) const {
    return Point(x - rhs.x, y - rhs.y, z - rhs.z);
  }

  // Rotations as in https://stackoverflow.com/a/16467849

  Point roll() const { return Point(x, -z, y); }

  Point turn() const { return Point(-y, x, z); }

  Point turn_inv() const { return Point(y, -x, z); }

  std::string to_string() const {
    std::stringstream ss;
    ss << "(" << x << ", " << y << ", " << z << ")";
    return ss.str();
  }
};

struct Scanner {
  std::vector<Point> points;

  // Algorithm for generating the 24 rotations inspired by https://stackoverflow.com/a/58471362

  void for_each_rotation(std::function<void(const std::vector<Point> &)> action) const {
    std::vector<Point> rotated{points};
    for (int r = 0; r < 6; r++) {
      for (Point &point : rotated) {
        point = point.roll();
      }
      action(rotated);
      for (int t = 0; t < 3; t++) {
        for (Point &point : rotated) {
          if (r % 2 == 0) {
            point = point.turn();
          } else {
            point = point.turn_inv();
          }
        }
        action(rotated);
      }
    }
  }
};

int parse_component(std::string input, int &i) {
  int start{i};
  i = input.find(',', start);
  int value{std::stoi(input.substr(start, i - start))};
  i++;
  return value;
}

Point parse_point(std::string line) {
  int i{0};
  int x{parse_component(line, i)};
  int y{parse_component(line, i)};
  int z{parse_component(line, i)};
  return Point(x, y, z);
}

bool parse_scanner(std::ifstream &file, Scanner &scanner) {
  std::string line;
  if (!std::getline(file, line)) return false;
  if (line.rfind("---", 0) != 0) return false;
  while (true) {
    if (!std::getline(file, line)) return false;
    if (line.empty()) break;
    scanner.points.push_back(parse_point(line));
  }
  return true;
}

int main() {
  std::ifstream file{"resources/demo.txt"};
  Scanner scanner;
  std::vector<Scanner> scanners;

  // Only for testing
  Scanner s{{Point(1, 2, 3)}};
  s.for_each_rotation([] (const std::vector<Point> &points) {
    std::cout << points[0].to_string() << std::endl;
  });

  while (parse_scanner(file, scanner)) {
    scanners.push_back(scanner);
    scanner = Scanner();
  }

  return 0;
}
