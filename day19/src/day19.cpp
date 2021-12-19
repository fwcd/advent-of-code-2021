#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <functional>
#include <unordered_set>
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

  bool operator==(Point rhs) const {
    return x == rhs.x && y == rhs.y && z == rhs.z;
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

  struct Hash {
    size_t operator()(const Point &point) const {
      return point.x ^ (point.y << 1) ^ (point.z << 2);
    }
  };
};

struct Scanner {
  std::unordered_set<Point, Point::Hash> points;

  // Algorithm for generating the 24 rotations inspired by https://stackoverflow.com/a/58471362

  bool for_each_rotation(std::function<bool(const std::vector<Point> &)> action) const {
    std::vector<Point> rotated{points.begin(), points.end()};
    for (int r = 0; r < 6; r++) {
      for (Point &point : rotated) {
        point = point.roll();
      }
      if (action(rotated)) return true;
      for (int t = 0; t < 3; t++) {
        for (Point &point : rotated) {
          if (r % 2 == 0) {
            point = point.turn();
          } else {
            point = point.turn_inv();
          }
        }
        if (action(rotated)) return true;
      }
    }
    return false;
  }

  bool try_merge(const Scanner &other) {
    return other.for_each_rotation([this] (const std::vector<Point> &other_points) {
      for (Point bp : points) {
        for (Point bq : other_points) {
          std::unordered_set<Point, Point::Hash> rel_base_points;
          std::unordered_set<Point, Point::Hash> rel_other_points;
          std::unordered_set<Point, Point::Hash> intersection;

          for (Point p : points) {
            rel_base_points.insert(p - bp);
          }
          for (Point q : other_points) {
            rel_other_points.insert(q - bq);
          }
          for (Point r : rel_base_points) {
            if (rel_other_points.find(r) != rel_other_points.end()) {
              intersection.insert(r);
            }
          }

          if (intersection.size() >= 12) {
            std::cout << "Found scanner at " << (bp - bq).to_string() << std::endl;
            for (Point r : rel_other_points) {
              points.insert(r + bp);
            }
            return true;
          }
        }
      }
      return false;
    });
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
    scanner.points.insert(parse_point(line));
  }
  return true;
}

int main() {
  std::ifstream file{"resources/demo.txt"};
  Scanner scanner;
  std::vector<Scanner> scanners;

  while (parse_scanner(file, scanner)) {
    scanners.push_back(scanner);
    scanner = Scanner();
  }

  Scanner combined{scanners[0]};

  for (int i = 1; i < scanners.size(); i++) {
    const Scanner &scanner{scanners[i]};
    if (combined.try_merge(scanner)) {
      std::cout << "Merged " << i << std::endl;
    }
  }

  std::cout << "Part 1: " << combined.points.size() << std::endl;

  return 0;
}
