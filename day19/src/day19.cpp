#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <functional>
#include <unordered_set>
#include <unordered_map>
#include <optional>
#include <vector>

struct Point {
  int x;
  int y;
  int z;

  Point(int x, int y, int z) : x(x), y(y), z(z) {}

  Point operator+(Point rhs) const {
    return Point(x + rhs.x, y + rhs.y, z + rhs.z);
  }

  Point operator-() const {
    return Point(-x, -y, -z);
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

  void merge(const Scanner &other, Point location) {
    for (Point p : other.points) {
      points.insert(p - location);
    }
  }

  std::optional<Point> locate(const Scanner &other) const {
    std::optional<Point> location;

    other.for_each_rotation([this, &location] (const std::vector<Point> &other_points) {
      for (Point bp : points) {
        for (Point bq : other_points) {
          std::unordered_set<Point, Point::Hash> rel_base_points;
          std::unordered_set<Point, Point::Hash> rel_other_points;
          int intersection{0};

          for (Point p : points) {
            rel_base_points.insert(p - bp);
          }
          for (Point q : other_points) {
            rel_other_points.insert(q - bq);
          }
          for (Point r : rel_base_points) {
            if (rel_other_points.find(r) != rel_other_points.end()) {
              intersection++;
            }
          }

          if (intersection >= 12) {
            location = bp - bq;
            return true;
          }
        }
      }
      return false;
    });

    return location;
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
  std::vector<std::unordered_map<int, Point>> neighbor_locations;
  std::unordered_set<int> merged;

  for (int i = 0; i < scanners.size(); i++) {
    neighbor_locations.push_back(std::unordered_map<int, Point>());
  }

  for (int i = 0; i < scanners.size(); i++) {
    for (int j = i + 1; j < scanners.size(); j++) {
      const Scanner &lhs{scanners[i]};
      const Scanner &rhs{scanners[j]};
      std::optional<Point> location{lhs.locate(rhs)};
      if (location) {
        std::cout << i << " located " << j << " at " << location->to_string() << std::endl;
        neighbor_locations[i].insert({j, *location});
        neighbor_locations[j].insert({i, -*location});
      }
    }
  }

  // TODO: Assemble neighbor_locations graph to actual points
  std::cout << "Part 1: " << combined.points.size() << std::endl;

  return 0;
}
