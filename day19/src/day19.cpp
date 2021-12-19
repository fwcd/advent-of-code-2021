#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <functional>
#include <unordered_set>
#include <unordered_map>
#include <optional>
#include <vector>

#define ROTATIONS 24

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

  bool for_each_rotation(std::function<void(const std::vector<Point> &)> action) const {
    std::vector<Point> rotated{points.begin(), points.end()};
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
    return false;
  }

  void merge(const Scanner &other, Point location) {
    for (Point p : other.points) {
      points.insert(p - location);
    }
  }

  std::optional<Point> locate(const Scanner &other) const {
    for (Point bp : points) {
      for (Point bq : other.points) {
        std::unordered_set<Point, Point::Hash> rel_base_points;
        std::unordered_set<Point, Point::Hash> rel_other_points;
        int intersection{0};

        for (Point p : points) {
          rel_base_points.insert(p - bp);
        }
        for (Point q : other.points) {
          rel_other_points.insert(q - bq);
        }
        for (Point r : rel_base_points) {
          if (rel_other_points.find(r) != rel_other_points.end()) {
            intersection++;
          }
        }

        if (intersection >= 12) {
          return bp - bq;
        }
      }
    }

    return std::nullopt;
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

void collect_points(
  int i,
  Point offset,
  const std::vector<Scanner> &scanners,
  const std::vector<std::unordered_map<int, Point>> &neighbor_locations,
  std::unordered_set<int>& visited,
  Scanner &combined
) {
  std::cout << "Scanner " << (i / ROTATIONS) << " (" << (i % ROTATIONS) << ") is at " << offset.to_string() << std::endl;
  combined.merge(scanners[i], offset);

  for (auto neighbor : neighbor_locations[i]) {
    int j{neighbor.first};
    Point location{neighbor.second};

    if (visited.find(j) == visited.end()) {
      visited.insert(j);
      collect_points(j, offset + location, scanners, neighbor_locations, visited, combined);
    }
  }
}

int main() {
  std::ifstream file{"resources/demo.txt"};
  Scanner scanner;
  std::vector<Scanner> scanners;

  while (parse_scanner(file, scanner)) {
    scanner.for_each_rotation([&scanners] (const std::vector<Point> &points) {
      scanners.push_back(Scanner({{points.begin(), points.end()}}));
    });
    scanner = Scanner();
  }

  std::vector<std::unordered_map<int, Point>> neighbor_locations;

  for (int i = 0; i < scanners.size(); i++) {
    neighbor_locations.push_back(std::unordered_map<int, Point>());
  }

  for (int i = 0; i < scanners.size() / ROTATIONS; i++) {
    for (int j = i + 1; j < scanners.size() / ROTATIONS; j++) {
      for (int ir = 0; ir < ROTATIONS; ir++) {
        for (int jr = 0; jr < ROTATIONS; jr++) {
          int ia{i * ROTATIONS + ir};
          int ja{j * ROTATIONS + jr};
          const Scanner &lhs{scanners[ia]};
          const Scanner &rhs{scanners[ja]};
          std::optional<Point> location{lhs.locate(rhs)};
          if (location) {
            neighbor_locations[ia].insert({ja, *location});
            neighbor_locations[ja].insert({ia, -*location});
          }
        }
      }
    }
  }

  // Assemble neighbor_locations graph to actual points
  Scanner combined;
  std::unordered_set<int> visited;
  collect_points(0, Point(0, 0, 0), scanners, neighbor_locations, visited, combined);
  std::cout << "Part 1: " << combined.points.size() << std::endl;

  return 0;
}
