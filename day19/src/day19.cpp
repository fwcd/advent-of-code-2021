#include <array>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <functional>
#include <unordered_set>
#include <unordered_map>
#include <optional>
#include <vector>

/// One of 24 axis-aligned rotations in 3D.
struct Rotation {
  std::array<int, 3> indices;
  std::array<int, 3> flips;

  Rotation(int ix, int iy, int iz, int flip_x, int flip_y, int flip_z)
    : indices({ix, iy, iz}), flips({flip_x, flip_y, flip_z}) {}

  Rotation compose(Rotation rhs) const {
    return {
      rhs.indices[indices[0]],
      rhs.indices[indices[1]],
      rhs.indices[indices[2]],
      flips[0] * rhs.flips[indices[0]],
      flips[1] * rhs.flips[indices[1]],
      flips[2] * rhs.flips[indices[2]]
    };
  }

  Rotation inverse() const {
    Rotation inv{0, 0, 0, 0, 0, 0};
    for (int i = 0; i < 3; i++) {
      inv.indices[indices[i]] = i;
      inv.flips[indices[i]] = flips[i];
    }
    return inv;
  }

  std::string to_string() const {
    std::stringstream ss;
    ss << "<" << (flips[0] < 0 ? "-" : "") << indices[0]
      << ", " << (flips[1] < 0 ? "-" : "") << indices[1]
      << ", " << (flips[2] < 0 ? "-" : "") << indices[2] << ">";
    return ss.str();
  }
};

// Algorithm for generating the 24 rotations inspired by
// https://stackoverflow.com/a/58471362 and https://stackoverflow.com/a/16467849

const Rotation ID{0, 1, 2, 1, 1, 1};
const Rotation ROLL{0, 2, 1, 1, 1, -1};
const Rotation TURN{1, 0, 2, -1, 1, 1};

std::vector<Rotation> generate_rotations() {
  Rotation rotation{ID};
  std::vector<Rotation> rotations;
  for (int r = 0; r < 6; r++) {
    rotation = ROLL.compose(rotation);
    rotations.push_back(rotation);
    for (int t = 0; t < 3; t++) {
      if (r % 2 == 0) {
        rotation = TURN.compose(rotation);
      } else {
        rotation = TURN.inverse().compose(rotation);
      }
      rotations.push_back(rotation);
    }
  }
  return rotations;
}

/// A point or vector in 3D space.
struct Point {
  std::array<int, 3> xyz;

  Point(int x, int y, int z) : xyz({x, y, z}) {}

  inline int x() const { return xyz[0]; }

  inline int y() const { return xyz[1]; }

  inline int z() const { return xyz[2]; }

  Point operator+(Point rhs) const { return {x() + rhs.x(), y() + rhs.y(), z() + rhs.z()}; }

  Point operator-() const { return {-x(), -y(), -z()}; }

  Point operator-(Point rhs) const { return {x() - rhs.x(), y() - rhs.y(), z() - rhs.z()}; }

  bool operator==(Point rhs) const { return x() == rhs.x() && y() == rhs.y() && z() == rhs.z(); }

  int manhattan() const { return std::abs(x()) + std::abs(y()) + std::abs(z()); }

  Point apply(Rotation rotation) const {
    return {
      xyz[rotation.indices[0]] * rotation.flips[0],
      xyz[rotation.indices[1]] * rotation.flips[1],
      xyz[rotation.indices[2]] * rotation.flips[2]
    };
  }

  std::string to_string() const {
    std::stringstream ss;
    ss << "(" << x() << ", " << y() << ", " << z() << ")";
    return ss.str();
  }

  struct Hash {
    size_t operator()(const Point &point) const {
      return point.x() ^ (point.y() << 1) ^ (point.z() << 2);
    }
  };
};

struct Scanner {
  std::unordered_set<Point, Point::Hash> points;

  Scanner operator+(Point offset) const {
    Scanner result;
    for (Point p : points) {
      result.points.insert(p + offset);
    }
    return result;
  }

  Scanner operator-(Point offset) const {
    return *this + (-offset);
  }

  Scanner apply(Rotation rotation) const {
    Scanner result;
    for (Point p : points) {
      result.points.insert(p.apply(rotation));
    }
    return result;
  }

  Scanner intersect(const Scanner &other) const {
    Scanner result;
    for (Point p : points) {
      if (other.points.find(p) != other.points.end()) {
        result.points.insert(p);
      }
    }
    return result;
  }

  void merge(const Scanner &other) {
    for (Point p : other.points) {
      points.insert(p);
    }
  }

  std::optional<Point> locate(const Scanner &other) const {
    for (Point bp : points) {
      Scanner rel{*this - bp};

      for (Point bq : other.points) {
        Scanner rel_other{other - bq};
        Scanner intersection{rel.intersect(rel_other)};

        if (intersection.points.size() >= 12) {
          return bp - bq;
        }
      }
    }

    return std::nullopt;
  }

  std::string to_string() const {
    std::stringstream ss;
    ss << "--- Scanner ---" << std::endl;
    for (Point p : points) {
      ss << p.to_string() << std::endl;
    }
    return ss.str();
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
  return {x, y, z};
}

bool parse_scanner(std::ifstream &file, Scanner &scanner) {
  std::string line;
  if (!std::getline(file, line)) return false;
  if (line.rfind("---", 0) != 0) return false;
  while (true) {
    if (!std::getline(file, line) || line.empty()) break;
    scanner.points.insert(parse_point(line));
  }
  return true;
}

void explore(
  int i,
  Point current,
  std::vector<Scanner> &scanners,
  Scanner &combined,
  std::vector<Point> &scanner_locations,
  std::unordered_set<int> &visited
) {
  visited.insert(i);
  const Scanner &lhs{scanners[i]};
  combined.merge(lhs + current);
  for (int j = 0; j < scanners.size(); j++) {
    if (visited.find(j) == visited.end()) {
      const Scanner &rhs{scanners[j]};
      for (Rotation rotation : generate_rotations()) {
        Scanner rotated{rhs.apply(rotation)};
        std::optional<Point> location{lhs.locate(rotated)};
        if (location) {
          // Yay, we found a match! Now let's orient the scanner to scanner 0's
          // coordinate system (the canonical one) and explore it...
          Point next{current + *location};
          std::cout << "Scanner " << i << " located " << j << " at " << next.to_string() << std::endl;
          scanner_locations[j] = next;
          scanners[j] = rotated;
          explore(j, next, scanners, combined, scanner_locations, visited);
          break;
        }
      }
    }
  }
}

int main() {
  std::vector<Scanner> scanners;

  // Parse the input
  {
    std::ifstream file{"resources/input.txt"};
    Scanner scanner;
    while (parse_scanner(file, scanner)) {
      scanners.push_back(scanner);
      scanner = {};
    }
  }

  Scanner combined;
  std::unordered_set<int> visited;
  std::vector<Point> scanner_locations;

  for (int i = 0; i < scanners.size(); i++) {
    scanner_locations.push_back({0, 0, 0});
  }

  // Traverse the graph of scanners depth-first
  explore(0, {0, 0, 0}, scanners, combined, scanner_locations, visited);

  size_t part1{combined.points.size()};
  std::cout << "Part 1: " << part1 << std::endl;

  // Compute largest Manhattan distance
  int part2{0};
  for (int i = 0; i < scanner_locations.size(); i++) {
    for (int j = i + 1; j < scanner_locations.size(); j++) {
      part2 = std::max(part2, (scanner_locations[i] - scanner_locations[j]).manhattan());
    }
  }
  std::cout << "Part 2: " << part2 << std::endl;

  return 0;
}
