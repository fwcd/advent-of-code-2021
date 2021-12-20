#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <stdexcept>
#include <functional>
#include <unordered_set>
#include <unordered_map>
#include <optional>
#include <vector>

struct Rotation {
  int ix;
  int iy;
  int iz;
  int flip_x;
  int flip_y;
  int flip_z;

  Rotation(int ix, int iy, int iz, int flip_x, int flip_y, int flip_z)
    : ix(ix), iy(iy), iz(iz), flip_x(flip_x), flip_y(flip_y), flip_z(flip_z) {}

  int operator[](int index) const {
    switch (index) {
    case 0: return ix;
    case 1: return iy;
    case 2: return iz;
    default: throw std::invalid_argument("Index out of bounds");
    }
  }

  int flip(int index) const {
    switch (index) {
    case 0: return flip_x;
    case 1: return flip_y;
    case 2: return flip_z;
    default: throw std::invalid_argument("Index out of bounds");
    }
  }

  Rotation compose(Rotation lhs) const {
    return {lhs[ix], lhs[iy], lhs[iz], flip_x * lhs.flip(ix), flip_y * lhs.flip(iy), flip_z * lhs.flip(iz)};
  }

  // Rotations as in https://stackoverflow.com/a/16467849

  std::string to_string() const {
    std::stringstream ss;
    ss << "<" << ix << ", " << iy << ", " << iz << ">";
    return ss.str();
  }
};

const Rotation ID{0, 1, 2, 1, 1, 1};
const Rotation ROLL{0, 2, 1, 1, 1, -1};
const Rotation TURN{1, 0, 2, -1, 1, 1};
const Rotation TURN_INV{1, 0, 2, 1, -1, 1};

struct Point {
  int x;
  int y;
  int z;

  Point(int x, int y, int z) : x(x), y(y), z(z) {}

  Point operator+(Point rhs) const { return {x + rhs.x, y + rhs.y, z + rhs.z}; }

  Point operator-() const { return {-x, -y, -z}; }

  Point operator-(Point rhs) const { return {x - rhs.x, y - rhs.y, z - rhs.z}; }

  bool operator==(Point rhs) const { return x == rhs.x && y == rhs.y && z == rhs.z; }

  int operator[](int index) const {
    switch (index) {
    case 0: return x;
    case 1: return y;
    case 2: return z;
    default: throw std::invalid_argument("Index out of bounds");
    }
  }

  Point apply(Rotation rotation) const {
    return {(*this)[rotation.ix], (*this)[rotation.iy], (*this)[rotation.iz]};
  }

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

  std::vector<Scanner> rotations() const {
    std::vector<Scanner> rotations;
    std::vector<Point> rotated{points.begin(), points.end()};
    for (int r = 0; r < 6; r++) {
      for (Point &point : rotated) {
        point = point.apply(ROLL);
      }
      rotations.push_back({{rotated.begin(), rotated.end()}});
      for (int t = 0; t < 3; t++) {
        for (Point &point : rotated) {
          if (r % 2 == 0) {
            point = point.apply(TURN);
          } else {
            point = point.apply(TURN_INV);
          }
        }
        rotations.push_back({{rotated.begin(), rotated.end()}});
      }
    }
    return rotations;
  }

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

  Scanner intersect(const Scanner &other) const {
    Scanner result;
    for (Point p : points) {
      if (other.points.find(p) != other.points.end()) {
        result.points.insert(p);
      }
    }
    return result;
  }

  void merge(const Scanner &other, Point location) {
    for (Point p : other.points) {
      points.insert(p + location);
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

void collect_points(
  int i,
  Point offset,
  const std::vector<Scanner> &scanners,
  const std::vector<std::unordered_map<int, Point>> &neighbor_locations,
  std::unordered_set<int>& visited,
  Scanner &combined
) {
  std::cout << "Scanner " << i << " is at " << offset.to_string() << std::endl;
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
  std::vector<Scanner> scanners;

  {
    std::ifstream file{"resources/demo.txt"};
    Scanner scanner;
    while (parse_scanner(file, scanner)) {
      scanners.push_back(scanner);
      scanner = {};
    }
  }

  std::vector<std::unordered_map<int, Point>> neighbor_locations;
  std::vector<bool> frozen;

  for (int i = 0; i < scanners.size(); i++) {
    neighbor_locations.push_back(std::unordered_map<int, Point>());
    frozen.push_back(false);
  }

  for (int i = 0; i < scanners.size(); i++) {
    for (int j = 0; j < scanners.size(); j++) {
      if (i != j) {
        Scanner lhs{scanners[i]};
        Scanner rhs{scanners[j]};
        auto rotations{frozen[j] ? std::vector<Scanner>{rhs} : rhs.rotations()};
        for (const Scanner &rotated : rotations) {
          std::optional<Point> location{lhs.locate(rotated)};
          if (location) {
            std::cout << "Scanner " << i << " located " << j << " at " << location->to_string() << std::endl;
            neighbor_locations[i].insert({j, *location});
            neighbor_locations[j].insert({i, -*location});
            scanners[j] = rotated;
            frozen[j] = true;
            break;
          }
        }
      }
    }
  }

  // Assemble neighbor_locations graph to actual points
  Scanner combined;
  std::unordered_set<int> visited;
  collect_points(0, {0, 0, 0}, scanners, neighbor_locations, visited, combined);
  std::cout << "Part 1: " << combined.points.size() << std::endl;

  return 0;
}
