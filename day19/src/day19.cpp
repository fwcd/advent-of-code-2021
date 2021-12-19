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

  Point operator+(Point rhs) {
    return Point(x + rhs.x, y + rhs.y, z + rhs.z);
  }

  Point operator-(Point rhs) {
    return Point(x - rhs.x, y - rhs.y, z - rhs.z);
  }

  std::string to_string() {
    std::stringstream ss;
    ss << "(" << x << ", " << y << ", " << z << ")";
    return ss.str();
  }
};

struct Scanner {
  std::vector<Point> points;
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

  while (parse_scanner(file, scanner)) {
    for (auto point : scanner.points) {
      std::cout << point.to_string() << std::endl;
    }
    std::cout << std::endl;
    scanners.push_back(scanner);
    scanner = Scanner();
  }

  return 0;
}
