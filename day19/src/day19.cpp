#include <iostream>
#include <fstream>

int main() {
  std::ifstream file{"resources/demo.txt"};
  std::string input;
  std::getline(file, input);
  std::cout << input << std::endl;
  return 0;
}
