#include <string>
#include <fstream>
#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <cctype>

using namespace std;

typedef struct Range{
  int low;
  int high;
} Range;

//returns list of ints in string
vector<int> parseLine(const string& line) {
  vector<int> list;
  string curr;
  for (char c : line) {
    if (isdigit(c)) {
      curr.push_back(c);
    } else if (curr != ""){
      list.push_back(stoi(curr));
      curr = "";
    }
  }
  if (curr != "") {
    list.push_back(stoi(curr));
  }
  return list;
}

//this function completely falls apart if the line is not the right format
std::tuple<Range, Range> lineToRangePair(const std::string& line) {
  vector<int> parsedLine = parseLine(line);
  Range first_elf = {parsedLine[0],parsedLine[1]};
  Range second_elf = {parsedLine[2],parsedLine[3]};
  return std::make_tuple(first_elf,second_elf);
}

bool containsRedundantPair(std::tuple<Range,Range>& pair) {
  Range first = std::get<0>(pair);Range second = std::get<1>(pair);
  if ((first.low <= second.low && first.high >= second.high)
      || (second.low <= first.low && second.high >= first.high)) {
    return true;
  } else {
    return false;
  }
}

bool containsOverlappingPair(tuple<Range,Range>& pair) {
  Range first = std::get<0>(pair);Range second = std::get<1>(pair);
  if (first.low <= second.high && first.high >= second.low) {
    return true;
  } else {
    return false;
  }
}

//takes a file name input and produces a vector of strings,each a line
std::vector<std::string> readFile (const std::string& filename) {
  std::ifstream input;
  input.open(filename);
  std::vector<std::string> lines;
  std::string line;
  while (std::getline(input,line)) {
    lines.push_back(line);
  }
  return lines;
}

void displayStringVector(const std::vector<std::string>& lines) {
  for_each(lines.begin(),lines.end(),
	   [](std::string line)->void{std::cout << line << '\n';});
}

int main (int argc, char **argv) {
  std::string inputFilename(argv[1]);
  std::vector<std::string> fileContent = readFile(inputFilename);;
  std::vector<std::tuple<Range,Range>> pairsVector;
  for (std::string line : fileContent) {
    pairsVector.push_back(lineToRangePair(line));
  }
  int totalRedundantPairs = 0;
  int totalOverlappingPairs = 0;
  for (std::tuple<Range,Range> pair : pairsVector) {
    if (containsRedundantPair(pair)) {
      totalRedundantPairs++;
      totalOverlappingPairs++;
    } else if (containsOverlappingPair(pair)) {
      totalOverlappingPairs++;
    }
  }
  std::cout << "Puzzle one answer:" << totalRedundantPairs << std::endl;
  std::cout << "Puzzle two answer:" << totalOverlappingPairs << std::endl;
}





