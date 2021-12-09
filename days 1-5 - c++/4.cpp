#include <iostream>
#include <utility>
#include <vector>
#include <algorithm>
#include <bitset>
#include <string>
#include <sstream>

std::string read() {
    static std::string inputLine;
    std::getline(std::cin, inputLine, '\n');
    return inputLine;
}

struct board {
    int fields[25];
    bool marked[25];

    int leftInRow[5];
    int leftInColumn[5];

    board(const std::string& a, const std::string& b, const std::string& c, const std::string& d, const std::string& e) {
        parseAndDump(0, a);
        parseAndDump(1, b);
        parseAndDump(2, c);
        parseAndDump(3, d);
        parseAndDump(4, e);
    }


    void print() {
        for(int y = 0; y < 5; y++) {
            std::cout << at(0, y) << " " << at(1, y) << " " << at(2, y) << " " << at(3, y) << " " << at(4, y) << "\n";
        }
    }

    void parseAndDump(int y, const std::string& line) {
        leftInRow[y] = 5;
        leftInColumn[y] = 5;
        for(int x = 0; x < 5; x++) {
            int from = x * 3;
            if(line[from] == ' ') from++;
            at(x, y) = std::stoi(line.substr(from, x * 3 + 2));
            isMarked(x, y) = false;
        }
    }

    int& at(int x, int y) {
        return fields[x + 5 * y];
    }

    bool& isMarked(int x, int y) {
        return marked[x + 5 * y];
    }

    bool mark(int i) {
        bool ret = false;
        for(int x = 0; x < 5; x++)
            for(int y = 0; y < 5; y++) {
                if(at(x, y) == i) {
                    marked[x + 5 * y] = true;
                    leftInRow[y]--;
                    leftInColumn[x]--;
//                    std::cout << "Marked at (" << x << " " << y << "), left in row: " <<leftInRow[y] << ", left in column: " << leftInColumn[x] << "\n";
                    if(leftInRow[y] == 0 || leftInColumn[x] == 0) ret = true;
                }
            }
        return ret;
    }

    int unmarkedSum() {
        int ret = 0;
        for(int x = 0; x < 5; x++)
            for(int y = 0; y < 5; y++) {
                if(!isMarked(x, y)) {
                    ret += at(x, y);
                }
            }
        return ret;
    }

};

int main() {
    std::stringstream numberInput(read());
    std::string numberReader;

    std::vector<int> allNumbers;
    while(std::getline(numberInput, numberReader, ',')) {
        allNumbers.push_back(std::stoi(numberReader));
    }

    std::cout << "Read all numbers: ";
    for(auto i : allNumbers) {
        std::cout << i << ", ";
    }
    std::cout << "\n\n";
    read();

    std::vector<board> allBoards;
    for(std::string str = read(); !str.empty(); str = read()) {
        allBoards.emplace_back(
            str, read(), read(), read(), read()
        );
        read();
    }

    std::cout << "Done reading\n";

    board* winningBoard = 0;
    int winningNumber = -1;
    for(auto i : allNumbers) {
        if(allBoards.size() > 1) {
            auto boardWins = [i](board &b) {
                return b.mark(i);
            };
            allBoards.erase(std::remove_if(allBoards.begin(), allBoards.end(), boardWins), allBoards.end());
        } else {
            winningBoard = &allBoards[0];
            if(winningBoard->mark(i)) {
                winningNumber = i;
                goto aftermath;
            }
        }
    }
    aftermath:
    int unmarked = winningBoard->unmarkedSum();
    std::cout << "Winning board:\n";
    winningBoard->print();

    std::cout << unmarked << "*" << winningNumber << "=" << (unmarked * winningNumber) << "\n";
    return 0;
}
