#include <iostream>
#include <utility>
#include <vector>
#include <algorithm>
#include <bitset>
#include <string>
#include <sstream>
#include <functional>

std::string read() {
    static std::string inputLine;
    std::getline(std::cin, inputLine, '\n');
    return inputLine;
}

struct line {
    int xMin, yMin;
    int xMax, yMax;

    line(int xMin, int yMin, int xMax, int yMax) : xMin(xMin), yMin(yMin), xMax(xMax), yMax(yMax) {}
    
    void traverse(std::function<void(int, int)>& forEach) {
        if (xMin == xMax) {
            if(yMin < yMax) for (int y = yMin; y <= yMax; y++) forEach(xMin, y);
            else for (int y = yMax; y <= yMin; y++) forEach(xMin, y);
        }
        else if (yMin == yMax) {
            if(xMin < xMax) for (int x = xMin; x <= xMax; x++) forEach(x, yMin);
            else for (int x = xMax; x <= xMin; x++) forEach(x, yMin);
        }
        else {
            for (int i = 0; i <= std::abs(xMax - xMin); i++) forEach((xMin < xMax ? xMin + i : xMin - i), (yMin < yMax ? yMin + i : yMin - i));
        }
    }
};

struct grid {

    int* values;
    int width, height;
    grid(int width, int height) : width(width), height(height), values((int*) malloc(width * height * sizeof(int))) {
        std::fill(values, values + width * height, 0);
    }

    int& at(const int& x, const int& y) { return values[x + width * y]; }

    void print() {
        for(int y = 0; y < height; y++) {
            for(int x = 0; x < width; x++) {
                if(at(x, y) > 0) std::cout << std::to_string(at(x, y));
                else std::cout << '.';
            }
            std::cout << '\n';
        }
    }

};

int main() {
    std::vector<line> allLines;
    int width = 0, height = 0;
    for(std::string str = read(); !str.empty(); str = read()) {
        auto middleIdx = str.find(" -> ");
        auto left = str.substr(0, middleIdx);
        auto right = str.substr(middleIdx + 4, str.length());
        auto commaLeft = left.find(',');
        auto commaRight = right.find(',');
        auto fromX = std::stoi(left.substr(0, commaLeft));
        auto fromY = std::stoi(left.substr(commaLeft + 1, left.length()));
        auto toX = std::stoi(right.substr(0, commaRight));
        auto toY = std::stoi(right.substr(commaRight + 1, right.length()));
        line l(
            fromX, fromY,
            toX, toY
        );
        allLines.push_back(l);
        if(l.xMax > width) width = l.xMax;
        if(l.xMin > width) width = l.xMin;
        if(l.yMax > height) height = l.yMax;
        if(l.yMin > height) height = l.yMin;
    }
    width++;
    height++;
    grid totalGrid(width, height);
    for(auto& line: allLines) {
        std::function<void(int, int)> action = [&totalGrid](int x, int y) {
            totalGrid.at(x, y)++;
        };
        line.traverse(action);
    }
//    totalGrid.print();

    int res = 0;
    for(int x = 0; x < width; x++)
        for(int y = 0; y < height; y++)
            if(totalGrid.at(x, y) > 1) res++;

    std::cout << res << "\n";
    return 0;
}
