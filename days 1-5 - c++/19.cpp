#include <fstream>
#include <string>
#include <utility>
#include <vector>
#include <sstream>
#include <iostream>

const int orientationCount = 24;

enum axis : char {
    x = 'x', y = 'y', z = 'z',
    neg_x = 'a', neg_y = 'b', neg_z = 'c'
};

struct position {
    int x, y, z;
    position(int x, int y, int z) : x(x), y(y), z(z) {}
    position(const position& other) : x(other.x), y(other.y), z(other.z) {}

    [[nodiscard]] int get(axis a) const {
        switch (a) {
            case ::x: return x;
            case ::y: return y;
            case ::z: return z;
            case neg_x: return -x;
            case neg_y: return -y;
            case neg_z: return -z;
        }
    }

    position operator-(position other) const {
        return {x - other.x, y - other.y, z - other.z};
    }
    position operator+(position other) const {
        return {x + other.x, y + other.y, z + other.z};
    }
    bool operator==(position other) const {
        return x == other.x && y == other.y && z == other.z;
    }
    [[nodiscard]] position abs() const { return {std::abs(x), std::abs(y), std::abs(z) }; }
    [[nodiscard]] int manhattanDistanceTo(const position& other) const {
        const position& diff = *this-(other);
        return std::abs(diff.x) + std::abs(diff.y) + std::abs(diff.z);
    }
};

struct scanner_reading {
    std::vector<position> readings;
    explicit scanner_reading() : readings(std::vector<position>()) {}
};

axis flip(axis a) {
    switch (a) {
        case x: return neg_x;
        case y: return neg_y;
        case z: return neg_z;
        case neg_x: return x;
        case neg_y: return y;
        case neg_z: return z;
    }
}

void rotate(axis* a, axis* b) {
    axis temp = *a;
    *a = flip(*b);
    *b = temp;
}

struct orientation {
    axis x, y, z;
    orientation(axis x, axis y, axis z) : x(x), y(y), z(z) {}
    orientation(): x(axis::x), y(axis::y), z(axis::z){}

    std::string to_string() {
        return {'(', x, ',', y, ',', z, ')'};
    }
};

std::vector<position> apply(const std::vector<position>& readings, orientation o) {
    std::vector<position> ret;
    ret.reserve(readings.size());
    for(auto pos : readings) {
        ret.emplace_back(pos.get(o.x), pos.get(o.y), pos.get(o.z));
    }
    return std::move(ret);
}

struct fixed_scanner_reading {
    std::vector<position> readings;
    std::vector<position> scannerPositions;
    explicit fixed_scanner_reading(const scanner_reading& original, const orientation o)
        : readings(apply(original.readings, o)), scannerPositions({position(0,0,0)}) {}

    bool tryJoin(const scanner_reading& other, const orientation* allOrientations) {
        for(int o = 0; o < orientationCount; o++) {
            auto rotated = apply(other.readings, allOrientations[o]);
            for(int t = 0; t < rotated.size() - 11; t++) {
                const position candidateT = rotated[t];
                for(int s = 0; s < readings.size(); s++) {
                    const position candidateS = readings[s];
                    const position diff = candidateS - candidateT; // Position of second scanner relative to first
                    int matchingCount = 0;
                    for(auto p : rotated) {
                        if(std::find(readings.begin(), readings.end(), p + diff) != readings.end())
                            matchingCount++;
//                        else {
//                            bool shouldBeKnownButIsnt = false;
//                            for(position center : scannerPositions) {
//                                position scannerDistance = (center - p).abs();
//                                if(scannerDistance.x < 1000 && scannerDistance.y < 1000 && scannerDistance.z < 1000) {
//                                    shouldBeKnownButIsnt = true;
//                                    break;
//                                }
//                            }
//                            if(shouldBeKnownButIsnt) {
//                                matchingCount = -1;
//                                break;
//                            }
//                        }
                    }
                    if(matchingCount < 12) continue;
                    // We have a match!
                    scannerPositions.push_back(diff);
                    for(auto p : rotated) {
                        p = p + diff;
                        if(std::find(readings.begin(), readings.end(), p) == readings.end())
                            readings.push_back(p);
                    }
                    return true;
                }
            }
        }
        return false; // I tried everything :(
    }

    int maxManhattanDistance() {
        int ret = 0;
        for(int a = 0; a < scannerPositions.size() - 1; a++) {
            for(int b = a + 1 ; b < scannerPositions.size(); b++) {
                int dist = scannerPositions[a].manhattanDistanceTo(scannerPositions[b]);
                if(dist > ret) ret = dist;
            }
        }
        return ret;
    }
};

int main() {
    orientation allOrientations[24];
    axis xTemplate = x;
    axis yTemplate = y;
    axis zTemplate = z;
    for(int i = 0; i < 3; i++) {
        axis a = yTemplate;
        axis b = zTemplate;
        for(int j = 0; j < 4; j++) {
            allOrientations[(j + i * 4) * 2] = orientation(xTemplate, a, b);
            allOrientations[(j + i * 4) * 2 + 1] = orientation(flip(xTemplate), flip(a), b);
//            std::cout << allOrientations[(j + i * 4) * 2].to_string() << "\n" << allOrientations[(j + i * 4) * 2+1].to_string() << "\n";
            rotate(&a, &b);
        }

        axis temp = xTemplate;
        xTemplate = yTemplate;
        yTemplate = zTemplate;
        zTemplate = temp;
    }

    std::vector<scanner_reading> allScanners {scanner_reading()};
    {
        std::ifstream file("../input/d19");
        std::string str;
        std::getline(file, str); // Skip first scanner header
        while (std::getline(file, str)) {
            if(str.empty()) {
                allScanners.emplace_back();
                std::getline(file, str); // Skip scanner header
            } else {
                std::stringstream stream(str);
                std::string segment;
                std::getline(stream, segment, ',');
                int x = std::stoi(segment);
                std::getline(stream, segment, ',');
                int y = std::stoi(segment);
                std::getline(stream, segment, ',');
                int z = std::stoi(segment);
                allScanners[allScanners.size() - 1].readings.emplace_back(x, y, z);
            }
        }
    }

    std::cout << "read everything\n";

    fixed_scanner_reading result = fixed_scanner_reading(allScanners[0], orientation(x, y, z));
    std::cout << "fixed first\n";
    allScanners.erase(allScanners.begin());

    while(!allScanners.empty()) {
        scanner_reading current = allScanners[0];
        bool wasSuccessful = result.tryJoin(current, allOrientations);
        allScanners.erase(allScanners.begin());
        if(!wasSuccessful) allScanners.push_back(current);
        std::cout << wasSuccessful << "(" << allScanners.size() << " left)\n";
    }

    std::cout << "Readings: " << result.readings.size() << "\n";
    int dist = result.maxManhattanDistance();
    std::cout << "ManhattanDistance: " << dist << "\n";
}