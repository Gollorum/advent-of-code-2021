#include <iostream>

std::string read() {
    static std::string inputLine;
    std::getline(std::cin, inputLine, '\n');
    return inputLine;
}

std::string forwardKey = "forward ";
std::string upKey = "up ";
std::string downKey = "down ";

int main() {
    int w = 0;
    int h = 0;
    int aim = 0;
    for(std::string str = read(); !str.empty(); str = read()) {
        if(str.rfind(forwardKey, 0) == 0) {
            int fwd = std::stoi(str.substr(forwardKey.length(), str.length() - forwardKey.length()));
            w += fwd;
            h += aim * fwd;
        } else if(str.rfind(upKey, 0) == 0) {
            aim -= std::stoi(str.substr(upKey.length(), str.length() - upKey.length()));
        } else if(str.rfind(downKey, 0) == 0) {
            aim += std::stoi(str.substr(downKey.length(), str.length() - downKey.length()));
        }
    }
    std::cout << (w * h) << "\n";
    return 0;
}
