#include <iostream>

std::string read() {
    static std::string inputLine;
    std::getline(std::cin, inputLine, '\n');
    return inputLine;
}

int main() {
    int res = 0;
    int a = std::stoi(read());
    int b = std::stoi(read());
    int c = std::stoi(read());
    for(std::string str = read(); !str.empty(); str = read()) {
        int d = std::stoi(str);
        int last = a+b+c;
        int current = b+c+d;
        a = b;
        b = c;
        c = d;
        if(last < current) res++;
    }
    std::cout << res << "\n";
    return 0;
}
