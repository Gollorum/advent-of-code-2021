#include <iostream>
#include <vector>
#include <algorithm>
#include <bitset>

std::string read() {
    static std::string inputLine;
    std::getline(std::cin, inputLine, '\n');
    return inputLine;
}

const int inputlen = 12;

int main() {
    int count = 0;
    int _[inputlen];
    int* ones = _;
    std::vector<int> allNumbers{};

    for(int i = 0; i < inputlen; i++) ones[i] = 0;
    for(std::string str = read(); !str.empty(); str = read()) {
        int i = std::stoi(str, nullptr, 2);
        allNumbers.push_back(i);
        count++;
        for(int o = 0; o < inputlen; o++) {
            if (i & 1) ones[o]++;
            i = i >> 1;
        }
    }
    auto calc = [](std::vector<int> numbs) {
        int* ons = (int*) malloc(inputlen * sizeof(int));
        for(int o = 0; o < inputlen; o++) ons[o] = 0;
        for(auto i : numbs) {
            for(int o = 0; o < inputlen; o++) {
                if (i & 1) ons[o]++;
                i = i >> 1;
            }
        }
//        std::cout << "Ons: ";
//        for(int o = inputlen - 1; o >= 0; o--) std::cout << ons[o] << ", ";
//        std::cout << "\n";
        return ons;
    };

    std::vector<int> oxN = allNumbers;
    std::vector<int> co2N = allNumbers;

    int ox, co2 = 0;
    for(int o = inputlen - 1; o >= 0; o--) {
        auto ons = calc(oxN);
        auto pred = [o, ons, oxN](int i) {
            int count = oxN.size();
            int b = ((i >> o) & 1);
            if(ons[o] * 2 == count) return b == 0;
            else return b != (ons[o] * 2 >= count);
        };
        oxN.erase(std::remove_if(oxN.begin(), oxN.end(), pred), oxN.end());
        free(ons);
        if(oxN.size() == 1) {
            ox = oxN[0];
            break;
        }
//        std::cout << "Kept ";
//        for(auto i : oxN) std::cout << std::bitset<inputlen>(i) << ", ";
//        std::cout << "\n";
    }
//    std::cout << "Took " << std::bitset<inputlen>(ox) << "\n";
    for(int o = inputlen - 1; o >= 0; o--) {
        auto ons = calc(co2N);
        auto pred = [o, ons, co2N](int i) {
            int count = co2N.size();
            int b = ((i >> o) & 1);
            if(ons[o] * 2 == count) return b == 1;
            else return b == (ons[o] * 2 >= count);
        };
        co2N.erase(std::remove_if(co2N.begin(), co2N.end(), pred), co2N.end());
        free(ons);
        if(co2N.size() == 1) {
            co2 = co2N[0];
            break;
        }
//        std::cout << "Kept ";
//        for(auto i : co2N) std::cout << std::bitset<inputlen>(i) << ", ";
//        std::cout << "\n";
    }
//    std::cout << "Took " << std::bitset<inputlen>(co2) << "\n";
    std::cout << ox << "*" << co2 << "=" << (ox * co2) << "\n";
    return 0;
}
