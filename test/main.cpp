#include "iter.hpp"
#include "extend_dollar.hpp"

#include <string>
#define L(...) (__VA_OPT__([&](auto&& _) -> decltype(auto) { return __VA_ARGS__; }))

float getsum(const std::array<float, 64>& a, const std::array<float, 64>& b, std::array<float, 64>& c) {
    using namespace xtd::literals;

    // float sum = 0;
    // // for (auto [a, i] : iter::zip(a, iter::indices) ) {
    // //     sum += a * i;
    // // }
    // // return sum;
    // for (int i = 0; i < 64; ++i) {
    //     sum += a[i] * i;
    // }
    // return sum;

    using namespace iter;
        // $$$ foreach(_, xtd::apply([](auto& a, auto& b, auto& c) {
        //     c = a * b; }));
    // float acc = 0;
    // for (int i = 0; i < 64; ++i) {
    //     acc += a[i] + b[i] * i;
    // }
    // return acc;
    return a
        // $(to_iter) ()
        // $(map) (L(_ * 2))
        $(map) (L(_ * 2))
        $(map) (L(_ < 3 ? _ : 0))
        // $(zip) (b)
        // $(filter) ([](auto& ab) {
        //     auto& [a, b] = ab;
        //     return a < 3; })
        // $(enumerate) ()
        $(sum) ();

    // float sum = 0;
    // for (int i = 0; i < std::size(a); ++i) {
    //     sum += a[i] + b[i];
    // }
    // return sum;
}

// int getsum(const std::vector<std::string>& v) {
//     using namespace xtd::literals;

//     auto i1 = v
//         $(iter::filter)  (L(_.length() > 4));

//     auto i2 = i1
//         $(iter::map)     (L(_ + _))
//         $(iter::flatmap) (L(_.length() > 4 ? std::optional(_) : std::nullopt));

//     auto [sum, count] = iter::zip(i1, i2)
//         // | iter::flatmap(_, [](auto& i) {
//         //     return i % 3 != 0 ? std::optional(i) : std::nullopt; })
//         // | iter::map(_, [](auto& s) {
//         //     return s + s; })
//         // | iter::flatmap(_, [](auto i) {
//         //     return std::array<int, 2>{i, i}; })
//         | iter::fold(_, std::tuple(0, 0), [](auto acc, auto& b) {
//             auto& [sum, count] = acc;
//             auto& [l, r] = b;
//             std::cout << "l: " << l << ", r: " << r << "\n";
//             return std::tuple(sum + l.length() + r.length(), count + 2); });
//     return sum/count;
//         // | iter::collect<std::vector>(_);

//     // int sum = 0;
//     // for (auto i : container) {
//     //     sum += i;
//     // }
//     // return sum;
// }

// #include <ranges>

// int getsum2(const std::vector<std::string>& v) {
//     using namespace xtd::literals;

//     int sum = 0, count = 0;
//     for (const auto& s : v
//             | std::views::filter([](auto& s) {
//                 return s.length() > 4; })
//             | std::views::transform([](auto& s) {
//                 return s + s; })
//                 ) {
//         sum += s.length();
//         count += 1;
//     }

//     return sum/count;

//     // int sum = 0;
//     // for (auto i : container) {
//     //     sum += i;
//     // }
//     // return sum;
// }

#include <iostream>
#include <map>

int main() {
    using namespace iter;
    using namespace xtd::literals;

    auto collected = 0 |until| 10
      |map| [](auto i) {
        return std::pair(i, -i); }
      |collect<std::map>
        ();

    for (auto [k, v] : collected) {
        std::cout << k << ", " << v << "\n";
    }
}