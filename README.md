# iter
Functional C++ iterator library

Small C++20 iterator library that aims to be simple, composable and easily [extendable](https://github.com/MarkMitch/extend). 

* Integrates non-intrusively with existing standard library containers and range-based for loops.
```
void multiply(std::vector<float> const& a, std::vector<float> const& b, std::vector<float>& c)
  for (auto [x, y, z] : iter::zip(a, b, c)) {
    c = a * b;
  }
}
```
* Supports infix notation
```
void multiply(std::vector<float> const& a, std::vector<float> const& b, std::vector<float>& c)
  a | iter::zip(_, b)
    | iter::zip(_, c) // for convenience, equivalent to a | iter::zip(_, b, c) 
    | iter::foreach(_, [](auto& abc) {
        auto& [a, b, c] = abc;
        c = a * b; });
}
```
* Functionality inspired by existing iterator libraries in other languages, such as rust, scala. 
```
float weighted_sum(std::vector<float> const& a)
  return a 
    | iter::enumerate(_) 
    | map(_, [](auto ai) { 
        auto& [a, i] = ai;
        return a * i; })
    | sum(_);
}
```
* Still gets autovectorised TODO: Put godbolt link here
