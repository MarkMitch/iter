# iter
Functional C++ iterator library

Small C++20 iterator library that aims to be simple, composable and easily [extendable](https://github.com/MarkMitch/extend). 

##### Integrates non-intrusively with existing standard library containers and range-based for loops.
```c++
void multiply(std::vector<float> const& x, std::vector<float> const& y, std::vector<float>& z) {
  for (auto [a, b, c] : iter::zip(x, y, z)) {
    c = a * b;
  }
}
```
##### Supports UFCS-style syntax via the [extend](https://github.com/MarkMitch/extend) library. All functions in iter namespace can be called `iter::fun(like, that...)` or `like | iter::fun(_, that...)` or `like |iter::fun| that` (for binary functions) or even `like $(iter::fun) (that...)` (with the help of opt-in macros provided by extend).
```c++
void multiply(std::vector<float> const& a, std::vector<float> const& b, std::vector<float>& c) {
  a | iter::zip(_, b)
    | iter::zip(_, c) // for convenience -- equivalent to: a | iter::zip(_, b, c) 
    | iter::foreach(_, [](auto& abc) {
        auto& [a, b, c] = abc;
        c = a * b; });
}
```
##### Functionality and nomenclature inspired by existing functional iterator libraries in other languages, such as rust, scala. Your previous experience should be transferrable.
```c++
float weighted_sum(std::vector<float> const& a) {
  return a 
    | iter::enumerate(_) 
    | map(_, [](auto ai) { 
        auto& [a, i] = ai;
        return a * i; })
    | sum(_);
}
```
* Still gets autovectorised TODO: Put godbolt link here
