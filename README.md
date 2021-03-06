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
##### Supports UFCS-style syntax via the [extend](https://github.com/MarkMitch/extend) library. All functions in iter namespace can be called `iter::fun(like, that...)` or `like | iter::fun(_, that...)` or `like |iter::fun| that` (for binary functions) or even `like $(iter::fun) (that...)` (using macros provided by extend).
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

## But, does auto-vectorization still work?
[Yes](https://godbolt.org/#g:!((g:!((g:!((h:codeEditor,i:(fontScale:14,j:1,lang:c%2B%2B,selection:(endColumn:74,endLineNumber:30,positionColumn:74,positionLineNumber:30,selectionStartColumn:74,selectionStartLineNumber:30,startColumn:74,startLineNumber:30),source:'%23include+%22https://raw.githubusercontent.com/MarkMitch/extend/1cef27069323e7c8b9f953d9b9c86765f73da697/main.cpp%22%0A%23include+%22https://raw.githubusercontent.com/MarkMitch/iter/bbc22a590e7ab102a918860a0c13025102118454/main.cpp%22%0A%0Ausing+namespace+xtd::literals%3B%0Ausing+namespace+iter%3B%0A%0A%0Avoid+multiply(std::array%3Cfloat,+64%3E+const%26+x,+std::array%3Cfloat,+64%3E+const%26+y,+std::array%3Cfloat,+64%3E%26+z)+%7B%0A++for+(auto+%5Ba,+b,+c%5D+:+zip(x,+y,+z))+%7B%0A++++c+%3D+a+*+b%3B%0A++%7D%0A%7D%0A%0A//+Demonstrating+infix+notation+(since+all+calls+are+to+binary+functions)%0Avoid+multiply2(std::array%3Cfloat,+64%3E+const%26+a,+std::array%3Cfloat,+64%3E+const%26+b,+std::array%3Cfloat,+64%3E%26+c)+%7B%0A++a+%7Czip%7C+b+%0A++++%7Czip%7C+c%0A++++%7Cforeach%7C+xtd::apply(%5B%5D(auto%26+a,+auto%26+b,+auto%26+c)+%7B%0A++++++++c+%3D+a+*+b%3B+%7D)%3B%0A%7D%0A%0A//+Demonstrating+dollar+syntax+from+macro+in+extend+library%0Afloat+weighted_sum(std::array%3Cfloat,+64%3E+const%26+a)+%7B%0A++return+a+%0A++++$(enumerate)+()%0A++++$(map)+(%5B%5D(auto+ai)+%7B+%0A++++++++auto%26+%5Ba,+i%5D+%3D+ai%3B%0A++++++++return+a+*+i%3B+%7D)%0A++++$(fold)+(0.f,+%5B%5D(auto+acc,+auto+a)+%7B%0A++++++++return+acc+%2B+a%3B+%7D)%3B+//+even+works+with+fold+(aka+sum+in+this+case)%0A%7D'),l:'5',n:'0',o:'C%2B%2B+source+%231',t:'0')),k:49.33855526544822,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((g:!((h:compiler,i:(compiler:g102,filters:(b:'0',binary:'1',commentOnly:'0',demangle:'0',directives:'0',execute:'1',intel:'0',libraryCode:'0',trim:'1'),fontScale:14,j:1,lang:c%2B%2B,libs:!((name:boost,ver:'175')),options:'-std%3Dc%2B%2B20+-Ofast',selection:(endColumn:1,endLineNumber:1,positionColumn:1,positionLineNumber:1,selectionStartColumn:1,selectionStartLineNumber:1,startColumn:1,startLineNumber:1),source:1),l:'5',n:'0',o:'x86-64+gcc+10.2+(Editor+%231,+Compiler+%231)+C%2B%2B',t:'0')),k:50.66144473455179,l:'4',m:50,n:'0',o:'',s:0,t:'0'),(g:!((h:output,i:(compiler:1,editor:1,fontScale:14,wrap:'1'),l:'5',n:'0',o:'%231+with+x86-64+gcc+10.2',t:'0')),header:(),l:'4',m:50,n:'0',o:'',s:0,t:'0')),k:50.66144473455179,l:'3',n:'0',o:'',t:'0')),l:'2',n:'0',o:'',t:'0')),version:4).
