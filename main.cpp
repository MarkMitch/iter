#include "https://raw.githubusercontent.com/MarkMitch/extend/main/main.cpp"

#include <optional>

namespace iter {
    xtd_function to_iter;
    xtd_function next;
    xtd_function has_next;

    // Random access functions
    xtd_function unsafe_get;
    xtd_function size;

    struct sentinel_t {};

    static constexpr sentinel_t sentinel;

    template<class T>
    concept pointer = std::is_pointer_v<T>;
    template<class T>
    static constexpr bool is_optional = false;
    template<class T>
    constexpr bool is_optional<std::optional<T>> = true;
    template<class T>
    concept optional = is_optional<T>;

    template<class T>
    concept pointer_iter = requires(T it) {
        { iter::next(it) } -> pointer;
    };
    template<class T>
    concept optional_iter = requires(T it) {
        { iter::next(it) } -> optional;
    };
    template<class T>
    concept iter = pointer_iter<T> || optional_iter<T>;
    
    template<class T>
    concept random_access_iter = iter<T> && requires (T it, size_t index) {
        iter::unsafe_get(it, index);
        { iter::size(it) } -> std::same_as<std::size_t>;
    };
    
    template<class T>
    concept pointer_iterable = pointer_iter<T> || requires (T&& it) {
        { iter::to_iter((T&&)it) } -> pointer_iter;
    };
    template<class T>
    concept optional_iterable = optional_iter<T> || requires (T&& it) {
        { iter::to_iter((T&&)it) } -> optional_iter;
    };
    template<class T>
    concept iterable = iter<T> || pointer_iterable<T> || optional_iterable<T>;

    template<class = void>
    static constexpr bool always_false = false;

    template<iter I> 
    struct iterator_traits;

    template<optional_iter I>
    struct iterator_traits<I> {
        using value_type = std::remove_reference_t<decltype(*iter::next(std::declval<I&>()))>;
    };

    template<pointer_iter I>
    struct iterator_traits<I> {
        using value_type = std::remove_reference_t<decltype(*iter::next(std::declval<I&>()))>;
    };

    // C++ style iterator_wrapper wrapper (ewww...)
    template<iter I>
    struct iterator_wrapper : iterator_traits<I> {
        using traits = iterator_traits<I>;
        using typename traits::value_type;

        template<class It>
        explicit iterator_wrapper(It&& it) : it{(It&&)it}
        {
        }

        I it;
        decltype(iter::next(it)) current = {};

        auto operator<=>(const iterator_wrapper&) const {
            static_assert(always_false<I>, "Cannot compare iterators");
        }
        bool operator!=(sentinel_t) {
            if constexpr (optional_iter<I>) {
                current.reset();
                new (&current) decltype(current)(iter::next(it));
                return current.has_value();
            } else {
                return (current = iter::next(it));
            }
        }
        friend auto& operator*(auto& self) {
            return *self.current;
        }
        auto& operator++() {
            return *this;
        }
    };

    template<class T>
    iterator_wrapper(T) -> iterator_wrapper<T>;

    template<class T>
    static constexpr bool is_iterator_v = false;
    template<class I>
    constexpr bool is_iterator_v<iterator_wrapper<I>> = true;

    template<class T>
    concept iterator = is_iterator_v<std::decay_t<T>>; 

    namespace detail {
        template<class T>
        struct force_iter;
        
        template<iterable I>
        struct force_iter<I> {
            using type = decltype(iter::to_iter(std::declval<I>()));
        };

        template<iter I>
        struct force_iter<I> {
            using type = I;
        };
    }

    template<class I>
    using iter_t = typename detail::force_iter<I>::type;

    template<iter I>
    using next_t = decltype(iter::next(std::declval<I&>()));

    namespace detail {
        template<class T>
        struct value_type;

        template<iterator I>
        struct value_type<I> {
            using type = typename I::value_type;
        };
        template<iter I> 
        struct value_type<I> {
            using type = typename iterator_traits<I>::value_type;
        };
        template<iterable I> 
        requires (!iter<I>)
        struct value_type<I> : value_type<iter_t<I>> {};
    }
    
    template<class T>
    using value_t = typename detail::value_type<T>::type;

    namespace detail {
        template<iter I> struct no_next;
        template<pointer_iter I> struct no_next<I> {
            static constexpr value_t<I>* value = nullptr;
        };
        template<optional_iter I> struct no_next<I> {
            static constexpr auto value = std::nullopt;
        };
    }

    template<iter I>
    static constexpr auto no_next = detail::no_next<I>::value;

    template<class Self, class... I>
    struct enable_random_access;
    
    template<class Self, class I>
    requires (!random_access_iter<I>)
    struct enable_random_access<Self, I> {
        static constexpr bool random_access = false;

    protected:
        using this_t = enable_random_access;
        using base_t = enable_random_access;

        template<class T>
        constexpr enable_random_access(T&& in) : i{(T&&)in} {}

        I i;
    };

    template<class Self, random_access_iter I>
    struct enable_random_access<Self, I> {
        static constexpr bool random_access = true;

    protected:
        using this_t = enable_random_access;
        using base_t = enable_random_access;

        template<class T>
        constexpr enable_random_access(T&& in) : i{(T&&)in} {}

        I i;
        size_t index = 0;

        constexpr auto impl_this(iter::size)(this_t const& base) {
            return iter::size(base.i);
        }
        constexpr auto impl_this(iter::next)(this_t& base) {
            auto index = base.index++;
            auto& self = static_cast<Self&>(base);
            auto size = iter::size(self);

            using get_t = decltype(iter::unsafe_get(self, index));
            if constexpr (std::is_lvalue_reference_v<get_t>)
                return (index < size) ? &iter::unsafe_get(self, index) : nullptr;
            else
                return (index < size) ? std::optional(iter::unsafe_get(self, index)) : std::nullopt;
        }
    };

    template<class Self, class... I>
    requires (sizeof...(I) > 1 && (!random_access_iter<I> || ...))
    struct enable_random_access<Self, I...> {
        static constexpr bool random_access = false;
    };

    template<class Self, random_access_iter... I>
    requires (sizeof...(I) > 1)
    struct enable_random_access<Self, I...> {
        static constexpr bool random_access = true;

    protected:
        using this_t = enable_random_access;
        using base_t = enable_random_access;

        size_t index = 0;
        size_t size = 0;

        constexpr auto impl_this(iter::size)(this_t const& base) {
            return base.size;
        }
        constexpr auto impl_this(iter::next)(this_t& base) {
            auto index = base.index++;
            auto& self = static_cast<Self&>(base);
            auto size = iter::size(self);

            using get_t = decltype(iter::unsafe_get(self, index));
            if constexpr (std::is_lvalue_reference_v<get_t>)
                return (index < size) ? &iter::unsafe_get(self, index) : nullptr;
            else
                return (index < size) ? std::optional(iter::unsafe_get(self, index)) : std::nullopt;
        }
    };


    template<iter::iterable T>
    constexpr auto begin(T&& iterable) {
        return iter::iterator_wrapper{iter::to_iter((T&&) iterable)};
    }

    template<iter::iterable T>
    constexpr auto end(T&&) {
        return iter::sentinel;
    }

}

// Make all iters iterable, since they are trivally convertible to iters
// This fulfills the iter::iterable concept which explicitly subsumes 
// the iter::iter concept to simplify overload resolution between iterable and iter.
// Without this specialisation, it would not be safe call iter::to_iter on everything
// matching the iter::iterable concept. In other words: this is tightly coupled with
// the iter::iterable concept -- DO NOT REMOVE.
template<iter::iter I>
constexpr auto impl(iter::to_iter) (I&& iter) -> auto&& {
    return (I&&)iter;
}

namespace iter {
    xtd_function sum;
}

template<iter::iterable I> // TODO GCC trunk can't deal with this
requires std::is_arithmetic_v<iter::value_t<I>>
constexpr auto impl(iter::sum) (I&& iterable) {
    std::remove_const_t<iter::value_t<I>> sum = 0;
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    while (auto val = iter::next(iter)) {
        sum += *val;
    }
    return sum;
}

namespace iter {
    xtd_function filter;

    template<iter I, std::predicate<value_t<I> const&> P>
    struct [[nodiscard]] filter_iter {
        using this_t = filter_iter;

        I i;
        P pred;

        constexpr next_t<I> impl_this(iter::next)(this_t& self) {
            while (auto val = iter::next(self.i)) {
                if (self.pred(*val)) {
                    return val;
                }
            } 

            return no_next<I>;
        }
    };

    template<class I, class P>
    filter_iter(I, P) -> filter_iter<I, P>;
}

template<iter::iterable I, std::predicate<iter::value_t<I> const&> P>
constexpr auto impl(iter::filter) (I&& iterable, P pred) {
    return iter::filter_iter{iter::to_iter((I&&) iterable), std::move(pred)};
}

namespace iter {
    xtd_function map;

    template<class T>
    static constexpr auto&& as_const(T&& in) {
        return (T const&&) in;
    }

    template<iter I, std::invocable<value_t<I> const&> F>
    struct [[nodiscard]] map_iter : enable_random_access<map_iter<I, F>, I> {
        using this_t = map_iter;

        template<class T, class U>
        constexpr map_iter(T&& i, U&& f) 
            : this_t::base_t{(T&&)i}
            , func{(U&&)f} 
        {}

    private:
        F const func;

        constexpr auto impl_this(iter::next)(this_t& self) 
            requires (!this_t::random_access)
        {
            auto val = iter::next(self.i);
            return val ? std::optional(self.func(iter::as_const(*val))) : std::nullopt;
        }

        constexpr auto impl_this(iter::unsafe_get)(this_t const& self, size_t index) 
            requires this_t::random_access
        {
            return self.func(iter::as_const(iter::unsafe_get(self.i, index)));
        }
    };

    template<class I, class F>
    map_iter(I, F) -> map_iter<I, F>;
}

template<iter::iterable I, std::invocable<iter::value_t<I> const&> F>
constexpr auto impl(iter::map) (I&& iterable, F&& func) {
    return iter::map_iter{iter::to_iter((I&&) iterable), (F&&) func};
}


namespace iter {
    xtd_function scan;

    // NOTE: Not random access, because state mutates after each iteration.
    // This alone would most likely trip up any theoretically possible auto-vectorisation.
    // Therefore, not being random access *probably* hasn't impacted performance.
    // If mutation is not needed, then scan shouldn't have been used in the first place!
    template<iter I, class T, std::invocable<T&, value_t<I> const&> F>
    struct [[nodiscard]] scan_iter { 
        using this_t = scan_iter;

        template<class IT, class TT, class FT>
        constexpr scan_iter(IT&& i, TT&& state, FT&& f) 
            : i{(IT&&) i}
            , state{(TT&&) state}
            , func{(FT&&) f} 
        {}

    private:
        I i;
        T state;
        F func;

        constexpr auto impl_this(iter::next)(this_t& self) {
            auto val = iter::next(self.i);
            return val ? std::optional(self.func(self.state, iter::as_const(*val))) : std::nullopt;
        }
    };

    template<class I, class T, class F>
    scan_iter(I, T, F) -> scan_iter<I, std::remove_cvref_t<T>, F>;
}

template<iter::iterable I, class T, std::invocable<std::remove_cvref_t<T>&, iter::value_t<I> const&> F>
constexpr auto impl(iter::scan) (I&& iterable, T&& state, F&& func) {
    return iter::scan_iter{iter::to_iter((I&&) iterable), (T&&) state, (F&&) func};
}

namespace iter {
    xtd_function inspect;
    xtd_function foreach;

    template<class F, class T>
    concept inspector = requires (F func, T const& t) {
        { func(t) } -> std::same_as<void>;
    };

    template<iter I, inspector<value_t<I>> F>
    struct [[nodiscard]] inspect_iter : enable_random_access<inspect_iter<I, F>, I> {
        using this_t = inspect_iter;

        template<class T, class U>
        constexpr inspect_iter(T&& i, U&& f) 
            : this_t::base_t{(T&&)i}
            , func{(U&&)f}
        {}

    private:
        F const func;

        constexpr auto impl_this(iter::next)(this_t& self)
            requires (!this_t::random_access)
        {
            auto val = iter::next(self.i);
            if (val) {
                self.func(*val);
            } 
            return val;
        }

        constexpr decltype(auto) impl_this(iter::unsafe_get)(this_t const& self, size_t index) 
            requires this_t::random_access
        {
            decltype(auto) val = iter::unsafe_get(self.i, index);
            self.func(val);
            return val;
        }
    };

    template<class I, class F>
    inspect_iter(I, F) -> inspect_iter<I, F>;
}

template<iter::iterable I, iter::inspector<iter::value_t<I>> F>
constexpr auto impl(iter::inspect) (I&& iterable, F func) {
    return iter::inspect_iter{iter::to_iter((I&&) iterable), std::move(func)};
}

template<iter::iterable I, iter::inspector<iter::value_t<I>> F>
constexpr auto impl(iter::foreach) (I&& iterable, F func) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    while(auto val = iter::next(iter)) {
        func(*val);
    }
}

namespace iter {
    xtd_function flatmap;

    template<iter I, std::invocable<value_t<I> const&> F>
    requires (iter<iter_t<std::invoke_result_t<F, value_t<I> const&>>>)
    struct [[nodiscard]] flatmap_iter {
        template<class T, class U>
        constexpr flatmap_iter(T&& i, U&& f) : i{(T&&)i}, func{(U&&)f} {}

    private:
        using this_t = flatmap_iter;
        using invoke_result = std::invoke_result_t<F, value_t<I> const&>;
        using inner_iter_t = iter_t<invoke_result>;
        using next_t = decltype(iter::next(std::declval<inner_iter_t&>()));

        I i;
        F func;

        std::optional<inner_iter_t> current = get_current();

        constexpr std::optional<inner_iter_t> get_current() {
            if (auto next = iter::next(i)) {
                if constexpr (std::is_same_v<invoke_result, inner_iter_t>)
                    return func(iter::as_const(*next));
                else 
                    return iter::to_iter(func(iter::as_const(*next)));
            }
            
            return std::nullopt;
        }

        constexpr next_t next() {
            while (true) {
                if (!current) {
                    return no_next<inner_iter_t>;
                }

                if (auto val = iter::next(*current)) {
                    return val;
                } else {
                    current.reset();
                    new (&current) decltype(current)(get_current());
                }
            }
        }

        constexpr auto impl_this(iter::next)(this_t& self) {
            return self.next();
        }
    };

    template<class I, class F>
    flatmap_iter(I, F) -> flatmap_iter<I, F>;
}

template<iter::iterable I, std::invocable<iter::value_t<I> const&> F>
constexpr auto impl(iter::flatmap) (I&& iterable, F&& func) {
    return iter::flatmap_iter{iter::to_iter((I&&) iterable), (F&&)func};
}

namespace iter {
    xtd_function zip;

    namespace detail {
        // Tie only those arguments that are lvalue-references
        template<class... Ts>
        static constexpr std::tuple<Ts...> half_tie(Ts&&... ins) {
            return {(Ts&&)ins...};
        }

        // Simply dereference pointers to avoid copy/move construction 
        // but unwrap optionals into new instances
        template<class T>
        static constexpr decltype(auto) unwrap_next(T&& in) {
            using t = std::decay_t<T>;
            if constexpr (optional<t>) {
                return typename t::value_type(std::move(*in));
            } else if constexpr(pointer<t>) {
                return (*in);
            }
        }
    }

    template<iter... I>
    requires (sizeof...(I) > 1)
    struct [[nodiscard]] zip_iter : enable_random_access<zip_iter<I...>, I...> {
        using this_t = zip_iter;

        template<iter... T>
        requires (sizeof...(T) > 1)
        friend struct zip_iter;

        template<class... Ts> 
        requires (sizeof...(Ts) == sizeof...(I))
        constexpr zip_iter(Ts&&... ins) 
            : i{(Ts&&)ins...} 
        {
            if constexpr (this_t::random_access) {
                this->size = std::apply([](auto&... iters) {
                    return std::min({iter::size(iters)...});
                }, i);
            }
        }

        template<class... Ts, class... Us> 
        constexpr zip_iter(zip_iter<Ts...>&& zi, Us&&... ins) 
            : zip_iter(std::index_sequence_for<Ts...>{}, std::move(zi), (Us&&) ins...)
        {}
        template<class... Ts, class... Us> 
        constexpr zip_iter(const zip_iter<Ts...>& zi, Us&&... ins) 
            : zip_iter(std::index_sequence_for<Ts...>{}, std::move(zi), (Us&&) ins...)
        {}
        
    private:
        template<size_t... Is, class... Ts, class... Us> 
        constexpr zip_iter(std::index_sequence<Is...>, zip_iter<Ts...>&& zi, Us&&... ins) 
            : zip_iter(std::move(std::get<Is>(zi.i))..., std::forward<Us>(ins)...)
        {}
        template<size_t... Is, class... Ts, class... Us> 
        constexpr zip_iter(std::index_sequence<Is...>, const zip_iter<Ts...>& zi, Us&&... ins) 
            : zip_iter(std::get<Is>(zi.i)..., std::forward<Us>(ins)...)
        {}
        
        std::tuple<I...> i;

        constexpr auto impl_this(iter::next)(this_t& self) 
            requires (!this_t::random_access) 
        {
            return std::apply([](auto&... is) {
                return std::invoke([]<class... Ts>(Ts&&... vals)  {
                    return (... && vals) 
                        ? std::optional(detail::half_tie(detail::unwrap_next((Ts&&)vals)...))
                        : std::nullopt;
                }, iter::next(is)...);
            }, self.i);
        }

        constexpr auto impl_this(iter::unsafe_get)(this_t const& self, size_t index) 
            requires this_t::random_access 
        {
            return std::apply([=](auto&... is) {
                return detail::half_tie(iter::unsafe_get(is, index)...);
            }, self.i);
        }
    };

    template<iter... I>
    zip_iter(I...) -> zip_iter<I...>;
    template<iter... ZI, iter... I>
    zip_iter(zip_iter<ZI...>, I...) -> zip_iter<ZI..., I...>;
}

template<iter::iterable... I>
constexpr auto impl(iter::zip) (I&&... iterables) {
    return iter::zip_iter{iter::to_iter((I&&) iterables)...};
}

namespace iter {
    xtd_function to_pointer_iter;

    template<optional_iter I>
    struct [[nodiscard]] to_pointer_iter_iter {
        using this_t = to_pointer_iter_iter;

        I i;
        next_t<I> to_pointer_iter = std::nullopt;

        constexpr auto impl_this(iter::next)(this_t& self) {
            self.to_pointer_iter.reset();
            new (&self.to_pointer_iter) decltype(self.to_pointer_iter)(iter::next(self.i));
            return self.to_pointer_iter ? &*self.to_pointer_iter : nullptr;
        }
    };

    template<optional_iter I>
    to_pointer_iter_iter(I) -> to_pointer_iter_iter<I>;
}

template<iter::iter I> // TODO GCC trunk may not be able to deal with this
constexpr decltype(auto) impl(iter::to_pointer_iter) (I&& iter) {
    if constexpr (iter::pointer_iter<I>) {
        return (I&&)iter;
    } else {
        return iter::to_pointer_iter_iter{(I&&)iter};
    }
}

namespace iter {
    namespace tag {
        template<std::size_t N = 2>
        struct partition : xtd::tagged_bindable<partition<N>> {};
    }

    template<std::size_t N = 2>
    static constexpr tag::partition<N> partition;

    template<std::size_t I>
    struct index_t : index_t<I+1> {
        template<std::size_t J>
        requires (J < I)
        constexpr index_t(index_t<J> j) : index_t<I+1>{j} {}
        constexpr index_t() : index_t<I+1>{I} {}
    protected:
        constexpr index_t(size_t i) : index_t<I+1>{i} {}
    };

    template<>
    struct index_t<12> {
        constexpr std::size_t value() const { return index; }
    protected:
        constexpr index_t(size_t i) : index{i} {}
        std::size_t const index;
    };

    template<std::size_t I>
    static constexpr auto index = index_t<I>{};

    template<std::size_t I>
    struct maximum {
        static constexpr auto values = []<std::size_t... Is>(std::index_sequence<Is...>) {
            return std::array<index_t<I>, I+1>{index_t<Is>{}...};
        }(std::make_index_sequence<I+1>{});
    };
}


template<size_t N, iter::iterable I, class F>
constexpr decltype(auto) impl_tag(iter::tag::partition<N>) (I&& iterable, F&& func) {
    return iter::partition<N>(iter::to_iter((I&&)iterable), (F&&)func);
}

template<size_t N, iter::iter I, class F>
requires (N > 1)
constexpr decltype(auto) impl_tag(iter::tag::partition<N>) (I&& iter, F&& func) {
    using arg_t = decltype(std::as_const(*std::declval<iter::next_t<std::decay_t<I>>>()));

    auto out = std::array<std::vector<iter::value_t<std::decay_t<I>>>, N>{};
    while (auto val = iter::next(iter)) {
        auto slot = std::invoke((F&&) func, std::as_const(*val));
        std::size_t index;
        if constexpr (std::is_same_v<bool, decltype(slot)>) {
            static_assert(N == 2, "Boolean predicate function only permitted with iter::partition<2>.");
            index = slot ? 0 : 1;
        } else {
            static_assert(std::is_same_v<iter::index_t<N-1>, decltype(slot)>, 
                "Function called in iter::partition<N> must return iter::index_t<N-1>.");
            index = slot.value();
        }

        if constexpr (iter::optional_iter<I>)
            out[index].emplace_back(std::move(*val));
        else
            out[index].emplace_back(*val);
    }
    return out;
}

namespace iter {
    xtd_function fold;
    xtd_function reduce;
}

template<iter::iterable I, class T, std::invocable<const T&, iter::value_t<I> const&> F>
constexpr T impl(iter::fold) (I&& iterable, T&& init, F func) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    auto acc = (T&&)init;
    while (auto val = iter::next(iter)) {
        acc = func(iter::as_const(acc), iter::as_const(*val));
    }
    return acc;
}

template<iter::iterable I, std::invocable<iter::value_t<I> const&, iter::value_t<I> const&> F>
constexpr std::optional<iter::value_t<I>> impl(iter::reduce) (I&& iterable, F&& func) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    if constexpr(iter::optional_iter<I>) {
        auto acc = iter::next(iter);
        if (acc) 
            acc.emplace(iter::fold(iter, std::move(*acc), (F&&)func));

        return acc;
    } else {
        if (auto acc = iter::next(iter)) 
            return iter::fold(iter, std::move(*acc), (F&&)func);

        return std::nullopt;
    }
}

#include <vector>

struct Iterable {
    using this_t = Iterable;

    int end;

    struct [[nodiscard]] iterator_wrapper {
        using this_t = iterator_wrapper;

        int i;
        int const end;
        
        constexpr auto impl_this(iter::next)(this_t& self) {
            auto j = self.i++;
            return j < self.end ? std::optional(j) : std::nullopt;
        }
    };

    template<class T>
    constexpr auto impl_this(iter::to_iter)(T&& self) {
        return iterator_wrapper{0, self.end};
    }
};

namespace iter {
    template<class Underlying>
    struct [[nodiscard]] container_to_iter {
        using this_t = container_to_iter;

        template<class... Ts>
        requires (!std::is_lvalue_reference_v<Underlying>)
        container_to_iter(std::in_place_t, Ts&&... ins) 
            : underlying{(Ts&&)ins...}
            , it{std::begin(get_underlying())}
        {}

        template<class... Ts>
        requires (std::is_lvalue_reference_v<Underlying>)
        container_to_iter(std::in_place_t, Underlying& under) 
            : underlying{&under}
            , it{std::begin(get_underlying())}
        {}

        container_to_iter(container_to_iter&& other)
            : underlying{std::move(other.underlying)}
            , it{std::begin(get_underlying())}
        {}
        
        container_to_iter(const container_to_iter& other)
            : underlying{other.underlying}
            , it{std::begin(get_underlying())}
        {}

        container_to_iter& operator=(container_to_iter&& other) {
            underlying = std::move(other.underlying);
            it = std::begin(get_underlying());
            return *this;
        }

        container_to_iter& operator=(const container_to_iter& other) {
            underlying = other.underlying;
            it = std::begin(get_underlying());
            return *this;
        }

    private:
        using underyling_t = std::conditional_t<std::is_lvalue_reference_v<Underlying>, std::remove_reference_t<Underlying>*, Underlying>;
        underyling_t underlying;
        decltype(std::begin(std::declval<std::remove_reference_t<Underlying>&>())) it;

        auto& get_underlying() {
            if constexpr (std::is_lvalue_reference_v<Underlying>) 
                return *underlying;
            else
                return underlying;
        }
        auto& get_underlying() const {
            if constexpr (std::is_lvalue_reference_v<Underlying>) 
                return *underlying;
            else
                return underlying;
        }

        using next_t = decltype(&*it);

        constexpr auto impl_this(iter::unsafe_get)(this_t const& self, size_t index) -> auto& {
            return self.get_underlying()[index];
        }

        constexpr auto impl_this(iter::size)(this_t const& self) {
            return std::size(self.get_underlying());
        }

        constexpr auto impl_this(iter::next)(this_t& self) -> next_t {
            if (self.it != std::end(self.get_underlying())) {
                auto* p = &*self.it;
                ++self.it;
                return p;
            } else {
                return nullptr;
            }
        }
    };
}

#include <array>
#include <vector>

namespace iter {
    namespace detail {
        template<class T>
        static constexpr bool is_array = false;

        template<class T, size_t N>
        constexpr bool is_array<std::array<T, N>> = true;
    }

    template<class T>
    concept array = detail::is_array<std::remove_cvref_t<T>>;
    
    namespace detail {
        template<class T>
        static constexpr bool is_vector = false;

        template<class T, class A>
        constexpr bool is_vector<std::vector<T, A>> = true;
    }

    template<class T>
    concept vector = detail::is_vector<std::remove_cvref_t<T>>;

    template<class T>
    concept container = array<T> || vector<T>;
}

template<iter::container T>
constexpr auto impl(iter::to_iter)(T&& container) {
    return iter::container_to_iter<T>{std::in_place, (T&&)container};
}

namespace iter {
    template<class T>
    struct optional_to_iter {
        using this_t = optional_to_iter;
        std::optional<T> option;
        constexpr auto impl_this(iter::next)(this_t& self) {
            auto r = std::move(self.option);
            self.option.reset();
            return r;
        }
    };

    template<class T>
    optional_to_iter(std::optional<T>) -> optional_to_iter<T>;

    template<class T>
    struct pointer_to_iter {
        using this_t = pointer_to_iter;
    private:
        T* ptr;
        constexpr auto impl_this(iter::next)(this_t& self) {
            auto r = self.ptr;
            self.ptr = nullptr;
            return r;
        }
    };

    template<class T>
    pointer_to_iter(T*) -> pointer_to_iter<T>;
}

template<iter::optional T>
constexpr auto impl(iter::to_iter)(T&& optional) {
    return iter::optional_to_iter{(T&&) optional};
}

namespace iter {
    namespace tag {
        template<template<class...> class C = std::vector, template<class> class A = std::allocator>
        struct collect : xtd::tagged_bindable<collect<C, A>> {};
    }

    template<template<class...> class C = std::vector, template<class> class A = std::allocator>
    static constexpr tag::collect<C, A> collect;
}

template<template<class...> class CT, template<class> class AT, iter::iter I>
constexpr auto impl_tag(iter::tag::collect<CT, AT>)(I&& iter) {
    using T = iter::value_t<I>;
    using A = AT<T>;
    CT<T, A> container;
    if constexpr (iter::random_access_iter<I>)
        container.reserve(iter::size(iter));

    while (auto val = iter::next(iter)) {
        if constexpr (iter::optional_iter<I>)
            container.emplace_back(std::move(*val));
        else
            container.emplace_back(*val);
    }
    return container;
}

template<template<class...> class CT, template<class> class AT, iter::iter I>
constexpr auto impl_tag(iter::tag::collect<CT, AT>)(I&& iter, size_t reserve) {
    using T = iter::value_t<I>;
    using A = AT<T>;
    CT<T, A> container;
    if constexpr (iter::random_access_iter<I>) {
        auto size = iter::size(iter);
        container.reserve(std::max(reserve, size));
    } else {
        container.reserve(reserve);
    }
    while (auto val = iter::next(iter)) {
        if constexpr (iter::optional_iter<I>)
            container.emplace_back(std::move(*val));
        else
            container.emplace_back(*val);
    }
    return container;
}

#include <limits>

namespace iter {
    xtd_function enumerate;
    xtd_function until;

    template<std::integral T = int>
    struct range {
        using this_t = range;
        constexpr range(T begin = 0, T end = std::numeric_limits<T>::max()) : begin{begin}, end{end} {}
    private:
        T begin;
        T const end;
        constexpr auto impl_this(iter::next)(this_t& self) {
            return self.begin < self.end ? std::optional(self.begin++) : std::nullopt;
        }
        constexpr size_t impl_this(iter::size)(this_t const& self) {
            return self.end - self.begin;
        }
        constexpr T impl_this(iter::unsafe_get)(this_t const& self, size_t index) {
            return self.begin + index;
        }
    };

    template<class T>
    range(T) -> range<T>;

    static constexpr struct {} indices;
    constexpr auto impl(iter::to_iter)(decltype(indices)) {
        return range{};
    }
}

template<iter::iterable I>
constexpr auto impl(iter::enumerate)(I&& iterable) {
    return iter::zip(iter::to_iter((I&&)iterable), iter::indices);
}

template<std::integral T>
constexpr auto impl(iter::until)(T begin, T end) {
    return iter::range{begin, end};
}

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
