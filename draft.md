
# Tuple and Variant as C++ native types

## Abstract
This abstract proposes language-supported versions of tuples and variants with corresponding literal forms

## Overview
Since braced initializers are not understood by the type system, many C++ idioms don't work as they do with other expressions
```c++
struct X {
    X(int i, vector<string> const &vi) : i(i), vs(vs) {}
    int i;
    vector<string> vs;
};
int a = 1, b = 2;

// Braced-initializers don't work with perfect forwarding
X x(3, { 4, "foo"});                       // OK
auto ux = make_unique<X>(3, { 4, "foo" }); // **Ill-formed**

// Braced-initializers don't work with return type deduction
auto f1() { return 3; }        // Ok
auto f2() { return {1, 2.3};}  // ** Ill-formed **

// Braced initializers don't work with deduction
map arguments = { // ** ill-formed **. See extended example below
    {Stage::Devel,      {DevDB, DBMode::Debug}}, 
    {Stage::Staging,    {StagingDB, DBMode::Audit}}, 
    {Stage::Production, {ProductionDB, DBMode::Performance}} 
};

// Braced initializers don't work with structured bindings
auto const &[x, y] = {a, b}; // ill-formed

// Braced initializers don't work as thread arguments
void f(int, vector<string>);
f(2, {100'000, "bar"}); // OK
thread t(f, 2, { 100'000, "bar"s }); // **Ill-formed**
// Note: The following does not workaround because construction occurs in main thread
thread t(f, 2, vector(100'000, "bar"s)); 
```
We propose that braced initializer lists have a new type denoted `<type1, type2,...>`.  For example, we propose that the type of `{4, "foo"}` be `<int, char const *>>`. 

Now, all of the ill-formed lines above would be legal and work as expected:
```c++
auto ux = make_unique<X>(3, { 4, "foo" }); // OK
auto f2() { return {1, 2.3};}  // OK. Returns <int, double>
map arguments = {              // OK. map<Stage, <DataBase, DBMode>>
     {Stage::Devel,      {DevDB, DBMode::Debug}}, 
     {Stage::Staging,    {StagingDB, DBMode::Audit}}, 
     {Stage::Production, {ProductionDB, DBMode::Performance}} 
};
auto const &[x, y] = {a, b};         // OK
thread t(f, 2, { 100'000, "bar"s }); // OK
```


We also note that this notation also provides C++ with a good native typelist. While C++ template code currently often uses `std::tuple` for typelists, this is problematic not just because the notation is cumbersome, but `std::tuple` is a very complex type that puts a lot of irrelevant stress on the compiler, which is an important issue for complex templates. 

## Native tuples and packs
We propose that all of the notation from P1858 work with native tuples. For example `{1, 2.3}.<1>` would be 2.3. We believe native tuples work very naturally with parameter packs as in the following example. 

```c++
void populateDB(DatabaseHandle, Mode);
enum class Stage { Devel, Staging, Production };
ostream &operator<<(ostream &os, Stage s) { /*...*/}
enum class DBMode { Debug, Audit, Performance };

map arguments = { {Stage::Devel,      {DevDB, DBMode::Debug} },
                  {Stage::Staging,    {StagingDB, DBMode::Audit}},
                  {Stage::Production, {ProductionDB, DBMode::Performance}} };

int main()
{
    populateDB(arguments[currentStage()].<:>...);
    cout << format("Populated DB for {}\n", arguments[currentStage()].<Stage>());
}
```
## What about designated initializers?
Designate initializers conveniently give names to the members of the native tuple making them a nice way to build aggregates. For example, 
```c++
auto x = {.a = 1, .b = "foo"};
cout << x.a << x.<0> << x.<int>; // prints 111

template<typename T> 
concept hasDoubleB() = requires(T x) { {x.b} -> convertible_to<double>; };
void f(hasDoubleB auto x) { cout << sqrt(x.b); }
f(x);            // OK
f({.b = 2.2});   // OK
```

If `x` is `{.a = 1, .b = "foo"}` as above, then we represent the type of `x` as `<.a = int, .b = char const *>`.

## Are native tuples a building block for anonymous classes?

No. While we are not opposed to having full-blown anonymous classes in C++, native tuples should behave somewhat differently and should not be conflated. For example, in the following, let us pretend that a class descriptor omitting the name was a notation for an anonymous class
```c++
// OK. Same kinds of tuples
static_assert(is_same_v< <.a = int, .b = double>, <.a = int, .b = double> >); 
// Ill-formed. Having the same members does not make classes the same
static_assert(is_same_v< class { int  a, double b}, class { int a, double b}>); 
```
We really have no choice in the above because making two occurrences of `<int, double>` different types would make native tuples useless as a typelist and equality of class definitions would quickly become impossible once methods etc. are added. 

Tuples are really their own thing, and trying to turn them into full-blown anonymous classes seems misguided to us. For this reason, we considered but did not prefer a more "class like" notation for the type of `x` as `<int a; char const *b>`. 

## Should empty native tuples be possible
Absolutely. Consider the following use case. It is natural to want to pass variable arguments to a logger and get a `std::source_location`, but that is impossible in C++20 (thanks to Matt Godbolt for pointing out)
```c++
// Ill-formed: Nothing allowed after variadic argument
template<typename... Ts> 
void log(string_view msg, Ts... args, source_location sl = source_location::current())
{   cerr << sl << ": " << format(msg, args...) }

log("Message without args");     // We wish
log("foo {}, bar {}", foo, bar); // We wish
```
As long as native tuples can be empty, we can solve this as follows
```c++
template<typename... Ts> 
void log(string_view msg, <Ts const &...> args, source_location sl = source_location::current())
{   cerr << sl << ": " << format(msg, args.[:]...) } // OK

log("Message without args");     // OK
log("foo {}, bar {}", {foo, bar}); // OK
```
As another example, consider the following
```c++
template<typename DataStruct, typename ...LocksProtectingDataStruct>
void process(DataStruct ds, LocksProtectingDataStruct &...l)
{
  /* ... */
  // Once preparatory calculations are done, lock if necessary
  scoped_lock sl(l...);
  for(x : ds) {
    // Do stuff with x
  }
}
 
vector sharedVec = {1, 2, 3};
mutex m;
 
void f()
{
  // Shared data structure. process() should lock
  process(sharedVec, m);
  // No concurrency. process() should not lock
  vector myVec = {2, 4, 6};
  process(myVec);
}
 
```
More generally, a quick look at ACTCD19 shows almost 1000 uses of `std::tuple<>`, so not allowing the empty tuple type `<>` would prevent the use of native tuples for such code. 

## Rationale for native variants
**Alex to fill in**

#Alex' text: to be merged
## Tuple

### Declaration and initialization
A native tuple shall behave as a type pack and it shall be represented by a comma delimited list of types surrounded by angled brackets. 
```c++
<A,B,C> => identifies a triple-type pack containing types A,B and C
<A> => identifies a single-type pack. Same as 'A'

std::tuple<A,B,C> t; //This is a STD tuple object
<A,B,C> t; //t is a native tuple
```

A native tuple shall exhibit the same behavior as its `std::tuple` counterpart, with the added exception that it can be expanded into individual types via the ellipsis operator `...`. The pack expansion shall be indicated as follows:
```c++
<A,B,C> t; //tuple declaration
<A,B,C>... => expanded types
t... => expanded values
```

### Usage as variadic class member
A generic notation for the tuple in a variadic template is `<T...>`.
```c++
//template class with a default pack
template <typename...T = <A,B>>
class MyTuple {
    T...   _p; //unpacks the types
    <T...> _t; //native tuple
};
```

### Return a tuple from a function
This is one of the most useful and common situations.
```c++
//Return a triple-type pack
<A,B,C> foo()
{
    A a; B b; C c;
    //set locals to something...
    return {a,b,c}; //returns 3 values
    return {a,b}; //return 2 values. 3rd value is default-initialized
}

//extract values via structured bindings
auto [a,b,c] = foo(); //extracts all 3 values from tuple
auto [a,b] = foo(); //extracts only the first 2 values
auto [...p] = foo(); //p is a named pack.
auto [a, ...rest] = foo(); //a binds to A, rest is a <B, C> pack
auto t = foo(); //p is a native tuple
<A,B,C> t = foo(); //explicitly declared. same as above
auto& [ra,rb,rc] = p...; //bind references from the expanded named pack
auto& [ra,rb,rc] = t; //bind to references from the native tuple (no expansion necessary)

//pass pack as input to a function
void bar(A, B, C, D, E);

//First 3 parameters of bar() can be extracted via pack expansion
bar(foo()..., D{}, E{}); 
bar(A{}, foo()..., E{}); //ill-formed. wrong-types unless B,C,D is convertible from A,B,C.
```

### Native tuple as function parameter
Native tuples can also be passed as function parameters. In this case, no expansion is necessary
```c++
<A,B,C> foo();
void bar(const <A,B,C>&, D, E);

bar(foo(), D{}, E{}); //pass return of foo() directly
```

### Type and value accessor 
The following document proposes  `T<size_t>` syntax which behaves similarly to `std::tuple_element<I,E>` for compile-time and `std::get<>` for run-time. 

If `T` is a native tuple, then `T<I>` represents the I-th type. If `t` is a tuple variable, then `p<I>` represents the I-th value in the tuple. The current indexing method shall also be applied to packs with a small variation as we shall see below.

_Note: for value indexing, it's debatable if_ `p.<I>` _is a better form?? The dot notation indicates member access which could be more consistent, however array indexing notation does not and this can also be seen as a form of array indexing._

```c++
<A,B,C> t; 
<A,B,C><0> a; //decltype(a) == A
decltype(t)<0> a; //ok. same as above
a = t<0>; //assign to 'a' the first value contained in t.
a = t.<0>; //using alternative form??
```
Example using a template function expecting a tuple as a single parameter. The requirement is explicit (1st version) or implicit (2nd version)
```c++
template <typename T> 
requires (sizeof...(T) >= 2)
class MyTuple
{
    T<0> _first;
    T<1> _second;
    T _all; //entire tuple
};

MyTuple<A,B> t1; //ok. t._first is defined as type 'A'
MyTuple<A> t2; //ill-formed. fails size requirement
```
Alternative proposed notation (2nd version)
```c++
//Expects T to be a tuple
template <typename <T...>>
class MyTuple; 
```

Contrary to a native tuple, pack indexing shall require the ellipsis expansion before being indexed.
```c++
template <typename ...T> 
requires (sizeof...(T) >= 2)
class MyPack
{
    T...<0> _first;
    T...<1> _second;
    T... _all; //unpacked types
}
```

Example using both classes.
```c++
MyTuple<<A,B,C>> t; //T == <A,B,C>
MyPack<A,B,C> p; //T... == A,B,C types
decltype(t._first) == decltype(p._first);
decltype(t._all...) == decltype(p._all);
```

### Sub-tuple (aka sub-packs)
Sub-tuple indexing can also be extracted with a range notation. 
`<5>` => represents the 5th type in the tuple or pack
`<2:4>` => represents a sub-tuple composed of types in the range [2,4].
`<2:>` => represents a sub-tuple starting comprised of all types starting at position 2 to the end.
`<:2>` => represents a sub-tuple of types up to the 2nd type inclusively. [0,2].
`<-2>` => represents the 2nd type from the end

Example:
```c++
<A,B,C,D,E,F,G> foo(); //returns a 7-type tuple

//using native tuples
void bar(C,D,E); //takes 3 arguments
void bar(<C,D,E>); //overload: takes a single tuple argument
auto t = foo(); //t is a tuple
bar(t<2:4>...); //call bar with an expanded sub-tuple of foo()
bar(t<2:4>); //calls single tuple overload

//using packs (more verbose than using native tuples)
auto [...p] = foo(); //p is a pack
bar(p...<2:4>...); //call using expanded sub-packs

//using MyTuple example from above
MyTuple<<A,B,C,D,E>> myTuple;
bar(myTuple._all<2:4>...); //unpack sub-tuple from MyTuple::_all 
MyPack<A,B,C,D,E> myPack;
bar(myPack._all...<2:4>...); //unpack sub-pack from MyTuple::_all 
```
**Note that <:> represents a pack-ification when used on packs, whereas when applied to tuples, it simply identifies a sub-tuple**.

### sizeof() operator
The sizeof operator shall apply to native tuples in the same way as it applies to packs
```c++
<A, B, C> t;
sizeof(t) == sizeof(A) + sizeof(B) + sizeof(C);
sizeof...(t) == 3
```
Similar for sub-packs/sub-tuples
```c++
<A, B, C> t;
sizeof...(t<0:1>) == 2;
```

### packname
`packname` is used to identify a pack when used within a template function or class. `packname` plays a similar role to `typename` in the sense that it declares a dependent type to be a pack. 
```c++
template <typename...T>
struct MyTuple
{
    using ...Types = T; //alias a pack
    Types<0> _first
    Types... _all; 
};

template <typename T>
void foo1(const T& arg) 
{
}
template <typename T>
void foo2(const T& arg) 
{
    T::Types<0> p; //ill-formed. 'Types' is not known to be a pack. 
    packname T::Types<0> p; //Ok
}
template <typename T>
void foo3(const T& arg) 
{
    T<0> p; //Ok only if T is a pack or a variant, otherwise ill-formed
}
template <typename ...T>
void foo4(T&&... args) 
{
}
//specializations of foo1() for packs (note the difference with foo4())
template <typename T>
void foo1<...T>(const T& arg); //1st version
template <packname T>
void foo1(const T& arg); //allow 'packname' in template defintion - 2nd version

MyTuple<A,B,C> myTuple;
foo1(myTuple); //ok. T == MyTuple<A,B,C>
foo2(myTuple); //ok. T == MyTuple<A,B,C> and decltype(p) == A
foo3(myTuple._all); //Ok. T deduces to a <A,B,C> pack and decltype(p) == A.
foo4(myTuple); //Ok. ...T deduces to MyType<A,B,C> similar to foo1();
foo4(myTuple._all...); //Ok. T expands to 3 types (unpacked)
```

### Accessing via named types
`std::get<>` allows indexing by type. This is useful for variants where types are unique, but for tuples this makes little or no sense and can lead to ambiguity. This is where named types come to the rescue. Consider the following:
```c++
<int,int,int> getUserIds(); 
auto i = getUserIds()<int>; //ambiguous...which int?
auto i = getUserIds()<0>; //hard to read at call site especially if the function declaration is not nearby.

//name the types
<int ssn, int driverLicenseId, int employeeId> getUserIds();
auto i = getUserIds()<ssn>; //access the first type in the pack. Easy to read and understand
```

## Variant
Propose to use double-angled brackets `<<...>>` for identifying the variant types. The internal implementation of the variant is compiler-dependent. The `<<...>>` variant can be used in all the places where `std::variant<>` is, including the visitor pattern for extracting values.
An empty variant is represented by `<<>>` and is ill-formed, except in the context of the `typeid` operator below.

### Declaration
```c++
<<A,B,C>> v; //uninitialized variant
A a;
<<A,B,C>> v{a}; //variant is initialized to type 'A'. Index == 0
<<A>> v; //equivalent to 'A' (compiler may optimize)
<<>> v; //ill-formed. Empty type set.
```
**Contrary to a native tuple, a native variant cannot be expanded via ellipsis operator.**

### Return from a function
```c++
<<A,B,C>> foo(); //function which returns a variant
auto v = foo(); //decltype(v) == <<A,B,C>>
```

### Indexing by type or position
This replaces `std::get<>` for a variant.
```c++
<<A,B,C>> foo(); //function which returns a variant
A a = foo()<A>;
A a = foo()<0>; //same as above
```

### Accessing via named types
```c++
<<A ssn, B userName, C address>> getInfo();
auto b = getInfo()<userName>; //may throw if getInfo() returns a variant with a different index
```

### indexof() 
New vocabulary type. Allows to get the index of a variant.  Returns `-1` if variant is unset.
```c++
<<A,B,C>> v = B{};
indexof(v) == 1;
<<A,B,C>> unset;
indexof(unset) == -1;
```

### typeof()
New vocabulary type. Returns the `std::type_info` belonging to the index type. If the variant is unset, `typeof()` shall return `typeid(<<>>)` which represents an empty variant. 
```c++
<<A,B,C>> v = B{};
typeid(v) == typeid(<<A,B,C>>); //true
typeof(v) == typeid(B); //true

<<A,B,C>> unset;
typeid(unset) == typeid(v); //true
typeof(unset) != typeof(v); //false

```

### <T*> accessor via pointer - replaces std::get_if<>
The following syntax replaces `std::get_if` for native variant types.
```c++
<<A,B,C>> v = B{};
B& b = v<B>; //Ok v is set to B
A& a = v<A>; //throws std::bad_variant_access;
A* a = v<A*>; //returns nullptr

//An unset variant shall always return nullptr
<<A,B,C>> unset;
unset<A*> == unset<B*> == unset<C*> == nullptr;
```

