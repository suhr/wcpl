# Writing a Concatenative Programming Language: Introduction

Yes, this is a yet another “write you a language”. But this one is a little bit different. First, the language I'm going to implement is rather different from languages you used to see in such tutorials. Second, I have little experience in creating programming languages, so a lot of things will be new to me as well.

## Philosophy

What's the point in doing the thing everybody else is doing? What's the point in solving a problem someone else already have solved? Guides show you how to make a yet another Haskell or Python, but if you like these languages so much why don't you just use existing implementations?

I do not like Python. And I always seeked for something that could make programming less annoying. The language I'd like to use doesn't exist, and probably will never exist. But a more pleasant language, in fact, could.

But the language will not write itself. Also, nobody will write such a language: most people prefer to do things everybody else does, and anyway nobody cares about things I care about.

So I build a toy language and hope it will grow into something more useful.

## Freedom from variables

There's more than one way to do it. The should be a way to do it without using any variables.

Most programming languages are extremely bad at programming without variables. For example, in C it is usually just impossible. In Haskell you have to use kludges like `(.).(.)` and end with unreadable code. In APL and J you end with character soup.

But in some languages, programming without variable can result into concise and clean code. Such languages are known as [concatenative](https://en.wikipedia.org/wiki/Concatenative_programming_language).

For example, in [Factor](http://factorcode.org/) you can write something like this:

```factor
"http://factorcode.org" http-get nip string>xml
"a" deep-tags-named
[ "href" attr ] map
[ print ] each
```

Clean [pipeline-like](http://concatenative.org/wiki/view/Pipeline%20style) style.

But concatenative are not perfect either. As shown in [Why Concatenative Programming Matters](https://evincarofautumn.blogspot.com/2012/02/why-concatenative-programming-matters.html), a simple expression `f(x, y, z) = x^2 + y^2 - abs(y)` becomes

`drop dup dup × swap abs rot3 dup × swap − +`

or

`drop [square] [abs] bi − [square] dip +`

But why can't you just write `drop dup (^2) + (^2) - abs` instead?

In Conc, the language I'm going to implement, you can do that. Also, in Conc you can do things you can't do in other languages, like pattern matching without variables. This is achieved with a very simple extention of the concatenative model.

## Motivation

Now you probably have a one big question.

![Why](https://www.linux.org.ru/images/16134/original.jpg){title="Why"}

A good question indeed. Well, reasons are many. I'll show some though.

Suppose, you have some kind of programming notebook, like Jupyter Notebook. In there, you write expressions in a frame to get results below.

So you can type

```
1200.0 * (3.0/2.0 log2)
```

and get

```
701.9550008653874
```

(a pure perfect fifth in [cents]( https://en.wikipedia.org/wiki/Cent_(music) )). Suppose, you want see a chain of pure fifths. Then you write

```
1200.0 * (3.0/2.0 log2) -> fifth

1..inf {* fifth} map
```

```
[701.9550008653874, 1403.9100017307749, ...]
```

How many pure fifths you need to approximate an octave (using octave equivalence)? It isn't hard to find out:

```
1200.0 * (3.0/2.0 log2) -> fifth

1..inf {* fifth} map
{(round `mod` 1200) < 10} take_while
```

(there should be a chain of 53 fifths)

You may consider a tuning made from this chain

```
1200.0 * (3.0/2.0 log2) -> fifth

1..inf {* fifth} map
{(round `mod` 1200) < 10} take_while
sort
```

Maybe it has a great approximation of a pure major third as well?

```
1200.0 * (3.0/2.0 log2) -> fifth

1..inf {* fifth} map
{(round `mod` 1200) < 10} take_while
sort
enumerate {second (id - 386 abs) < 5} find
```

Now you can see a very nice feature of concatenative languages: they are good for interactive coding. There's no need to make unnecessary variables. There's no need to put everthing in dozens of parentheses or write code backwards. You just add more code to get new results.

Everything is an expression. Every expression is a function. The code is concise yet readable. There's no special syntax and the basic syntax is very simple and consistent.

Also, Conc functions are pure. The programming notebook could execute the code every time you move to a new line. It could memoize results to avoid expensive computations. It could use hot swapping technique, like [in Elm](http://elm-lang.org/blog/interactive-programming), allowing you to modify your program while it's running.

The second reason to create a new language is because the concatenative notation makes some things cleaner. Consider the famous Functor and Monad rules:

```
fmap id      = id
fmap (p . q) = (fmap p) . (fmap q)

return a >>= k                  =  k a
m        >>= return             =  m
m        >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
```

In Conc they become:

```
{id} map  ≡ id
{f g} map ≡ {f} map {g} map

return >>= {k}   ≡ k
>>= {return}     ≡ id
>>= {k >>= {h}}  ≡ (>>= {k}) >>= {h}
```	

Not only Conc version is shorter, it also allows you to *see* why the third Monad rule is called “associativity”.

By the way, the concatenative style plays nicely with an effect system:

```
name_do : -> +IO
name_do =
    "What is your first name? " print
    get_line -> first
    "And your last name? " print
    get_line -> last

    first ++ " " ++ last
    "Pleased to meet you, " ++ id ++ "!" print_ln
```

And affine types:

```
Array.new
1 push
2 push
3 push
id !! 1
⇒ Array 2
```

A yet another reason to make a new language is to avoid this:

![Tuples in Scala](https://i.imgur.com/qYcGYgD.png)

It is much easier if you have `,` and functions that can return multiple values.

A yet another reason is the associativity of concatenation. While [arrows](https://www.haskell.org/arrows/syntax.html) in Haskell look nice with *two* outputs, I wonder how they play with *three* outputs.

Consider a simple example:

```
f : a -> b c
g : a -> b
p : a b -> c
q : a -> b
```

You can write:

```
f,g
p,q
```

But you also can write:

```
f,g
q,p
```

You can't reproduce this using only pairs, because

```
T[T[a, b], c] ≠ T[a, T[b, c]]
```

But in Conc `((a, b), c) ≡ (a, (b, c)) ≡ a, b, c`.

## The language

```
data a Maybe = a Some | None

map : a Some (a -> b) -> b Some
map (mb f -- mb) = case 2id:
    Some,id -> apply Some
    None,_  -> None

fizzbuzz (n -- str) =
    -> n
    case (n fizz),(n buzz):
        True, False -> "Fizz"
        False, True -> "Buzz"
        True, True  -> "FizzBuzz"
        _, _        -> n to_string
    where
        fizz = (3 mod) == 0
        buzz = (5 mod) == 0

main : -> +IO
main = 1 101 range {fizzbuzz println} each
```

Ok, I have to explain some things. The first thing you have to know, Conc is an algebra of two operations on functions. The first operation is *generalized composition*. It works like ordinary composition in concatenative languages:

```
add : Int Int -> Int
mul : Int Int -> Int
add mul : Int Int Int -> Int

2 3 5 add mul ⇒ 2 (3 5 add) mul ⇒ 2 8 mul ⇒ 16
```

The second operation is *parallel concatenation*:

```
add,mul : Int Int Int Int -> Int

2 2 3 3 add,mul ⇒ (2 2 add),(3 3 mul) ⇒ 4 9
```

Both operations are monoids:

```
( ) f ≡ f ( ) ≡ f
( ),f ≡ f,( ) ≡ f

(f g) h ≡ f (g h)
(f,g),h ≡ f,(g,h)
```

Now notice that if `c` is a constant (a function that doesn't take any arguments) than composition is equvalent to concatenation:

```
f c ≡ f,c
```

Also, if output arity of `f` is lesser than input arity of `g`,

```
f g ≡ idN,f g
```

where N is the difference between arities.

This implies that the generalized composition is probably too general. This leads us to the concept of the *canonical concatenative form*.

Expression `e1 e2` is in CCF if both `e1` and `e2` are in CCF and the output arity or `e1` equals the input arity of `e2`.

This is our old examples in CCF:

```
2,3,5 id,add mul
2,2,3,3 add,mul
```

Now let's define an infix notation:

```
f `h` g ⇒ f,g h
```

This allows you to write expressions in a usual way:

```
2*2 + 3*3 ⇒ (2,2 (*)),(3,3 (*)) (+) ⇒ 4,9 + ⇒ 13
```

But it also allows you to write expression in an unusual way:

```
2 2 3 3 (*) + (*) ⇒ 2,2,3,3 (*),(*) (+) ⇒ ... ⇒ 13
```

It is also nice to have some sugar:

```
(`h` g) ⇒ idN,g h
(f `h`) ⇒ f,idM h
```

Where `N = max(arity_in(h) - arity_out(g), 0)`.

Now the meaning of  `drop dup (^2) + (^2) - abs` should be crystal clear to you:

```
x y z drop dup (^2) + (^2) - abs ⇒
x,y,y (^2) + (^2) - abs ⇒
((x,2 (^)),(y,2 (^)) (+)),(y abs) (-) ⇒ ...
```

Visually it is more like

```
x,y,y (^2) + (^2) - abs ⇒ (x^2) + (y^2) - (y abs)
```

Simple and intuitive.

### Concatenative pattern matching

Operation `,` is not only about writing nice expressions. It also allows to introduce a concatenative version of pattern matching without variables.

Consider an expression:

```
case foo of
	Some a -> ...
	None   -> ...
```

What does `Some a ->` mean? Well, assuming that `foo = Some bar`, it defines `a = bar` and evaluates expression after `->`. `Some` is a function that constructs a value, and pattern matching *inverts* this function.

In concatenative world, `Some bar` becomes `bar Some` (postfix notation!). If there'd be `unSome: a Maybe -> a`, you could write

```
bar Some unSome ⇒ bar
```

But `unSome` doesn't exist. But what if we reproduce `unSome` behaviour in the place where it makes sense?

```
case (bar Some):
	Some -> ⇒ bar
	None -> ...
```

That's basically the essence of concatenative pattern matching. Together with `id`, type constructors and composition form a [right-cancellative monoid](https://en.wikipedia.org/wiki/Cancellation_property).

What does it have to do with `,`? Well, consider an another example:

```
case foo of
	Cons(Some(x), xs) -> ...
```

How to express this in the concatenative pattern matching? Let's rewrite `Cons(Some(x), xs)` in concatenative:

```
(x Some),xs Cons ⇒ x,xs Some,id Cons
```

The `case` statement becomes:

```
case foo:
	Some,id Cons -> ⇒ x,xs
```

So `,` allow us to do advanced pattern matching without any variables.

### Miscellaneous

#### Cross-cutting statements
Consider a branching statement:

```
if foo:
	bar
else:
	baz
```

If `foo` is a constant, the meaning of this statement is clear. But that if `foo` is a function?

In Conc, statement are cross-cutting. That means that the function inside uses values outside:

```
a b if (==):
	...
⇒
if a == b:
	...
```

#### Stack effect comments

Concatenative notation has many advantages. But it also have some drawbacks.

```
magic = abra cadabra
```

What does this function do?

Raw types are not helpful either:

```
mysterious: Int Int -> Int
```

Clearly, we need something else.

In Forth, people use *stack effect comments* to show what a word does:

```
: SQUARED   ( n -- nsquared )     DUP *  ;
```

Conc has a similar thing too:

```
map (mb f -- mb) = ...
```

But unlike Forth, `(mb f -- mb)` is more than just a comment. The compiler can use it to check the arity, and error if the arity of the stack effect doesn't match the arity of the function.

## A puzzle

Enough about Conc, let's talk about something more abstract. Specifically: concatenative combinators.

[The Theory of Concatenative Combinators](http://tunes.org/~iepos/joy.html) gives a good introduction into the subject. My set of combinators is a little bit different though:

```
  A dup  ⇒ A A
A B swap ⇒ B A
A B drop ⇒ A

{f} {g} comp  ⇒ {f g}
  … {f} apply ⇒ … f
```

This surely isn't a minimal set. But it is a very convenient one. Each combinator correspondes to some fundamental concept.

`dup`, `drop` and `swap` correspond to structural rules. Indeed:

* Contraction rule allows to use a variable twice. And `dup` allows to use a value twice.
* Weaking rule allows to not use a variable. And `drop` allows to not use a value.
* Exchange rule allows to use variables in a different order. And `swap` allows to use values in a different order.

`comp` and `apply` are, of course, a composition and an application.

What can you do with this set of combinators? Well, a lot of things. Let's build the famous fixed-point combinator `y`.

By definition:

```
{f} y ⇒ {f} y f ⇒ {f} y f f ⇒ ...
```

Let's try `rec = dup apply`:

```
{f} rec ⇒ {f} {f} apply ⇒ {f} f
```

Not very interesting. But:

```
{rec} rec ⇒ {rec} rec ⇒ ...
```

It recurses. And if we add some `f`:

```
{rec f} rec ⇒ {rec f} rec f ⇒ {rec f} rec f f ⇒ ...
```

Here we go. Now we only have to construct `{rec f}` from `{f}`:

```
{f} {rec} swap comp ⇒ {rec f}
```

So:

```
y = {dup apply} swap comp dup apply
```

Nice.

But remember that Conc is an algebra of *two* operations. This suggests an additional combinator:

```
{f} {g} conc ⇒ {f,g} conc
```

But this immediately leads to a question...

**Problem #1:** How do you define `,` in an untyped context?

Suppose you defined it somehow. Now let `f: A B → C` be a function that takes two arguments and return only one. You can nest it:

```
f,f f : A B C D → E
f,f,f,f f,f f : A … H → K
```

By analogy, let `g: A → B C`:

```
g g,g : A → B C D E
g g,g g,g,g,g → ...
```

**Problem #2**: define `2y` and `y2` such as

`{f} 2y ⇒ {f,f} 2y f ⇒ {f,f,f,f} 2y f,f f ⇒ ...`

and

`{g} y2 ⇒ g {g,g} y2 ⇒ g g,g {g,g,g,g} y2 ⇒ ...`

## Follow up

In the next post I'll describe and implement the Conc parser.
