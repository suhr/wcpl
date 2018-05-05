title: Writing a Concatenative Programming Language: Type System, Part 1

This post should have been *after* the post about parsing. But there's no post about parsing yet. You may wonder, why.

Well, the reason is simple. I already wrote the parser and started to implement the analysis. But quite immediately I got stuck: I don't quite know to do things I want to have in my language, and also nobody did something like this before. And though a lot of things can be thrown away and then implemented later, some things you can't just omit. The type system is such a thing.

This post is my attempt to solve this problem. I figure out what the type system should look like, and try to *explain* it. And explaining it helps me to *understand* it. And once I understand it well, I will be able to actually implement it.

## Logic, formal and computational

Type systems are defined by a set of inference rules. An inference rule is a rule that takes *premises* and returns a conclusion. An example of such rule:

$$\frac{A, A → B}{B}$$

This is *modus pones,* the equvalent of function application in logic. This rule derives “streets are wet” from “it's raining” and “If it's raining, then streets are wet”.

Inference rules have one significant drawback: they are not directly computable. This is fine when everything is already set in stone, but if things are fluid, you really want being able to run code to experiment.

So instead of inference rules we will use Prolog.

Prolog is a solver of clauses like $u ← p \land q \land … \land t$, which are FOL analog of inference rules. When an inference rule looks like this:

$$\frac{A}{B}$$

We will write something like this:

```
B :- A
```

Of course, it's not that simple. Prolog is more a programming language than theorem solver, so the code inevitably contain workarounds.

## Tier 0: typed concatenative algebra

We start with typing the concatenative algebra, described in [[calg](https://suhr.github.io/papers/calg.html)].

The proper composition is straightforward:

$$\frac{Γ \vdash e_1 : α^{*} → β^{*}\quad Γ \vdash e_2 : β^{*} → γ^{*} }{Γ \vdash  (e_1 ∘ e_2) : α^{*} → γ^{*}}( \mathtt{Comp} )$$

The concatenation is simple as well:

$$\frac{Γ \vdash e_1 : α^{*} → β^{*}\quad Γ \vdash e_2 : ξ^{*} → η^{*} }{Γ \vdash (e_1 \mathbin , e_2) : α^{*} \mathbin , ξ^{*} → β^{*} \mathbin , η^{*}}( \mathtt{Conc} )$$

Note that $α^{ * }$, $β^{ * }$, etc are *type lists*, not individual types.

Primitive rewiring functions have these types:

$$
\begin{align}
\mathtt{id} &: α → α \\
\mathtt{dup} &: α → α\mathbin{,}α \\
\mathtt{drop} &: α → \\
\mathtt{swap} &: α\mathbin{,}β → β\mathbin{,}α
\end{align}
$$

Overall, the Prolog code for typed concatenative algebra looks like this:

```prolog
:- use_module(library(lists)).

% Some notes:
% - `comp` is composition, lists are concatenation
% - We do not use any context, so `ty_chk` takes none

% Rule for composition
ty_chk( ty(comp(E1, E2), fn(A, C)) ) :-
    ty_chk( ty(E1, fn(A, B)) ), ty_chk( ty(E2, fn(B, C)) ).

% Rule for concatenation
ty_chk( ty([ ], fn([ ], [ ])) ).
ty_chk( ty([E1 | E2], fn(AX, BY)) ) :-
    ty_chk( ty(E1, fn(A, B)) ), ty_chk( ty(E2, fn(X, Y)) ),
    append(A, X, AX), append(B, Y, BY).

% Primitive functions
ty_chk( ty(id, fn(X, X)) ).
ty_chk( ty(dup, fn([X], [X, X])) ).
ty_chk( ty(drop, fn([_], [ ])) ).
ty_chk( ty(swap, fn([X, Y], [Y, X])) ).
```

Quite straightforward! Note that this code can both check and infer the type of an expression.

## Tier ½: specifying the typechecking

Instead of relying on Prolog unification, let's detail the process of type checking and type inference.

We need a way to specify type variables. To do so, we'll use indexes. So $\mathtt{swap} : α\mathbin{,}β → β\mathbin{,}α$ becomes $\mathtt{swap} : τ_0\mathbin{,}τ_1 → τ_1\mathbin{,}τ_0$. In Prolog, we write $τ_n$ as `tau(N)`.

When we concatenate types, we need to increment the indices:

$$
\frac{
    \mathtt{drop} : τ_0 →\ \quad \mathtt{id} : τ_0 → τ_0
} {
    (\mathtt{drop} \mathbin{,} \mathtt{id}) : τ_0\mathbin{;}τ_1 → τ_1
}
$$

This is type concatenation with reindexing:

```prolog
% Type concatenation with reindexing
alpha_conc(fn(AX, BY), fn(A, B), fn(X, Y)) :-
    append(A, B, AB), upper_bound(N, AB),
    inc_vars(Xi, X, N), inc_vars(Yi, Y, N),
    append(A, Xi, AX), append(B, Yi, BY).

% Upper bound: N < P for all tau(N)
upper_bound(0, [ ]).
upper_bound(P, [V|X]) :-
    upper_bound(Q, X),
    (
        V = tau(N) ->
            P is max(Q, N + 1)
        ;
            P = Q
    ).

% Increment all tau by N
inc_vars([ ], [ ], _).
inc_vars([Vi|Xi], [V|X], N) :-
    inc_vars(Xi, X, N),
    (
        V = tau(P) ->
            Vi = tau(Q), Q is P + N
        ;
            Vi = V
    ).
```

`X -> Y ; Z` is the Prolog version of “If X, then Y, otherwise Z”.

Function composition requires more sophisticated transformation:

$$
\frac{
	42 : \mathrm{Nat} \quad
	\mathtt{id} : τ_0 → τ_0 \quad
	\mathtt{swap} : τ_0\mathbin{,}τ_1 → τ_1\mathbin{,}τ_0
} {
    (42 \mathbin{,} \mathtt{id}) ∘ \mathtt{swap} : τ_0 → τ_0\mathbin{,} \mathrm{Nat}
} 
$$

In `f g`, first we unify the output type of `f` with the input type of `g`. Then replace type variables of `g` with corresponding type variables of `f`. Also, we need to replace some type variables with concrete types.

This way:

```prolog
% Function composition with unification
unify_comp(fn(Au, Cu), fn(A, B1), fn(B2, C)) :-
    unify(S, B1, B2), backward_s(Sb, S),
    subst(Cu, S, C), subst(Au, Sb, A).

% Correspond variables to values
unify([], [], []).
unify(S, [X|Xs], [Y|Ys]) :-
    X = Y ->
        unify(S, Xs, Ys); 
    S = [u(X, Y)|Sx], X = tau(_) ->
        unify(Sx, Xs, Ys);
    S = [u(X, Y)|Sx], Y = tau(_) ->
        unify(Sx, Xs, Ys).

backward_s([], []).
backward_s(Sb, [u(X, Y)|Sx]) :-
    X = tau(_), Y \= tau(_) ->
        Sb = [u(X, Y)|Sbx],
        backward_s(Sbx, Sx)
    ;
        backward_s(Sb, Sx).

subst([], _, []).
subst([Y|Ys], S, [X|Xs]) :-
    replace(Y, S, X), subst(Ys, S, Xs).

replace(X, [], X).
replace(Y, [E|S], X) :-
    X = tau(_), E = u(Y, X), Y \= tau(_) ->
        true;
    E = u(X, Y) ->
        true
    ;
        replace(Y, S, X).
```

Now we can write the typechecking rules:

```prolog
ty_chk(ty(E, T)) :- primal_ty(E, T).

% Rule for composition
ty_chk( ty(comp(E1, E2), F) ) :-
    ty_chk( ty(E1, F1) ),
    ty_chk( ty(E2, F2) ),
    unify_comp(F, F1, F2).

% Rule for concatenation
ty_chk( ty([ ], fn([ ], [ ])) ).
ty_chk( ty([E1 | E2], F) ) :-
    ty_chk( ty(E1, fn(A, B)) ),
    ty_chk( ty(E2, fn(X, Y)) ),
    alpha_conc(F, fn(A, B), fn(X, Y)).
```

# Tier 1: infering the unknown

Sometimes you cannot to infer the type of an expression immediately. For example, consider this code:

```
fact (n ~ fact) =
    if dup <= 1:
        drop 1
    else:
        dup id * (id - 1 fact)
```

Here, function `fact` refers to itself, so inferring its type recursively will never terminate.

Instead of trying to infer the type, let's assume that

$$\mathtt{fact} : α → β$$

From `id - 1`, we can know, that $α = \mathrm{Integer}$. From `drop 1`, we know that $β = \mathrm{Integer}$. Thus, we know that

$$\mathtt{fact} : \mathrm{Integer} → \mathrm{Integer}$$

Formally, this is the algorithm:

1. Assume that $\mathtt{f} : χ_1 → χ_2$, there $χ_n$ is a *hole*. In Prolog, it is written as `hole(N)`
2. The typechecker returns not only types but also an unification list
3. Propagate holes:
	- When unifying $χ_n$ and $χ_m$, add $χ_n = χ_m$ to the unification list
	- When unifying a hole with a type variable, replace the type variable with the hole
	- When unifying $χ_n$ with a type $η$, add $χ_n = η$ to the unification list, replace the $χ_n$ with $η$

The implementation of this algorithm is straightforward.

## Tier 2: typed generalized composition

TODO

## Tier 3: lol, generics

TODO
