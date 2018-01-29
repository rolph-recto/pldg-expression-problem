---
title: The Expression Problem
subtitle: A Cornell PLDG Tutorial
author:  Rolph Recto and Josh Acay
date: January 31, 2018

colortheme: whale
fontsize: 9pt
---

# The Problem

## The Problem

![Phil Wadler](wadler.gif){ width=50% }

Coined by Phil Wadler in a mailing list discussion


## The Problem

```
Date: Thu, 12 Nov 1998 14:27:55 -0500
From: Philip Wadler <wadler@research.bell-labs.com>

                The Expression Problem
             Philip Wadler, 12 November 1998

The Expression Problem is a new name for an old problem.
The goal is to define a datatype by cases, where one can
add new cases to the datatype and new functions over the
datatype, without recompiling existing code, and while
retaining static type safety (e.g., no casts).
```

## The Problem

Wadler's criteria for solutions:

* must be able to simultaneously add new operations and new variants

* cannot modifying existing code (e.g. you don't own the source code)

* type-safe


## An Example

**Scenario**: You are a software engineer at BFC (Big Friendly Corporation) and
your application needs a way to manipulate arithmetic expressions. You use a
library to support this feature. *You have no access to the library's source
code.*

. . .

###

**Datatype**: `EXPR ::= n | EXPR + EXPR

###

**Operations**: `eval`, `print`


## Haskell Implementation

Source code of library in Haskell:

```Haskell
data Expr
  = Lit Int
  | Add Expr Expr

eval :: Expr -> Int
eval (Lit val)   = val
eval (Add e1 e2) = (eval e1) + (eval e2)

print :: Expr -> String
print (Lit val)   = show val
print (Add e1 e2) = (print e1) ++ " + " ++ (print e2)
```


## Java Implementation

Source code of library in Java:

```Java
interface Expr {
  int eval();
  String print();
}

class Lit implements Expr {
  int val;
  Lit(int v) { val = v; }

  int eval() { return val; }
  String print() {
    return Integer.toString(val);
  }
}

class Add implements Expr {
  Expr e1, e2;
  Add(Expr arg1, Expr arg2) { e1 = arg1; e2 = arg2; }

  int eval() { return arg1.eval() + arg2.eval(); }
  String print() {
    return arg1.print() + " + " + arg2.print();
  }
}
```


## Java Implementation

```Java
class Add implements Expr {
  Expr e1, e2;
  Add(Expr arg1, Expr arg2) { e1 = arg1; e2 = arg2; }

  int eval() { return arg1.eval() + arg2.eval(); }
  String print() {
    return arg1.print() + " + " + arg2.print();
  }
}
```


## Adding New Operations

Suppose you want to add a new operation to compute size of expressions...

. . .

In **Haskell**: Easy! Just create a new function:

```Haskell
size :: Expr -> Int
size (Lit x)     = 1
size (Add e1 e2) = 1 + size e1 + size e2
```


## Adding New Operations

TODO: align to the top, so works with previous slide.

Suppose you want to add a new operation to compute size of expressions...

In **Java**: Hard! Have to change `Expr` interface, but you don't have access
to the library source code.

```Java
interface Expr {
  int eval();
  String print();
  // int size();
}

// Also modify all implementations...
```


## Adding New Cases

Suppose you want to add multiplication.

. . .

In **Haskell**: Hard! Need to change definition of `Expr` and
implementations of `eval` and `print`, but you don't have access
to the library source code.

```Haskell
data Expr
  = Lit Int
  | Add Expr Expr
  -- | Mul Expr Expr

eval :: Expr -> Int
eval (Lit val)   = val
eval (Add e1 e2) = (eval e1) + (eval e2)
-- eval (Mul e1 e2) = (eval e1) * (eval e2)

print :: Expr -> String
print (Lit val)   = show val
print (Add e1 e2) = (print e1) ++ " + " ++ (print e2)
-- print (Mul e1 e2) = (print e1) ++ " * " ++ (print e2)
```


## Adding New Cases

Suppose you want to add multiplication.

In **Java**: Easy! Just create a new class `Mul`:

```Java
class Mul implements Expr {
  Expr e1, e2;
  Mul (Expr arg1, Expr arg2) { e1 = arg1; e2 = arg2; }

  int eval() { return arg1.eval() * arg2.eval(); }
  String print() {
    return arg1.print() + " * " + arg2.print();
  }
}
```


## Other Problems (Notes, TODO: remove or move at the end)

* Extending the return type
* Binary methods (like equality)
* Effects (type checking requires context, use state monad)


## The Problem

+-----+------+-------+------+
|     | eval | print | size |
+=====+======+=======+======+
| Lit |      |       |      |
+-----+------+-------+------+
| Add |      |       |      |
+-----+------+-------+------+
| Mul |      |       |      |
+-----+------+-------+------+
| Neg |      |       |      |
+-----+------+-------+------+

. . .

###

**Functional languages**: hard to add new variants (rows)

###

**Object-oriented languages**: hard to add new operations (columns)


## The Problem

TODO: wording is confusing

> * Functional languages group behaviors of all variants for a single operation

> * Object-oriented languages group behaviors for all operations of a single
    variant

> * Can we do both at the same time, in a type-safe manner, without modifying
    existing code?

# Solutions

## Solutions

How could the arithmetic expressions library have been written to facilitate
easier extensibility?


## Object-oriented Solutions

### Visitor Pattern

Instead of adding new operations on the datatype's interface, define a new
interface for operations. The only operation that the datatype's interface has
to define is a method to "accept" visitors.

## Object-oriented Solutions: Visitor Pattern

```Java
interface Expr {
  <R> R accept(ExprVisitor<R> v);
}

interface ExprVisitor<R> {
  R lit(int val);
  R add(Expr e1, Expr e2);
  R mul(Expr e2, Expr e2);
}
```

## Object-oriented Solutions: Visitor Pattern

```Java
class Lit implements Expr {
  int val;
  Lit(int v) { val = v; }
  <R> R accept(ExprVisitor<R> v) { return v.lit(val);  }
}

class Add implements Expr {
  Expr e1, e2;
  Add(Expr arg1, Expr arg2) { e1 = arg1; e2 = arg2; }
  <R> R accept(ExprVisitor<R> v) { return v.add(e1, e2);  }
}
```


## Object-oriented Solutions: Visitor Pattern

```Java
class Eval implements ExprVisitor<Integer> {
  Integer lit(int val) { return val; }
  Integer add(Expr e1, Expr e2) {
    return e1.accept(this) + e2.accept(this);
  }
}

class Print implements ExprVisitor<String> {
  String lit(int val) { return Integer.toString(val); }
  String add(Expr e1, Expr e2) {
    return e1.accept(this) + " + " + e2.accept(this);
  }
}
```

## Object-oriented Solutions: Visitor Pattern

What was hard for Java is now easy: we can add new operations without modifying
the library.

###
```Java
class Size implements ExprVisitor<Integer> {
  Integer lit(int val) { return 1; }
  Integer add(Expr e1, Expr e2) {
    return 1 + e1.accept(this) + e2.accept(this);
  }
}
```


## Object-oriented Solutions: Visitor Pattern

However, what was easy is now hard: to add new kinds of expressions, we need to
change the `ExprVisitor` interface of the library.

###
**Visitors flip the expression problem for object-oriented languages**.


## Object-oriented Solutions: Visitor Pattern

Can we use inheritance to make adding new kinds of expressions easy?

###
```Java
interface ExprWithMul extends Expr {
  <R> R accept(ExprWithMulVisitor<R> v);
}

interface ExprWithMulVisitor<R> extends ExprVisitor<R> {
  R mul(ExprWithMul e);
}

class MulExpr implements ExprWithMul {
  Expr e1, e2;
  Mul(Expr arg1, Expr arg2) { e1 = arg1; e2 = arg2; }

  <R> R accept(ExprWithMulVisitor<R> v) { return v.mul(expr); }
  <R> R accept(ExprVisitor<R> v) { /* what do we call here?? */ }
}
```


## Object-oriented Solutions: Object Algebras

> * "Extensibility for the Masses" by Oliveira and Cook, ECOOP 2012

> * inspired by Church encodings

> * very similar to finally tagless style (Carette et al, JFP 2009)


## Object-oriented Solutions: Object Algebras

```Java
interface ExprAlgebra<E> {
  E lit(int val);
  E add(E e1, E e2);
}

class Eval implements ExprAlgebra<Integer> {
  Integer lit(int val) { return val; }
  Integer add(Integer e1, Integer e2) { return e1 + e2; }
}

class Print implements ExprAlgebra<String> {
  String lit(int val) { return Integer.toString(val); }
  String add(String e1, String e2) { return e1 + " + " + e2; }
}
```

## Object-oriented Solutions: Object Algebras

###
Like the visitor pattern, we can add operations by creating a new class that
implements the `ExprAlgebra<E>` interface.

###
```Java
class Size implements ExprAlgebra<Integer> {
  Integer lit(int val) { return 1; }
  Integer add(Integer e1, Integer e2) { return 1 + e1 + e2; }
}
```

## Object-oriented Solutions: Object Algebras

We can extend the `ExprAlgebra<E>` interface to support new variants, and can
extend existing operations easily.

###
**This satisfies Wadler's solution criteria for the expression problem!**

## Object-oriented Solutions: Object Algebras

```Java
interface ExprMulAlgebra<E>  {
  E mul(E e1, E e2);
}

class EvalMul
extends EvalMul implements ExprMulAlgebra<Integer> {
  Integer mul(Integer e1, Integer e2) { return e1 * e2; }
}

class PrintMul
extends Print implements ExprMulAlgebra<String> {
  String mul(String e1, String e2) { return e1 + " * " + e2; }
}

class SizeMul
extends Size implements ExprNegAlgebra<Integer> {
  Integer mul(Integer e1, Integer e2) { return 1 + e1 + e2; }
}
```


## Object-oriented Solutions: Object Algebras

How do we use object algebras?

TODO: finish this


## Object Algebras vs. Finally Tagless Style

Aside: Compare object algebras with finally tagless style

```Haskell
class Expr a where
  lit :: Int -> a
  add :: a -> a -> a

newtype Eval = Eval Int
instance Expr Eval where
  lit val = Eval val
  add (Eval e1) (Eval e2) = Eval (e1 + e2)

newtype Print = Print String
instance Expr Print where
  lit val = Print (show val)
  add (Print e1) (Print e2) = Print (e1 ++ " + " ++ e2)
```


## Object Algebras vs. Finally Tagless Style

```Haskell
class Expr a => ExprMul a where
  mul ::: a -> a -> a

instance ExprMul Eval where
  mul (Eval e1) (Eval e2) = e1 * e2

instance ExprMul Print where
  mul (Print e1) (Print e2) = Print (e1 ++ " * " ++ e2)
```


## Object Algebras vs. Finally Tagless Style

Bonus Haskell trivia: What is the type of `x`?

```Haskell
Prelude > let x = add (lit 2) (lit 3)
Prelude > :t x
```

. . .

###
Depends on the type annotation you give it!

```Haskell
Prelude > :t x
x :: Expr t => t

Prelude > x :: Eval
x :: Eval 5

Prelude > x :: Print
x :: Print "2 + 3"
```


## Visitor Pattern

+-----+------+-------+------+
|     | eval | print | size |
+=====+======+=======+======+
| Lit |      |       |      |
+-----+------+-------+------+
| Add |      |       |      |
+-----+------+-------+------+
| Mul |      |       |      |
+-----+------+-------+------+


## Outline

There are a lot of solutions! We will focus on these.

* Multimethods

* Polymorphic variants

* Visitors 
    * Traditional visitors
    * "Self-types" approach
    * Object Algebras / Finally Tagless style

* Data types a la carte

## Multimethods

problems

* too slow
* how to determine which to dispatch among multiple viable methods?

* can only be practically used in Haskell


## Polymorphic Variants

```Haskell

fun x = x + 1

```

Pros:

- makes 
-


## Data types a la carte

*à la carte* /ah luh kahrt, al-uh/: _as separately priced items from a menu, not as part of a set._


## Data types a la carte

*Idea*: use open recursion, i.e. take recursive occurrences as an argument.

```Haskell
data Expr' e
  = Lit Int
  | Add e e -- Use e not Expr!
```

. . .

```Haskell
eval' :: Expr' Int -> Int
eval' (Lit x)     = x
eval' (Add x1 x2) = x1 + x2

print' :: Expr' String -> String
print' (Lit x)     = show x
print' (Add e1 e2) = e1 ++ " + " ++ e1
```

. . .

Note how we assume that recursive calls have already been made!

`Expr'` encodes one level of the recursive type. Subexpressions
are left abstract.

## Tying the Knot

To get the full type, just tie the recursive knot!

. . .

```Haskell
data Expr' e
  = Lit Int
  | Add e e

data Expr = In (Expr' Expr)
```

. . .

```Haskell
eval :: Expr -> Int
eval (In (Lit x))     = eval' (Lit x)
eval (In (Add e1 e2)) = eval' (Add (eval e1) (eval e2))

print :: Expr -> String
print (In (Lit x))     = print' (Lit x)
print (In (Add s1 s2)) = print' (Add (print s1) (print s2))
```

. . .

`eval` and `print` look similar. What pattern is that...?

. . .

**Functor!**


## Tying the Knot Reloaded

Let's try again with functors.

```Haskell
instance Functor Expr' where
  fmap f (Lit x)     = Lit x
  fmap f (Add e1 e2) = Add (f e1) (f e2)

data Expr = In (Expr' Expr)

eval :: Expr -> Int
eval (In e) = eval' (fmap eval e)

print :: Expr -> String
print (In e) = print' (fmap print e)
```

. . .

They _still_ look similar...

. . .

It's just a **fold**!


## Tying the Knot Revolutions

Factoring fold out:

```Haskell
instance Functor Expr' where
  fmap f (Lit x)     = Lit x
  fmap f (Add e1 e2) = Add (f e1) (f e2)

data Expr = In (Expr' Expr)

fold :: (Expr' a -> a) -> Expr -> a
fold f (In e) = f (fmap (fold f) e)
```

. . .

```Haskell
eval :: Expr -> Int
eval = fold eval'

print :: Expr -> String
print = fold print'
```

Fold takes a function that can reduce one level of the tree, and applies it repeatedly to reduce the whole tree.


## Adding New Cases

Let's define a language with multiplication:

```Haskell
data Mul' e = Mul e e

instance Functor Mul' where
  fmap f (Mul e1 e2) = Mul (f e1) (f e2)

mulEval' :: Mul' Int -> Int
mulEval' (Mul x1 x2) = x1 * x2

mulPrint' :: Mul' String -> String
mulPrint' (Mul e1 e2) = e1 ++ " + " ++ e1
```

. . .

Tie it:

```Haskell
data Mul = MulIn (Mul' Mul)

mulFold :: (Mul' a -> a) -> Mul -> a
mulFold f (MulIn e) = f (fmap (mulFold f) e)
```

. . .

`Mul` and `mulFold` look exactly like `Expr` and `fold`. We must generalize!


## Initial Algebras

It's easy to factor out "knot tying":

```Haskell
data Fix f = In (f (Fix f))

fold :: Functor f => (f a -> a) -> Fix f -> a
fold f (In t) = f (fmap (fold f) t)
```

. . .

`Expr` and `Mul` are now simplified, and `fold` works on both:

```Haskell
type Expr = Fix Expr'

type Mul = Fix Mul'

>>> fold eval' $ In (Add (In (Lit 5)) (In (Lit 4))) ==> 9
```


## Better Syntax

Being forced to use `mulEval` and `mulPrint` is annoying. Let's use type classes:

```Haskell
class Functor f => Eval f where
  eval :: f Int -> Int

class Fuctor f => Print f where
  print :: f String -> String


instance Eval Expr' where
  eval (Lit x) = x
  eval (Add x1 x2) = x1 + x2

instance Eval Mul' where
  eval (Mul x1 x2) = x1 * x2

instance Print ...
```

. . .

Nice! How do we combine `Expr'` and `Mul'` though?


## Adding New Cases

To combine two open data types, or signatures, take their coproduct.

This is similar to an `Either` but over type constructors:

```Haskell
data Either a b = Left a | Right b

data Sum f g e = Left (f e) | Right (g e)
```

. . .

Coproducts of two functors is a functor:

```Haskell
instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap f (Left e)  = Left (fmap f e)
  fmap f (Right e) = Right (fmap f e)
```

. . .

and extends operations:
```Haskell
instance (Eval f, Eval g) => Eval (Sum f g) where
  eval (Left e)  = eval e
  eval (Right e) = eval e

instance (Print f, Print g) => Print (Sum f g) where
  ...
```


## Adding New Cases

Now we are ready to combine `Expr'` and `Mul'`:

```Haskell
type ExprWithMul' = Sum Expr' Mul'

type ExprWithMul = Fix ExprWithMul'

>>> fold eval $
>>>   In (Right (Mul (In (Left (Lit 3))) (In (Left (Lit 4)))))
>>>   ==> 12
```

. . .

OK, that example is ugly... Let's throw in smart constructors and type classes.


## Syntactic Sugar

This is the hacky part of Data types a la carte.

TODO


## Data types a la carte: Lessons

Really a hack to emulate polymorphic variants.


## Extensions

* Open result type
* Products of signatures (cref auto location tagging and compositional data types)
* Data type a la carte talks about direct open recursion
* Problems with type classes
* Monads
