---
title: The Expression Problem
subtitle: A Cornell PLDG Tutorial
author: Josh Acay and Rolph Recto
date: January 31, 2016
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

* cannot modifying existing code

* type-safe


## An Example

**Scenario**: You are a software engineer at BFC (Big Friendly Corporation) and
your application needs a way to manipulate arithmetic expressions. You use a
library to support this feature. *You have no access to the library's source
code.*

. . .

###

**Datatype**: `EXPR ::= n | EXPR + EXPR | EXPR * EXPR`

###

**Operations**: `evalExpr`, `printExpr`


## An Example

Source code of library in Haskell:

```Haskell
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr

evalExpr :: Expr -> Int
evalExpr (Lit val)    = val
evalExpr (Add e1 e2)  = (eval e1) + (eval e2)
evalExpr (Mul e1 e2)  = (eval e1) * (eval e2)

printExpr :: Expr -> String
printExpr (Lit val)   = show val
printExpr (Add e1 e2) = (printExpr e1) ++ " + " ++ (printExpr e2)
printExpr (Mul e1 e2) = (printExpr e1) ++ " * " ++ (printExpr e2)
```


## An Example

Source code of library in Java:

```Java
interface Expr {
  int evalExpr();
  String printExpr();
}

class LitExpr implements Expr {
  int val;
  LitExpr(int v) { val = v; }

  int evalExpr() { return val; }
  String printExpr() {
    return Integer.toString(val);
  }
}
```


## An Example

```Java
class AddExpr implements Expr {
  Expr e1, e2;
  AddExpr(Expr arg1, Expr arg2) { e1 = arg1; e2 = arg2; }

  int evalExpr() { return arg1.evalExpr() + arg2.evalExpr(); }
  String printExpr() {
    return arg1.printExpr() + " + " + arg2.printExpr();
  }
}

class MulExpr implements Expr {
  Expr e1, e2;
  MulExpr(Expr arg1, Expr arg2) { e1 = arg1; e2 = arg2; }

  int evalExpr() { return arg1.evalExpr() * arg2.evalExpr(); }
  String printExpr() {
    return arg1.printExpr() + " * " + arg2.printExpr();
  }
}
```


## An Example

You happily use the arithmetic expressions library for Haskell/Java above for a
while, but you soon run into problems.

. . .

###

Suppose you want to add a new operation to calculate the size of expressions.

. . .

> * **Haskell**: Easy! Just create a new function `sizeExpr`.
> * **Java**: Hard! Have to change `Expr` interface, but you don't have access
  to the library source code.

. . .

###

Suppose you want to add a new kind of expression for negative numbers.

. . .

> * **Haskell**: Hard! Need to change implementation of `evalExpr` and
  `printExpr`, but you don't have access to the library source code.
> * **Java**: Easy! Just create a new class `NegExpr`.


## The Problem

+-----+------+-------+------+
|     | eval | print | size |
+=====+======+=======+======+
| Lit |    |     |    |
+-----+------+-------+------+
| Add |    |     |    |
+-----+------+-------+------+
| Mul |    |     |    |
+-----+------+-------+------+
| Neg |    |     |    |
+-----+------+-------+------+

. . .

###

**Functional languages**: hard to add new variants (rows)

###

**Object-oriented languages**: hard to add new operations (columns)


## The Problem

> * Functional languages group behaviors of all variants for a single operation

> * Object-oriented languages group behaviors for all operations of a single
    variant

> * Can we do both at the same time, in a type-safe manner, without modifying
    existing code?

# Solutions

## Solutions

How could the arithmetic expressions library have been written to facilitate
easier extensibility?


## Object-oriented solutions

### Visitor pattern

Instead of adding new operations on the datatype's interface, define a new
interface for operations. The only operation that the datatype's interface has
to define is a method to "accept" visitors.

## Object-oriented solutions: Visitor pattern

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

## Object-oriented solutions: Visitor pattern

```Java
class LitExpr implements Expr {
  int val;
  LitExpr(int v) { val = v; }
  <R> R accept(ExprVisitor<R> v) { return v.lit(val);  }
}

class AddExpr implements Expr {
  Expr e1, e2;
  AddExpr(Expr arg1, Expr arg2) { e1 = arg1; e2 = arg2; }
  <R> R accept(ExprVisitor<R> v) { return v.add(e1, e2);  }
}

class MulExpr implements Expr {
  Expr e1, e2;
  AddExpr(Expr arg1, Expr arg2) { e1 = arg1; e2 = arg2; }
  <R> R accept(ExprVisitor<R> v) { return v.add(e1, e2);  }
}
```


## Object-oriented solutions: Visitor pattern

```Java
class EvalExpr implements ExprVisitor<Integer> {
  Integer lit(int val) { return val; }
  Integer add(Expr e1, Expr e2) {
    return e1.accept(this) + e2.accept(this);
  }
  Integer mul(Expr e1, Expr e2) {
    return e1.accept(this) * e2.accept(this);
  }
}

class PrintExpr implements ExprVisitor<String> {
  String lit(int val) { return Integer.toString(val); }
  String add(Expr e1, Expr e2) {
    return e1.accept(this) + " + " + e2.accept(this);
  }
  String mul(Expr e1, Expr e2) {
    return e1.accept(this) + " * " + e2.accept(this);
  }
}
```

## Object-oriented solutions: Visitor pattern

What was hard for Java is now easy: we can add new operations without modifying
the library.

###
```Java
class SizeExpr implements ExprVisitor<Integer> {
  Integer lit(int val) { return 1; }
  Integer add(Expr e1, Expr e2) {
    return e1.accept(this) + e2.accept(this) + 1;
  }
  Integer mul(Expr e1, Expr e2) {
    return e1.accept(this) + e2.accept(this) + 1;
  }
}
```


## Object-oriented solutions: Visitor pattern

However, what was easy is now hard: to add new kinds of expressions, we need to
change the `ExprVisitor` interface of the library.

###
**Visitors flip the expression problem for object-oriented languages**.


## Object-oriented solutions: Visitor pattern

Can we use inheritance to make adding new kinds of expressions easy?

###
```Java
interface ExprWithNeg extends Expr {
  <R> R accept(ExprWithNegVisitor<R> v);
}

interface ExprWithNegVisitor<R> extends ExprVisitor<R> {
  R neg(ExprWithNeg e);
}

class NegExpr implements ExprWithNeg {
  Expr expr;
  NegExpr(Expr e) { expr = e; }
  <R> R accept(ExprVisitor<R> v) { /* what do we call here?? */ }
  <R> R accept(ExprWithNegVisitor<R> v) { return v.neg(expr); }
}
```


## Object-oriented solutions: Object algebras

> * "Extensibility for the Masses" by Oliveira and Cook, ECOOP 2012

> * inspired by Church encodings

> * very similar to finally tagless style (Carette et al, JFP 2009)


## Object-oriented solutions: Object algebras

```Java
interface ExprAlgebra<E> {
  E lit(int val);
  E add(E e1, E e2);
  E mul(E e1, E e2);
}

class EvalExpr implements ExprAlgebra<Integer> {
  Integer lit(int val) { return val; }
  Integer add(Integer e1, Integer e2) { return e1 + e2; }
  Integer mul(Integer e1, Integer e2) { return e1 * e2; }
}

class PrintExpr implements ExprAlgebra<String> {
  String lit(int val) { return Integer.toString(val); }
  String add(String e1, String e2) { return e1 + " + " + e2; }
  String mul(String e1, String e2) { return e1 + " * " + e2; }
}
```

## Object-oriented solutions: Object algebras

###
Like the visitor pattern, we can add operations by creating a new class that
implements the `ExprAlgebra<E>` interface.

###
```Java
class SizeExpr implements ExprAlgebra<Integer> {
  Integer lit(int val) { return 1; }
  Integer add(Integer e1, Integer e2) { return e1 + e2 + 1; }
  Integer mul(Integer e1, Integer e2) { return e1 + e2 + 1; }
}
```

## Object-oriented solutions: Object algebras

We can extend the `ExprAlgebra<E>` interface to support new variants, and can
extend existing operations easily.

###
**This satisfies Wadler's solution criteria for the expression problem!**

## Object-oriented solutions: Object algebras

```Java
interface ExprNegAlgebra<E>  {
  E neg(E expr);
}

class EvalNegExpr
extends PrintExpr implements ExprNegAlgebra<Integer> {
  Integer neg(Integer e) { return 0 - e; }
}

class PrintNegExpr
extends PrintExpr implements ExprNegAlgebra<String> {
  String neg(String e) { return "-(" + e + ")"; }
}

class SizeNegExpr
extends SizeExpr implements ExprNegAlgebra<Integer> {
  Integer neg(Integer e) { return e + 1; }
}
```


## Object-oriented solutions: Object algebras

How do we use object algebras? 

TODO: finish this


## Object-oriented solutions: Object algebras

Aside: Compare object algebras with finally tagless style

```Haskell
class Expr a where
  lit :: Int -> a
  add :: a -> a -> a
  mul :: a -> a -> a

newtype ExprEval = ExprEval Int
instance Expr ExprEval where
  lit val = ExprEval val
  add (ExprEval e1) (ExprEval e2) = ExprEval (e1 + e2)
  mul (ExprEval e1) (ExprEval e2) = ExprEval (e1 * e2)

newtype ExprPrint = ExprPrint String
instance Expr ExprPrint where
  lit val = ExprPrint (show val)
  add (ExprPrint e1) (ExprPrint e2) = ExprPrint (e1 ++ " + " ++ e2)
  mul (ExprPrint e1) (ExprPrint e2) = ExprPrint (e1 ++ " * " ++ e2)
```


## Object-oriented solutions: Object algebras

```Haskell
class Expr a => ExprNeg a where
  neg ::: a -> a

instance ExprNeg ExprEval where
  neg (ExprEval e) = 0 - e

instance ExprNeg ExprPrint where
  neg (ExprPrint e) = "-(" ++ e ++ ")"
```


## Object-oriented solutions: Object algebras

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

Prelude > x :: ExprEval
x :: ExprEval 5

Prelude > x :: ExprPrint
x :: ExprPrint "2 + 3"
```


## Visitor pattern

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


## Polymorphic variants

```Haskell

fun x = x + 1

```

Pros:

- makes 
-


## Data types a la carte


eval : ('a -> int)  -> [Const int or Add 'a * 'a] -> int
eval f expr = 
  match expr with
  | `Const v   -> v
  | `Add e1 e2 -> (f e1) + (f e2)


evalWithMul f expr =
  | `Mul e1 e2 -> (f e1) * (f e2)
  | _          -> eval f expr

