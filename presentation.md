---
title: The Expression Problem
subtitle: A PLDG Tutorial
author: Josh Acay and Rolph Recto
date: January 31, 2016
fontsize: 8pt
---

# The Problem

```Haskell
data Expr =
    Const Int
  | Add Expr Expr

evalExpr :: Expr -> Int
evalExpr (Const v) = v
evalExpr (Add e1 e2) = (eval e1) + (eval e2)

printExpr :: Expr -> String
printExpr (Const v)   = show v
printExpr (Add e1 e2) = (printExpr e1) ++ " + " ++ (printExpr e2)
```

How to add new variants of `Expr`?


# History


# Solutions

There are a lot of solutions! We will focus on these.

* Multimethods

* Polymorphic variants

* Visitors 
    * Traditional visitors
    * "Self-types" approach
    * Object Algebras / Finally Tagless style

* Data types a la carte

# Multimethods

problems

* too slow
* how to determine which to dispatch among multiple viable methods?

* can only be practically used in Haskell


# Multi

* multimethods

# Polymorphic variants

```Haskell

fun x = x + 1

```

Pros:

- makes 
-


# Data types a la carte


eval : ('a -> int)  -> [Const int or Add 'a * 'a] -> int
eval f expr = 
  match expr with
  | `Const v   -> v
  | `Add e1 e2 -> (f e1) + (f e2)


evalWithMul f expr =
  | `Mul e1 e2 -> (f e1) * (f e2)
  | _          -> eval f expr

