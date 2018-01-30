
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


## Other Problems (Notes, TODO: remove or move at the end)

* Extending the return type
* Binary methods (like equality)
* Effects (type checking requires context, use state monad)


## Syntactic Sugar

This is the hacky part of Data types a la carte.

TODO


## Extensions

* Open result type
* Products of signatures (cref auto location tagging and compositional data types)
* Data type a la carte talks about direct open recursion
* Problems with type classes
* Monads

