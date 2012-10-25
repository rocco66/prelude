Clojure editing
===============

Clojure is a really neat Lisp for JVM. There is also a subset of
Clojure called ClojureScript that lacks all Clojure's dynamical
compilation features but still looks better than JS, in which it can
be compiled (there are also some experimental backends like C).

Paredit
-------

This is a package to edit parenthesis sctructurally. While this sounds
obscure, the idea is pretty simple: you can only work with a *pair* of
brackets; you can't add or delete just one bracket. You can drop an
S-expr in which your coursor is now, or move it's
"boundaries". Somewhat complete reference of most useful paredit
bindings is provided in "paredit-cheatsheet.pdf" file.

Closing-bracket trick
---------------------

There is a small snippet in selemacs.el to help with nested S-exps:
when you open new bracket and hit enter, an extra line will be
insterted:

```
(foo (bar|))
[hit "Enter"]
(foo (bar
      |
      ))
```

And when you hit closing bracket, all nested brackets will be properly
"pulled" to a cursor's position:

```
(foo (bar
      baz|
      ))
[hit ")"]
(foo (bar
      baz))
```

TODO
----

SLIME integration
