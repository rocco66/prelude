Selemacs manual
===============

Selemacs is an adaption of Bootstrap Emacs package for needs and
wishes of Selectel's developers.

In this dir you can find:

-clojure editing manual
-paredit cheatsheet

General tips&tricks
===================

Here comes the stuff that is too small for it's own file.

expand-region is an awesome module that is shown here:
http://emacsrocks.com/e09.html It works by expanding current region to
the next semantical block. **Default keybinding is `C-=`**.

Some bindings, [extra] means that this isn't standard binding

- `F3`: start recording macro
- `F4`: stop recording macro & execute
- `F11`: full screen mode [extra]
- `F12`: toggle menu bar [prelude]
- `C-=`: expand-region [prelude]
- `C-x ^` / `M-^`: join current line to previous one
- `C-k`: kill an entire line
- `M-z`: zap-to-char (kill everything from cursor to provided character)
- `C-l`: recenter (toggle 3 positions of window w.r.t. cursor)
- `M-%` aka `query-replace`: replaces smth, asking one by one
- `M-g g`: jump to line
- `C-`: jump to particular character on the screen (ace-jump) [extra]
- `C->` / `C-<`: shift region to right/left [extra]
- `C-0`: scale text to default font size [extra]
- `M-S-x`: like M-x, but for current major mode only (smex) [extra]
- `C-x f`: recent files (ido) [extra]


TODO:
====

-Erlang (jump-to-definition at least; EDTS package?)
-Haskell
-Dired
-Magit + Gist
-Orgmode (?)
-yasnippet
