## Turtle Geometry

![system setup](./pics/system.png "System setup.")

[Logo](https://en.wikipedia.org/wiki/Logo_(programming_language))
functionality in Common Lisp. Inspired by the
book
[Turtle Geometry](https://mitpress.mit.edu/books/turtle-geometry).

### Usage

The interface is a hybrid graphical text interface. A graphical
windows is created with SDL2, and using Emacs and Slime, the Logo
commands are inputted.

`shapes.lisp` stores code from the book.

Hotkeys in `main.lisp`:

```

ESC - quit

LALT+r - clear

LCTRL+MOUSE - change camera direction

LCTRL+{W,A,S,D} - move up, down, left, right

LCTRL+MOUSE_WHEEL - zoom in and out

LSHIFT+TAB - get camera to face the turtle

Z - decrease camera speed

X - increase camera speed

C - get camera info

```

#### License

[MIT](LICENSE)
