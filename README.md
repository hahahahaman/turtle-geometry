## Turtle Geometry

![system setup](./pics/system.png "System setup.")

[Logo](https://en.wikipedia.org/wiki/Logo_(programming_language))
functionality in Common Lisp. Inspired by the
book
[Turtle Geometry](https://mitpress.mit.edu/books/turtle-geometry).

Tested with [SBCL](http://www.sbcl.org).

### Install

Get [quicklisp](https://www.quicklisp.org).

```
cd quicklisp/local-projects/

git clone https://github.com/hahahahaman/turtle-geometry.git
```

In the REPL use quicklisp to install all dependencies and to build
the package:

```common-lisp

(ql:quickload :turtle-geometry)

```
### Usage

In order to save on typing, I run the commands while I'm inside the
turtle-geometry package

REPL:

```common-lisp
(in-package :turtle-geometry)

(run)

(dotimes (x 100) (square x))
```

#### Interface

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

C - print camera info to console

```

#### Design

The code is built around immutable objects.

##### Turtle

[turtle.lisp](src/turtle.lisp)

The turtle is an immutable map of attributes, which includes:
position, color, rotation, and pen up/down.

#### License

[MIT](LICENSE)
