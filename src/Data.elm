module Data exposing (startupDocument, doc5, doc2, doc3, doc4)

import Time exposing(Posix)

startupDocument = {
    identifier = "jxxcarlson.pure-elm-markdown-parser.1568-700-834",
    title = "A Pure Elm Markdown Parser"
  , authorID = "jxxcarlson"
  , content = startupText
  , timeCreated = Time.millisToPosix 1568700834000
  , timeUpdated = Time.millisToPosix 1568700834000
  , tags = ["elm", "markdown", "math", "mathjax"]
  , public = True
  }


doc2 = {
    identifier = "jxxcarlson.notes-on-futhark.15687-008-834"
  , title = "Notes on Futhark"
  , authorID = "jxxcarlson"
  , content = text2
  , timeCreated = Time.millisToPosix 1568700834000
  , timeUpdated = Time.millisToPosix 1568700834000
  , tags = ["futhark", "gpu", "parallel computation"]
  , public = True
  }

doc3 = {
    identifier = "jxxcarlson.math-markdown-roadmap-and-progress-report.1568-700-834"
  , title = "Math Markdown Progress Roadmap & Report"
  , authorID = "jxxcarlson"
  , content = text3
  , timeCreated = Time.millisToPosix 1568769977000
  , timeUpdated = Time.millisToPosix 1568851146000
  , tags = ["markdown", "elm", "mathjax"]
  , public = True
  }

doc4 = {
    identifier = "jxxcarlson.graphql-queries-and-mutations.1568-700-834"
  , title = "GraphQL Queries and Mutations"
  , authorID = "jxxcarlson"
  , content = text4
  , timeCreated = Time.millisToPosix 1568930218000
  , timeUpdated = Time.millisToPosix 1568930218000
  , tags = ["graphql"]
  , public = True
  }

doc5= {
    identifier = "jxxcarlson.elm-resources.1568-981-582"
  , title = "Elm resources"
  , authorID = "jxxcarlson"
  , content = text5
  , timeCreated = Time.millisToPosix 1568981582000
  , timeUpdated = Time.millisToPosix 1568981582000
  , tags = ["elm"]
  , public = True
  }


text5 = """
# Elm Resources

This is a work in progress, and only represents resources of which I have made considerable use.

## Books

- [Evan Czaplicki, Elm Guide](https://guide.elm-lang.org/)
- [Jeremy Fairbank, Programming Elm](https://www.goodreads.com/book/show/37824829-programming-elm)
- [Richard Feldman, Elm in Action](https://www.manning.com/books/elm-in-action)
- [Alex Korban, Practical Elm for a Busy Developer](https://korban.net/elm/book/)


## Web sites

- [elmprogramming](https://elmprogramming.com)
- [Teaching Elm (Slides)](https://slides.com/sebbes/the-web-teaching-server-elm-intro#/)


## Development Tools

- [Elm-doc-preview/applications](https://github.com/dmy/elm-doc-preview/blob/master/README.md#applications-support)
- [Elm Greenwood: New Packages RSS](https://elm-greenwood.com/)
- [Ilias' compilation of Elm tools](https://dev.to/zwilias/elm-tools-571a)
- [elm-xref](https://github.com/zwilias/elm-xref/blob/master/README.md)
- [Dmy, Bash completion](https://github.com/dmy/elm-sh-completion)


## Videos

[Richard Feldman, Scaling Elm Apps](https://www.youtube.com/watch?v=DoA4Txr4GUs)

## Articles

- [Janiczek: defunctionalization](http://martin.janiczek.cz/clanek/defunctionalization-in-elm/)


## Exercises

- [Joël Quenneville, 	Generic types](https://gist.github.com/JoelQ/98c5c6a17fd82d8ddd932a75eceb4276)


## Recommended

- [Evan Czaplicki, Structuring Elm Apps](https://guide.elm-lang.org/webapps/structure.html)


## Notes

[Tests](https://github.com/klazuka/intellij-elm/blob/1a3d56a9a38b5b8d83d57fe3ce77166061e158e3/src/test/kotlin/org/elm/lang/core/resolve/ElmFunctionResolveTest.kt#L16)








"""

text4 = """
# Queries and Mutations

## Resources

- [howtographql.com/](https://www.howtographql.com/)
   - [Intro](https://www.howtographql.com/basics/0-introduction/)
   - [Core concepts](https://www.howtographql.com/basics/2-core-concepts/)
   - [Relations](https://www.howtographql.com/graphql-scala/7-relations/)

- Fauna
   - [Index tutorial](https://docs.fauna.com/fauna/current/tutorials/indexes/)
   - [Indexes](https://docs.fauna.com/fauna/current/api/fql/indexes)

## Schema

```
type Document {
   # format: username.compessedTitle.posix
   identifier: String!
   title:String!
   content:String!
   author:User!
   tags:[String!]!
   timeCreated : Int!
   timeUpdated : Int!
   public : Boolean!
   children: [String!]!
}

type User {
   # The username
   username : String!
   email : String!
   public : Boolean!
   firstName : String
   lastName : String
   timeEnrolled : Int!
   timeUpdated : Int!
   admin: Boolean!
   documents: [Document!]!
}



type Query {
   # Get user by username :
   findUserByUsername(username: String!): User
   # Get all users:
   allUsers: [User!]!
   # Get one document
   document(identifier: String!): Document
   # Get all documents of given author
   # Get all documents
   allDocuments: [Document!]!
}
```

## Queries and Mutations

### Users


#### Create User

```
mutation {
  createUser(
    data: {
      admin: true
      email: "jxxcarlson@gmail.com"
      timeUpdated: 1568894608
      timeEnrolled: 1568894608
      public: true
      documents: []
      username: "jxxcarlson"
    }
  ) {
    _id
  }
}
```

#### Delete user

```
	mutation DeleteUser {
	  deleteUser(id: 243941509973410322) {
	    _id
	    username
	  }
	}
```

#### Find all users

```
  query FindAllUsers {
   allUsers {
     data {
       _id
      email
     }
   }
 }
```


#### FindUserById
```
query FindUserByID {
  findUserByID (id:"243942680436933139") {
    username
    email
    documents {
      title
      identifier
    }
  }
}
```

#### FindUserByUsername

```
query FindUserByUsername {
  findUserByUsername (username:"jxxcarlson") {
    username
    email
    documents {
      title
      identifier
    }
  }
}
```

### Documents

#### Create document

```
mutation CreateDocument {
  createDocument(
     data: {
      author: {connect: 243942680436933139}
      children: []
      identifier: "jxxcarlson.test.1568894608"
      timeUpdated: 1568894608
      tags: []
      timeCreated: 1568894608
      public: true
      content: "Ths **is** a test!"
      title: "Test"
    }
  )
   {
    author { username }
    identifier
    title
  }
}

```

#### Delete document

```
mutation DeleteDocument {
  deleteDocument (id: 243890700844794388) {
    _id
    title
  }
}
```

#### Find all documents

```
 query FindAllDocuments {
   allDocuments {
     data {
       _id
       identifier
       title
       content
     }
   }
 }
```

#### Find document by author identifier

```
query FindDocumentByAuthor {
  documentsByAuthor(author: "1") {
      _id
      identifier
      title
      content
    }
  }
```
"""

text3 = """# Math Markdown Progress Roadmap & Report

## Recent Additions

A great deal of the work is "under the hood."  Here are some visible changes:

1. App should read current window dimensions and use them to set its dimensions (9/19/2019)

1. Automatically synchronize document title with the first heading of the document body (9/18/2019)

1. Display metadata in footer: author, document identifier, tags, and dates of creation and last update (9/18/2019)

1. Add word count to footer of document; cleaner user interface  (9/17/2019)

1. Add automatically generated table of contents in sidebar (9/17/2019)

## Roadmap

Here is a list of things to be done, not necessarily in this order:

1. Attach app to backend so that documents will be saved

1. Implement user sign up and sign in

1. Search: title, tags, text, user (author)

1. Move to MathJax 3 for rendering of math text.  It will be much faster and will
likely eliminate "jumping text" during editing.

1. Auto-detect screen size and present simplified reader-only version for smart phones.

1. Configurable "Front page" for authors.

1. Shortcut links for articles.

1. Document collections and intra-app links between documents

1. Rendering inconsistencies

   - handling of space before a period and after a newline.
   - handling of list numbering (see below, ha ha!)

1. Better adherence to Commonmark spec
"""

startupText =
    """# A Pure Elm Markdown Parser

## Introduction

![Galaxy-slice](http://noteshare-images.s3.amazonaws.com/galaxy-slice-bw2.png) *An image, just for fun*


This document illustrates what one can do with the pure Elm markdown
library [jxxcarlson/elm-markdown](https://package.elm-lang.org/packages/jxxcarlson/elm-markdown/latest/).
The idea is to be able to do Markdown + Math, like this:

$$
\\int_{-\\infty}^\\infty e^{-x^2} dx = \\pi
$$

Formulas written n TeX/LaTeX are rendered using [MathJax](https://mathjax.org).

Here we use the library to build a prototype for an interactive editor
— try the **Edit** and **Read** buttons. The goal is a content management system for things like
lecture notes and class assignments in technical fields like math,
science, and engineering.  Stay tuned!


### Try it out

Feel free to use the **Edit** button to change the text. Or press **Tools**, then **Create Document**. In the
not-to-distant future, we will offer this Markdown as a service so that one
 can create and distribute class notes and other materials via a user-friendly
 searchable interface.


### Flavors of Markdown

The library provides three flavors of Markdown: *Standard*,
*Extended* and *ExtendedMath*. The Extended option provides strike-through
text, verbatim blocks, poetry blocks, and tables.  The ExtendedMath
option handles formulas as well.

In all flavors
of the language, one can generate
an active table of contents.  Depending on the library rendering
function used, the table of contents can be placed at the top
of the document, on the side, as in this app, or can be absent.


### A Work in Progress

This project is a work in progress: more to to do make
it adhere as closely as possible to the CommonMark spec, for example. Write me at jxxcarlson@gmail.com
with comments and bug reports,
or post an issue on the
[GitHub repo](https://github.com/jxxcarlson/elm-markdown).

Among the near-term plans
is addition of a renderer whose output is  LaTeX.  This seemingly retro feature
will permit one to produce PDF documents and therefore to print.

For installation and use of the library, see the notes
at the end or the documentation on the
[Elm package manager](https://package.elm-lang.org/packages/jxxcarlson/elm-markdown/latest/).


## Demo

Below we illustrate various features of the present Markdown library — elements: images, links, headings, etc.

![Hummingbird](https://www.allaboutbirds.org/guide/noindex/photo/60395551-1280px.jpg)

Hummingbird (Meditation)

Link: [New York Times](http://nytimes.com)

Text styles: **bold** *italic* ~~strike it out~~


## Inline Math

This is a test: $a^2 + b^2 = c^2$.

## Display Math

So is this:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$


## Code

He said that `a := 0` is an initialization statement.

```
# Partial sum of the harmonic series:

sum = 0
for n in range(1..100):
  sum = sum + 1.0/n
sum
```

## Verbatim and Tables (Extensions)

A verbatim block begins and ends with four tick marks.
It is just like a code block, except that there is no
syntax highlighting.  Verbatim blocks are an extension
of normal Markdown.

````
Verbatim text has many uses:

   Element    |    Z
   --------------------
   Altium     |    4/5
   Brazilium  |    7/5
   Certium    |    9/5
````

But better is to use Markdown tables:
  
|  Element  | Symbol |  Z | A |
| Hydrogen  | H      |  1 | 1.008   |
| Helium    | He     |  2 |  4.0026 |
| Lithium   | Li     |  3 |  6.94   |
| Beryllium | Be     |  4 |  9.0122 |
| Boron     | B      |  5 | 10.81   |
| Carbon    | C      |  6 | 12.011  |
| Nitrogen  | N      |  7 | 14.007  |
| Oxygen    | O      |  8 | 15.999  |
| Flourine  | F      |  9 | 18.998  |
| Neon      | Ne     | 10 | 20.180  |


## Lists

Indent by four spaces for each level.  List items
are separated by blank lines.

- Solids

    - Iron *(metal)*

        - Iron disulfide (Pyrite): $FeS_2$, crystalline

        - Iron(II) sulfed $FeS$, not stable, amorphous

    - Selenium *(use for solar cells)*

- Liquids

    - Alcohol *(careful!)*

    - Water *(Ok to drink)*

## Numbered lists

### Problem Set 18

1. Compute the coefficient of $a^5b^2$ in $(a + b)^7$.

    1. Do also: coefficient of $a^5b^5$ in $(a + 2b)^{10}$

    2. Do also: coefficient of $a^7b^5$ in $(a - b)^{12}$

4. If $f'(2) = 0$, what can you say about the graph of $f$ at $x = 2$?

6. Suppose that in addition, $f''(2) > 0$. What else can say about the graph?


### Problem Set 19

4. Show that $u(x,t) = f(x - ct)$ is a solution to the equation $\\partial u(x,t)/\\partial x + c^{-1} \\partial u(x,t)/\\partial t = 0$.

3. State the wave equation and show that $u(x,t)$ as above is a solution to it.

2. In what direction does the wave defined by $u(x,t) = f(x - ct)$ move?

4.  Find a solution of the wave equation that represents a pulse moving in the opposite direction.



## Quotations


Quotations are offset:

> Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.

> Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.

> But, in a larger sense, we can not dedicate—we can not consecrate—we can not hallow—this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us—that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion—that we here highly resolve that these dead shall not have died in vain—that this nation, under God, shall have a new birth of freedom—and that government of the people, by the people, for the people, shall not perish from the earth.

— Abraham Lincoln, *Gettysbug Address*

## Poetry (Extension)

Poetry blocks, an extension of normal Markdown,
 begin with ">>"; line endings are respected.

>> Twas brillig, and the slithy toves
Did gyre and gimble in the wabe:
All mimsy were the borogoves,
And the mome raths outgrabe.

>> Beware the Jabberwock, my son!
The jaws that bite, the claws that catch!
Beware the Jubjub bird, and shun
The frumious Bandersnatch!


Etcetera!

___


NOTE: this Markdown implementation is an option for writing documents on [knode.io](https://knode.io).
Knode also offers MiniLaTeX, a web-friendly subset of TeX/LaTex.  To see
how it works without a sign-in, please see [demo.minilatex.app](https://demo.minilatex.app).


___

## Installation


To compile, use

```
elm make --output=Main.js
```

Then open `index.html` to run the app.


"""

--- ONE MORE ---



text2 = """
# Notes onFuthark

## Introduction

In this document I describe how I got started
using Futhark and will discuss various matters that  may be of interest to
beginners of various kinds — in functional programming, in programming for GPUs.
The syntax of Futhark is rather like Haskell — not coincidentally, the
Futhark compiler is written in Haskell.  While a passing knowledge of Haskel
l is useful in reading the below, it is not a prerequisite.

### Installation

On Mac:

````
brew install futhark
````


Or if you have it installed and want the latest version, do `brew upgrade futhark`.  As of this writing (May 16, 2019) the version is 0.10.2.

### The Repl

The Futhark repl is a great way to experiment with Futhark.  To begin, we can do arithmetic:

````
$ futhark repl

> 1 + 1
2i32
````

OK, it works! Note that results are printed with their type.  In this case, $2$ is a 32-bit signed integer.

### Functions

Functions are defined in ML style:

```
> let f x = x*x
> f 3
9i32
```

The arguments of a function as well as the return
value can be annotated with a type:

```
> let f (x:i32): i32 = x*x
> f 233
54289i32
```

Functions can be mapped over a list:

```
> map f [1,2,3,4]
[1i32, 4i32, 9i32, 16i32
```

Then `f 3` and `f 3:i32` work
but  `f 3:u8` does not.  The compiler
can do a better job when type information is
provided.

Functions can be partially applied (curried):

````
> let add x y = x + y
> add 2 3
5i32

> let add2 = add 2
> add2 3
5i32
````
`Loading code from files`

Just do `load FILENAME`.  Once a file is loaded, it can be reloaded with `:load`, or just `:l`

### Reduce

Reduce is the substitute in functional languages for many loop constructs in imperative languages. Let's use it to compute the sum of a list of numbers:

````
> reduce (+) 0 [1,2,3]
6i32
````

Reduction works according to the rules

````
reduce f a [] = a
reduce f a [x, y, z, ...] = reduce f (f x a) [y, z, ..]
````on

Thus we have

```
reduce (+) 0 [1, 2, 3]
reduce (+) 1 [2, 3]
reduce (+) 3 [3]
reduce (+) 6 [ ]
6
```

We can now define a funcdtion for computing the sum of a list of numbers:

```
let sum list = reduce (+) 0 list
```

Then the code below accomplishes the same thing as our first example in this section:

```
sum [1,2,3]
6i32
```

#### Note on reduce

The previous example works because  `+` is the function


```
(+) x y = x + y
```


It has type signature $a \\to a \\to a$.


**Note.** The type of `reduce` in Futhark is

$$
a \\to a \\to a) \\to a \\to [\\ \\ ]a \\to a
$$

rather than the more general

$$
\\label{eq:reduce2}
(a \\to b \\to b) \\to b \\to [\\ \\ ]a \\to b
$$

that one finds in  functional languages like Haskell and Elm.  This is because the reducer
$a \\to a \\to a$ in \\eqref{eq:reduce} is associative.  For functons of the
more general type  \\eqref{eq:reduce2} , $a \\to b \\to b$, it may not even make sense
to ask whether it is associative.  The practical import of this fact is that reducers
of the form  \\eqref{eq:reduce}  behave well under parallelization,
whereas the more general kind does not.  See the section \\italic{Some notes on theory} for more on this topic.

#### Harmonic series

We can learn a lot from computing partial sums of the harmonic series,


$$
h(n) = 1 + \\frac{1}{2} +  \\frac{1}{3} + \\cdots +  \\frac{1}{n}
$$

The obvious, seemingly good solution is in fact quite bad:

```
let harmonic (n:i32): f32 =
   reduce (\\acc k -> acc + 1.0/k) 0.0
     (map f32.i32 (1...n))
```

The result in the computation above is correct, and  If one runs this code in the Futhark repl, all may seem fine. But if one compiles and runs it, one can get incorrect results! The reason has to with associatiivy of the reducer: the reduction operator is not associative, so the execution results are undefined. One should write something this instead:


```
let harmonic2 (n:i32): f32 =
   reduce (+) 0.0 (map (1/)
      (map f32.i32 (1...n)))


> harmonic 1000)
7.48447
```

The difference here is in the first argument to `reduce`.  It is a simple addition.

### Loop


[text 20 html#basic-language-features)(Section.2.2.4 of Read-the-Book])
```
let fib(n: i32): i32 =
  let (x, _) = loop (x, y) = (1,1) for i < n do (y, x+y)
  in x
```

See (https://futhark-book.readthedocs.io/en/latest/language.

The semantics of this loop is precisely as in the tail-recursive function formulation. In general, a loop

```
loop pat = initial for i < bound do loopbody
```

has the following semantics:


>>>.1    Bind pat to the initial values given in initial.

.2   While `i < bound`, evaluate loopbody, rebinding pat to be the value returned by
the body. At the end of each iteration, increment i by one.

.3   Return the final value of pat.



### Caveat

One has to be careful!  The code below

```
let harmonic (n:i32): f32 =
   reduce (\\acc n -> acc + 1.0/n) 0.0
     (map f32.i32 (range 1 (n+1) 1))
```

gives incorrect results when compiled to parallel code.  Here is
Troels Henriksen's explanation (and fix):

\\italic{The reduction operator is not associative, so the execution results are undefined.  You should write it like this instead:}

```
   let harmonic (n:i32): f32 = reduce (+) 0.0
     (map (1/) (map f32.i32 (range 1 (n+1) 1)))
```

*This is one of the most unusual restrictions on data-parallel
programming, because it's a concern that you don't have in any other
programming paradigm.  Even worse, it is in general undecidable to
automatically determine associativity of an arbitrary function, so the
compiler doesn't complain about it (although in the long run I'd like to
do a little analysis to find the obvious cases).*

*Even my colleagues and I, who do have some experience by now, get this
wrong from time to time.  Our current practice is to be extremely
sceptical of any nontrivial reduce/scan operator (e.g. anything else
than addition and multiplication) and think very very hard about whether
it really is associative, and has the provided neutral element.}

*In most cases, such as harmonic series, a nonassociative reduction
should be written as a composition of a summation or product, combined
with a 'map' that does input preprocessing.  There's no overhead to
this, because the compiler will still fuse together the 'map' and
'reduce' parts (in a safe way that does not violate associativity).*

There is a further discussion of associatiivty in the section *Some notes on theory*.


### Random numbers

First, som libraries must be installed:

```
$ futhark pkg add github.com/diku-dk/cpprandom
$ futhark pkg sync
```

Now we can do this:

```
$ futhark repl
> import "lib/github.com/diku-dk/cpprandom/random"
> let rng = minstd_rand.rng_from_seed [123]
> module d = uniform_real_distribution f32 minstd_rand

> d.rand (0,1) rng
(281253711u32, 0.13096896f32)
```

See [jxxcarlson/rng-futhark](https://github.com/jxxcarlson/rng-futhark) for a simple random number generator. It is written for the purpose of understanding Futhhark, not for production use.
"""
