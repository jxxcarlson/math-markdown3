module Data exposing (loadingPage, rhsUserText)

import Document exposing (DocType(..), Document, MarkdownFlavor(..))
import Utility exposing (getId)


loadingPage : Document
loadingPage =
    { id = getId 1
    , title = "Getting Started"
    , authorIdentifier = "jxxcarlson"
    , content = loadingPageText
    , tags = [ "system" ]
    , public = True
    , slug = "firstPage"
    , docType = Markdown MDExtendedMath
    , childInfo = []
    , permissions = []
    }


loadingPageText =
    """# Getting Started

- Use the search box (upper right) to search for public documents by word
  or fragment of a word in the title.  Try *phys*.
  Or just press the **All** button.

   - You can search by keyword (tag).  Try *notebook/k*

   - Use the buttons **R** and **A** to switch between sort by most recent and sort by alphabetical order.

- For help (documentation), press the **Help** button, upper right.


## Creating and Editing Documents

For the moment, the available document format is Markdown, with the possibility of
writing mathematical text using TeX/LaTeX:

$$
\\int_{-\\infty}^\\infty e^{-x^2} dx = \\pi
$$

We will add MiniLaTeX, a subset of LaTeX, in the near future.

To create and edit documents, you must have set up an account
and be signed in to it.  Then an **Edit** button will appear
to the left of the **Read** button.  For editing shortcuts
and more information on editing
please see the [manual](#id/6ee8484c-0688-4a44-8648-e02288a26a6e).

## Sharing documents

Documents can be public or private.  To send get a link to a
public document, press the button **Share** in the footer.

"""


image =
    """
  ![Hummingbird](http://noteimages.s    3.amazonaws.com/jxxcarlson/hummingbird.jpg)
         Hummingbird (Meditation)
"""


rhsUserText =
    """
# Welcome!

## About this app

This app is a platform for creating, editing, and publishing
documents written in Markdown, with the possibility
of writing mathematical formulas:

$$
\\int_{-\\infty}^\\infty e^{-x^2} dx = \\pi
$$

Formulas written in TeX/LaTeX are rendered using [MathJax](https://mathjax.org).
Documents may be written in Markdown or MiniLaTeX, a subset of LaTeX.

## Getting Started

- To browse without signing in: press the **Read** button, upper left.


- To create and edit documents, please sign in.  You have to have previously created
  an account by signing up.

## Some links

It is easy to make links to documents on the system, e.g.,

- [User manual](#id/6ee8484c-0688-4a44-8648-e02288a26a6e)

- [Art notebook](#id/f3b333ff-377b-470f-b737-3bb0b30333f3)

- [Draft physics book (very rough, markdown)](#id/0089c6c9-aa54-4c42-8844-c821d2684c9c)

- [QFT Notes (MiniLaTeX)](https://math-markdown.netlify.com/#id/b8c37c0b-bbcf-4737-bbf7-cfc80f47bc0b)

If a document title is rendered in blue and is preceded by a plus sign,
click on it to reveal its table of contents.

## Plans

This app is a work in progress and is under active development.  Eventually
it will become a paid service for authors, but at a very, very low annual 
membership fee.  If an author elects to not renew their membership, their
documents remain in the system, whether they be public or private.

If you have comments or questions, please contact Jim Carlson:
jxxcarlson (gmail).

## Technical stuff

We use custom parsers for  [Markdown](https://package.elm-lang.org/packages/jxxcarlson/elm-markdown/latest/)
and [MiniLaTeX](https://package.elm-lang.org/packages/jxxcarlson/meenylatex/latest/), both
written in [Elm](https://elm-lang.org).


![Hummingbird](http://noteimages.s3.amazonaws.com/jxxcarlson/hummingbird.jpg)
         Hummingbird (Meditation)

## Thanks

I wish to acknowledge the generous help that I have received throughout this project from
the Elm and MathJax communities,  ith special thanks to Ilias van Peer (@ilias),
Luke Westby (@luke), Davide Cervone (MathJax.org), and Evan Czaplicki (@evancz).


___

"""


rhsUserText2 =
    """
# Welcome!

## About this app

This little app  demonstrates what one can do with the pure Elm markdown
library [jxxcarlson/elm-markdown](https://package.elm-lang.org/packages/jxxcarlson/elm-markdown/latest/).
The idea is to be able to do Markdown + Math, like this:

$$
\\int_{-\\infty}^\\infty e^{-x^2} dx = \\pi
$$

Formulas written in TeX/LaTeX are rendered using [MathJax](https://mathjax.org)

We are growing the app into a content management system which
can be useful both for personal notes and classroom materials: problem sets, handouts, lecture notes,
and the like.  We also plan to add  live editing and rendering
of LaTeX documents (MiniLaTeX flavor), as well as the ability to convert Markdown documents to MiniLaTeX.

For more information, click the **Read** tab (or do ctrl-R).  Some public articles to look at:

-  *A Pure Elm Markdown Parser*
- *Keyboard commands*
- *Search and Sort*
- *News and Plans*

## Tips

### Getting info/help

To find the documents for this app, put `docs/k` or `help/k` in the search box.  The `k` is for *keyword search*.

### Reading documents

 To read documents, press the **Read** button or do `ctrl R` .  More about keyboard shortcuts in the docs.  See previous tip.

### Saving documents

 When you are editing a document, it is saved every four seconds if it has changes.  Look at the red/green indicator in the footer. To force a save, press **Save** or do `ctrl-S` .

## News

### Images

 Use the model `![TITLE::left](IMAGE LOCATION)` to float an image on the left.

To float on the right use `![TITLE::right](IMAGE LOCATION)`

The plain vanilla version, `![TITLE](IMAGE LOCATION)` scales the image to 100%.

For a smaller centered image, use `![TITLE::center](IMAGE LOCATION)`

## Plans

### LaTeX

Add MiniLaTeX, as subset of LaTeX, as a document format.  You will be able to convert Markdown documents, along with their mathematical formulas, to MiniLaTex.  You will also be able to export MiniLatTeX documets.

"""
