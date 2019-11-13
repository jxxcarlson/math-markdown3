module Data exposing (loadingPage, rhsUserText)

import Document exposing (DocType(..), Document, MarkdownFlavor(..))
import Utility exposing (getId)


loadingPage : Document
loadingPage =
    { id = getId 1
    , title = "Welcome!"
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
    """# Welcome!

## Getting started

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
We will soon add the [MiniLaTeX](https://demo.minilatex.app/)    document format (a subset of LaTeX).

## Getting Started

- Press the **Read** button, upper left, to read public documents without signing in.

- To create and edit documents, please sign in.  You have to have previously created
  an account by signing up.

## Some links

- [User manual](#id/6ee8484c-0688-4a44-8648-e02288a26a6e)

- [Sample document](#id/0089c6c9-aa54-4c42-8844-c821d2684c9c)

If a document title is rendered in blue and is preceded by a plus sign,
click on it to reveal its contents.

## Plans

This app is a work in progress and is under active development.  Eventually
it will become a paid service for authors, but at a very, very low annual 
membership fee.  If an author elects to not renew their membership, their
documents remain in the system, whether they be public or private.

    
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
