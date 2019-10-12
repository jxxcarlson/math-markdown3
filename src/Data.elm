module Data exposing (loadingPage, rhsUserText)

import Document exposing (DocType(..), Document, MarkdownFlavor(..))
import Utility exposing (getId)


loadingPage : Document
loadingPage =
    { id = getId 1
    , title = "Notes on Futhark"
    , authorIdentifier = "jxxcarlson"
    , content = loadingPageText
    , tags = [ "system" ]
    , public = True
    , slug = "loading"
    , docType = Markdown MDExtendedMath
    , children = []
    , childLevels = []
    , childInfo = []
    }


loadingPageText =
    """# Loading ...


![Hummingbird](https://www.allaboutbirds.org/guide/noindex/photo/60395551-1280px.jpg)
Please stand by.  We are loading your documents

$$
\\int_{-\\infty}^\\infty e^{-x^2} dx = \\pi
$$

"""


image =
    """
  ![Hummingbird](http://noteimages.s3.amazonaws.com/jxxcarlson/hummingbird.jpg)
         Hummingbird (Meditation)
"""


rhsUserText =
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
