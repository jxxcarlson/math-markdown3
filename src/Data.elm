module Data exposing (loadingPage, rhsUserText)

import Utility exposing (getId)


loadingPage =
    { id = getId 1
    , title = "Notes on Futhark"
    , authorIdentifier = "jxxcarlson"
    , content = loadingPageText
    , tags = [ "system" ]
    , public = True
    , slug = "loading"
    }


loadingPageText =
    """# Loading ...


![Hummingbird](https://www.allaboutbirds.org/guide/noindex/photo/60395551-1280px.jpg)
Please stand by.  We are loading your documents

$$
\\int_{-\\infty}^\\infty e^{-x^2} dx = \\pi
$$

"""


rhsUserText =
    """
## Welcome!

![Hummingbird](https://www.allaboutbirds.org/guide/noindex/photo/60395551-1280px.jpg)
Hummingbird (Meditation)


This little app  demonstates what one can do with the pure Elm markdown
library [jxxcarlson/elm-markdown](https://package.elm-lang.org/packages/jxxcarlson/elm-markdown/latest/).
The idea is to be able to do Markdown + Math, like this:

$$
\\int_{-\\infty}^\\infty e^{-x^2} dx = \\pi
$$

Formulas written in TeX/LaTeX are rendered using [MathJax](https://mathjax.org)

We are growing this app into a content management system which
can be useful both for personal notes and classroom materials. Lecture note,
problem sets, and the like.  We also plan to add live editing and rendering
of LaTeX documents (MiniLaTeX flavor).

For more information, click the **Read** tab and take a look at the article *A Pure Elm Markdown Parser*.
"""
