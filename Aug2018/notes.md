# Elm and CSS

There are a few ways to do CSS in your Elm app, some obvious, some less obvious. Tonight we're going to look at one of the bad options :)

First, the good options.

## Good Options

1. Use CSS the 'normal' way, with one (or many) .css files that you reference in your html file.
1. Use elm-bootstrap, elm-mdl (Material Design Lite), or another design-library package
1. style-elements
1. Brutalism

1. Show the code briefly, explain to anyone unfamiliar with Elm what's going on.
1. Show the DOM, how it's rendered as inline styling.
1. Read quote below.
1. Add a few more CSS functions (the ones below -----)
1. Show the DOM again, how it's getting cluttered.
1. Combine CSS functions into a single function.
1. Explain that CSS isn't too bad these days, you don't get type safety but you don't get real type safety this way either. At least with CSS most text editors/IDEs will tell you what's valid (demonstrate).
1.

> HTML is meant for conveying structured information. CSS is built to style that structured information. When inline styles are used, this clear separation between structured information and styling is blurred. By separating the CSS from the HTML, the markup can be semantic, which means that it can convey as much meaning as possible without being muddled by visual effects. -- [Codecademy Article](https://www.codecademy.com/articles/html-inline-styles)
