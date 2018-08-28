# Notes

## Interoperability

The simplest test for interoperability is to be able to define a variable in one language and access
it from another. For instance, how would you define an ID for a particular element of the web page?
You could define it, say, in Java, as an integer, and initialize it to a unique number. Now you'd
like to use this number (as a string) in your HTML to identify a marked-up element. Then, in CSS, to
define the style of this element and, occasionally, in your JavaScript code to dynamically change
its properties in response to user actions.

Because Yesod is written in Haskell, which is a great language for embedding domain specific
languages, such horizontal integration is easy. HTML, CSS, and JavaScript are simply embedded in
Haskell code. The code written in these languages is quasi-quoted and then pre-parsed by Haskell.
Because of that last step, it's possible to interpolate Haskell code inside those quasi-quotes. So
it's perfectly natural to define an integral identifier in Haskell and embed it in HTML, CSS, or
JavaScript.

## Template Languages

Yesod uses the Shakespearean family of template languages as its standard approach to HTML, CSS and
Javascript creation (Hamlet, Julius, Cassius, and Lucius). There is nothing inherently tying Yesod to these languages, or the other way
around: each can be used independently of the other.

### Hamlet

[simple example](/demo/templates/profile.hamlet)

[more complicated example](/demo/templates/default-layout.hamlet)

### Julius

[example](/demo/templates/homepage.julius)
