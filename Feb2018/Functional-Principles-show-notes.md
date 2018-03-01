# Teaching Functional Programming

## 1. Why Does Functional Programming Matter?

Programmers want to be more productive, sometimes obsessively so. There are literally questions on Quora and StackOverflow about which font size will make me the most productive. Functional programming has made us at ITProTV more productive, in a matter of months. We adopted Elm in October, and Haskell in late November. Before that, we were primarily imperative, JavaScript developers, but at this point, if we had to estimate the exact same story using Haskell or using JavaScript, we would all throw lower numbers for Haskell. And that's just 3 months! Imagine how much more productive we're going to feel in a year.


Functional programming is going to make you more productive through a variety of means, and we'll scratch the surface on many of them right now. I'm going to list common characteristics of functional languages, then we'll go into more depth on each.

1. Pure functions
1. And therefore, memoization
1. Type Signature
1. Lazy Evaluation
1. Immutability
1. And therefore, parallelism

While object-oriented languages are not necessarily imperative (think of the changes JS has gone through over the last few years), OOP and imperative paradigms tend to go together, while functional programming tends to be separate.

### Pure Functions

A pure function has 2 characteristics: 1) it always evaluates the same result value given the same argument value(s), and 2) it does not cause side effects when evaluated.

Any function that uses a non-local variable is potentially impure, for example `increment x = x + a`. The value of `increment` depends on the value of `a`, which could be anything at any point in the program.

A side effect is the modification of some kind of state, such as changing the value of a variable.


Pure functions are easier to think about because you can assume they won't make changes to anything else. You can trust your functions, unlike in OOP where a function can, and often must, generate side effects, or depend on state that may change before the next time you evaluate it.


### Memoization

Since there are no side effects, we are guaranteed to get the same result no matter how many times we evaluate a function with the same arguments. This means that the compiler can cache the result of a function after it is called the first time. This can result in some huge speed increases, especially when your program calls an expensive function several times.

Here's an example of implementing memoization in JavaScript, stolen from http://inlehmansterms.net/2015/03/01/javascript-memoization/:

```JavaScript
function memoize(func) {
  var cache = {};
  return function() {
    var key = JSON.stringify(arguments);
    if(cache[key]) {
      return cache[key];
    }
    else {
      var val = func.apply(this, arguments);
      cache[key] = val;
      return val;
    }
  };
}

var factorial = memoize(function(num) {
  console.log('working for factorial(' + num + ')');
  if(num === 1) { return 1 };
  return num * factorial(num - 1);
});

// first call
console.log(factorial(3));
//=> working for factorial(3)
//=> working for factorial(2)
//=> working for factorial(1)
//=> 6

// successive calls
console.log(factorial(3)); //=> 6
console.log(factorial(3)); //=> 6

// short circuit higher factorial calls
console.log(factorial(4));
//=> working for factorial(4)
//=> 24
```


This doesn't necessarily increase your poductivity, but it's pretty cool. And you get it for free in many functional languages, you don't have to create your own `memoize` function like we did in the above JavaScript example.

### Type Signature

For Haskell, a type signature looks like this first line,

```Haskell
exampleFunction :: Int -> Int -> Int
exampleFunction x y = x * y
```

It takes a little bit of time to learn how to read this, but once you do, you'll realize that type signatures make code clearer and safer. In this example function, it's telling you that it takes 2 integers and returns an integer. So let's break down the type signature. The first thing in the type signature is the function name, and then the double-colon. All type signatures start like that. But what comes after the double-colon will change from function to function. To keep things simple, I am skipping a little bit, but for now let's say that the first thing that comes after the double-colon is the first argument to the function, and then a right arrow, then another argument, another arrow, and so on. The very last thing in the type signature is the value that the function will return when executed.

 That's pretty simple, but in Haskell you can define new types, such as `Age`, and say that it's a non-negative integer less than 120. Then you can do something very 'safe', like this:

```Haskell
ageInMonths :: Age -> Int
ageInMonths age = age * 12
```

The type signature is telling you that it takes an Age, not an integer, and returns an integer.


This function is guaranteed to be safer than a function that takes an Int, or worse, any type, like in JavaScript. And it's easy to see at a glance what the function is doing. You don't need as much documentation, and you don't need nearly as much testing, which will make you more productive.

### Lazy Evaluation

```haskell
fibGen :: Int -> Int -> [Int]
fibGen x y = x:fibGen y (x + y)

-- this creates an infinite list with all fibonacci numbers
allFibs = fibGen 1 1

-- lets print the first 10
take 10 allFibs
-> [1,1,2,3,5,8,13,21,34,55]
```

vs.

```java
class FibGen : Iterator<Integer> {

	private int x;
	private int y;

    Iterator (int x,  int y) {
		this.x = x;
		this.y = y;
    }

    public boolean hasNext() {
		return true;
    }

    public Integer next() {
		int x0 = x;

        x = y;
        y = (x0 + y);

		return x0;
    }

	void remove()
	{
		throw new UnsupportedOperationException();
    }
}

...
FibGen fib = new FibGen(1, 1);
for (int i=0 ; i< 10;  i++)
{
    System.out.println(fib.next());
}

```

That is a LOT less code, much easier to create, explain, and think about. That's going to make you more productive.

### Immutability

De-emphasises code that modifies data and instead focus on functions that take immutable values as input and produce new values as output.

Consider this code:

```Python
a = [1, 2, 3]

a.reverse()

a # [3, 2, 1]

b = a.reverse()

a # [1, 2, 3]

b # (No output)
```

Why would you lie to the computer like that?

```Haskell
let a = [1, 2, 3]

reverse a -- [3, 2, 1]

a -- [1, 2, 3]
```


First, it's easier to think through code when you don't have to try to figure out what the value of a variable is at any given point in the code. You don't do as much mental juggling on the unimportant stuff, so you'll create less bugs, and your code is more maintainable. It's also much more difficult to create an invalid state, where a variable is referenced and yet it doesn't exist yet, for instance. You'll have an easier time testing because of this, as well.

### Parallelism

The last in our list (which remember, is not exhaustive), is parallelism. What I mean is, running multiple threads at the same time. Processor speeds have started to plateau recently, and unless something changes, it's difficult to see how processors will get much faster. Instead, we're going to have to rely on multi-core processors. But very few apps are making good use of more than one core at a time, and part of the reason is that it's note easy to get imperative programming to play nice with multiple cores. Functional programming, on the other hand, plays very nice due to parallelism.

Parallelism is easier in a functional language because they usually include two characteristics that we've already talked about: no side effects, and immutability. If there are no side effects and everything is immutable, it doesn't matter if things are done in a different order.

This is another characteristic, like memoization, that doesn't necessarily make you more productive, but it is a really powerful feature of functional programming. But let me make a caveat: fully automatic parallelization by a compiler is still a ways off, though functional languages are better equip to make use of it, and are already making limited use of automatic parallelization

#### Extending existing programs

(I totally ripped this from a Quora questions)

One other fundamental difference involves extending an existing program. To see this, suppose you want to model something that appears in multiple different forms; say, a spaceship in a game that features a bunch of different kinds of spaceships, each with different characteristics and capabilities.

A traditional functional approach might be to define a data type with one variant for each kind of ship. Then, to implement the ships' behavior, you'd write some functions that operate on instances of that ship data, and most of those functions will have some conditional branching to determine what kind of ship they've received as input.

A traditional OO approach might be to define an interface or superclass that specifies what actions one can perform on a ship, and then define one subclass for each kind of ship in the game. To implement the ships' behavior, you'd write methods in each subclass specifying how the ships behave differently, and likely some methods in the superclass for behaviors that all ships have in common. (Some functional languages support subtyping of struct-like data that lets you accomplish the same thing.)

Suppose you want to add a new kind of ship. In the functional program, you'd have to add a new variant to your data type, then find all your functions that take ships as input, and modify each function to deal with the new variant. This is potentially a lot of work, if you haven't assiduously grouped all those functions in one place or kept a list. [But you _have_ grouped those functions in one place because you structured your code well. Also, the compiler is going to complain and point out all the places that no longer handle all of the possible branches, so you just follow where the compiler is complaining, fix it, and you're finished. It's like having high-quality tests already written for you] In the OO program, you'd write a new subclass, and implement each method required by the interface. That's likely easier.

Suppose you want to add a new capability to the ships -- say, a way to upgrade a ship's weapons during the game. In the OO program, you'd add a method to the superclass if you're lucky and all ships should have the same behavior; if they shouldn't, then you need to add a method to each ship's subclass. In the functional program, you'd write a new function that deals with all the variants listed by the definition of your data type. That's likely easier.

## 2. What are some of the drawbacks with Functional Programming?

It can be difficult to learn because it's a different paradigm than imperative programming, and if you're not coming from a math background it can be confusing. There are terms like `monad`, `functor`, etc. Consider this fun sentence, `All told, a monad in X is just a monoid in the category of endofunctors of X, with product × replaced by composition of endofunctors and unit set by the identity endofunctor.` If any of you understand that sentence, you're hired. I don't understand it yet.

But these are all subjective things that come down to people's experience and preference. Some of the actual issues of functional programming include the fact that pure functions and IO don't mix. By definition these two things are mutually exclusive. So now you're stuck in a place where you either have to use a functional language to deal with IO, which can sometimes feel like digging a ditch with a ball-peen hammer, or not dealing with IO, which is not easy. And the last issue is that using a lot of immutable values and recursion can use up enough RAM that it causes some real slowdown.

Beyond these two issues, however, anything else is language-specific.

So I hope I was able to communicate how functional programming will make you more productive as a developer. Functional languages have several characteristics, including pure functions, memoization, type signature, lazy evaluation, immutability, and parallelism, that make it easier to write concise, bug-free code that is easier to reason about.
