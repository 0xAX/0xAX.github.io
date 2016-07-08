+++
Categories = ["ghci", "haskell"]
Tags = ["haskell",  "ghci"]
date = "2014-05-11"
title = "GHCi runtime linker found a duplicate definition error"
+++

Despite Haskell programming language is not young language, it has a really great and helpful community, it has big amount of libraries, unfortunately Haskell is not popular programming language. I'm not against this programming language, opposite, haskell's unpopularity makes me sad. I will not write about haskell popularity in industry, I'll try to explain my thoughts about "Why Haskell is not popular" not as Haskell expert, not as professional Haskell developer (i don't get payment for Haskell programming), but from position of usual developer who started to learn/experiment with Haskell some months ago and already don't afraid monads :). I can't So I will try to explain my opinion about Why Haskell is not popular in this post.


Myths
-------------

I'm not long time with Haskell and I don't know all myths/stories/jokes and other funny things about Haskell, I know one popular myth (for my look it's a myth) that Haskell is a difficult. Difficult to learn, difficult to use, not important, it is difficult. I think it's old myth, I remember it before I started to learn Haskell. I think that it is really myth and Haskell can be not so hard as you can think about it. It is not difficult, it is different. If you're using python,ruby,C++,javascript,java,something else... Haskell will be really different. Why it is different? There are many reasons for this like: another programming paradigm, lazy evaluations, different concepts and etc... But of course it is not a big problem and relatively easy to solve. Developer just need to spend more time to learn it than with another programming languages like python,javascript and etc... (Except C++ of course :))

Lack of documentation
-----------------------

The Lack of documentation is a first problem in this list that related with practical side of Haskell usage. I don't mean that Haskell as language has bad documentation, it is not true. I'm speaking about Haskell libraries documentation. It's the one of big problem that stands on the road to Haskell popularity. Sooner or later after start of Haskell learning developer will want to use libraries written in Haskell. We can easily find library by name, or functions/data types name with Hoogle, it is good, but how to use this libraries if developer doesn't program in Haskell a couple years and library has no or has but bad documentation. For example some time ago I played with WAI and i was need in websockets. Fortunately WAI has [wai-websockets](http://hackage.haskell.org/package/wai-websockets-3.0.0) package but let's look on it's [documentation](http://hackage.haskell.org/package/wai-websockets-3.0.0/docs/Network-Wai-Handler-WebSockets.html). How to use it? I don't know how about you, but I don't understand. I see only one way out of this: to read [wai-websockets](https://github.com/yesodweb/wai/blob/master/wai-websockets/server.lhs) source code or examples. Is it good? I'm not sure.

Standard Library
------------------

Now let's talk about Haskell's standard library. On my look it has many really useful things for haskell and has really little amount things for Real World play. What it means when I am telling about Real World. It's simple, i mean that Haskell standard library has many things like Control.Category, Control.Arrow, Data.Typeable and etc... Again, I don't know how is it for you, but for me and I think other newbie Haskell developers: Category, Arrow and other magic words are just words without any meaning. Not, i know Arrows, Monads are very useful in Haskell, but where is the something like Network.TcpClient, Network.HttpServer and etc... I know that Haskell has separate libraries for TCP, HTTP and other network and not only network things. But imagine, for exmaple I just started with Haskell and I want to write simple example like sending HTTP request and getting response, i need to understand where to find library for this, how to install it and etc... I don't speak that Haskell standard library must have all things for all case, but things like http client in stdlib is a standard case. Or i'm wrong? Let's look on golang for example. I think that it has a perfect standard library. Look on it and haskell standard library, do you feel difference? Golang is only five years and Haskell is 24, so big difference. Of course I have no statistic, but I see that golang is much popular for this moment. I don't think that it is main reason but one of.

Why to learn
--------------

I think it's not only Haskell problem, but other languages too. I see only one answer for this question: To get/improve knowledges in functional programming and look on your working programming language with other eyes after it. Yes, getting new knowledges it is very good. But what about practical side? I know that somewhere developers who get money for Haskell programming, but I don't see tons of vacancies for Haskell developers like for javascript, ruby or other tools and it is problem. Let's look in another side. For example i want to start learn Haskell for my super-cool pet project. But why Haskell in this case? For web development I can take habitual ruby/python/php, for system programming I can take C/C++ or maybe Rust, for concurrent programming I will choose erlang. So why i need to learn new programming language and in addition so different from my standard tools?

Lazyness
-----------

When we start to learn Haskell we can read something like this: Haskell - general purpose programming language with non-strict evaluation. I think that many developers know about lazy evaluations, but I am really not sure that all of they knows how it works, how to correctly use it and etc... In this way, I as beginner in Haskell must learn not only another programming paradigm, but also another evaluation order. It is much harder, because it is much implicit. For example let's take a look at popular ByteString library. It provides two implementations lazy strings and strict. But I still don't know where to use first and where to use second.

Different abstractions
-----------------------

It is problem of Haskell learning. Haskell uses different abstractions than other programming languages. And if you know python for example it will be much easy to learn ruby than Haskell. Let's look on simple echo example. You need to read input from stdin and prtin this string again. How we do it with python language:

```golang
import sys
data = sys.stdin.readlines()
print data
```

All is transparent enough. We do this task with all imperative programming languages in this way. First we reading from stdin and put result to a variable and than pass this variable to the printing function. Let's look at the same Haskell example:

```haskell
main = do
  getLine >>= putStrLn
```

Ok. Developer can guess about getLine and putStrLn, but what is it >>=. If we open documentation we will read something like this: >>= combine two monadic values... "combine two monadic values...". What is it Monad, How to use Monad and many many different questions with not easy answers sometimes. And it is only monads (concept which standard developer could not hear never), but there are many different concepts like Functors, Comonads and many many others which you can't meet in standard programming languages.

Conclusion
-----------

So it was a short list of my thoughts why Haskell is not popular. I am very interesting what do you think about Haskell popularity.
In the end I want to remind that all from this post only my opinion and if you're agree or disagree with me write me a comment.
