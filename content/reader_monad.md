Title: Reader monad understanding
Date: 2014-03-04
Tags: haskell
Authors:  Alexander Kuleshov

Some time ago I started to learn Haskell. It's not the first time (and event not second) that I started to learn this language. Every time I met different problems with understanding different abstractions like a Monads, Arrows, Monad Transformers and etc... I think that I'm not alone with this :) But, in this time, I decided not to throw up training until I can not say: yes I can write in Haskell without pain. I started (as every time i did it) with the most popular book (for newbies) about Haskell - Learn You a Haskell and stopped at Reader monad. Spending some time I understand it (at least I think that I understand :)) and decided to share my understanding of this monad for the same newbie haskellers as me.

Let's imagine simple and ubiquitous case that we have application which needs in some initial configurations. Application will get configuration form configuration file, let it be conf.js with very simple structure like this:

```javascript
{
  "username" : "0xAX"
}
```

It just has 1 `username` key. Of course it has very little resemblance with the real application configuration, but it is not important now and enough for the example. We will read it with great Aeson library from Bryan O'Sullivan and use it in our application. Let's start with reading configuration file content and decoding json.

Aeson
------------

For the start we must define data type for mapping json data in it with Aeson. As you can see above, we have simple json structure with username key. So we will map this json structure to the following data type:

```haskell
data ConfigStructure =
  ConfigStructure {
           key1 :: String,
           key2 :: String
         } deriving(Generic)
```

The `ConfigStructure` has the same structure as our json, it has same fields: username. After data type creation we must read our configuration file with json data and decode it with aeson:

```haskell
configPath :: String
configPath = "conf.json"

getConfigContent :: IO ConfigStructure
getConfigContent = do
  configContent <- BSL.readFile configPath
  let Just decodedConfig = decode configContent :: Maybe ConfigStructure
  return decodedConfig
```

Here you can see that we reading json file with readFile function from Data.ByteString.Lazy. It has following type:

```
    Prelude> import qualified Data.ByteString.Lazy as BSL
    Prelude BSL> :t BSL.readFile
    BSL.readFile :: FilePath -> IO BSL.ByteString
```

where FilePath is just String. Read configuration file and get it's content. After getting content of configuration file using Data.Aeson.decode function for decoding json data from it. After successful decoding we get Just ConfigStructure and now we can build Config data type for Reader and pass configuration file content to it.

Reader Monad
----------------

And now we come to the main point of this post. Main point of Reader monad to share read-only environment between couple of functions. Let's see how Reader declared in Control.Monad.Reader:

```haskell
newtype Reader r a = Reader { runReader :: r -> a}
```

We can see that it made as wrapper for runReader function with type r -> a where:

* `r` - is a reader which need to run and get final value from it;
* `a` - is an environment which need to share

Another words we can build abstraction that will share configuration file content between different functions. Let's see how to implement it.

```haskell
type Config = Reader ConfigStructure String
```

Here we can see Config type synonym for Reader ConfigStructure String. Here ConfigStructure (see above) will be environment which we will share between functions and String return type. Now we write function for getting value of username field from configuration file:

```haskell
getKeyOne :: Config
getKeyOne = do
  config <- ask
  return (key1 config)
```


As you can see it's pretty simple. Here is only one thing which we don't know - ask function. ask function retrieves the monad environment. Pretty simple. Now we can get username in any place of our application with runReader function. As you can remember runReader function has following type:

```haskell
runReader :: r -> a
```

Where r is a reader which need to run (getKeyOne in our case) and a is environment (configuration file content in our case). Again pretty simple. We can get value of username with:

```haskell
-- read configuration file
decodedConfig <- getConfigContent
-- get & print value of key1
putStrLn $ runReader getKeyOne $ decodedConfig
```
