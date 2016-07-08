Title: Elixir + Cowboy
Date: 2014-03-22
Tags: elixir, http, cowboy
Authors:  Alexander Kuleshov

As you can know from previous blog post i started to learn/use Elixir language for my hobby project. Elixir is programming language which built on top of Erlang virtual machine and we can use libraries which was written in Erlang. In this post i will try to tell how to use  Elixir with Cowboy web server. You can find some different examples in the Internet about usage Elixir with cowboy:

* [elixir_cowboy](https://github.com/clofresh/elixir_cowboy)
* [elixir-cowboy-example](https://github.com/dry/elixir-cowboy-example)
* [elixir_hello_world](https://github.com/ninenines/cowboy/tree/master/examples/elixir_hello_world)
* [ws-elixir](https://github.com/alco/ws-elixir)

I will tell how to use Elixir with :cowboy_static handler. First of all we must create Elixir project with mix:

```
mix test
```

The `mix` util will generate Elixir project skeleton. Now let's make frontend part. Create priv directory in the our project root directory. And there index.html, js and css directories. I put my favorite Angular.js and bootstrap.css to the js and css directory, but you of course can use any js/css libraries which you will want. Now add simple html template:

```html
<!DOCTYPE html>
<html ng-app>
  <head>
    <meta charset=utf-8>
    <title>Elixir and cowboy</title>
    <link rel="stylesheet" type="text/css" href="css/bootstrap.css">
  </head>
  <body ng-controller="TestController">
    <button type="button" class="btn btn-primary">Default</button>
    <ul ng-repeat="n in nums">
      <li>{{n * 2}}</li>
    </ul>
  <script type="text/javascript" src="js/angular.min.js"></script>
  <script type="text/javascript" src="js/testJS.js"></script>
  </body>
</html>
```

And set up routing in cowboy and start it. Open `lib/testElixirWithCowboy.ex` and add there:

```elixir
defmodule TestElixirWithCowboy do
    use Application.Behaviour

    def start(_type, _args) do
        dispatch = :cowboy_router.compile([
                {:_, [{"/css/[...]", :cowboy_static, [
                        {:directory, {:priv_dir, :testElixirWithCowboy, [<<"public/css">>]}},
                        {:mimetypes, [{<<".css">>, [<<"text/css">>]}]}
                      ]},

                      {"/js/[...]", :cowboy_static, [
                        {:directory, {:priv_dir, :testElixirWithCowboy, [<<"public/js">>]}},
                        {:mimetypes, [{<<".js">>, [<<"application/javascript">>]}]}
                      ]},

                      {"/[...]", :cowboy_static, [
                        {:directory, {:priv_dir, :testElixirWithCowboy, [<<"public/">>]}},
                        {:mimetypes, [{<<".html">>, [<<"text/html">>]}]},
                      ]}
                     ]}
            ])

        {:ok, _} = :cowboy.start_http(:http, 100, [port: 8080], [env: [dispatch: dispatch]])
        TestElixirWithCowboySup.start_link
    end
    def stop(_state) do
        :ok
    end
end
```

And run application with executing:

```
iex -S mix
```
