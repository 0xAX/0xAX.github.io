Title: Elixir in action
Date: 2014-04-14
Tags: elixir, erlang
Authors:  Alexander Kuleshov

*NOTE* that development of the `Weber` is finished. And if you're interested in web development with elixirlang, take a look on [phoenix](https://github.com/phoenixframework/phoenix).

As you can know from my previous blog posts (`Using elixir to write ybot plugins`, `Example of usage elixir with cowboy` and `Started to dive into elixir-lang`) some times ago i started to learn and use Elixir language. I'm using elixir not very much time, but already have some feels about it. Practical part of usage of elixir is - [Weber](https://github.com/elixir-web/weber). It is a MVC web framework for Elixir, and yesterday i released [Weber-0.0.1](https://twitter.com/0xAX/status/379319481694048257).

In this post i will try to describe my feels about Elixir.

Mix
--------------

First of all about mix tool. Mix is a first Elixir's tool with which we meet in practice when started to use the elixir.

Mix is a build tool that provides tasks for creating, compiling, testing (and soon deploying) Elixir projects. Mix is inspired by the Leiningen build tool for Clojure and was written by one of its contributors.

We can create new Elixir project with: mix new project_name Build it with: mix compile Handle dependencies: mix deps.get && mix deps.compile And the main mix's feature as i think is a support of custum tasks. Mix task is just a Elixir module with mixed Mix.Task and run function. For example i'm using custom mix task in Weber for creating new project template:

```elixir
defmodule Mix.Tasks.Weber do

    @moduledoc """

       Create a new weber project template.

       Usage:

         mix weber /home/user/myWebApp - Creates myWebApp directory with weber project skeleton.
         mix weber --version - Prints weber version.
    """

    @shortdoc "Create a new weber project"

    use Mix.Task

    @version Weber.Mixfile.project[:version]

    def run([]) do
        usage
    end

    def run(["--help"]) do
        usage
    end

    def run(["--version"]) do
        Mix.shell.info "Weber v#{@version}"
    end

    def run([args]) do
      #
      # Create here new project template
      #
      .....
    end
end
```

OTP
-----------------

As Elixir is built on top of Erlang virtual machine, we can easily to build OTP application with Elixir:

```elixir
defmodule Weber do
    use Application.Behaviour

    def start(_type, _args) do
        Weber.Supervisor.start_link
    end

    def stop(_state) do
        :ok
    end
end
```

Root supervisor:

```elixir
defmodule Weber.Supervisor do
    use Supervisor.Behaviour

    def start_link do
        :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
    end

    def start_app(app_name, routes, root_directory, config) do
        :supervisor.start_child(__MODULE__, [app_name, routes, root_directory, config])
    end

    def init([]) do
      children = [ worker(Weber.App, [])]
      supervise children, strategy: :simple_one_for_one
    end

end
```

And simple gen_server:

```elixir
defmodule Weber.App do
    use GenServer.Behaviour

    defrecord WeberApp,
      name:   nil,
      routes: nil,
      config: nil,
      root:   nil,
      static_dir: nil,
      views_dir:  nil

    def start_link(app_name, routes, root_directory, config) do
        :gen_server.start_link({:local, app_name}, __MODULE__, [app_name, routes, root_directory, config], [])
    end

    def init([app_name, routes, root_directory, config]) do
        :gen_server.cast(:erlang.self(), :init)
        { :ok, WeberApp.new name: app_name,
                            routes: routes,
                            root: root_directory,
                            config: config,
                            static_dir: root_directory ++ '/lib/static/',
                            views_dir:  root_directory ++ '/lib/views/' }
    end

    def handle_cast(:init, state) do
        {:noreply, state}
    end

    def handle_call(:routes, _from, state) do
      { :reply, state.routes, state }
    end

end
```

compile and start it with:

```
$ iex -S mix
```

Testing
-----------------

There is ExUnit! It is a unit test framework that ships with Elixir. Let see in routing functions from the Weber:

```elixir
def on(path, controller, action) do
        [[path: path, controller: controller, action: action]]
    end

    def on(routesList, path, controller, action) do
        :lists.append(routesList, [[path: path, controller: controller, action: action]])
    end

    @doc """
      Router attribute
    """
    def otherwise(path, controller, action) do
        on(path, controller, action)
    end

    def otherwise(routesList, path, controller, action) do
        on(routesList, path, controller, action)
    end
```

And now we can write unit tests for it with ExUnit:

```elixir
 test "Test for Weber.Route.on and Weber.Route.otherwise" do
      r = on("/", 'controller1', 'main_action')
          |> on("/user/0xAX/add", 'controller1', 'action2')
          |> on("/user/:user/delete", 'controller1', 'action2')
          |> otherwise(404, 'controller1', 'notfound')

      assert(r == [[path: "/", controller: 'controller1', action: 'main_action'],
                   [path: "/user/0xAX/add", controller: 'controller1', action: 'action2'],
                   [path: "/user/:user/delete", controller: 'controller1', action: 'action2'],
                   [path: 404, controller: 'controller1', action: 'notfound']
                  ])
  end
```

Libraries
-----------

Although Elixir is now young language, it has good set of libraries:

* [exjson](https://github.com/guedes/exjson) - JSON parser and genarator in Elixir;
* [ecto](https://github.com/elixir-lang/ecto) - A database wrapper and language integrated query for Elixir;
* [httpotion](https://github.com/myfreeweb/httpotion) - The HTTP client for Elixir;
* [amrita](https://github.com/josephwilk/amrita) - A polite, well mannered and thoroughly upstanding testing framework for Elixir;
* [elixir-socket](https://github.com/meh/elixir-socket) - Socket wrapping for Elixir;
* [genx](https://github.com/yrashk/genx) - Elixir-style library for most important OTP functionality;
* [elixir-datastructures](https://github.com/meh/elixir-datastructures) - Datastructures for Elixir.

and many more. Or you of course can use any erlang libraries. Just include it to your mix.exs file as:

```elixir
...
 defp deps do
    [
      {:cowboy, "0.8.6", github: "extend/cowboy"},
      {:ecto, github: "elixir-lang/ecto"},
      {:pgsql, github: "ericmj/pgsql", branch: "elixir"},
      {:exjson, github: "guedes/exjson"}
    ]
  end
...
```

and execute

```
$ mix deps.get
```

In the end of this post i want to say many thanks to the Elixir community which helped me all this day, you're great!
