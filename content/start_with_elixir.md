Title: Getting started with elixir
Date: 2014-05-10
Tags: elixir
Authors:  Alexander Kuleshov

Some times ago i decided to start new hobby project and in front of me had the task which instrument to choose for this task implementation. From the header you can guess that i decided to use Elixir. Why Elixir? The main reason is simple: I like Erlang and all it's infrastructure, but in the same time i want to try something new and i think Elixir the best candidate for it. Now more details why i choose Elixir.

First of all quote from Elixir site:

```
Elixir is a functional, meta-programming aware language built on top of the Erlang VM. It is a dynamic language with flexible syntax and macro support that leverages Erlang's abilities to build concurrent, distributed and fault-tolerant applications with hot code upgrades.
```

Yes, meta-programming built on top of Erlang VM, very good start, what we have at the current moment.

1. Familiar to us patter matching:

```elixir
iex(1)> a = 5
5
iex(2)> 5 = a
5
iex(3)> list = [1,2,3]
[1, 2, 3]
iex(4)> [_, _, third] = list
[1, 2, 3]
iex(5)> third
3
```

2. Immutability:

```elixir
list = [1,2,3,4,5,6]
[1, 2, 3, 4, 5, 6]
iex(2)> list2 = [0 | list]
[0, 1, 2, 3, 4, 5, 6]
```

the `list` will never change, so it simply constructs a new list with a head of 0 and a tail of list.

3. Familiar and extended data types. Elixir’s built-in types are:

• Value types:
  – Arbitrary-sized integers
  – Floating point numbers
  – Atoms
  – Regular expressions

• System types:
  – PIDs and Ports
  – References

• Collection types
  – Lists
  – Tuples
  – Binaries

And also: Keyword Lists, Ranges, Dictionaries, Maps and etc.

4. High Order functions

```elixir
iex(1)> f = :lists.map(fn elem -> elem * 5 end, [1,2,3,4,5])
[5, 10, 15, 20, 25]
```

5. The Amazing Pipes Operator

```elixir
iex(1)> import Enum
nil
iex(2)> (1..10) |> map(&1*&1) |> filter(&1 < 40)
[1, 4, 9, 16, 25, 36]
```

6. Of course [OTP](http://www.erlang.org/doc/design_principles/users_guide.html):

```elixir
defmodule OtpServer do
  use GenServer.Behaviour

  def init(current_number)
    when is_number(current_number) do
      { :ok, current_number }
    end

  def handle_call(:next_number, _from, current_number) do
    { :reply, current_number, current_number+1 }
  end
end
```

And many many more...
