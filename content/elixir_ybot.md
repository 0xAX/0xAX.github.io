Title: Using Elixir to write Ybot plugins
Date: 2014-06-01
Tags: elixir
Authors:  Alexander Kuleshov

[Ybot](https://github.com/OtpChatBot/Ybot) - is a customizable bot which was inspired by GitHub's Hubot and written with Erlang programming language. You can create pligin for Ybot in one of following languages:

* Python
* Ruby
* Shell
* Perl
* Erlang/OTP
* Elixir
* Scala

In this post i will tell you to create Ybot plugin with Elixir. For example we need to create Ybot's plugin which will get information about commits from the Github's repository and will send it to chat if somebody updated your repository. We will use Github API v3. For getting info about Github repository commits we must send request:

```
GET /repos/:owner/:repo/commits
```

Let's create Elixir module and send request to the Github API:

```elixir
defmodule GithubCommitsNotification do

    @moduledoc """
        Github commits notification Ybot plugin.
    """

    @repo 'YBOT'
    @author '0xAX'
    @github_api_url  'https://api.github.com/repos/' ++ @author ++ '/' ++ @repo ++ '/commits'

    @ybot_api_url 'http://localhost:8090/memories/'
    @ybot_plugin_api_url 'http://localhost:8090/memories/?plugin=elixir_github_commits_notification'

    :ok = :application.start :inets
    :ok = :application.start :asn1
    :ok = :application.start :crypto
    :ok = :application.start :public_key
    :ok = :application.start :ssl

    # Send request to the Github API
    {:ok, {_, _, body}} = :httpc.request(:get, {@github_api_url, []}, [{:ssl,[{:verify,0}]}], [])
    # parse response
    [resp | _] = JSON.parse(body)
end
```

Here you can see simple Elixir module with some attributes (@repo, @author and etc...), and sending http 'GET' request to the Github commits API. We must get response like this:

```erlang
[
  {
    "url": "https://api.github.com/repos/octocat/Hello-World/commits/6dcb09b5b57875f334f61aebed695e2e4193db5e",
    "sha": "6dcb09b5b57875f334f61aebed695e2e4193db5e",
    "commit": {
      "url": "https://api.github.com/repos/octocat/Hello-World/git/commits/6dcb09b5b57875f334f61aebed695e2e4193db5e",
      "author": {
        "name": "Monalisa Octocat",
        "email": "support@github.com",
        "date": "2011-04-14T16:00:49Z"
      },
      "committer": {
        "name": "Monalisa Octocat",
        "email": "support@github.com",
        "date": "2011-04-14T16:00:49Z"
      },
      "message": "Fix all the bugs",
      "tree": {
        "url": "https://api.github.com/repos/octocat/Hello-World/tree/6dcb09b5b57875f334f61aebed695e2e4193db5e",
        "sha": "6dcb09b5b57875f334f61aebed695e2e4193db5e"
      }
    },
    "author": {
      "login": "octocat",
      "id": 1,
      "avatar_url": "https://github.com/images/error/octocat_happy.gif",
      "gravatar_id": "somehexcode",
      "url": "https://api.github.com/users/octocat"
    },
    "committer": {
      "login": "octocat",
      "id": 1,
      "avatar_url": "https://github.com/images/error/octocat_happy.gif",
      "gravatar_id": "somehexcode",
      "url": "https://api.github.com/users/octocat"
    },
    "parents": [
      {
        "url": "https://api.github.com/repos/octocat/Hello-World/commits/6dcb09b5b57875f334f61aebed695e2e4193db5e",
        "sha": "6dcb09b5b57875f334f61aebed695e2e4193db5e"
      }
    ]
  }
]
```

Now we must get some fields from this response, like a commit's author, commit message and etc...:

```elixir
# get sha
{<<"sha">>, sha} = :lists.keyfind(<<"sha">>, 1, resp)

# get author
{_, commit} = :lists.keyfind(<<"commit">>, 1, resp)
{_, author} = :lists.keyfind(<<"author">>, 1, commit)
{_, name} = :lists.keyfind(<<"name">>, 1, author)

# get commit message
{<<"message">>, message} = :lists.keyfind(<<"message">>, 1, commit)
```

Ybot has an own storage with REST API, thank you to [@tajgur](https://twitter.com/tajgur). You can find documentation for it - here. And also Ybot has notifications support, in other words you can set up Ybot that it will execute your plugin by timeout and send result to you. For example you can configure Ybot that it will send to you status of your system every hour and etc... We have memory API in Ybot and we can get last information about repository commits, check it, save last commit if it changed and will send update to the chat. Remeber that Ybot's plugin must write it's result to the `STDOUT` in the end of execution. Here is the full source code of this plugin:

```elixir
defmodule GithubCommitsNotification do

    @moduledoc """
        Github commits notification Ybot plugin.
    """

    @repo 'YBOT'
    @author '0xAX'
    @github_api_url  'https://api.github.com/repos/' ++ @author ++ '/' ++ @repo ++ '/commits'

    @ybot_api_url 'http://localhost:8090/memories/'
    @ybot_plugin_api_url 'http://localhost:8090/memories/?plugin=elixir_github_commits_notification'

    :ok = :application.start :inets
    :ok = :application.start :asn1
    :ok = :application.start :crypto
    :ok = :application.start :public_key
    :ok = :application.start :ssl

    # Send request to the Github API
    {:ok, {_, _, body}} = :httpc.request(:get, {@github_api_url, []}, [{:ssl,[{:verify,0}]}], [])
    # parse response
    [resp | _] = JSON.parse(body)

    # get sha
    {<<"sha">>, sha} = :lists.keyfind(<<"sha">>, 1, resp)

    # get author
    {_, commit} = :lists.keyfind(<<"commit">>, 1, resp)
    {_, author} = :lists.keyfind(<<"author">>, 1, commit)
    {_, name} = :lists.keyfind(<<"name">>, 1, author)

    # get commit message
    {<<"message">>, message} = :lists.keyfind(<<"message">>, 1, commit)

    result = 'New commit to the repo - ' ++ @repo ++  ' sha: ' ++ :erlang.binary_to_list(sha) ++ ' author: ' ++ :erlang.binary_to_list(name)
             ++ ' message: ' ++ :erlang.binary_to_list(message)

    {:ok, {_, _, body}} = :httpc.request(:get, {@ybot_plugin_api_url, []}, [], [])

    case body do
        '[]' ->
            data = JSON.generate([plugin: "elixir_github_commits_notification",
                                  key: "sha",
                                  value: sha])

            # save new record to Ybot storage
            :httpc.request(:post, {@ybot_api_url, [], 'application/json', data}, [], [])
            # write result to stdout
            :io.format("~p~n", [result])
        _ ->
            {_, val} = :lists.keyfind(<<"value">>, 1, :lists.nth(1, JSON.parse(body)))
            {_, id}  = :lists.keyfind(<<"id">>, 1, :lists.nth(1, JSON.parse(body)))

            cond do
                val == val ->
                    :io.format ""
                true ->
                    # delete old commit
                    :httpc.request(:delete, {@ybot_api_url ++ binary_to_list(id), []}, [], [])

                    data = JSON.generate([plugin: "elixir_github_commits_notification",
                                          key: "sha",
                                          value: sha])

                    # save new record to Ybot storage
                    :httpc.request(:post, {@ybot_api_url, [], 'application/json', data}, [], [])
                    # write result to stdout
                    :io.format("~p~n", [result])
            end
    end
end
```

Put this plugin to Ybot's `notifications` directory and set up it in configuration file:

```erlang
{notification, [
    {github_commits_notification, [irc, twitter], 600}
]},
```

Where:

* github_commits_notifications - plugin name
* [irc, twitter] - list of transports in which Ybot will send report
* 600 - timeout in seconds

You can set up any transport which Ybot supports:

* IRC
* XMPP
* Campfire
* HipChat
* Skype
* HTTP
* FlowDock
* SMTP
* Twitter
