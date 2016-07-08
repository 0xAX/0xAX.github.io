Title: Implementing tcp socket acceptor pool with golang
Date: 2014-04-06
Tags: golang
Authors:  Alexander Kuleshov

So, as maybe some of you knows that i have started to use Golang programming language for my pet projects and since to solve "Project Euler" tasks is very useful deal, but very boring in the same time, i decided to create something more closer to real life. I decided to write XMPP server in golang and in this post i will tell about one part of this server - netpool/tcp library. netpool/tcp - is a tcp based socket acceptor pool. The idea is very simple, you will launch TCP listener which will launch many acceptors besides one, in this way you no need to spend time for creating new connection acceptor.

First of all let's create new tcp package and import all libraries which will we need in this project:

```golang
package tcp

import "io"
import "log"
import "net"
import "bufio"
import "strconv"
import "crypto/tls"
import "crypto/rand"
import "net/textproto"
```

Now let's define types which we will need in our acceptor pool:

```golang
const (
    RefuseConnection = iota
    IncreaseConnection
)

const defaultIncreaseAcceptors = 50

//
// Listener instance
//
type Listener struct {
    Accnb int
    Port  int
    Handler Handler
    Lc    chan string
    OverFlowStrategy int
    Ssl   map[string]string
}

//
// Connection handler
//
type Handler func(string, Connection)

//
// Connection wrapper
//
type Connection struct {
    Conn  net.Conn
    Write chan []byte
    Quit chan bool
    Listener *Listener
}
```

Let's go by order. First enumeration defines strategies when connection listener handles more connections than we define.

* `RefuseConnection` - listener will refuse connections after connections amount overflow;
* `IncreaseConnection` - listener will start yet another 50 acceptors.

The next is simpe constant value which defines how many acceptors need to start after connections amount overflow. Next is `ListenerStruct` structure. It is a main structure which we will pass to StartListener function for launching new listener. It has following fields:

*  `Accnb` - number of acceptors;
*  `Port` - connection port;
*  `Handler` - function with 2 parameters: input string and conn tcp.Connection. It will calls when client send something to server;
*  `Lc` - listener channel, it needs for communicating with listener;
*  `OverFlowStrategy` - It can be 0 || 1, see note about overflow strategies;
*  `Ssl` - it is a map, we need to use it if we want to use ssl. Just create map with 2 string keys: pam and key. Next if will show how to use it.

Also it has API which consists now only from one function - StopListener without any arguments, you can stop current connection with it. Next is a Connection structure. We have it every time as second parameter in connection handler function. It has simple API for manipulating with connection:

* `Connection.Close` - close current connection;
* `Connection.Send`  - send response.

So, now web have all types which we will use in this library. Now proceed to implementation. All API of netpool/tcp consists only from one function:

```golang
func StartNewListener(listener *Listener) {

}
```

It's a main function of this library and it launches new tcp listener and some acceptors. Let's see how it implemented. First of all it defines some variables like: acceptorCounter for count acceptors, checks is it connection over SSL or not and launches listener which depends on this:

```golang
var err error
var ln net.Listener
var cert tls.Certificate

if listener.Ssl == nil {
    ln, err = net.Listen("tcp", ":" + strconv.Itoa(listener.Port))
} else {
    cert, err = tls.LoadX509KeyPair(listener.Ssl["pem"], listener.Ssl["key"])
    config := tls.Config{Certificates: []tls.Certificate{cert}}
    config.Rand = rand.Reader
    ln, err = tls.Listen("tcp", ":" + strconv.Itoa(listener.Port), &config)
}

if err != nil {
    log.Print("[Error] TCP listener didn't start: ", err)
    return
}
```

Next it creates 2 channels:

* `connectionCounter` - channel for communicating between acceptor goroutines and listener. If connection was closed it sends message to listener and listener decrease connection numbers, if new connection was accepted it increase connection number;
* `closeConnection` - channel for commuicating between listener and acceptor for closing current connection.

and starts acceptors:

```golang
connectionCounter := make(chan int)
closeConnection      := make(chan bool)

// start all acceptors
for accs := 0; accs < listener.Accnb; accs++ {
    go acceptor(accs, ln, connectionCounter, listener, closeConnection, acceptorsCounter)
}
```

After this listener waits for messages from another goroutines for closing current listener, closing connection, connection removed/created and etc...:

```
for {
    switch listener.OverFlowStrategy {
    case 0:
        if acceptorsCounter >= listener.Accnb {
            conn, _ := ln.Accept()
            conn.Close()
        }
        case 1:
        listener.Accnb += 50
        for i := 0; i < defaultIncreaseAcceptors; i++ {
            go acceptor(i, ln, connectionCounter, listener, closeConnection, acceptorsCounter)
        }
    }

    select {
    case msg := <-connectionCounter:
        if msg == -1 {
            acceptorsCounter--
        } else {
            acceptorsCounter++
        }
    case msg := <-listener.Lc:
        if msg == "stop" {
            closeConnection <- true
            close(listener.Lc)
            close(connectionCounter)
            ln.Close()
            return
        }
    }
}
```

So what about acceptors... It's pretty simple as a listener implementation, it starts from accepting new connection and waits for incoming messages from connected client. If there are any incoming data from client it call handler function and pass input data and `Connection` structure to it:

```
func acceptor(acc int, ln net.Listener, counterChannel chan int, listener *Listener, closeChannel chan bool, accCounter int) {

    conn, err := ln.Accept()

    if err != nil {
        log.Print("[Error] Tcp listener can't accept new connection: ", acc)
        return
    }

    // send to the listener info about new accepted connection
    counterChannel <- 1
    // create new connection struct
    newConnection := &Connection{conn, make(chan []byte), make(chan bool), listener}

    //
    // start to handle new connction
    //
    for {
        line, inputErr := textproto.NewReader(bufio.NewReader(conn)).ReadLine()

        if inputErr == io.EOF {
            log.Print("inputErr: ", inputErr)
            // close connection
            conn.Close()
            // tell to listener that one acceptor died
            counterChannel <- -1

            return
        }

        go listener.Handler(line, *newConnection)

        //
        // ....
        //
}
```

After this it as a listener checks incoming message from other goroutines, to stop connection and etc...

So it's end. As we can saw concurrency is a pretty simple with Golang.

As i said in previous post i'm not a golang super-hacker, if you will have any recommendations for my golang code or you want to take part in XMPP server developing with golang i will be very pleased to hear it, write me in comment or ping me in [twitter](https://twitter.com/0xAX). Also i made this library primarily for future extd XMPP server but i tried to make it generic so i think it can be useful not only for me.
