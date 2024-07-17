# Exercises

:::note
The solutions for the following exercises should be considered together with my modest mathetmatical knowledge. As I am not a professional mathematician, I can not give you 100% guarantee that all of this is correct. It is only my try. If you see the mistake, please feel free to open issue or create a pull request.
:::

## Exercise 3.3-1

> Show that if $f(n)$ and $g(n)$ are monotonically increasing functions, then so are the functions $f(n) + g(n)$ and $f(g(n))$, and if $f(n)$ and $g(n)$ are in addition nonnegative, then $f(n) * g(n)$ is monotonically increasing.

First of all let's remember what is the `monotonically increasing` functions:

> A function $f(n)$ is **monotonically increasing** if $m \le n$ implies $f(m) \le f(n)$.

In accordance with the conditions of the exercise, the both our functions $f(n)$ and $g(n)$ are monotonically increasing functions. Let's consider the function $h(n) = f(n) + g(n)$. We have to prove that it is also monotonically increasing function. If we will take two numbers $m$ and $n$ where $m \le n$, we will have: $f(m) \le f(n)$ and $g(m) \le g(n)$ by the definition of monotonically increasing functions. It implies that $f(m) + g(m) \le f(n) + g(n)$. 

In the same way since the $g(m) \le g(n)$ and the $f(n)$ function is monotonically increasing, the $f(g(m)) \le f(g(n))$.

If in the same time our both functions are nonnegative, we can safely multiple them and according to the facts above it also should be monotonically increasing function.

We may have even simple proof using [coq](https://coq.inria.fr/) proof assistant:

```coq
Require Import Coq.Arith.Le.

Parameter f : nat -> nat.
Parameter g : nat -> nat.

Axiom f_monotonic : forall x y : nat, x <= y -> f x <= f y.
Axiom g_monotonic : forall x y : nat, x <= y -> g x <= g y.

Theorem sum_of_monotonic_funs : forall x y : nat, x <= y -> f x + g x <= f y + g y.
Proof.
  intros x y H.
  apply Nat.add_le_mono.
  - apply f_monotonic. assumption.
  - apply g_monotonic. assumption.
Qed.

Theorem application_of_monotonic_funs : forall x y : nat, x <= y -> f(g(x)) <= f(g(y)).
Proof.
  intros x y H.
  apply f_monotonic.
  apply g_monotonic.
  assumption.
Qed.
```

## Exercise 3.3-2

> Prove that $\lfloor \alpha * n \rfloor + \lceil (1 - \alpha)n \rceil = n$ for any integer $n$ and real number \alpha in the range of $0 \le \alpha \le 1.

The cases when $\alpha = 0$ or $\alpha = 1$ are trivial.

If $\alpha = 0$ we will have:

$$
\lfloor \alpha \cdot n \rfloor + \lceil (1 - a) \cdot n \rceil = \lfloor 0 \rfloor + \lceil (1 - 0) \cdot n \rceil = 0 + n = n
$$

if $\alpha = 1$ we will have:

$$
\lfloor \alpha \cdot n \rfloor + \lceil (1 - a) \cdot n \rceil = \lfloor 1 \cdot n \rfloor + \lceil (1 - 1) \cdot n \rceil = n + 0 = n
$$

If $0 < \alpha < 1$ we may use the equations that is given in the book: $-\lceil x \rceil = \lfloor -x \rfloor$ and $-\lfloor x \rfloor = \lceil -x \rceil$. And in addition the fact that the integer number $n$ and real number $x$ we may have \lfloor n + x \rfloor = n + \lfloor x \rfloor. So let's take a look at the given expression:

$$
\lfloor \alpha * n \rfloor + \lceil (1 - \alpha)n \rceil = \lfloor \alpha * n \rfloor + \lceil n - n \cdot \alpha \rceil = \lfloor \alpha * n \rfloor + n + \lceil (- (\alpha \cdot n)) \rceil = \lfloor \alpha \cdot n \rfloor + n - \lfloor \alpha \cdot n \rfloor = n
$$

In addition we can see that the growth of both functions are equal:

```python
import math
import matplotlib.pyplot as plt

x = []
for i in range(0, 1001):
    x.append(i)

y1 = []
for i in x:
    y1.append(i)

y2 = []
for i in x:
    y2.append(math.floor(0.3 * i) + math.ceil((1 - 0.3) * i))
    
plt.plot(x, y1, linewidth=6, color='red', label ='f(n) = n')
plt.plot(x, y2, '-.', linewidth=6, color='green', label ='f(n) = ⌊αn⌋ + ⌈(1 - α)n⌉')

plt.ylim(ymin=0)
plt.xlim(xmin=0)
plt.xlabel("X")
plt.ylabel("Y")
plt.legend()
plt.title('f(n) = ⌊αn⌋ + ⌈(1 - α)n⌉ and f(n) = n')
plt.show()
```

The result we may see: ![alpha-function](https://gist.github.com/user-attachments/assets/68607e24-e28a-4d3c-8794-293151c83280)


## Exercise 3.3-3

> Use equation (3.14) or other means to show that $(n + o(n))^k \in \theta(n^k)$ for any real constant $k$. Conclude that $\lfloor n \rfloor^k \in \theta(n^k)$.

At the beginning the `Use equation (3.14)` confused me pretty much. As the equation mentioned in the exercise text is $1 + x \le e^x$. I have spent enough time thinking about it but did not find a way how to apply this equation to the exercise. Luckily I remembered about the [Errata for Introduction to Alogorithms](https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/11599/e4-bugs.html) and according to it, it turns out that it is a typo. It must be `Use the equation 3.13`. This equation says: $n^b \in o(a^n)$ or that any [exponential function](https://en.wikipedia.org/wiki/Exponential_function) than any [polynomial function](https://en.wikipedia.org/wiki/Polynomial). Maybe this note will help you, but it did not help me. So I decided to look at this using the most straightforward way. The $(n + o(n))^k$ is:

$$
(n + o(n))^k = \sum_{i = 0}^{k} \binom{k}{i} n^{k-i} \cdot f(n)^{i}
$$

The term with the highest power of $n$ is $n^k$ which is $\theta(n^k)$. For all other terms we will have $n^{k-i}$ and $f(n)^i$. As our $f(n) \in o(n)$ by definition, it means that with the growth of $n$ our $f(n)$ will tend to $0$ and as a result will bring insignificant growth to $n^{k-i}$.

## Exercise 3.3-4

> Proove the following:
>
> *a.* Equation (3.21)
>
> *b.* Equations (3.26)-(3.28)
>
> *c.* $log(\theta(n)) \in \theta(log(n))$

### Equation (3.21)

The equation *3.21* is:

$$
a^{log_{b}(c)} = c^{log_{b}(a)}
$$

Let's try to prove it:

$$
a^{log_{b}(c)} = c^{log_{b}(a)} = с^{\frac{log_{c}(a)}{log_{c}(b)}} = \sqrt[log_{c}(b)]{c^{log_{c}(a)}} = a^{\frac{1}{log_{c}(b)}} = a^{log_{b}{c}}
$$

### Equations (3.26)

The equation *3.26* is:

$$
n! \in o(n^n)
$$

We can try to prove it using [Stirling's approximation](https://en.wikipedia.org/wiki/Stirling%27s_approximation):

$$
n! = \sqrt{2 \cdot \pi \cdot n} \cdot (\frac{n}{e})^n \cdot (1 + \theta(\frac{1}{n}))
$$

The definition of $o$ is:

$$
\begin{align*}
& o(g(n)) = f(n) : \text{ for any positive constant } c > 0, \text{ there exist a constant } \\
& \qquad \qquad \ \ \ n_{0} \text{ such that } 0 \le f(n) < cg(n) \text{ for all } n >= n_{0} 
\end{align*}
$$

Which says us that $f(n)$ grows significantly slower than $g(n)$. So let's see what we can get out of it:

$$
\lim_{n\to\infty} \frac{n^n}{\sqrt{2 \cdot \pi \cdot n} \cdot (\frac{n}{e})^n \cdot (1 + \theta(\frac{1}{n}))} = \lim_{n\to\infty} \frac{e^n}{\sqrt{2 \cdot \pi \cdot n} \cdot (1 + \theta(\frac{1}{n}))} = 2 \cdot \pi \lim_{n\to\infty} \frac{e^n}{\sqrt{\cdot n} \cdot (1 + \theta(\frac{1}{n}))} = \lim_{n\to\infty} \frac{e^n}{\sqrt{n}}
$$

Now using the [L'Hôpital's rule](https://en.wikipedia.org/wiki/L%27H%C3%B4pital%27s_rule):

$$
\lim_{n\to\infty} (\frac{e^n}{\sqrt{n}})’ = \lim_{n\to\infty} \frac{e^n}{\frac{1}{2 \cdot \sqrt{n}}} = \lim_{n\to\infty} e^n \cdot \sqrt{n} = \infty
$$

### Equation (3.27)

The equation *3.27* is:

$$
n! \in \omega(2^n)
$$

We can do the same what we did in the previous exercise - to apply **Stirling's approximation** and see what we can get. But before this, let's try to remember the definition of the $\omega$:

$$
\begin{align*}
& \omega(g(n)) = f(n) : \text{ for any positive constant } c > 0, \text{ there exist a constant } \\
& \qquad \qquad \ \ \ n_{0} \text{ such that } 0 \le cg(n) < f(n) \text{ for all } n >= n_{0} 
\end{align*}
$$

Or in other words $g(n)$ grows significantly slower than $f(n)$. So let's see:

$$
\lim_{n\to\infty} \frac{2^n}{\sqrt{2 \cdot \pi \cdot n} \cdot (\frac{n}{e})^n \cdot (1 + \theta(\frac{1}{n}))} = \lim_{n\to\infty} \frac{2^n}{(\frac{n}{e})^n} = \lim_{n\to\infty} \frac{(2 \cdot e)^n}{n^n}
$$

Since $n^n$ grows much faster than $(2 \cdot e)^n$ the limit will strive to $0 => n! \omega(n^n)$.

### Equations (3.28)

The equation *3.28* is:

$$
log(n!) \in \theta(n \cdot log(n))
$$

Going over the same way, using the **Stirling's approximation** we will get:

$$
log(n)! = log(\sqrt{2 \cdot \pi \cdot n} \cdot (\frac{n}{e})^{n} \cdot (1 + \theta(\frac{1}{n}))) = log(\sqrt{2 \cdot \pi \cdot n }) + log((\frac{n}{e})^n) = \theta(sqrt_{n}) + n \cdot log(\frac{n}{e})
$$

In the last expression the leading term is $n \cdot log(\frac{n}{e})$ or $nlog(n) - n(log(e))$. Skipping the least significant terms we will get what we need to prove: $log(n!) \in \theta(n \cdot log(n))$.

### Equation (c)

According to the definition of $\theta$ we need to show that:

$$
log(c_{1} \cdot n) \le log(f(n)) \le log(c_{2} \cdot n)
$$

We can consider $f(n)$ as a [linear function](https://en.wikipedia.org/wiki/Linear_function) so we can rewrite our inequation as:

$$
log(c_{1} \cdot n) \le log(f(a \cdot n + b)) \le log(c_{2} \cdot n)
$$

In other words, we need to find such constants $c_{1}$ and $c_{2}$ and $n \ge n_{0}$ after which this inequation will be true. For this case we can consider $c_{1} = \frac{1}{n}$ and $c_{2} \le n$. Let's take a look at the left part of the inequation:

$$
\begin{align*}
& log(c_{1} \cdot n) \le log(f(a \cdot n + b)) \\
& log(c_{1}) + log(n) \le log(f(a \cdot n + b)) \\
& log(\frac{1}{n}) + log(n) \le log(a \cdot n + b) \\
\end{align*}
$$

The left part gives $0$ for any $n$. 

Now we consider the right side of inequation with $c_{2} \le n$ or even the case $c_{2} = n$:

$$
\begin{align*}
& log(f(a \cdot n + n)) \le log(c_{2} \cdot n) \\
& log(f(a \cdot n + n)) \le log(c_{2}) + log(n) \\
& log(f(a \cdot n + n)) \le log(n) + log(n) \\
& log(f(a \cdot n + n)) \le 2 \cdot log(n) \text{ since } a \text{ and } b \text{ are constants }
\end{align*}
$$

## Exercise 3.3-5

> Is the function $\lceil log_{2}(n) \rceil!$ polynomially bounded? Is the function $\lceil log_{2}(log_{2}(n)) \rceil!$ polynomially bounded?

From the chapter *Polynomials* we know that `polynomially bounded` means - $f(n) \in O(n^{k})$. The definition of the `O` is:

$$
\begin{align*}
& O(g(n)) = f(n) : \text{ there exist positive constants c and } n_{0} \text{ such that } \\
& \qquad \qquad \ \ \ 0 <= f(n) <= cg(n) \text{ for all } n >= n_{0} 
\end{align*}
$$

So we basically should prove (or refute) that $\lceil log(n) \rceil! \in O(n^{k})$ and $\lceil log_{2}(log_{2}(n)) \rceil! \in O(n^{k})$. 

As soon as we see [factorial function](https://en.wikipedia.org/wiki/Factorial), the first thing that comes to mind from this book is [Stirling's approximation](https://en.wikipedia.org/wiki/Stirling%27s_approximation). Let's remember how its definition looked in the book:

$$
n! = \sqrt{2 \cdot \pi \cdot n} \cdot (\frac{n}{e})^{n} \cdot (1 + \theta(\frac{1}{n}))
$$

First of all let's consider the $\lceil log_{2}(n) \rceil!$ function and will try to apply `Stirling's approximation` to it:

$$
\lceil log_{2}(n) \rceil! = \sqrt{2 \cdot \pi \cdot log_{2}(n)} \cdot (\frac{log_{2}(n)}{e})^{log_{2}(n)} \cdot (1 + \theta(\frac{1}{log_2{(n)}}))
$$

The leading term here is $(\frac{log_{2}(n)}{e})^{log_{2}(n)}$ so we can skip others and consider only it. By the using well known formula of logarithms $a^{log_{b}(c)} = c^{log_{b}(a)}$ we can rewrite our expression as:

$$
\left(\frac{\log_{2}n}{e}\right)^{\log_{2}n} = n^{\log_{2}\left(\frac{\log_2{n}}{e}\right)} = n^{\log_{2}(\log_{2}n - \log_{2}e)}
$$

To answer the question does it polynomially bounded, we need to compare it with $n^{k}$. As $k$ is constant (as well as $\log_{2}e$, we will have: $n^{\log_{2}(\log_{2}n - \log_{2}e)} > n^k$ with growth of $n$ which means $\lceil \log(n) \rceil!$ is not polynomially bounded.

The another and more easy way to prove that could be considering $n = 2^k$. In this case the $\lceil log_{2}(n) \rceil!$ becomes just $k!$. The factorial function is $1 \cdot 2 \cdot 2 \cdot 3 \cdot \ldots \cdot k$. From another side our polynom is $2^{mk}$. As we know the factorial function growth faster than polynom, thus the function $\lceil log_{2}(n) \rceil!$ is not polynomially bounded.

For the second function $\lceil log(log(n)) \rceil!$ we can consider that $n$ is reresented by the $2^{2^{k}}$. By the logarithm rules it becomes $k! \le 2^{2^{k}}$. The $k!$ is $1 \cdot 2 \cdot 3 \cdot \ldots \cdot k$. The $2^{2^{k}}$ is $4^1 \cdot 4^2 \cdot 4^3 \cdot \ldots \cdot 4^k$. Thus our main expression $k! \le 2^{2^{k}}$ is true which means the $\lceil log(log(n)) \rceil!$ function is polynomially bounded.

## Exercise 3.3-6

> Which is asymptotically larger: $log^\*(log(n))$ or $log(log^*(n))$?

Personally for me it was much easier to use the definition of the **iterated logarithm** from [wikipedia](https://en.wikipedia.org/wiki/Iterated_logarithm):

$$
log^\*(n) =
\begin{cases}
0 & \qquad if n \le 1; \\
1 + log^\*(log(n)) & \qquad if n > 1 \\
\end{cases}
$$

So basically **iterated logarithm** is a number of times we need to apply [logarithm](https://en.wikipedia.org/wiki/Logarithm) function to $n$ while we will not get `1`. To understand it better, we can consider the following simple examples:

$$
log(log^\*(65536)) = log(1 + log^\*(16)) = log(1 + 1 + log^\*(4)) = log(1 + 1 + 1 + log^\*(2)) + log(1 + 1 + 1 + 1) = 2
$$

and:

$$
log^\*(log(65536)) = log^\*(16) = 1 + log^\*(log(4)) = 1 + log^\*(2) = 1 + 1 + 1 = 3
$$

To understand which one function growth faster, we may divide one into another and to check the result. If the result is `0` the function that is in the dividend growth slower and vice versa. So let's consider the following expression in assumption that $n = 2^m$:

$$
\frac{log^\*(log(n))}{log(log^*(n))} = \frac{log(1 + log^\*(m))}{log^\*(m)} = \frac{log(1 + 1 + log^\*(k))}{1 + log^\*(k)} = \frac{log(1 + 1 + 1 + \ldots + 1)}{1 + 1 + 1 + \ldots 1}
$$

Obviously the function in the dividend growth slower than in divisor. This means the $log(log^*(n))$ function grows assymptotically faster than $log^\*(log(n))$ function.

## Exercise 3.3-7

> Show that golden ratio $\phi$ and its conjugate $\overline \phi$ both satisfy the equation $x^2 = x + 1$

Let's look closer at the equation:

$$
\begin{align*}
& x^2 = x + 1     \\
& x^2 - x - 1 = 0 \\
& x_{1} = \frac{1 + \sqrt{1^2 - (4 \cdot 1 \cdot -1}}{2 \cdot 1)} = \frac{1 + \sqrt(5)}{2} \\
& x_{2} = \frac{1 - \sqrt{1^2 - (4 \cdot 1 \cdot -1}}{2 \cdot 1)} = \frac{1 - \sqrt(5)}{2} \\
\end{align*}
$$

Where $x_{1}$ and $x_{2}$ are exactly $\phi$ and $\overline \phi$.

## Exercise 3.3-8

> Prove by induction that the $i_{th}$ Fibonacci number satisfies the equation $F_{i} = \frac{\phi^i - \overline \phi^i}{\sqrt{5}}$

Using [mathematical induction](https://en.wikipedia.org/wiki/Mathematical_induction), we should consider the base of induction first of all:

$$
P(1) = \frac{\frac{1 + \sqrt(5)}{2} - \frac{1 - \sqrt(5)}{2}}{\sqrt(5)} = \frac{\frac{1 + \sqrt{5} - 1 + \sqrt{5}}{2}}{\sqrt{5}} = 1
$$

As for the $i = 1$ the Fibonacci number is $1$ the base of induction is correct. For the Fibonacci numbers for $i = i - 1$ and $i = i - 2$ we will have:

$$
\begin{align*}
& F_{i} = F_{i - 1} + F_{i - 2} = \\
& \frac{\phi^{i - 1} - \overline \phi^{i - 1} + \phi^{i - 2} - \overline \phi^{i - 2}}{\sqrt{5}} = \\
& \frac{\phi^{i - 2}(\phi + 1) - \overline \phi^{i - 2}(\overline \phi + 1)}{\sqrt{5}} = \\
& \frac{\phi^{i - 2} \cdot \phi^2 - \overline \phi^{i - 2} \cdot \overline \phi^2}{\sqrt{5}} = \\
& \frac{\phi^i - \overline \phi^i}{\sqrt{5}}
\end{align*}
$$

## Exercise 3.3-9

> Show that $k \cdot log(k) \in \theta(n)$ implies $k = \theta(n / log(n))$

By the rule of **symmetery** we have $f(n) \in \theta(g(n)) \text{ if and only if } g(n) \in \theta(f(n))$. According to this rule, we may rewrite our expression as:

$$
n \in \theta(k \cdot log(k))
$$

Let's take logarithm of both parts:

$$
log(n) = log(k \cdot log(k)) = log(k) + log(log(k))
$$

The $log(log(k))$ grows significantly slower than $log(k)$ so we may omit it. Now let's devide $n$ to $log(n)$:

$$
\frac{n}{log(n)} = \frac{k \cdot log(k)}{log(k)} = k \in \theta(k)
$$

So we should that $k \in \theta(n / log(n))$.
