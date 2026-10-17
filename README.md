# symmetric-list

## Motivation / Goal

Haskell lists (`[a]`) are singly linked: operating on the **front** is cheap,
but any operation on the **end** forces a traversal of the whole list.

The goal of this project is to improve the complexity of those inefficient
operations while keeping an interface similar to that of `Data.List`.

| Operation             | `[a]`        | `SymmetricList a`        |
|-----------------------|--------------|--------------------------|
| `cons`, `head`        | $O(1)$       | $O(1)$                   |
| `tail`, `uncons`      | $O(1)$       | $O(1)$ amortized         |
| `snoc` (`xs ++ [x]`)  | $O(n)$       | $O(1)$                   |
| `last`                | $O(n)$       | $O(1)$                   |
| `init`, `unsnoc`      | $O(n)$       | $O(1)$ amortized         |
| `reverse`             | $O(n)$       | $O(1)$                   |
| `length`              | $O(n)$       | $O(1)$                   |
| `append` (`<>`)       | $O(m)$       | $O(\min(m, n))$          |

### Usage

The module is designed to be imported qualified, since many of its names
(`head`, `last`, `reverse`, `map`, ...) clash with the `Prelude`:

```haskell
import Data.SymmetricList (SymmetricList)
import qualified Data.SymmetricList as SymmetricList

xs :: SymmetricList Int
xs = SymmetricList.snoc 4 (SymmetricList.fromList [1, 2, 3])

-- SymmetricList.last xs           == 4
-- SymmetricList.toList (SymmetricList.init xs) == [1, 2, 3]
-- xs <> xs                         == SymmetricList.append xs xs
```

This convention follows the guide
[Designing Haskell libraries for qualified import](https://mrcjkb.dev/posts/2026-10-06-design-for-qualified-import.html):
the type and the functions that operate on it live in a module with the same
name, meant to be imported qualified, and operators (such as `<>`) are used
unqualified.

## Representation

We start from a list $l$ and split it into two parts:

```math
l = xs \mathbin{+\!\!+} ys
\qquad\text{for example}\qquad
[1,2,3,4,5,6] = [1,2,3] \mathbin{+\!\!+} [4,5,6]
```

```math
ys' = \mathrm{reverse}\ ys
\quad\Longrightarrow\quad
ys = \mathrm{reverse}\ ys'
\quad\Longrightarrow\quad
l = xs \mathbin{+\!\!+} \mathrm{reverse}\ ys'
```

```math
\langle xs,\ ys \rangle \;\equiv\; xs \mathbin{+\!\!+} \mathrm{reverse}\ ys
```

If $xs = [x_1, \dots, x_k]$ and $ys = [y_1, \dots, y_m]$, the represented list
is:

```math
[\;\underbrace{x_1,\ x_2,\ \dots,\ x_k}_{xs}\;,\;\underbrace{y_m,\ \dots,\ y_2,\ y_1}_{\mathrm{reverse}\ ys}\;]
```

This way the **first** element is at the head of $xs$ ($x_1$) and the **last**
one at the head of $ys$ ($y_1$): both ends are one step away.

In Haskell:

```haskell
data SymmetricList a = SL !Int !Int ([a], [a])
--                        lenL lenR  (xs,  ys)
```

Storing the lengths makes `length` $O(1)$ and lets rebalancing avoid
traversing the lists to measure them.

### Invariant

If one of the two lists is empty, the other has at most one element:

```math
\mathrm{null}\ xs \Rightarrow |ys| \le 1
\qquad\qquad
\mathrm{null}\ ys \Rightarrow |xs| \le 1
```

This guarantees that `head` and `last` always find their element in $O(1)$.

### Inserting at the end: `snoc`

Adding to the end means adding to the head of $ys$:

```math
\mathrm{snoc}\ z\ \langle xs,\ ys \rangle = \langle xs,\ z : ys \rangle
```

If $xs$ is empty, by the invariant $ys$ has at most one element, which
becomes the left side: $\mathrm{snoc}\ z\ \langle [\,],\ ys \rangle = \langle ys,\ [z] \rangle$.

Example, starting from `fromList [1..6]` (which splits the elements in half):

```math
\begin{array}{lcl}
\mathrm{fromList}\ [1,2,3,4,5,6] & = & \langle [1,2,3],\ [6,5,4] \rangle \\
\mathrm{snoc}\ 7\ \langle [1,2,3],\ [6,5,4] \rangle & = & \langle [1,2,3],\ [7,6,5,4] \rangle
\end{array}
```

### Removing from the end: `init` / `unsnoc`

Removing the last element means removing the head of $ys$:

```math
\mathrm{unsnoc}\ \langle xs,\ y : ys \rangle = (\langle xs,\ ys \rangle,\ y) \qquad \text{if } ys \neq [\,]
```

The interesting case is when $ys$ has a single element: removing it would
leave it empty and break the invariant. In that case the list is
**rebalanced**, splitting the remaining elements in half between both sides:

```math
\mathrm{unsnoc}\ \langle xs,\ [y] \rangle = (\mathrm{balance}\ \langle xs,\ [\,] \rangle,\ y)
```

```math
\mathrm{balance}\ \langle xs,\ ys \rangle = \langle\, \mathrm{take}\ \lfloor n/2 \rfloor\ l,\ \ \mathrm{reverse}\,(\mathrm{drop}\ \lfloor n/2 \rfloor\ l) \,\rangle
\quad \text{where } l = xs \mathbin{+\!\!+} \mathrm{reverse}\ ys,\ \ n = |l|
```

Example:

```math
\begin{array}{lcl}
\mathrm{unsnoc}\ \langle [1,2,3],\ [6,5,4] \rangle & = & (\langle [1,2,3],\ [5,4] \rangle,\ 6) \\
\mathrm{unsnoc}\ \langle [1,2,3],\ [5,4] \rangle   & = & (\langle [1,2,3],\ [4] \rangle,\ 5) \\
\mathrm{unsnoc}\ \langle [1,2,3],\ [4] \rangle     & = & (\mathrm{balance}\ \langle [1,2,3],\ [\,] \rangle,\ 4) = (\langle [1],\ [3,2] \rangle,\ 4)
\end{array}
```

`cons`, `tail` and `uncons` are symmetric: they operate on $xs$ and rebalance
when $xs$ becomes empty. `reverse` simply swaps the sides:
$\mathrm{reverse}\ \langle xs,\ ys \rangle = \langle ys,\ xs \rangle$.

### Why $O(1)$ amortized?

`balance` costs $O(n)$, but it only happens when one side has become empty,
and it leaves both sides with half of the elements. Taking as potential
function

```math
\Phi(\langle xs,\ ys \rangle) = \big|\, |xs| - |ys| \,\big|
```

each cheap operation changes $\Phi$ by at most $1$, and a rebalance of cost
$n$ happens when $\Phi \approx n$ and brings it down to $\Phi \le 1$. The cost
of the rebalance is paid with the accumulated potential, so a sequence of $k$
operations on the ends (`cons`, `snoc`, `tail`, `init`, ...) costs $O(k)$.

## Benchmarks

Comparison against `[a]` with [`tasty-bench`](https://hackage.haskell.org/package/tasty-bench),
on the list `[1..n]` (see [bench/Bench.hs](bench/Bench.hs)). Compiled with
GHC 9.10.3 and `-O2`; absolute times depend on the machine.

For the operations that return a list (`snoc`, `init`, `unsnoc`), the `[a]`
version is evaluated to normal form: because of lazy evaluation, evaluating to
WHNF would only measure the first constructor and not the traversal.

### Individual operations, $n = 100\,000$

| Operation | `SymmetricList` | `[a]`   | Speedup     |
|-----------|----------------:|--------:|------------:|
| `last`    | 6.88 ns         | 939 μs  | ~136 000×   |
| `reverse` | 15.5 ns         | 1.19 ms | ~77 000×    |
| `snoc`    | 18.6 ns         | 1.65 ms | ~89 000×    |
| `init`    | 15.5 ns         | 1.65 ms | ~106 000×   |
| `unsnoc`  | 21.7 ns         | 5.25 ms | ~242 000×   |
| `length`  | 8.38 ns         | 825 μs  | ~98 000×    |

The time of `SymmetricList` stays constant as $n$ grows, while that of `[a]`
grows linearly:

| `last`        | $n = 1\,000$ | $n = 10\,000$ | $n = 100\,000$ |
|---------------|-------------:|--------------:|---------------:|
| `SymmetricList` | 8.11 ns    | 6.95 ns       | 6.88 ns        |
| `[a]`         | 1.50 μs      | 17.6 μs       | 939 μs         |

### Amortized cost: emptying from the end

`unsnoc` is applied repeatedly until the list is empty, so that the cost of
the rebalances is included in the measurement. With `[a]` this is $O(n^2)$,
with `SymmetricList` it is $O(n)$:

| $n$       | `SymmetricList` | `[a]`   |
|-----------|----------------:|--------:|
| 1 000     | 38.1 μs         | 13.0 ms |
| 10 000    | 418 μs          | 1.41 s  |
| 100 000   | 6.50 ms         | —       |

(With `[a]` it is not run for $n = 100\,000$ because it takes too long.)

### Running the benchmarks

```sh
cabal bench symmetric-list-bench
# export results
cabal bench symmetric-list-bench --benchmark-options='--csv bench.csv --svg bench.svg'
```
