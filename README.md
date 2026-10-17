# symmetric-list

## Motivación / Objetivo

Las listas de Haskell (`[a]`) son simplemente enlazadas: operar sobre el
**principio** es barato, pero cualquier operación sobre el **final** obliga a
recorrer la lista completa.

El objetivo de este proyecto es mejorar la complejidad de esas operaciones
ineficientes manteniendo una interfaz similar a la de `Data.List`.

| Operación             | `[a]`        | `SymmetricList a`        |
|-----------------------|--------------|--------------------------|
| `cons`, `head`        | $O(1)$       | $O(1)$                   |
| `tail`, `uncons`      | $O(1)$       | $O(1)$ amortizado        |
| `snoc` (`xs ++ [x]`)  | $O(n)$       | $O(1)$                   |
| `last`                | $O(n)$       | $O(1)$                   |
| `init`, `unsnoc`      | $O(n)$       | $O(1)$ amortizado        |
| `reverse`             | $O(n)$       | $O(1)$                   |
| `length`              | $O(n)$       | $O(1)$                   |
| `append` (`<>`)       | $O(m)$       | $O(\min(m, n))$          |

### Uso

El módulo está pensado para importarse calificado, ya que muchos nombres
(`head`, `last`, `reverse`, `map`, ...) chocan con el `Prelude`:

```haskell
import Data.SymmetricList (SymmetricList)
import qualified Data.SymmetricList as SymmetricList

xs :: SymmetricList Int
xs = SymmetricList.snoc 4 (SymmetricList.fromList [1, 2, 3])

-- SymmetricList.last xs           == 4
-- SymmetricList.toList (SymmetricList.init xs) == [1, 2, 3]
-- xs <> xs                         == SymmetricList.append xs xs
```

Esta convención sigue la guía
[Designing Haskell libraries for qualified import](https://mrcjkb.dev/posts/2026-10-06-design-for-qualified-import.html):
el tipo y las funciones que operan sobre él viven en un módulo con su mismo
nombre, pensado para importarse calificado, y los operadores (como `<>`) se
usan sin calificar.

## Representación

Partimos de una lista $l$ y la cortamos en dos partes:

```math
l = xs \mathbin{+\!\!+} ys
\qquad\text{por ejemplo}\qquad
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

Si $xs = [x_1, \dots, x_k]$ e $ys = [y_1, \dots, y_m]$, la lista representada
es:

```math
[\;\underbrace{x_1,\ x_2,\ \dots,\ x_k}_{xs}\;,\;\underbrace{y_m,\ \dots,\ y_2,\ y_1}_{\mathrm{reverse}\ ys}\;]
```

Así el **primer** elemento está en la cabeza de $xs$ ($x_1$) y el **último**
en la cabeza de $ys$ ($y_1$): ambos extremos quedan a un paso.

En Haskell:

```haskell
data SymmetricList a = SL !Int !Int ([a], [a])
--                        lenL lenR  (xs,  ys)
```

Guardar las longitudes permite que `length` sea $O(1)$ y que el rebalanceo
no tenga que recorrer las listas para medirlas.

### Invariante

Si una de las dos listas está vacía, la otra tiene a lo sumo un elemento:

```math
\mathrm{null}\ xs \Rightarrow |ys| \le 1
\qquad\qquad
\mathrm{null}\ ys \Rightarrow |xs| \le 1
```

Esto garantiza que `head` y `last` siempre encuentran su elemento en $O(1)$.

### Insertar al final: `snoc`

Agregar al final es agregar a la cabeza de $ys$:

```math
\mathrm{snoc}\ z\ \langle xs,\ ys \rangle = \langle xs,\ z : ys \rangle
```

Si $xs$ está vacía, por el invariante $ys$ tiene a lo sumo un elemento, que
pasa a ser el lado izquierdo: $\mathrm{snoc}\ z\ \langle [\,],\ ys \rangle = \langle ys,\ [z] \rangle$.

Ejemplo, partiendo de `fromList [1..6]` (que reparte los elementos a la mitad):

```math
\begin{array}{lcl}
\mathrm{fromList}\ [1,2,3,4,5,6] & = & \langle [1,2,3],\ [6,5,4] \rangle \\
\mathrm{snoc}\ 7\ \langle [1,2,3],\ [6,5,4] \rangle & = & \langle [1,2,3],\ [7,6,5,4] \rangle
\end{array}
```

### Sacar del final: `init` / `unsnoc`

Sacar el último elemento es sacar la cabeza de $ys$:

```math
\mathrm{unsnoc}\ \langle xs,\ y : ys \rangle = (\langle xs,\ ys \rangle,\ y) \qquad \text{si } ys \neq [\,]
```

El caso interesante es cuando $ys$ tiene un solo elemento: al sacarlo
quedaría vacía y se rompería el invariante. En ese caso se **rebalancea**,
repartiendo los elementos restantes a la mitad entre ambos lados:

```math
\mathrm{unsnoc}\ \langle xs,\ [y] \rangle = (\mathrm{balance}\ \langle xs,\ [\,] \rangle,\ y)
```

```math
\mathrm{balance}\ \langle xs,\ ys \rangle = \langle\, \mathrm{take}\ \lfloor n/2 \rfloor\ l,\ \ \mathrm{reverse}\,(\mathrm{drop}\ \lfloor n/2 \rfloor\ l) \,\rangle
\quad \text{donde } l = xs \mathbin{+\!\!+} \mathrm{reverse}\ ys,\ \ n = |l|
```

Ejemplo:

```math
\begin{array}{lcl}
\mathrm{unsnoc}\ \langle [1,2,3],\ [6,5,4] \rangle & = & (\langle [1,2,3],\ [5,4] \rangle,\ 6) \\
\mathrm{unsnoc}\ \langle [1,2,3],\ [5,4] \rangle   & = & (\langle [1,2,3],\ [4] \rangle,\ 5) \\
\mathrm{unsnoc}\ \langle [1,2,3],\ [4] \rangle     & = & (\mathrm{balance}\ \langle [1,2,3],\ [\,] \rangle,\ 4) = (\langle [1],\ [3,2] \rangle,\ 4)
\end{array}
```

`cons`, `tail` y `uncons` son simétricas: operan sobre $xs$ y rebalancean
cuando $xs$ se vacía. `reverse` simplemente intercambia los lados:
$\mathrm{reverse}\ \langle xs,\ ys \rangle = \langle ys,\ xs \rangle$.

### ¿Por qué $O(1)$ amortizado?

`balance` cuesta $O(n)$, pero sólo ocurre cuando un lado se vació, y deja
ambos lados con la mitad de los elementos. Tomando como función potencial

```math
\Phi(\langle xs,\ ys \rangle) = \big|\, |xs| - |ys| \,\big|
```

cada operación barata cambia $\Phi$ en a lo sumo $1$, y un rebalanceo de costo
$n$ ocurre cuando $\Phi \approx n$ y lo baja a $\Phi \le 1$. El costo del
rebalanceo se paga con el potencial acumulado, por lo que una secuencia de $k$
operaciones sobre los extremos (`cons`, `snoc`, `tail`, `init`, ...) cuesta
$O(k)$.

## Benchmarks

Comparación contra `[a]` con [`tasty-bench`](https://hackage.haskell.org/package/tasty-bench),
sobre la lista `[1..n]` (ver [bench/Bench.hs](bench/Bench.hs)). Compilado con
GHC 9.10.3 y `-O2`; los tiempos absolutos dependen de la máquina.

Para las operaciones que devuelven una lista (`snoc`, `init`, `unsnoc`), la
versión con `[a]` se evalúa a forma normal: por la evaluación perezosa, a WHNF
sólo se mediría el primer constructor y no el recorrido.

### Operaciones individuales, $n = 100\,000$

| Operación | `SymmetricList` | `[a]`   | Speedup     |
|-----------|----------------:|--------:|------------:|
| `last`    | 6.88 ns         | 939 μs  | ~136 000×   |
| `reverse` | 15.5 ns         | 1.19 ms | ~77 000×    |
| `snoc`    | 18.6 ns         | 1.65 ms | ~89 000×    |
| `init`    | 15.5 ns         | 1.65 ms | ~106 000×   |
| `unsnoc`  | 21.7 ns         | 5.25 ms | ~242 000×   |
| `length`  | 8.38 ns         | 825 μs  | ~98 000×    |

El tiempo de `SymmetricList` se mantiene constante al crecer $n$, mientras que
el de `[a]` crece linealmente:

| `last`        | $n = 1\,000$ | $n = 10\,000$ | $n = 100\,000$ |
|---------------|-------------:|--------------:|---------------:|
| `SymmetricList` | 8.11 ns    | 6.95 ns       | 6.88 ns        |
| `[a]`         | 1.50 μs      | 17.6 μs       | 939 μs         |

### Costo amortizado: vaciar desde el final

Se aplica `unsnoc` repetidamente hasta vaciar la lista, de modo que el costo
de los rebalanceos queda incluido en la medición. Con `[a]` esto es $O(n^2)$,
con `SymmetricList` es $O(n)$:

| $n$       | `SymmetricList` | `[a]`   |
|-----------|----------------:|--------:|
| 1 000     | 38.1 μs         | 13.0 ms |
| 10 000    | 418 μs          | 1.41 s  |
| 100 000   | 6.50 ms         | —       |

(Con `[a]` no se corre para $n = 100\,000$ porque tarda demasiado.)

### Correr los benchmarks

```sh
cabal bench symmetric-list-bench
# exportar resultados
cabal bench symmetric-list-bench --benchmark-options='--csv bench.csv --svg bench.svg'
```
