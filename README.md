# Haskell Interpreter for Magda

This work is based on J. Kusmierek's PhD dissertation, which contains
the formal definition of Magda, an Object Oriented, mixin-based,
statically typed, imperative language [^1]. 

## Building the interpreter 

In order to download and build the interpreter follow these steps:

 1. Install \texttt{parsec}, needed for parsing, follow the
    instructions in the dedicated hackage page[^parsec]. The easiest
    way to achieve this is through the package manager \texttt{cabal},
    using:
	
```{.sh}
 $ cabal install parsec
```

 2. Clone the project repository.

```{.sh}
 $ git clone https://gitlab.com/magda-lang/hi-magda.git
```

 3. Then move in the project folder and simply run \texttt{make} to
	build the binary.

```{.sh}
 $ cd hi-magda
 $ make
```

 4. The \texttt{magda} executable will be found in the \texttt{bin}
	directory inside the project's root directory. To execute your
	magda program run (from the project's root directory):
	
```{.sh}
 $ ./bin/magda <your-magda-program>
```

## Downloading the interpreter

To download and install the latest version, available only for the
linux-x86_64 platform, (v1.0) run:

```{.sh}
 $ curl -o /usr/local/bin/magda https://gitlab.com/magda-lang/hi-magda/uploads/51973f43db63ff90435854a6ab8944c8/magda-v1.0-linux-x86_64
```

[^1] : <https://www.mimuw.edu.pl/~jdk/mixiny.pdf>
[^ghc]: <https://www.haskell.org/ghc/> Home - The Glasgow Haskell Compiler
[^parsec]: <https://hackage.haskell.org/package/parsec> Parsec - Hackage

