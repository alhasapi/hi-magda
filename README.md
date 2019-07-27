# Haskell Interpreter for Magda

The Haskell Interpreter for Magda is based on [J. Kusmierek's PhD
dissertation](https://www.mimuw.edu.pl/~jdk/mixiny.pdf), which
contains the formal definition of Magda, an Object Oriented,
mixin-based, statically typed, imperative language.

## Building the interpreter

For the building process the GHC compiler is required, follow the
instructions on its [project's homepage](https://www.haskell.org/ghc/)
in order to install GHC for your distribution.

In order to download and build the interpreter follow these steps:

 1. Install parsec, needed for parsing, follow the instructions from
    the dedicated [Hackage
    page](https://hackage.haskell.org/package/parsec). The easiest way
    to achieve this is through the package manager cabal, using:
	
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

 4. Install the interpreter. 
	
```{.sh}
 $ sudo make install
```

 5. Run your Magda programs!
 
```{.sh}
 $ magda <your-program-filename-here>
```

## Downloading the interpreter

To download and install the latest version (v.1.1) binary, available
only for the linux-x86_64 platform, run:

```{.sh}
 $ curl -o /usr/local/bin/magda https://gitlab.com/magda-lang/hi-magda/uploads/83edb7cbad1e06ec5ba1433e35c25c73/magda-v1.1-linux-x86_64
```
