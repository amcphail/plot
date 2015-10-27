THIS README COPIED FROM THE diagrams PACKAGE

Graphics.Rendering.Plot provides an embedded domain-specific
language (EDSL) for creating plots rendered with Cairo

For some examples of use, see [Plot examples on Haskell.org](http://code.haskell.org/plot/examples).

# Installing Plots with Stack

## Ubuntu

```
sudo apt-get install libblas-dev libgsl0-dev liblapack-dev libatlas-base-dev
sudo apt-get install libghc-gtk-dev
stack install plot
```

If you're using a different operating system, key off these package names to figure out what you need (roughly, blas, lapack, cairo, and GTK) and add instructions back here when you figure it out please.

# Old install instructions

To install the Plots library:

1. Get the dependencies

    The plots library uses Haskell bindings to the Cairo vector
    graphics library.  In order to build the plots library, you
    will first need the following:

    * The Cairo library itself.  This is probably available through
      your system's package manager and may even already be installed.
      On Ubuntu, for example, it is available from the 'libcairo'
      package.

    * The Haskell cairo bindings, which are packaged as part of
      gtk2hs.  
           cabal install gtk2hs-buildtools
	   cabal install gtk

    * The colour library, which is available from Hackage.  If you use
      the cabal-install build option described below, the colour
      library will be downloaded and installed for you automatically.

2. Build

   * Option 1: use cabal-install

     If you have cabal-install, *after* installing gtk2hs, you can
     install plots and the remaining dependencies with
     cabal-install:

       cabal install plot

     Optionally, you can also pass options such as --user
     --prefix=$HOME to install locally.

  * Option 2: manual build

    Once all the dependencies are built and installed, you can build
    and install plots as follows:
      
      runhaskell Setup.lhs configure --prefix=$HOME --user
      runhaskell Setup.lhs build
      runhaskell Setup.lhs install

    (Optionally, you can omit the --prefix and --user arguments to the
    configure step, and run the install step with 'sudo' in order to
    install the library systemwide.)

3. Building Haddock documentation (recommended)

     runhaskell Setup.lhs haddock

   Once the documentation has been built, you can access it by 
   pointing your browser to dist/doc/html/plot/index.html.
