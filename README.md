# Yesod Web App
To setup haskell on your computer, see the next section titled "Haskell Setup". This will take forever, since it downloads
all relevant Haskell files and compiler.
Then, git clone this project and `cd` into it.
Finally, run the following command: `stack runghc helloworld.hs` to run the correct file on localhost, port 3000.

The main file is in helloworld.hs, which contains everything regarding the website (ie. routing and databases, etc.). 
This file also contains some notes taken while reading https://www.yesodweb.com/book at the end of the file.

To see the Html/Css/Js in the file helloworld.hs search for the following:
- `hamlet` for Html code
- `cassius` for Css code
- `julius` for Js code

Other files of interest, that may not be used directly:
- `app/main.hs` is the main entry point of the file
- `test3.sqlite3` and `test2.sqlite3` are the SQLite database files
- `foo.hs` contains a simple 
- `src/Application` contains the definition appMain that is usually run
- `README.md` is this current file
- `package.yaml`, `stack.yaml`, and `test2.cabal` contain settings for packages

Below is the default readme file that was automatically generated.

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

## Tests

```
stack test --flag test2:library-only --flag test2:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.
