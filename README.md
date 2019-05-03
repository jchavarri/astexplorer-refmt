# astexplorer-refmt

JavaScript wrapper for [refmt](https://github.com/facebook/reason/tree/master/src/refmt) parser to be used to analyze and show Reason and OCaml AST in [astexplorer.net](http://astexplorer.net/).

Not intended to be used as a library

## Development

You need Esy, you can install it using [npm](https://nodejs.org/en/download/):

    % npm install -g esy

Then you can install the project dependencies using:

    % esy install

Then build the project dependencies along with the project itself:

    % esy build

To test the compiled JS executable, open `index.html` in your browser.

To generate the production build (without sourcemaps, and minified) run:

    % yarn run build:prod

The output bundle will be stored in the `./dist` folder.

## Running without astexplorer

Add some logging in `AstExplorerRefmt.re`, for example:

```reason
log("parse", parseReason("let f = a => \"1\"; /* Comment */ let a = 2;"));
```

Then open `src/index.html` to see the parsed JavaScript object in the console.
