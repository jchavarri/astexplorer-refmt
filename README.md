# astexplorer-refmt

JavaScript wrapper for [refmt](https://github.com/facebook/reason/tree/master/src/refmt) parser to be used to analyze and show Reason and OCaml AST in [astexplorer.net](http://astexplorer.net/).

Not intended to be used as a library.

## Development

The project requires `opam` and `pnpm` to build, and it uses `make` to run the project.

```
make init
```

You can run the watch mode to automatically build the project when you make changes:

```
make dev
```

To build the project as production mode:
(The output bundle will be stored in the `./dist` folder)

```
make prod
```

To run an example, you can use the `demo` command:

```
make demo
```


### Running with astexplorer

- `pnpm link` in the project root folder
- Clone [`astexplorer`](https://github.com/fkling/astexplorer/) locally.
- In `website` folder of `astexplorer`, call `yarn link <path-to-astexplorer-refmt>`.

### Running without astexplorer

Edit the code variable in `src/example/example.js` to parse some code.
```js
let code = "let x = 1";

let ast = window.parseReason(code);
window.document.querySelector("#app").innerHTML = ast;
```

Then run `make demo` and open `http://localhost:3030/` in your browser.