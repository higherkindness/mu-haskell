# Development recommendations

Before continuing, make sure you've read:

- [Alejandro's post on setting up a Haskell development environment](https://www.47deg.com/blog/setting-up-haskell/).
- [Kowainik's Haskell Style Guide](https://kowainik.github.io/posts/2019-02-06-style-guide).

## VSCode extensions

To make our lives easier while developing in Haskell, we use the following extensions:

- [ghcide](https://marketplace.visualstudio.com/items?itemName=DigitalAssetHoldingsLLC.ghcide), the best thing that happened to Haskell for editors/IDEs! ❤️
- [hlint](https://marketplace.visualstudio.com/items?itemName=hoovercj.haskell-linter), another great extension to have suggestions and refactors in Haskell 🛠
- [stylish-haskell](https://marketplace.visualstudio.com/items?itemName=vigoo.stylish-haskell), the formatter we use to prettify the code 💅🏼
- [editorconfig](https://marketplace.visualstudio.com/items?itemName=EditorConfig.EditorConfig), to have consistency between different editors and envs 🐀

## stylish-haskell 💅🏼

Regarding the formatter, we use the `master` version of [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) to be able to use language pragmas with lowercase, so you'll need to do this locally:

```sh
$ git clone https://github.com/jaspervdj/stylish-haskell
$ ...
$ cd stylish-haskell && stack install
```

We don't provide any git hook or tool that enforces our style. However, before you propose any PR please make sure to run `stylish-haskell` yourself, and to follow our style guide mentioned above to the extent possible. 😊

If you wan't to automate this for your VSCode, add the following to your `.vscode/settings.json`:

```json
{
  "editor.formatOnSave": true
}
```

Happy hacking! 👏🏼
