# Development recommendations

Most of our developers use using [Visual Studio Code](https://code.visualstudio.com/). In that case, you just have to open the project, and install the recommended extensions. Before continuing, make sure you've read:

- [Alejandro's post on setting up a Haskell development environment](https://www.47deg.com/blog/setting-up-haskell/), but forget about the Visual Studio Code configuration outlined there.
- [Kowainik's Haskell Style Guide](https://kowainik.github.io/posts/2019-02-06-style-guide).

## Visual Studio Code extensions

To make our lives easier while developing in Haskell, we use a set of recommended extensions. The first time you open the project, the editor should suggest to install all those you do not have:

- [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell), the best thing that happened to Haskell for editors/IDEs! ❤️
- [editorconfig](https://marketplace.visualstudio.com/items?itemName=EditorConfig.EditorConfig), to have consistency between different editors and envs 🐀

## Styling 💅🏼

We loosely follow [Kowainik's Haskell Style Guide](https://kowainik.github.io/posts/2019-02-06-style-guide). In order to automate styling, we use [stylish-haskell](https://github.com/jaspervdj/stylish-haskell), for which we provide a `.stylish-haskell.yaml` configuration file.

We don't provide any git hook or tool that enforces our style. By default the provided `.vscode/settings.json` file runs styling on every save. However, before you propose any PR please make sure to run `stylish-haskell` yourself, and to follow our style guide mentioned above to the extent possible. 😊

Happy hacking! 👏🏼
