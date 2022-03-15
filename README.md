# Eslint-RC for Emacs

[![MELPA](http://melpa.org/packages/eslint-rc-badge.svg)](http://melpa.org/#/eslint-rc)

eslint-rc is a function that searches for local rc rules and eslintignore rules per project and use it to format the current buffer using
[eslint](https://github.com/eslint/eslint). The package also exports a minor mode that applies `(eslint-rc)` on save.

## Configuration

### Requirements

To install eslint per-project which will add it the `devDependencies` section in the `package.json` file, type:

```bash
npm install --save-dev --save-exact eslint
```

To install eslint globally, via `npm`, type:

```bash
npm install -g eslint
```

On a macOS, you can use [brew](https://brew.sh/) to install eslint.

```bash
brew install eslint
```

By default, it will use the locally installed eslint (via `npm`).
If it cannot find the eslint under the `node_modules` folder, a fallback to the eslint found in the PATH will be used.

To check if eslint program is installed in the PATH, type:

```bash
which eslint
```

### Basic configuration

First require the package:

```elisp
(require 'eslint-rc)
```

Then you can hook to your favorite javascript/typescript mode:

```elisp
(add-hook 'typescript-mode-hook 'eslint-rc-mode)
(add-hook 'js2-mode-hook 'eslint-rc-mode)
(add-hook 'web-mode-hook 'eslint-rc-mode)
...
```

## Customization

This package is customizable via custom.el:

```
M-x customize-group eslint-rc
```

- `eslint-rc-use-package-json` If non-nil, eslint-rc will use `package.json` file.
- `eslint-rc-use-eslintignore` If non-nil, eslint-rc will use `.eslintignore` file.
- `eslint-rc-use-node-modules-bin` If non-nil, eslint-rc will search `node_modules` for `eslint` bin. Fallback to `PATH` if not found.
