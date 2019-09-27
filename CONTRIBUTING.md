Contributing
============


## Code of Conduct

Please read the [GTIRB Code of Conduct](CODE_OF_CONDUCT.md).

## pre-commit

In general, code must follow a unified format. To make compliance with this format easier,
we recommend the use of `[pre-commit](https://pre-commit.com/)`
with the provided configuration file, `.pre-commit-config.yaml`, to manage formatting.
To use `pre-commit`:

1. If `pre-commit` is not already installed on your system, install it now with `[pip](https://pypi.org/project/pip/)`
   ```shell
      pip3 install pre-commit
   ```
2. If `[clang-format](https://clang.llvm.org/docs/ClangFormat.html)`
   is not already installed on your system, install it now.
3. Install the formatters as a pre-commit hook. In the gtirb root directory:
   ```shell
    pre-commit install
   ```
   If you prefer to run `pre-commit` manually instead, run this before all commits:
   ```shell
   pre-commit run
   ```

## General Guidelines

- Text files may not have trailing whitespace.

- Text files must end with a trailing newline.

- Code should be tested on Linux using GCC and Clang,
  and on Windows using Visual Studio.

- All CMake files shall be formatted with `cmake-format`. A `.cmake-format` file is
  provided in the root directory for the project,
  and a pass through this tool is included as part of our `pre-commit` configuration.

## C++ Code Requirements

- All code shall be formatted with [clang-format](https://clang.llvm.org/docs/ClangFormat.html).
  A `.clang-format` is provided in the root directory for the project,
  and a pass through this tool is included as part of our `pre-commit` configuration.

- Code should generally follow the C++ Core Guidelines recommendations.

- Code should generally allow for thread safety.
	- No static variables.
	- No globals
	- Free functions should not maintain state.
	- Use caution when using iterators to guard against invalidation.

- Maintain const-correctness.

- Use UpperCamelCase for type names.

- Use UpperCamelCase for enum members.

- Use UpperCamelCase for variable and class members.

- Use lowerCamelCase for function and method names.

- Avoid `using namespace std`

- Use `auto` when the deduced type is explicitly spelled out in the
  initialization or if the deduced type is an abstract type
  alias.  Always explicitly specify type qualifiers, pointers, and
  references.  E.g.,
  ```c++
  const auto *Ptr = dynamic_cast<const Foo *>(SomePtr);
  auto Val = static_cast<unsigned>(SomeValue);
  for (auto Iter = SomeContainer.begin(), End = SomeContainer.end(); Iter != End; ++Iter) {}
  ```

- Use `auto` to make code more readable, but prefer `auto &` or `auto *`
  to avoid unexpected copies.

- `#include` as little as possible to reduce compile times. Use
  forward declarations of classes when possible to avoid including
  their definitions.


### Testing Development

- All code you care about should be tested.
- Any code you don't care about should be removed.
- Code testing is done via Google Test.
- Test names are prefixed with the type of test they are (`Unit_`, `System_`, `Integration_`).
- No unit test should take more than 0.5 seconds.


### Documentation

The GTIRB documentation consists of complete documentation for all
components of the GTIRB API, along with examples and other usage
information.


#### Building Documentation

You will need `cmake` and `Doxygen`.

1. Create and change to a temporary build directory. We will refer to
   this directory as `build`.

   ```bash
   > mkdir build
   > cd build
   ```


2. Build the documentation.

   ```bash
   build> cmake <PATH_TO_GTIRB>/doc/doxy/
   build> cmake --build . --target doc
   ```

3. Open the documentation home page `build/html/index.html`
   in your browser.


#### Contributing Markdown Documentation

To add a new markdown document to the documentation:

1. Create the new document as a child of /doc.
   - File names start with `gtirb`.
   - File extension is `.md`.
   - Use github markdown syntax.
   - Wrap your markdown documents at 80 columns.

2. Edit `/doc/doxy/Doxyfile.in` to add the basename of your new markdown
   document to the `INPUT` rule setting. Note that the ordering of
   file names here corresponds to table of contents ordering.

3. Edit `/doc/doxy/CMakeLists.txt` to add your new markdown document
   to  `MDFILES_IN`. Ordering is not important.

4. [Build the documentation](#building-documentation) and check that
   your new page is present and rendered correctly.
   - If it is not rendered correctly, you may need to add a new
     preprocessing step to `doc/doxy/preprocmd.py` to rewrite the
     corresponding github-style markdown into something Doxygen
     can handle correctly.

#### Graphviz

- File names start with `gtirb`.
- The color palette is `black`, `lightblue`, `cornflowerblue`, and `coral`.
- Render `.dot` files to the same file name with a `.png` extension.
	* Example: `dot -Tpng gtirbScope.dot > gtirbScope.png`
- Use the `arial` font.

## Python Code Requirements

- Code must be [PEP8](https://www.python.org/dev/peps/pep-0008/) compliant.
  To check for PEP8 compliance, [flake8](https://pypi.org/project/flake8/) is recommended,
  and included as part of our `pre-commit` configuration.

- All code must be formatted with [Black](https://pypi.org/project/black/)
  (set to line lengths of 79, for PEP8 compliance).
  A pass through this tool is included as part of our `pre-commit` configuration.
  - Please note that `black` only works on Python version 3.6 and newer.
    This is newer than what is available on some OSes by default (for example, Ubuntu 16),
    so you may have to install Python 3.6 or newer to run `black`.

- While committing changes to the Python API requires Python 3.6 or later (see above),
  the Python API should be made to run on all version of Python 3.

- Use `UpperCamelCase` for type names, `UPPER_CASE` for constant names,
  and `snake_case` for other identifier names.

### Testing Development

- All code you care about should be tested.
- Any code you don't care about should be removed.
- Code testing is done via the built-in `unittest` framework.
- No unit test should take more than 0.5 seconds.

### Documentation

As with the C++ API, The GTIRB documentation consists of complete documentation for all
components of the GTIRB API, along with examples and other usage information.

<!-- TODO: documentation via Sphinx -->
