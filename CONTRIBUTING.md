Contributing               {#CONTRIBUTING}
============


## Code of Conduct

Please read the [GTIRB Code of Conduct](CODE_OF_CONDUCT.md).

## General Guidelines

- Text files may not have trailing whitespace.

- Text files must end with a trailing newline.

- All tests should be able to run and pass.
  This can be checked by running `make check` on your build directory
  after running `cmake`.

- All CMake files shall be formatted with
  [cmake-format](https://pypi.org/project/cmake-format/).  A
  `.cmake-format` file is provided in the root directory for the
  project, and a pass through this tool is included as part of our
  `pre-commit` configuration (see below for details).

### pre-commit

In general, code must follow a unified format. To make compliance with
this format easier, we recommend the use of
[`pre-commit`](https://pre-commit.com/) with the provided
configuration file, `.pre-commit-config.yaml`, to manage formatting.
To use `pre-commit`:

1. If `pre-commit` is not already installed on your system, install it
   now with [`pip`](https://pypi.org/project/pip/).
   ```shell
      pip3 install pre-commit
   ```
2. If [`clang-format`](https://clang.llvm.org/docs/ClangFormat.html)
   is not already installed on your system, install it now.
3. Install the formatters as a pre-commit hook. In the gtirb root directory:
   ```shell
    pre-commit install
   ```
   If you prefer to run `pre-commit` manually instead, run this before all commits:
   ```shell
   pre-commit run
   ```

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
  ```cpp
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
- C++ code should be tested on Linux using GCC and Clang, and on Windows using Visual Studio.
- Code testing is done via Google Test.
- Test names are prefixed with the type of test they are (`Unit_`, `System_`, `Integration_`).
- No unit test should take more than 0.5 seconds.


### Documentation

The GTIRB documentation consists of complete documentation for all
components of the GTIRB API, along with examples and other usage
information.


#### Building Documentation

At minimum, you will need [CMake](https://cmake.org/) and
[Doxygen](http://www.doxygen.nl/).  In addition, for the other APIs
you may have enabled:


* For the Python API, [Sphinx](https://www.sphinx-doc.org/en/master/)
  and [related plugins](https://pypi.org/project/sphinx-autodoc-typehints/)
  are required. To install these via [pip](https://pip.pypa.io/en/stable/), run:

  ```bash
  pip3 install sphinx sphinx-autodoc-typehints
  ```

  You will also need the Python `protobuf` module so that Sphinx can
  properly handle the API implementation. If you haven't already
  installed this, do so now.

  ```bash
  pip3 install protobuf
  ```

* For the Common Lisp API, [simpler-documentation-template (SDT)](https://github.com/eschulte/simpler-documentation-template)
  is required. This package should automatically be downloaded via the build process;
  see `cl/README.md` for details on how to prepare the Common Lisp API.


To build the documentation:


1. Create and change to a temporary build directory. We will refer to
   this directory as `build`.

   ```bash
   > mkdir build
   > cd build
   ```

2. Build the documentation.

   ```bash
   build> cmake <PATH_TO_GTIRB> [<api_options>]
   build> cmake --build . --target doc
   ```

3. Open the documentation home page `build/doc/html/index.html`
   in your browser.


The `<api_options>` are as follows

- `-DGTIRB_CXX_API=OFF` : do not generate C++ API documentation.

  If this option is not specified, `cmake` will attempt to generate
  C++ API documentation, failing (along with the documentation build
  as a whole) if [Doxygen](http://www.doxygen.nl/) is not available.


- `-DGTIRB_CL_API=OFF` : do not generate Common Lisp API documentation.

  If this option is not specified, `cmake` will attempt to generate
  Common Lisp API documentation if and only if it can locate a
  SBCL/Quicklisp installation, failing if
  [simpler-documentation-template
  (SDT)](https://github.com/eschulte/simpler-documentation-template)
  is not available.

- `-DGTIRB_PY_API=OFF` : do not generate Python API documentation.

  If this option is not specified, `cmake` will attempt to generate
  Python API documentation if and only if it can locate a Python

  installation, failing if [Sphinx](https://www.sphinx-doc.org/en/master/),
  [sphinx-autodoc-typehints](https://pypi.org/project/sphinx-autodoc-typehints/),
  or the Python `protobuf` module are not available.


#### Contributing Markdown Documentation

To add a new markdown document to the documentation:

1. Create the new document as a child of `/doc`.
   - File names start with `gtirb`.
   - File extension is `.md`.
   - Use github markdown syntax.
   - Wrap your markdown documents at 80 columns.

2. Edit `/doc/general/Doxyfile.in` to add the basename of your new markdown
   document to the `INPUT` rule setting.

3. Edit `/doc/general/CMakeLists.txt` to add your new markdown document
   to  `MDFILES_IN`. Ordering is not important.

4. Integrate your new markdown document into the documentation, either
   by linking to it from an existing page or by updating
   `/doc/general/DoxygenLayout.xml` to add an entry to the **More Information**
   tab.

5. [Build the documentation](#building-documentation) and check that
   your new page is present and rendered correctly.
   - If it is not rendered correctly, you may need to add a new
     preprocessing step to `doc/general/preprocmd.py` to rewrite the
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
    If installing Python 3.6+ on your system is not possible, there exists
    [an online interface to Black](https://black.now.sh/?version=stable&state=_Td6WFoAAATm1rRGAgAhARYAAAB0L-Wj4AA-ACxdAD2IimZxl1N_W1ktIvcnCRyzdeeGA586U8RMKbisP9D6xUd8v4usX-jR3lIAACNC8ndFJAQXAAFIPxtdQK4ftvN9AQAAAAAEWVo=)
    you can manually enter files into.

- The Python API should be made to run on all version of Python 3.

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
