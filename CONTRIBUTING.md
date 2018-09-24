Contributing
============

Branches And Pull Requests
--------------------------

We use Gitflow as our branch management system. Please read up on it
[here](https://www.atlassian.com/git/tutorials/comparing-workflows/feature-branch-workflow). The
main points you should know are:

* All feature branches should be based on `develop` and have the
  format `feature/branch_name`.
* The branch name is the exact name of the corresponding issue in
  GitLab (Yes.  Create an issue if you don't have one.  It will be
  given a number.  `#1234` is your branch name.)
* Minor bug fixes should be based on `master` and have the format
  `hotfix/branch_name`.
* All pull requests should involve a single change. Pull Requests that
  involve multiple changes (it is our discretion what precisely this
  means) will be rejected with a reason.
* All commits should involve logical units. Please do not put all
  changed code in one commit, unless it is a very minor change.
* Work in progress pull requests should have `[WIP]` in front of the
  Pull Request title.
* Please document all code written. Write Doxygen style comments for
  methods in the header files, and use inline code comments where it
  makes sense, i.e. for non-obvious code chunks.
* All commit messages should be client ready.  That is, they should
  all be brief, instructive, and professional.

Testing Development
-------------------

- All code you care about should be tested.
- Any code you don't care about should be removed.
- Code testing is done via Google Test.  
- Test names are prefixed with the type of test they are (`Unit_`, `System_`, `Integration_`).
- No unit test should take more than 0.5 seconds.

Documentation
-------------

The GT-IRB documentation consists of complete documentation for all
components of the GT-IRB API, along with examples and other usage
information.


### Building Documentation

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


### Contributing Markdown Documentation

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

### Graphviz

- File names start with `gtirb`.  
- The color palette is `black`, `lightblue`, `cornflowerblue`, and `coral`.
- Render `.dot` files to the same file name with a `.png` extension.
	* Example: `dot -Tpng gtirbScope.dot > gtirbScope.png`
- Use the `arial` font.

Code Requirements
-----------------

- All code shall be formatted with clang-format.  A '.clang-format' is
  provided in the root directory for the project.
- Code should generally follow the C++ Core Guidelines recommendations.
- Code should generally allow for thread safety.
	- No static variables.
	- No globals
	- Free functions should not maintain state.
	- Use caution when using iterators to guard against invalidation.
- Code should be tested on Linux using GCC and Clang and on Windows using Visual Studio.
- Maintain const-correctness.
- Use UpperCamelCase for type names.
- Use UpperCamelCase for enum members.
- Use UpperCamelCase for variable and class members.
- Use lowerCamelCase for function and method names.
- Avoid `using namespace std`
- Use `auto` to make code more readable, but prefer `auto &` or `auto *` 
  to avoid unexpected copies.
- `#include` as little as possible to reduce compile times. Use
  forward declarations of classes when possible to avoid including
  their definitions.
  
