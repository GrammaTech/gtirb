Contributing
============

Code
----

- All code shall be formatted with clang-format.  A '.clang-format' is provided in the root directory for the project.
- Libraries and executables (with the exception of test executables) are prefixed with `gtirb`.  
- Test executables are prefixed with `Test` and then followed by the name of the library they are testing.

Branches And Pull Requests
--------------------------

We use Gitflow as our branch management system. Please read up on it [here](https://www.atlassian.com/git/tutorials/comparing-workflows/feature-branch-workflow). The main points you should know are:

* All feature branches should be based on `develop` and have the format `feature/branch_name`.
* The branch name is the exact name of the corresponding issue in GitLab (Yes.  Create an issue if you don't have one.  It will be given a number.  `#1234` is your branch name.)
* Minor bug fixes should be based on `master` and have the format `hotfix/branch_name`.
* All pull requests should involve a single change. Pull Requests that involve multiple changes (it is our discretion what precisely this means) will be rejected with a reason.
* All commits should involve logical units. Please do not put all changed code in one commit, unless it is a very minor change.
* Work in progress pull requests should have `[WIP]` in front of the Pull Request title. 
* Please document all code written. Write Doxygen style comments for methods in the header files, and use inline code comments where it makes sense, i.e. for non-obvious code chunks.
* All commit messages should be client ready.  That is, they should all be brief, instructive, and professional.

Testing Development
-------------------

- All code you care about should be tested.
- Any code you don't care about should be removed.
- Code testing is done via Google Test.  
- Test names are prefixed with thet type of test they are (`Unit_`, `System_`, `Integration_`).
- No unit test should take more than 0.5 seconds.
- Do not use 'using namespace' inside test cases.  Fully qualify everything.

Documentation
-------------
- Documentation is stored in the `/doc` folder.
- Graphs located in the `/doc/dot` folder.
- HTML API documentation (Doxygen) is located in the `/doc/html` folder.  A Doxyfile is located inthe root directory for the project.

### Markdown Documents

- File names start with `gtirb`.  
- Soft wrap your markdown documents in your editor.

### Graphviz

- File names start with `gtirb`.  
- The color palette is `black`, `lightblue`, `cornflowerblue`, and `coral`.
- Render `.dot` files to the same file name with a `.png` extension.
	* Example: `dot -Tpng gtirbScope.dot > gtirbScope.png`
- Use the `arial` font.
