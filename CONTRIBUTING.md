Contributing
============

Code
----

Testing
-------

- All code you care about should be tested.
- Any code you don't care about should be removed.
- Code testing is done via Google Test.  
- Test names are prefixed with thet type of test they are ("Unit_", "System_", "Integration_").
- No unit test should take more than 0.5 seconds.
- Do not use 'using namespace' inside test cases.  Fully qualify everything.


Documentation
-------------
- Documentation is stored in the `/doc` folder.

### Markdown Documents

- File names start with `gtirb`.  
- Soft wrap your markdown documents in your editor.

### Graphviz

- File names start with `gtirb`.  
- The color palette is `black`, `lightblue`, `cornflowerblue`, and `coral`.
- Render `.dot` files to the same file name with a `.png` extension.
	* Example: `dot -Tpng gtirbScope.dot > gtirbScope.png`
- Use the `arial` font.
