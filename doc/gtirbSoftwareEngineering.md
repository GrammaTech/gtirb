GT-IRB Software Engineering
==========================

Executive Summary
=================

This is a companion document to the _GT-IRB Roadmap_.  The develpment of GT-IRB is a significant effort. As such, it provides an opportunity to re- evaluate and improve our software engineering principles, processes, and tools. This document attempts to capture the high-level goals we are working towards and provides suggestions for where the processes and tools can evolve to better compliment the overall software development ecosystem within the company.

Overview
========

### Technical Goals and Requirements

- Leverage Modern Software Engineering Principles and Modern C++.
  * Pick a modern language standard to target.
    - The threshold requirement is C++ 14. The objective is C++17.
    - Define a minimum compiler version which is to be supported. Test on this
      minimum version and the latest version at all stages.
  * Strongly favor following the C++ Core Guidelines. References to the Core Guidelines are shown in this document inside parentheses. (E.g ([CP.1](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rconc-multi)))
  * Follow _SOLID_ design principles.
    - **S**ingle Responsibility Principle
    - **O**pen/Closed Principle
    - **L**iskov Substitution Principle
    - **I**nterface Segregation Principle
    - **D**ependency Inversion Principle
  * Follow the DRY design principle:
    -  The _Don’t Repeat Yourself_ (DRY) principle states that duplication in logic should be eliminated via abstraction; duplication in process should be eliminated via automation.
- Develop meaningful, verifiable, and enforceable standards on practices and methods used to develop the GT-IRB.
  * Encourage the use of standards and modern language features.
  * Use tooling to automate quality control.
- The GT-IRB should not be dependent on any more technologies than is necessary, nor should it impose any unnecessary technical obligations on the users.
  * Reduce (aim to eliminate) the use of Third Party libraries in the GT-IRB implementation.
    - Keep only the most necessary libraries, but also don’t reinvent the wheel.
  * Users should be able to utilize their scripting language of choice.
- Build a Strong Testing Regime
  * Create code that works.
    - Prove that it works through testing.
  * When code breaks, the tests should be sufficient pinpoint the source of the error.
  * Prioritize testing requirements based on risk and complexity of the code under test.
  * Design for automation.
  * Adapt the testing regime to meet new challenges that are discovered during development.
  * New code should be accompanied by new unit tests.
  * Changes to the API should be accompanied by new integration tests.  


### Business Requirements and Goals

Beyond the technical goals, there may be goals which are more focused on GrammaTech as a company. By documenting these goals and their motivation, we can make better technical and investment decisions during the execution of GT-IRB development.

- Build a strong enterprise of processes, tools, libraries which can be leveraged to turn intellectual capital into technical capital.
  * Evolving processes and tools should address the company’s growth and business targets.
  * The ecosystem for the GT-IRB can serve as a model for the firm.
- Reduce infrastructure overhead.
  * Reduce the time and effort spent on infrastructure development and maintenance.
  * Having a wide variety of supporting tools in the ecosystem creates costs in both direct administration as well as creating and training on bespoke processes that quickly diverge among various product teams.

Software Engineering Standards
==============================

## Eliminate Magic Numbers

The term “magic number” refers to the anti-pattern of using numbers directly in source code. (This should be caught in code review.) Replacing all significant magic numbers with named constants makes programs easier to read, understand, and maintain. This will be an on-going maintenance topic as code is updated. Removal of magic numbers immediately fixes subtle bugs where inconsistencies were going unnoticed. From “C++ Coding Standards: 101 Rules, Guidelines, and Best Practices by Andrei Alexandrescu, Herb Sutter”:

> “Programming isn’t magic, so don’t incant it: Avoid spelling literal constants like _42_ or _3.14159_ in code. They are not self-explanatory and complicate maintenance by adding a hard-to-detect form of duplication. Use symbolic names and expressions instead.”

## Enforce RAII

Going forward, unit testing will be increasingly integrated into the GT-IRB code base. (Deterministic tests are required for effective testing.) All POD types should be explicitly initialized in an object’s constructor.

> “To avoid leaks and the complexity of manual resource management. C++'s  language-enforced constructor/destructor symmetry mirrors the symmetry inherent in resource acquire/release function pairs such as *fopen*/*fclose*, *lock*/*unlock*, and *new*/*delete*. Whenever you deal with a resource that needs paired acquire/release function calls, encapsulate that resource in an object that enforces pairing for you -- acquire the resource in its constructor, and release it in its destructor.”* - ([R.1](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rconc-multi), [E.6](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rconc-multi))

## Establish and Enforce File Naming Conventions

Start at user-facing code and work inward. Establish a 1:1 class declaration (`.h`) to class definition (`.cpp`) ([SF.1](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rs-file-suffix), [SF.2](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rs-inline)).  There is a volume of work here, but it is not difficult work. It will have an impact to the `#include` files in external projects, but these types of transformations are easily automated.

## General Code Modernization

There are many minor activities that can be accomplished on an as-seen basis as general code cleanup activities. The specific smells/bugs to look for (and their mitigation approach) should be documented. An initial list has been started here.

- **Begin eliminating preprocessor macros.** - CMake can eliminate
    some, others can simply be replaced by better coding practices.
- **Reconcile duplicate code and functions.** - There are, for example, numerous places where strings are tokenized using different support libraries and techniques, even though they are all processing command lines. This increases the opportunity for bugs whereas we could be leveraging a single function or technique for this across the board.
- **Fix non-virtual destructors.** - ([C.127](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rh-dtor))
- **Replace `struct` with `class`** - ([C.8](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rc-class)) 
  * Organize class definitions uniformly.
- **Replace enums with `enum class`** - ([Enum.3](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Renum-class))
- **Fix Enumeration naming and values.** - ([Enum.5](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Renum-caps), [Enum.6](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Renum-unnamed))
- **Apply a standard formula to `#include` guards.** - ([SF.8](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rs-guards))
- **Utilize `override` with all `virtual` functions to indicate intent.** - ([C.128](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rh-override))
- **Utilize `constexpr`** - ([F.4](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rf-constexpr))
- **Utilize `noexcept`** - ([F.6](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rf-noexcept))
- **Replace C-style fixed arrays with std::array** - This may be a `clang-tidy` feature.
- **Simplify in the face of obvious premature optimization.**

## Improve the Physical Layout

Where appropriate, implement the PIMPL idiom within classes. ([I.27](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Ri-pimpl)) This will insulate consumers of the library from more invasive internal changes both in subsequent phases as well as after deployment. This will also have the benefit of reducing dependencies and improving compile times. (This will be effected by reducing header load and preventing recompilation entirely when implementation details change.)

John Lakos, the author of “Large-Scale C++ Software Design”, has published an [on-line reference](http://www.cs.unc.edu/~stotts/COMP204/lakos/) providing high-level guidance on physical design. Each of these are reasonable and achievable goals that we should work towards over the course of this project. This brief list also highlights the magnitude of work that must be done here; virtually every one of these guidelines is violated within the code required for use within the GT-IRB:

> 1.  Keep class data private
> 
> 2.  Avoid data with external linkage at file scope.
> 
> 3.  Avoid free functions (except operator functions) at file scope in `.h` files; 
> 
> 4.  Avoid free functions with external linkage (including operator functions) in `.c` files.
> 
> 5.  Avoid enumerations, typedefs, and constants at file scope in `.h` files.
> 
> 6.  Avoid using preprocessor macros in header files except as include guards.
> 
> 7.  Only classes, structures, unions, and free operator functions should be declared at files scope in a '.h' file;  only classes, structures, unions, and inline (member or free operator) functions should be defined at file scope in a '.h' file.
> 
> 8.  Place a unique and predictable (internal) include guard around the contents of each header file.
> 
> 9.  Logical entities declared within a component should not be defined outside that component.
> 
> 10. The .c file of every component should include its own .h file as the first substantive line of code.
> 
> 11. Avoid definitions with external linkage in the '.c' file of a component that are not declared explicitly in the corresponding '.h' file.
> 
> 12. Avoid accessing a definition with external linkage in another component via a local declaration; instead, include the .h file for that component.
> 
> 13. Prepend every global identifier with its package prefix.
> 
> 14. Prepend every source file name with its package prefix.
> 
> 15. Avoid cyclic dependencies among packages.
> 
> 16. Only the '.c' file that defines main is authorized to redefine global new and delete.
> 
> 17. Provide a mechanism for freeing any dynamic memory allocated to static constructs within a component.
> 
> 18. Place a redundant (external) include guard around each preprocessor include directive in every header file.
> 
> 19. Use a consistent method (such as a 'd_' prefix) to highlight class data members.
> 
> 20. Use a consistent method (such as uppercase first letter) to distinguish type names.
> 
> 21. Use a consistent method (such as all uppercase with underscore) to identify immutable values such as enumerators, const data, and preprocessor constants.
> 
> 22. The root names of the .c file and the .h file that comprise a component should match exactly.
> 
> 23. Never pass a user-defined type (i.e., class, struct, or union) to a function by value.
> 
> 24. Never attempt to delete an object passed by reference.
> 
> 25. In every class that declares or is declared from a class that declares a virtual function, explicitly declare the destructor as the first virtual function in the class and define it out of line.
> 
> 26. Avoid depending on the order in which data members are defined in an object during initialization.
> 
> 27. When implementing memory management for a general, parameterized container template, be careful not to use the assignment operator of the contained type when the target of the assignment is uninitialized memory.

## Organize Related Data

Where data is independent, but logically 1:1, build a structure to programmatically enforce the relationship. For example, there are examples in the code where a class contains two arrays. Each array contains data that is directly related to the data at the same position in the other array. In such cases there should logically be only a single array where each element contains a structure with the correlated data.

This is a minor rewrite in many places, but hopefully impacts only the implementation (by and large). It will immediately make the code more expressive and possibly eliminate subtle bugs and the overhead of bookkeeping.

## Compile Warning Free

The goal is to not disable any warnings on any compiler on any platform. Any warnings that are disabled must be strongly documented and justified. Preferably, these exceptions are not made in such a way as to require clients of the GT-IRB to disable the warnings as well for their code.

## namespace gt;

The development of the GT-IRB inside of a namespace is important from both technical and business perspectives. Technically, utilization of a namespace will prevent name collisions. If any third party wanted to utilize GT-IRB, it is critical that the act of simply including GT-IRB does not break their code. This is accomplished at a high level from use of a custom namespace.

The namespace also brands our code. It means that anywhere client code uses GT-IRB, it is clear that they are leveraging GrammaTech-developed technology.

Moving into the future, usage of a namespace will bring uniformity and firewalls into our corporate code base. If properly implemented and enforced, we could even build custom tooling that leverages the fact that code lives in our own namespace.

## Segregate the Business Logic from the Data Structure

Remove custom console loggers, file loggers, etc. while providing a way to hand all logging information to the API user. There are many strategies available to accomplish this including using a visitor pattern or providing a generic logger with custom sinks and well formatted messages.

## Eliminate all global (and static) variables ([I.2](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Ri-global), [R.6](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rr-global))

Herb Sutter, Andrei Alexandrescu, C++ Coding Standards, Item 10 states: _"Avoid shared data, especially global data."_

The prototype GT-IRB found extensive use of both global and static variables. This will be a significant effort and likely inform the restructuring of the GT-IRB. The code review process checklist should be updated to include extra scrutiny for such coding practices.

## Eliminate all singletons (I.3) (Except where you can’t) 

The Singleton pattern does have its place, but it should be used with care in large systems. It may be found that much of the use of Singleton can be removed from GT-IRB itself and pushed out to users of the GT-IRB. If the GT-IRB is structured properly, it will likely be nothing more than a purpose-built data structure. This should largely remove any requirement to have a Singleton pattern used within in the library code. (One obvious exception may be the use of the Factory pattern for plugins, which utilizes a Singleton. This is still external to the data structure itself, however.)

## Enforce const-Correctness ([Con.2](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rconst-fct), [Con.3](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rconst-ref), [Con.4](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rconst-const), [Con.5](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rconst-constexpr))

Long term, this is a requirement for a good library. Const-correctness is notoriously difficult to enforce on a legacy code base which did not originally consider it. Where possible, it should be enforced moving forward and in all new code being developed for GT-IRB.

## Remove all malloc/free Calls in the Code Base ([R.10](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rr-mallocfree), [R.22](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rr-make_shared),  [R.23](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rr-make_unique))

As we are compiling all code as C++, there is no need to use malloc/free. Further, malloc and free do not know about constructors and destructors. Reference Item \#3 from Scott Meyer’s “Effective C++”.

## Remove all Naked new/delete Calls in the Code Base ([R.11](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rr-newdelete), [R.22](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rr-make_shared), [R.23](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rr-make_unique))

Where pointers to objects must be constructed, utilize `std::make_unique` and `std::make_shared`.

1.  **Option 1: Remove `ref_ptr`** - It is recommended that we remove the
    custom `ref_ptr` implementation from the GT-IRB library. Where appropriate,
    it can be replaced with `std::shared_ptr` ([R.20](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rr-owner), 
    [R.21](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rr-unique)). 
    It may be revealed during the redesign that the need to share object
    ownership is completely unnecessary in the data structure. (Or even largely
    unnecessary, leaving only special cases where the `std::shared_ptr` is
    maintained.)
    
2.  **Option 2: Keep `ref_ptr`** - If there exists a requirement that prevents
    us from using the standard infrastructure, then we should (at a minimum)
    ensure that `ref_ptr` is conformant with the standard. 
    ([R.31](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rr-smart))

Software Engineering Processes
==============================

## Eliminate the In-Source Build

In-source builds do not scale. Elimination of an in-source build has numerous benefits which will be leveraged immediately within the GT-IRB development.

- Allow to scale to having multiple builds with identical source (debug vs. release, for example)
- It makes the code easier to clean
- Discourages having multiple copies of source on a single machine.

## Evaluate Creating a “Build Engineer” Role

Consider creating the role of “Build Engineer”. The Build Engineer would be responsible for maintaining the DevOps environment, looking at test failures, and various automation processes.

## Best Practices Course

Begin training developers on the best practices for C++ development going forward. The aim would be to help ensure that the work being done in GT-IRB development is not undone by old habits. The course would be developed and lead in a way to ensure that high priority items are emphasized while encouraging higher quality standards are enforced on a peer-to-peer basis.

## Postmortem

Create the framework for and implement a postmortem process. This is a formal meeting that serves as a retrospective for the phase (or sprint) with the aim of building knowledge within the team which can be applied to the next phase (or sprint).

Postmortems are sometimes perceived as only following a project failure, but this is not the true intent. The aim of the software postmortem is to understand the root causes for what worked well as much as what did not. Even when a project (or phase in this case) is not a failure, there are always aspects where improvements can be made. In this way, we begin to see more objectively where the team has strengths, weaknesses, and biases. Negative patterns can be addressed while positive ones can be reinforced. The first exercise of this process would be to help evaluate the current GT-IRB at this phase and help drive additional requirements and goals for the future phases. Procedurally, a postmortem can be overly simplified. But to be effective, it is a deeper process than can be expressed on a checklist.

To quote Mike Gunderloy:

> “The difference between average programmers and excellent developers is not a matter of knowing the latest language or buzzword-laden technique. Rather, it can boil down to something as simple as not making the same mistakes over and over again. Fortunately, there's a powerful tool that any developer can use to help learn from the past: the project postmortem.”

The postmortem improves both the quality of the code and the culture where acknowledgement and understanding of mistakes is encouraged. The guidance suggested for postmortems comes not from the software engineering world, but from the business world. Consider Ray Dalio’s principles on the topic of root cause analysis of mistakes:

> Create a Culture in Which It Is OK to Make Mistakes but Unacceptable Not to Identify, Analyze, and Learn From Them.
> 
> -   Recognize that effective, innovative thinkers are going to make mistakes.
> -   Do not feel bad about your mistakes or those of others. Love them!
> -   Observe the patterns of mistakes to see if they are a product of weaknesses.
> -   Do not feel bad about your weaknesses or those of others.
> -   Don’t worry about looking good—worry about achieving your goals.
> -   Get over “blame” and “credit” and get on with “accurate” and “inaccurate.”
> -   Don’t depersonalize mistakes.
> -   Write down your weaknesses and the weaknesses of others to help remember and acknowledge them.
> -   When you experience pain, remember to reflect.
> -   Be self-reflective and make sure your people are self-reflective.
> -   Teach and reinforce the merits of mistake-based learning.”

This procedural change is important to GT-IRB development because we will need honest, intellectual feedback on the current implementation to drive an improved implementation. Through setting ground rules and working to ingrain this into a normal part of our engineering processes, we will be able to produce better software.

## GitFlow

Begin to manage GT-IRB development using a standardized process called GitFlow. As there may be many developers working in parallel on the code base, having a formalized and appropriately scalable process for managing the code is critical.

<img src="./media/Image_04.png" data-canonical-src="./media/Image_04.png" width="512" height="*" align="middle" style="display:block; margin-left: auto; margin-right: auto;"/>

GitFlow manages the branching mechanism with a standardized approach when developing new features, releases, and hotfixes. It is designed to integrate into many available commercial and open source tools.  (GitFlow is natively supported by Atlassian tools and works well with both GitHub and GitLab.) The branching mechanism also allows for tight coupling between issues, bugs, source code changes, test development, test results, and documentation.

Git’s default branching flexibility tends towards complexity in its management. GitFlow directly addresses this with codified and tool-enforced mechanisms. It unifies the entire team’s branch process, building consistency. Switching branches is simplified while it promotes code review, pull requests, and maintenance of a clean repository.

Software Testing
================

## Testing Architecture

Integrate a consistent testing architecture. GT-IRB, especially if shared with external organizations, must be thoroughly tested. Testing goes hand in hand with maintenance. To reduce maintenance, we must have effective testing.

> “Finding and fixing bugs has been the most expensive task in software for more than 50 years. A synergistic combination of defect prevention and pre- test inspections and static analysis are needed to achieve this goal.” - Pete Pizzutillo, VP Corporate Marketing at CAST

Having a unified testing architecture that we do not have to train on, which enforces consistency, can be integrated into existing tooling, and is commercially supported would be a great improvement to the code base.

The C++ Core Guidelines offers good advice on overuse of bespoke tooling ([P.12](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rp-tools)):

> “Be careful not to become dependent on over-elaborate or over-specialized tool chains. Those can make your otherwise portable code non-portable.”

After reviewing several common unit testing architectures to include Boost Test, CppUnit, CppTest, it was found that Google Test (sometimes referred to as Gtest) would best fit our needs. There is a great deal of commonality among these libraries, but Google Test is clearly an advanced and well supported architecture with features we will be able to leverage to improve our overall testing regime.

Some advantages listed out on a Stack Overflow thread on the topic include:

- Tests build to self-contained executables that do not require any third party software to run.
- `INSTANTIATE_TEST_CASE_P` to instantiate a test case with any set of parameters you want, including Cartesian products. (For example, the test could discover all binaries in a test database and construct a unique set of test cases for each binary, all of which are automatically added to all reporting and dashboards in the DevOps environment.)
- `FRIEND_TEST` for testing private class members (for all the legacy code)
- Turning asserts into breakpoints
- Non-fatal asserts
- "out of the box" googlemock integration
- Automatic tests detection, no need to enumerate them
- Tests can be disabled and enabled
- Tests to run can be selected using name patterns
- Value/type-parameterized tests
- User-defined predicate asserts
- Death tests
- A rich set of asserts (including type asserts)
- Asserting on subroutines
- Additional debug info can be added to asserts using `<<`;
- RecordProperty emits last value of property to the XML output
- `SCOPED_TRACE` helps understand the context of an assertion failure coming from inside a sub-routine or loop.
- xUnit XML output, can be shown by Jenkins right away without a XSLT transformation in-between.
- Custom Types Printer Support
- Time Consumed by Test Measurements
- Test event listener API (user-defined plug-ins. This extremely powerful in scaling advanced testing techniques and custom test result collection.)
- Test Shuffling

A review of the current bespoke testing programs was unable to locate features which were not expressed within the Google Test framework. Google Test has many basic to advanced features which we can use uniformly across the GT-IRB test surface (to include mocking). We can tailor it to support various levels of tests. This will allow only "Unit" tests to quickly run on all branches prior to code review, but historical "Regression" level tests run every night, etc. Further, it allows for death tests (ensuring that a program crashes in specific ways). Google test output is concise () and easily (even natively) imported in to virtually all common DevOps environments.

<img src="./media/Image_03.png" data-canonical-src="./media/Image_03.png" width="512" height="*" align="middle" style="display:block; margin-left: auto; margin-right: auto;"/>

Using CMake, we can even require that certain tests pass in order for a build to succeed locally, improving the level of quality prior to commiting or code review.

Google Test includes the Google Mock library as well. From the Google Mock documentation:

> When you write a prototype or test, often it's not feasible or wise to rely on real objects entirely. A mock object implements the same interface as a real object (so it can be used as one), but lets you specify at run time how it will be used and what it should do (which methods will be called? in which order? how many times? with what arguments? what will they return? etc).

Advanced features of Google Test would also allow us to create highly customized test processing tooling such as automatic report generation. These reports could be human-readable and suitable for deliverables to clients.

Transitioning tests to Google Test will likely be an on-going effort. It is suggested that core tests are moved quickly to Google Test. But tests that are touching pieces that may be eliminated or completely rewritten in later phases not be ported until necessary. When the GT-IRB project is complete, however, all testing should be done with Google Test.

## Build a Strong Unit Test Suite

Migrate/duplicate existing regression tests. Establish a methodology for leveled, targeted testing. That is, make it trivial to run “just the unit tests” from any developer machine, for example.

Build up new tests to stress areas where we can anticipate problems in the further re-engineering efforts to be conducted to help ensure a working data structure. (The suggestion is not to implement formal Test Driven Development (TDD), but ensure that we are testing for core functionality and that anything we like is tested.)

Consider adopting Behavior Driven Development (BDD). BDD is a set of best practices for writing automated tests. The general idea is that you should test the implementation’s behavior.

The use of Google Test can allow us to build *typed tests*. The idea is that we can write test that scale to a large number of different concrete types with identical API’s (E.g plugins to the architecture, derived types). Through this, we can ensure that classes are appropriately testable. Google Test also offers *friend* testing for use where our designs are non-optimal for traditional testing.

Tooling could also be designed to write smoke tests for classes, fuzz testing, standards conformance, and load testing. Such tooling can be quickly developed to target our testing architecture and scale across the code base.

## Categorize All Tests

Create strict categories that all tests must fit into.  These categories would allow us to filter tests and allow us to test more often and to break testing when certain phases do not complete.  An example of these categories is as follows:

- Unit Tests
- Integration Tests
- Functional Tests
- GUI Tests
- End-to-End Tests

These are standard categories with well accepted definitions.  We could bake these types into our Google Test architecture. This would allow anyone to quickly filter tests by type:

```bash
TestGT_Core --gtest_filter=Unit*
```


## Cross-Platform Build and Test

Begin to build and test on Linux and Windows using GCC, Clang, and Visual Studio. Often, each compiler will find different code smells while also ensuring that our GT-IRB is ready for prime time on multiple platforms. Working towards the goal of having a manageable number of reports from Code Sonar: starting to build and test on a variety of compilers and hardware will ensure the baseline does not develop unnecessary cross platform issues moving forward.

## Dis-Entangle Legacy Tests

Improve the automated testing so that one test can utilize the artifacts created by another test. Where dependencies exist, strongly enforce them through the build tool. Ensure, to whatever extent possible, that duplication in testing is not occurring. Such dependencies would allow up-stream failures to not propagate down to dependent tests, stop failing builds faster, and help pinpoint the source of errors.

## Track Code Coverage

As the change velocity for the GT-IRB will be increasing, addressing developers being notoriously lax in creating new tests gains importance. Even legacy tests may not be touching some critical paths which we may have incorrectly assumed were getting exercised.

Admittedly, having 100% code coverage doesn’t mean your code is 100% tested. However, measuring it can give you meaningful information about where you have insufficient coverage. A tool should be integrated into the CT environment which reports on test coverage to guide issue creation and keep the team aware of where blind spots might be.

We should perform a thorough survey of available tools. (A cursory search turned up *Atlassian Clover* and *Coveralls (https://coveralls.io/)*, both of which look to be fully featured and integrate with the Atlassian suite as well as GitHub and GitLab.)

## Require New Test Development

Implement a new rule that all new features integrated into the GT-IRB project are accompanied by a corresponding set of tests. This should be verified during code review. This will help build up the testing regime for the library. Further, it ensures that the library is, in fact, testable. (The inability to succinctly test the current GT-IRB implementation has been an ongoing frustration for developers.) The test suite can also serve double-duty as being a source for example usage of various library features. This should not be discounted for a library which we must train new employees on (and existing employees should the structure significantly evolve), not to mention supporting an external user community. Enforcement of this process will also begin to build a development culture where testing the code is as important as writing it.

Engineering-Level Tooling
=========================

This section focuses on infrastructure and automation. While tool changes are not necessarily required to accomplish many of the technological and process goals for GT-IRB, they are universally put fourth here to improve the products from both a technical and a business perspective.

## CMake

As GT-IRB may ultimately be a public-facing library, it will also be an ambassador for the company. It must have clean code, good documentation, and be easily adoptable. To that end, it is strongly suggested that we migrate the GT-IRB build system to CMake from SCons.

There are both technical and cultural reasons to do this. While this represents a big departure from an ingrained part of the code base, it will pay dividends.

The C and C++ communities have already widely adopted CMake as a standard cross- platform build generator. By and large, any active C or C++ developer is very familiar with how to build CMake-based projects. This, alone, reduces training time and bugs in our own code base with new developers.

On GitHub, which represents a strong cross-cut of open source development, the usage of SCons and CMake were compared. By each measure CMake has at least one order of magnitude more community support.

| Contributors | Watchers | Stars | Forks | SCons/CMake LOC In Use | Projects Tagged |      |
|:-------------|:---------|:------|:------|:-----------------------|:----------------|:-----|
| SCons        | 54       | 14    | 82    | 31                     | 801,000         | 18   |
| CMake        | 576      | 128   | 1685  | 867                    | > 10,000,000    | 2188 |

The reasons for this community preference are clear. CMake supports many build tools, is generally faster than SCons, it has a powerful scripting ability, and supports virtually all platforms under development (Linux, Unix, Windows, iOS, Android, Playstation 4, XBox, etc. This native cross-platform capability can help expand our markets in the future without the need for additional custom tooling to duplicate a process we already have in place.)

The features of SCons being used were reviewed during the GT-IRB prototype development. Following this, CMake was utilized to build the prototype GT-IRB project. The migration was straight forward. Advanced features (such as querying local compiler supported features, pointer sizes, etc.) are also easily ported to a CMake build, though this was not done under the prototype project. (Some of these features, such as querying the local pointer size, are natively supported by CMake.)

Finally, SCons itself makes a case for switching to CMake. While also noting some of its own positives, the SCons wiki itself states:

> CMake is a principal competitor to both GNU Autotools and SCons. It is a build system generator, i.e. after running CMake, the user has a native Visual Studio file at his disposal, or a native Makefile, or nmake file, or whatever their preference is. Off-the-shelf build capabilities are comprehensive and proven for large scale software development. The implementation architecture is far more unified than GNU Autotools and it runs much faster. CMake has its own scripting language that runs on all platforms that CMake targets. It is Yet Another Scripting Language, which puts some people off, but it has the advantage of not introducing any additional language dependencies to a project. When compared to SCons, CMake is:
> 
> -   Faster
> -   Requires less code for common tasks
> -   Arguably more stable
> -   Supports outputting to projects like Code::Blocks, Xcode, etc. which SCons does not

A detailed study was done in 2010 by Electric Cloud (a build optimization company) comparing SCons to GMake and sh (). The duration of the build for scons grew significantly as the total size of the source code base grew.

The figure below shows a comparison of build system compilation times vs. the number of files being compild.  SCons is clearly imposing unnecessary build overhead.

<img src="./media/Image_02.png" data-canonical-src="./media/Image_02.png" width="*" height="*" align="middle" style="display:block; margin-left: auto; margin-right: auto;"/>

Even more, after transitioning to CMake, we will also be able to experiment with other build tools beyond _make_. We could easily (and natively) experiment with build time reducing strategies and technologies such as Incredibuild, creating unity builds, or utilizing precompiled headers. These techniques could vastly improve our compile times.

By reducing our compile times, we reduce our cycle times for acceptance/rejection of code under review. Both the build and test times must be reduced significantly to affect this, and CMake can directly impact the former and facilitate the latter.

CMake integrates well with both its own testing library as well as Google Test, it can build product installers, generate documentation, and help enforce physical structure on the code. It is simply a purposefully and well designed tool that is right for the job.

Currently, to compile CodeSurfer and CodeSonar, there are roughly 15,000 GT source code files (`.c` and `.cpp`) that need compiled.  There are an additional 18,000 third-party source code files that also need built.  Consider that header files must also be processed and (ideally) tracked by the build system and the number increases dramatically.  There are a total of almost 75,000 files which must be managed by our build system.

## Clang Format

The code base being leveraged for GT-IRB does not have a consistent format. A consistent format can have a meaningful impact on the success of a project. Not in its details, but in its application. Having a consistently (and programmatically) applied style eliminates bikeshedding and highlights code functionality in code review rather than wasting mental cycles on fixing brace positions, for example.

Integration of Clang Format would provide the code base with a consistent and well supported cross-platform tool (that integrates with Git, Visual Studio, etc.) to automatically format code consistently. That takes issues of code format off the table and allows us to get on with less trivial matters.

### Clang Tidy

Much of the work leading up to the actual re-engineering of the GT-IRB structure has to do with modernizing the code base. The modernization should be done first to ensure that bugs and testing keep pace with the re-engineering work itself. Some of the modernization may be able to be accomplished through the use of clang-tidy. While clang-tidy is a static analysis tool, it is also capable of transforming (fixing) supported checks as well. Wholesale application and acceptance of clang-tidy modifications is not being proposed. However, applying it to focused pieces of GT-IRB may provide cost and time savings while improving code conformance to C++ standards and guidelines.

### Code Sonar XUnit Support

Add the ability for Code Sonar to produce XUnit formatted reports (if it does not already) suitable for integration into at CT environment. This will enable us to run Code Sonar with a COTS DevOps environment (E.g. Atlassian Bamboo, Jenkins, GitLab) and have it report errors just as any test case would. Not only would this be immensely useful internally, commercial consumers would view this as a positive differentiator from other static analysis tools.

### Static Analysis CI/CT Integration

If not already completed, begin integrating Code Sonar testing into the DevOps environment. (Other tools such as CppDepend, CppCheck, Microsoft Visual Studio’s Core Guidelines checkers would be valuable as well.)  Since the GT-IRB will be a publicly available set of code, the goal should be a code base where we can justify anything flagged by static analysis tools (or have sufficiently few errors that we can reasonably create issues to fix them.) Static Analysis should be conducted by individual developers, but having a formal track of its progression (or regression) would help raise and maintain quality standards for the code base.

### Dynamic Analysis CI/CT Integration

Use supporting tools as appropriate ([P.12](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rp-tools)).

- **Integrate automated usage and reporting with Clang Address Sanitizer** - If possible. Clang’s Address Sanitizer cannot work on some types of binaries. Track its output via XUnit (may require a custom tool to convert the output, which we could publish to the community via our GitHub page.)
- **Integrate automated usage and reporting with Clang Memory Sanitizer** - Track its output via XUnit (may require a custom tool to convert the output, which we could publish to the community via our GitHub page.)
- **Integrate automated usage and reporting with Clang Thread Sanitizer** - (CP.9) This will help verify concurrent code is functioning properly. Track its output via XUnit (may require a custom tool to convert the output, which we could publish to the community via our GitHub page.)
