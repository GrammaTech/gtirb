AuxDataContainer
================

An AuxDataContainer object provides functionality for associating
[auxiliary data](../AuxData.md) with elements of the representation.

Guaranteed Properties
---------------------

- An **AuxDataContainer** must have all the guaranteed properties of a
  [Node](Node.md).
  - It is *not* required that **AuxDataContainer** be a subclass of
    **Node**, even if the API language would support this.

- All **AuxDataContainer** objects store a (possibly empty) map of
  strings to [AuxData](../AuxData.md) entries.

- The string->AuxData map must provide the following functionality.
  - Insert or overwrite AuxData given a name, a type hint string, and
    a value of the correct target-language type.
  - Erase an existing AuxData given its name.
  - Retrieve AuxData of a given name (and given target-language type,
    if the target language needs this information).
  - Inspect AuxData of a given name and determine its type hint.

- The AuxData system must have the following properties.
  - All core types in the AuxData system must have a corresponding
    type in the target language.
  - AuxData of all types, including unknown types, must be retrievable
    as a byte array.





API Implementations
-------------------

The guaranteed functionality is provided as follows.

### AuxDataContainer Classes and Node properties

| Language    | AuxDataContainer Class  | guaranteed Node Properties           |
|:------------|:------------------------|:-------------------------------------|
| C++         | gtirb::AuxDataContainer | through inheritance from gtirb::Node |
| Python      | gtirb.AuxDataContainer  | through inheritance from gtirb.Node  |
| Common Lisp | No explicit implementation; relevant classes **ir** and **module** implement required functionality directly. | . |

### string->AuxData map



| Language    | Get AuxData by name  | Insert/Overwrite AuxData  | Erase AuxData   |
|:------------|:---------------------|:--------------------------|:----------------|
| C++         |  gtirb::AuxDataContainer::getAuxData () | gtirb::AuxDataContainer::addAuxData(), gtirb::AuxData::operator=() | gtirb::AuxDataContainer::removeAuxData() |
| Python      | dict lookup in gtirb.AuxDataContainer.auxdata | dict insert/overwrite on gtirb.AuxDataContainer.auxdata | dict pop on gtirb.AuxDataContainer.auxdata |
| Common Lisp | **aux-data** *object* => *result* has **module** and **gtirb** specializations; extract the required named data extraction from the *result* alist |  extract auxiliary data alist with **aux-data** accessor, update alist as needed, then use (setf (**aux-data** *object*) *new-value*) which has **module** and **gtirb** specializations | as for insert/overwrite |


### AuxData Inspection


| Language    | Get AuxData data                         | Get AuxData type hint                    |
|:------------|:-----------------------------------------|:-----------------------------------------|
| C++         | gtirb::AuxData::get()                    | gtirb::AuxData::typeName()               |
| Python      | gtirb.AuxData.data                       | gtirb.AuxData.type_name                  |
| Common Lisp | **aux-data-data** *aux-data* => *result* | **aux-data-type** *aux-data* => *result* |
