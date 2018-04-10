//
// - Class nesting to show parent/child relationships only.
// - Word wrap at column 100.
// - API Functions use action words and positive tests:
//   - get[Foo]
//   - set[Foo] (for single values)
//   - add[Foo] (for containers)
//   - remove[Foo] (for containers)
//   - getIs[Foo]Enabled
//   - apply[Foo]
// - Member variable names should be identical to their set/get functions in the API.
//   - `setFoo(Bar x) { this->foo = x; }`
// - Specialized exception types shall inherit from gtirb::Exception and be suffixed with "Error".
// - API Size functions are called "size".  Do not use "count", "num", etc.
// - When possible, STL-compatability is provided with the same semantics.
//   - push_back, begin, end, size, empty, clear.
// - There shall be one class per header file.
// - The name of the header file and the class it declares shall be identical.
// - Generally, there shall be one way to do something.  
//   - "helper" types of functions shall be avoided inside classes.
//   - "helper" free functions in their own headers is acceptable.
// - There shall be no inlining of functions without measurements proving it is useful.
// - There shall be no unnecessary implementation code in headers.
// - Unit tests shall be written for each class and each function in each class.
// - Doxygen-style documentation should be provided for each class and each public function within the class.
// - #ifdef to conditionally compile code is not desireable.
// - All code should compile on gcc, clang, and visual studio.
// - Implementation shall not rely on user paths, environment variables, or other externals.
// - Reduce (aim to eliminate) use of 3rd party libraries.
//   - Limited use of Boost is expected.
// - Generally aim to conform to the C++ Core Guidelines.
// 

///
/// \namespace gtirb
///
/// The GrammaTech IR for Binaries (GT-IRB)
///
namespace gtirb
{
///
/// Boost Log v2 Logging
///
InitializeLogging();

///
/// \class Exception
///
/// The base class for all custom exception classes.
/// Allows for encoding the location of the exception's origination.
/// Could automatically log the exception on construction.
///
class Exception : public std::exception
{
public:
    Exception(std::string msg, std::string file{""}, int line{0});
    const char* what() override;

private:
    std::string msg;
    std::string file;
    int line{0};
};

///
/// \class LogicError
///
/// Defines a type of object to be thrown as exception. It reports errors that are a consequence of 
/// faulty logic within the program such as violating logical preconditions or class invariants and 
/// may be preventable.
///
class LogicError : public Excption
{
public:
};

///
/// \class RuntimeError
///
/// Defines a type of object to be thrown as exception. It reports errors that are due to events 
/// beyond the scope of the program and can not be easily predicted.
///
class RuntimeError : public Excption
{
public:
};

///
/// \class NodeError
///
/// Indates that a node itself is not properly formed.
///
class NodeError : public Exception
{
public:
    template<typename T>
    void setNodeType()
    {
        this->setNodeType(typeid(T).name());
    }

    void setNodeType(std::string x);
    std::string getNodeType() const;
}


///
/// \class NodeStructureError
///
/// Indicates that the node tree structure is invalid.
///
class NodeStructureError : public NodeError
{
public:
}

///
/// \class TableStore
///
/// The TableStore acts like a look-up table for data for either cross-module or intra-module data.
/// While some data may be very node-specific, the table stores arbitrary data that spans many nodes.
///
template<typename R = std::weak_ptr<gtirb::Node>, typename C = std::string, typename T = std::any> 
class TableStore
{
public:
    void TableStore(std::string x);
    virtual ~TableStore();

    std::string getTableName() const;

    /// Creates the row if it doesn't exist.
    /// Creates the column if it doesn't exist.
    void setTableData(const R& row, const C& column, T data);
    void setTableData(const R& row, const C& column, T&& data);

    T getTableData(const R& row, const C& column) const;
    std::vector<std::pair<C, T>> getTableColumn(const R& row) const;
    std::vector<std::pair<R, T>> getTableRow(const C& column) const;

    std::unordered_map<R, std::unordered_map<C, T>>& getTable();
    const std::unordered_map<R, std::unordered_map<C, T>>& getTable() const;

    ///
    /// Swaps rows and columns and returns a new data structure.
    ///
    std::unordered_map<C, std::unordered_map<R, T>> getRotated();

private:
    static std::set<std::string> uniqueTableNames;
    std::unordered_map<R, std::unordered_map<C, T>> table;
};

///
/// \class Node
///
/// Base class for all IR Data (IR, Modules, Symbols, etc.)
///
/// Allows constructing an STL-like tree of arbitrary types and complexity.
/// 
/// IR(1)
/// |
/// |
/// |
/// Module(N)
/// |
/// |---------------------------------------------------------------------------
/// |          | | |                 | |                 | |                 | |
/// Symbols    | | RegionGlobal      | RegionHeap        | RegionModule      | RegionAbstract      
///            | ProcedureInfo       CFGNode(N)          RegionStack         RegionExtern        
///            Procedure(N)
/// 
class Node
{
public:
    ///
    /// Default constructor.
    /// Add custom validation functions inside the constructor.
    /// Automatically assigns the node a default UUID.
    ///
    Node();
    
    ///
    ///
    ///
    virtual ~Node();

    ///
    /// Add the contents of another node to this node.
    /// Children will share ownship of resources.
    ///
    void operator+=(gtirb::Node& x);

    ///
    /// Support STL algorithms and sorting of nodes.
    ///
    virtual bool operator<(gtirb::Node& x);

    ///
    /// Support string conversion.
    ///
    virtual operator std::string() const;

    ///
    /// Universally Unique ID (UUID)
    /// Though automatically assigned on construction, it can be manually set.
    ///
    void setUUID(boost::uuids::uuid x);

    ///
    /// Universally Unique ID (UUID)
    ///
    boost::uuids::uuid getUUID() const;

    ///
    /// \return     Null if it is a root node.
    ///
    gtirb::Node* getOwner() const;

    ///
    /// Be able to limit who a node can be a child of.
    /// 
    template<typename T> 
    void addValidParentType()
    {}

    template<typename T>
    bool getIsValidParentType() const
    {}

    ///
    /// Runs user custom validation functions.  (Recursive)
    ///
    /// Allow the user to implement a validation function for the Node.
    /// This should validate the tree structure as well as the node's construction.
    /// Throws a "NodeError" exepction and a "NodeStructureError" exception.
    ///
    bool getIsValid() const;

    ///
    /// Adds a child node.
    ///
    /// API is modeled after the STL.
    /// Unline the STL, this returns true on success.
    /// Executes functions added via Node::addPushBackValidator.  Will not add the node
    /// if the validator returns false.
    ///
    /// Will return false if "x" cannot be a child of this Node type (Using RTTI).
    ///
    ///
    bool push_back(std::unique_ptr<gtirb::Node>&& x);
    
    ///
    /// Determines if there are any child nodes.
    ///
    /// API is modeled after the STL.
    ///
    /// \return     True of there are not any child nodes.
    ///
    bool empty() const;

    ///
    /// Returns the number of elements in the container. 
    /// The number of child nodes.
    /// API is modeled after the STL.
    /// Constant complexity.
    ///
    /// \return     Zero for an empty structure, or the number of child nodes.
    ///
    size_t size() const;

    ///
    /// Allows for indexing child nodes.
    ///   
    Node* operator[](size_t x);

    ///
    /// Clears all children.
    ///
    /// API is modeled after the STL.
    ///
    void clear();

    ///
    /// Returns a custom iterator to the first child node.
    /// API is modeled after the STL.
    /// This allows for the use of 'std::begin' and 'std::end' and other STL algorithms.
    ///
    /// \return     Returns an iterator to the beginning of the container.
    ///             1)  Returns exactly c.begin(), which is an iterator to the beginning of the 
    ///                 sequence represented by c. This returns C::iterator when c is not 
    ///                 const-qualified, and C::const_iterator otherwise.
    ///             2)  Returns exactly std::begin(c), with c always treated as const-qualified. If 
    ///                 C is a standard Container, this always returns C::const_iterator.
    ///
    NodeIterator begin();
    ConstNodeIterator begin() const;

    template<typename T> TypedNodeIterator<T> begin();
    template<typename T> ConstTypedNodeIterator<T> begin() const;

    ///
    /// Returns a custom iterator to the end of the child nodes.
    /// API is modeled after the STL.
    ///
    /// \return     Returns an iterator to the end (i.e. the element after the last element) of the 
    ///             container c or array array. 
    ///             1)  Returns exactly c.end(), which is an iterator one past the end of the 
    ///                 sequence represented by c. This returns a C::iterator when c is not 
    ///                 const-qualified, and a C::const_iterator otherwise.
    ///             3)  Returns exactly std::end(c), with c always treated as const-qualified. 
    ///                 This always returns a C::const_iterator.
    ///
    NodeIterator end();
    ConstNodeIterator end() const;

    template<typename T> TypedNodeIterator<T> end();
    template<typename T> ConstTypedNodeIterator<T> end() const;

    ///
    /// Automatically constructs a vector of pointers to the children of the given type.
    ///
    /// Equivelant to 'auto vec = std::vector<T>(node->begin<T>(), node->end<T>());'
    /// Given the templated iterators, this violates the principle of "minimally complete" API 
    /// design.
    ///
    /// Similar to the STL "::data()" member function.
    ///
    template<typename T>
    std::vector<T*> getChildren()
    {
        return std::vector<T*>(this->begin<T>(), this->end<T>())
    }

    template<typename T>
    const std::vector<T*> getChildren() const
    {
        return std::vector<T*>(this->begin<T>(), this->end<T>())
    }

    ///
    /// Adds a callback which will be executed on every call to Node::push_back()
    ///
    /// \param      f   A std::function object which takes a non-null pointer to a Node.
    ///
    void addPushBackCallback(std::function<void(gsl::not_null<gtirb::Node*>)> f);

    ///
    /// Sets an arbitrary property on the node.
    ///
    void setLocalProperty(std::string name, std::any value);
    std::any getLocalProperty(const std::string& name);
    bool removeLocalProperty(const std::string& name);

    ///
    /// This is not ideal as it exposes internal implementation.
    ///
    const std::map<std::string, std::any>& getLocalProperties() const;

    ///
    /// Create a table store owned by this node.
    ///
    void addTableStore(std::unique_ptr<gtirb::TableStore*>&& x);

    ///
    /// Remove a table store owned by this node.
    ///
    bool removeTableStore(gtirb::TableStore* x);

    ///
    /// Get a table store by name.
    /// Starting at this node, will search "up" the tree until the given table is found or return null.
    ///
    gtirb::TableStore* getTableStore(const std::string& x);

    void traverseDepthFirst(std::function<void(gsl::not_null<gtirb::Node*>)> x);
    void traverseBreadthFirst(std::function<void(gsl::not_null<gtirb::Node*>)> x);

protected:
    ///
    /// Adds a custom validation function.
    ///
    /// Allow the user to implement a validation function for the Node.
    /// This should validate the tree structure as well as the node's construction.
    /// Throws a "NodeError" exepction and a "NodeStructureError" exception.
    /// These functions should be added in the object constructor.
    /// These are stored on the node themselves.
    ///
    void addCustomValidator(std::function<bool(gtirb::Node const * const self)> f) const;

    ///
    /// Adds a custom validation function.
    /// These are stored on the node themselves.
    ///
    void addPushBackValidator(std::function<bool(gtirb::Node const * const parent)> f) const;

private:
    boost::uuids::uuid uuid;
};

///
/// \class EA
///
/// A special class to store an Effective Address.
/// This is initialized to "EA::BadAddress".
/// It is compatible with a uint64_t for 64-bit address storage.
///
class EA
{
public:
    /// The initial value for an EA.  
    constexpr BadAddress = std::limits<uint64_t>::max();

    /// Used by Microsoft's debug HeapAlloc() to mark uninitialized allocated heap memory
    constexpr BadFood = 0xbaadf00d;

    /// Used by Mach-O to identify flat (single architecture) object files.
    constexpr FaceFeed = 0xcefaedfe

    /// Dead beef, it's a hex code of 4 bytes, typically used as an example IP address. 0xDEADBEEF 
    /// ("dead beef") is used by IBM RS/6000 systems, Mac OS on 32-bit PowerPC processors and the 
    /// Commodore Amiga as a magic debug value. On Sun Microsystems' Solaris, it marks freed kernel 
    /// memory. 
    constexpr DeadBeef = 0xdeadbeef;

    ///
    constexpr DeadDead = 0xdeaddead;

    EA(uint64_t x) explicit;

    void set(uint64_t x);
    uint64_t get() const;

    operator std::string() const;

private:
    uint64_t effectiveAddress{gtirb::EA::BadAddress};
};

///
/// \class IR
///
/// This is the top-level node for a binary.
/// It can contain multiple Modules.
///
class IR final : public Node
{
public:
    ///
    /// \class Module
    ///
    /// A module represents the binary for a single "library" file within the IR.
    ///
    class Module final : public Node
    {
    public:
        ///
        /// \class ModuleData
        ///
        /// This class exists to create the type.  The type can then be controlled as only being 
        /// able to be a child of a module, thus controlling the structure.
        ///
        class ModuleData : public Node
        {
            // No special functions.
        };

        ///
        /// \class ModuleBase
        ///
        /// A base class for Module sections (Summary, Core, Aux).
        ///
        class ModuleBase : public ModuleData
        {
        public:
            virtual ~ModuleBase();

            bool getIsSetupComplete() const;
            bool getIsReadOnly() const;

        protected:
            void setIsSetupComplete(bool x);
            void setIsReadOnly(bool x);
        }

        ///
        /// \class Functions
        ///
        class Functions final : public ModuleBase
        {
        public:
            // No special functions.
            // What goes here?
        };

        ///
        /// \class Globals
        ///
        class Globals final : public ModuleBase
        {
        public:
            // No special functions.
            // What goes here?
        };

        ///
        /// \class Symbols
        ///
        /// Owns all symbols.
        ///
        class Symbols : public ModuleBase
        {
        public:
            ///
            /// \class Symbol
            ///
            /// Users can inherit from this and build their own custom symbol types, if required.
            /// Classes such as XType could be added by the user via properties or children.
            ///
            class Symbol final : public ModuleData
            {
            public:
                enum class Type
                {
                    Undefined,
                    Normal,
                    DualCode,
                    NoEA
                };

                enum class DeclarationKind
                {
                    Undefined,
                    Var,
                    Func,
                    Type,
                    Param,
                    MallocSite,
                    Result,
                    Return,
                    ParamTemp,
                    Enumerator,
                    StringLiteral,
                    Temporary,
                    Uninit,
                    Label,
                    IndirectFunc,
                    UnknownSym
                };

                enum class LinkType
                {
                    Undefined,  /// undefined
                    D, /// initialized data
                    B, /// BSS)
                    C  /// common
                    V  /// weak object
                    R  /// read-only
                };

                enum class StorageKind
                {
                    Undefined,
                    Normal,
                    Static,
                    Extern
                };

                void setName(std::string x);
                std::string getName() const;

                void setEA(gtirb::EA x);
                gtirb::EA getEA() const;

                void setType(Symbol::SymbolType x);
                Symbol::SymbolType getType() const;

                void setOffset(int64_t x);
                int64_t getOffset();

                void setElementSize(int64_t x);
                int64_t getElementSize() const;

                /// The total number of bits for this symbol.
                void setBitSize(int64_t);
                int64_t getBitSize() const;

                void setIsFormal(bool x);
                bool getIsFormal() const;

                void setEnableForceName(bool x);
                bool getEnableForceName() const;

                void setEnableGapSize(bool x);
                bool getEnableGapSize() const;

                void setIsNameOnly(bool x);
                bool getIsNameOnly() const;

                void setRegion(std::weak_ptr<Symbol::Region> x);
                Symbol::Region* getRegion() const;

                void setDeclarationKind(Symbol::DeclarationKind x);
                Symbol::DeclarationKind getDeclarationKind() const;

                void setLinkType(Symbol::LinkType x);
                Symbol::LinkType getLinkType() const;

                void setStorageKind(Symbol::StorageKind x);
                Symbol::StorageKind getStorageKind() const;

                void setIsGlobal(bool x);
                bool getIsGlobal() const;

                // Should we make this our parent in the tree or forward to it?
                void setParentSymbol(std::weak_ptr<gtirb::Symbol> x);
                gtirb::Symbol* getOwnerSymbol() const;

                void setForwardSymbol(std::weak_ptr<gtirb::Symbol> x);
                gtirb::Symbol* getForwardSymbol() const;

                void setReplacementSymbol(std::weak_ptr<gtirb::Symbol> x);
                gtirb::Symbol* getReplacementSymbol() const;

            };
        };

        ///
        /// \class AddrRanges
        ///
        class AddrRanges : public ModuleData
        {
        public:
            virtual operator std::string() const;

            ///
            /// Get all of the ranges that have been added.
            ///
            std::vector<std::pair<gtirb::EA, gtirb::EA>>& getRangeVector();
            
            ///
            /// Get all of the ranges that have been added.
            ///
            const std::vector<std::pair<gtirb::EA, gtirb::EA>>& getRangeVector() const;

            ///
            /// Given all of the ranges, how many bytes exist between all pairs?
            ///
            /// \return         The number of bytes in all of the ranges in the
            ///                 vector.
            ///
            size_t getBytesCoveredByRanges() const;
        };

        ///
        /// \class ProcedureInfo
        ///
        class ProcedureInfo final : public ModuleData
        {
        public:
            bool operator<(gtirb::ModuleData& x);
            virtual operator std::string() const override;

            /// Symbols are owned by the IR->Module->Symbols
            void setProcedureNameSymbol(std::weak_ptr<gtirb::Symbol> x)
            gtirb::Symbol* getProcedureNameSymbol() const;

            // Can there be more than one per Module?  That is, do we need to set this or can we 
            // just walk up the tree and get it?
            void setModuleSummary(std::weak_ptr<gtirb::ModuleSummary> x);
            gtirb::ModuleSummary* getModuleSummary();

            /// Symbols are owned by the IR->Module->Symbols
            std::map<std::weak_ptr<gtirb::Symbol>, std::weak_ptr<gtirb::Symbol>>& getSymbolToSymbolMap();
            const std::map<std::weak_ptr<gtirb::Symbol>, std::weak_ptr<gtirb::Symbol>>& getSymbolToSymbolMap() const;

            /// Symbols are owned by the IR->Module->Symbols
            std::map<std::weak_ptr<gtirb::Symbol>, std::weak_ptr<gtirb::Symbol>>& getSaveToRestoreSymbolMap();
            const std::map<std::weak_ptr<gtirb::Symbol>, std::weak_ptr<gtirb::Symbol>>& getSaveToRestoreSymbolMap() const;

            /// The RegionHeap is owned by the IR->Module
            std::map<gtirb::EA, std::weak_ptr<gtirb::RegionHeap>>& getEAToHeapRegionMap();
            const std::map<gtirb::EA, std::weak_ptr<gtirb::RegionHeap>>& getEAToHeapRegionMap() const;

            /// Symbols are owned by the IR->Module->Symbols
            void addSummarizedRegisterConditionalKills(std::weak_ptr<gtirb::Symbol> x);
            std::weak_ptr<gtirb::Symbol> getSummarizedRegisterConditionalKills() const;

            /// Address relative to?
            void setFrameBase(int64_t);
            int64_t getFrameBase() const;

            /// Address relative to?
            void setReturnAddress(int64_t);
            int64_t getReturnAddress() const;

            /// Address relative to?
            void setSizeOfSaveRegisterSlot(int64_t);
            int64_t getSizeOfSaveRegisterSlot() const;

            /// Address relative to?
            void setOffsetOfSaveRegisterSlot(int64_t);
            int64_t getOffsetOfSaveRegisterSlot() const;
        };

        class Region : public ModuleData
        {
        public:
            // Convienence function.
            std::vector<std::weak_ptr<Symbol>> getSymbols();

            void operator+=(Region& x);
            bool operator<(Region& x);
        };

        class RegionGlobal final : public Region
        {
        public:
            // What goes here?
        };

        class RegionModule final : public RegionGlobal
        {
        public:
            // What goes here?
        };

        class RegionAbstract final : public Region
        {
        public:
            class Scope : public ModuleData
            {
            public:
                ///
                /// Should these be EAs instead of int64_t?
                ///
                /// \return     False if the lower bound is greater than the upper bound.
                ///
                bool setBounds(std::pair<int64_t, int64_t> lowerUpper);
                std::pair<int64_t, int64_t> getBounds() const;

                // What is this the count of?
                void setCount(int64_t x);
                int64_t getCount() const;
            };

            /// Symbols are owned by the IR->Module->Symbols
            gsl::not_null<Symbol*> getReturnSymbol() const;
            gsl::not_null<Symbol*> getResultSymbol() const;

            /// Symbols are owned by the IR->Module
            gsl::not_null<ProcedureInfo*> getProcedureInfo() const;

            void setProcID(int64_t x);
            int64_t getProcID() const;

            /// Symbols are owned by the IR->Module->Symbols
            std::vector<gsl::not_null<Symbol*>>& getLocals();
            const std::vector<gsl::not_null<Symbol*>>& getLocals() const;
        };

        class RegionHeap final : public Region
        {
        public:
            enum class HeapSubtype 
            {
                MRAB,
                NMRAB,
                Distinct,
            };

            gsl::not_null<RegionHeap*> getCompanionMRAB() const;
            gsl::not_null<RegionHeap*> getCompanionNMRAB() const;

            gsl::not_null<RegionHeap*> getCompanionDistinct(size_t x) const;
            size_t getCompanionDistinctSize() const;

            gsl::not_null<Symbol*> getCountSymbol();
            gsl::not_null<Symbol*> getSizeSymbol();

            void setHeapSubtype(RegionHeap::HeapSubtype x);
            RegionHeap::HeapSubtype getHeapSubtype() const;
        };

        class RegionStack final : public Region
        {
        public:
            // What goes here?
        };

        class RegionExtern final : public Region
        {
        public:
            // What goes here?
        };

        class CFGNode final : public ModuleData
        {
        public:
            enum Kind {
                Unknown, /// Default
                ActualIn,
                ActualOut,
                Call,
                ControlPoint,
                ControlTarget,
                Goto,
                Break,
                Label,
                VarDecl,
                EndCall,
                Enter,
                Exit,
                Exp,
                FormalIn,
                FormalOut,
                Return,
                Switch,
                EndReturn,
                Indirect,
                UserDefined /// The last slot in the enumeration so users can add their own types 
                            /// using this as an offset.
            };

            class CFGNodeAttribute : public ModuleData
            {
            public:
                // What goes here?
            };

            class RegisterType : public CFGNodeAttribute
            {
            public:
                /// Symbols are owned by the IR->Module->Symbols.
                void set(std::weak_ptr<gtirb::Symbol> x);
                gtirb::Symbol* get() const;
            }

            constexpr DecodeModeDefault{0};
            constexpr DecodeModeUnknown{std::limits<uint64_t>::max()};

            void setVertexID(int64_t);
            int64_t getVertexID() const;

            ///
            /// Based on CFGNode::Kind enumeration.
            ///
            void setKind(int x);
            int getKind() const;

            void setEA(gtirb::EA x);
            gtirb::EA getEA() const;

            void setAffiliatedEA(gtirb::EA x);
            gtirb::EA getAffiliatedEA() const;

            void setInstructionStackPointerDelta(int64_t x);
            int64_t getInstructionStackPointerDelta() const;

            void setRelativeStackPointer(int64_t x);
            int64_t getRelativeStackPointer() const;

            void setRelativeBasePointer(int64_t x);
            int64_t getRelativeBasePointer() const;

            void setFrameDelta(int64_t x);
            int64_t getFrameDelta() const;

            void setReturnSpAdjust(int64_t x);
            int64_t getReturnSpAdjust() const;

            void setSpAdjust(int64_t x);
            int64_t getSpAdjust() const;

            void setIsWideningPoint(bool x);
            bool getIsWideningPoint() const;

            void setDecodeMode(uint64_t x);
            uint64_t getDecodeMode();
        };

        class Procedure : public ModuleData
        {
        public:
            class Instruction : public ModuleData
            {
            public:
            };

            /// Procedure Linkage Table.
            /// These entries are basically the "thunks" for calls to things in shared libraries.
            std::set<EA>& getPLTEntries();
            const std::set<EA>& getPLTEntries() const;
        };

        virtual operator std::string() const;

        void setBinaryPath(std::filesystem::path x);
        std::filesystem::path getBinaryPath() const;

        void setFileFormat(CFGNode::FileFormat x);
        CFGNode::FileFormat getFileFormat() const;

        void setRebaseDelta(int64_t x);
        int64_t getRebaseDelta() const;

        gsl::not_null<ModuleSummary*> getModuleSummary();
        gsl::not_null<ModuleSummary const * const> getModuleSummary() const;
        
        gsl::not_null<ModuleCore*> getModuleCore();
        gsl::not_null<ModuleCore const * const> getModuleCore() const;

        gsl::not_null<ModuleAux*> getModuleAux();
        gsl::not_null<ModuleAux const * const> getModuleAux() const;

        void setCacheModuleName(std::string x);
        std::string getCacheModuleName() const;

        ///
        /// \return     False if the pair's first is > the pair's second.
        ///
        bool setEAMinMax(std::pair<gtirb::EA, gtirb::EA> x);
        std::pair<gtirb::EA, gtirb::EA> getEAMinMax() const;

        void setPreferredEA(gtirb::EA x);
        gtirb::EA getPreferredEA() const;

        ///
        /// Extract an eight bit vector of data starting at a given EA for a given number of bytes.
        ///
        std::vector<uint8_t> getChildrenBytes8(gtirb::EA x, size_t nbytes) const;
        std::vector<uint8_t> getChildrenBytesUntil(gtirb::EA x, size_t nbytes, std::function<bool(uint8_t)> passFunction) const;
    };

    bool operator<(IR& x);
    operator std::string() const override;

    ///
    /// Run a vector of validation functions on the entire IR.
    /// Each validation function gets a pointer to its node and a stream to output error messages on.
    ///
    /// \return true if all validation functions returned true.
    ///
    bool applyValidation(std::vector<std::function<bool(const Node* const, ostream& os = std::cerr)>> validators);
};

///
/// \class AST
///
/// The base for all AST nodes.
/// Compatible with std::hash<>
///
class AST : public Node
{
public:
    class ASTAbstract : public AST
    {
    public:
    };

    class ASTConcrete : public AST
    {
    public:
    };

    virtual bool operator==(const AST& x);
    virtual bool operator!=(const AST& x);
    virtual bool operator>(const AST& x);
    virtual bool operator<(const AST& x);

    void setClassName(std::string x);
    std::string getClassName() const;

    void setClassID(int64_t x);
    int64_t getClassID()const;

    virtual std::unique_ptr<AST> join(gsl::not_null<AST const * const> x) const;
    virtual std::unique_ptr<AST> widen(gsl::not_null<AST const * const> x) const;
    virtual std::unique_ptr<AST> meet(gsl::not_null<AST const * const> x) const;
};

/// Custom hash can be a standalone function object:
/// http://en.cppreference.com/w/cpp/utility/hash
struct ASTHash
{
    std::size_t operator()(AST const& x) const noexcept
    {
        const auto h1 = std::hash<std::string>{}(x.getClassName());
        const auto h2 = std::hash<int64_t>{}(x.getClassID());

        // or use boost::hash_combine (see Discussion)
        return h1 ^ (h2 << 1); 
    }
};
}

// -------------------------------------------------------------------------------------------------
// EXAMPLE UTILITIES
// -------------------------------------------------------------------------------------------------

namespace gtirb
{
std::vector<uint16_t> ByteArray8To16(const std::vector<uint8_t>& x);
std::vector<uint32_t> ByteArray8To32(const std::vector<uint8_t>& x);
std::vector<uint64_t> ByteArray8To64(const std::vector<uint8_t>& x);

void PrettyPrint(gsl::not_null<gtirb::IR*>, std::filesystem::path outFile);
Value* GTIRB2LLVM(gsl::not_null<gtirb::IR*>);
std::unique_ptr<gtirb::Module> LoadModule(std::filesystem::path& x);
}

// Conversion with legacy IR types.
AnalysisEnvironment* GTIRB2AnalysisEnvironment(gsl::not_null<gtirb::IR*>, FATOBJ_STORE_ID store_id);
ModuleIR* GTIRB2ModuleIR(gsl::not_null<gtirb::Module*>, FATOBJ_STORE_ID store_id);

std::unique_ptr<gtirb::IR> ModuleIR2GTIRB(ModuleIR* x);
std::unique_ptr<gtirb::IR> AnalysisEnvironment2GTIRB(AnalysisEnvironment* x);

// -------------------------------------------------------------------------------------------------
// EXAMPLE USEAGE
// -------------------------------------------------------------------------------------------------

class Foo : public ModuleData
{
public:
    void setMyData(int64_t x);
    int64_t getMyData() const;
};

// -------------------------------------------------------------------------------------------------
// Example

void DecorateAllSymbols()
{
    auto ir = std::make_shared<gtirb::IR>();
    ir.push_back(gtirb::LoadModule("/foo/bar"));
    
    auto module = ir[0];
    auto symbols = module->getChildren<Symbols>();

    for(auto symbol : symbols)
    {
        auto foo = std::make_unique<Foo>();
        foo->setMyData(rand());

        symbol->push_back(foo);

        std::cout << static_cast<std::string>(*symbol) << ", ";
        std::cout << symbol->getChildren<Foo>()[0]->getMyData() << std::endl;
    }
}

// -------------------------------------------------------------------------------------------------
// Example

void DecorateAllSymbols2()
{
    auto ir = std::make_shared<gtirb::IR>();
    ir.push_back(gtirb::LoadModule("/foo/bar"));
    
    auto module = ir[0];
    auto symbols = std::vector<Symbols*>(module->begin<Symbols>(), module->end<Symbols>());

    for(auto& symbolsNode : symbols)
    {
        std::for_each(symbolsNode->begin<Symbol>(), symbolsNode->end<Symbols>() 
            [](Symbol* s)
            {
                auto foo = std::make_unique<Foo>();
                foo->setMyData(rand());
                s->push_back(foo);
            });
    }
}

// -------------------------------------------------------------------------------------------------
// Example

void AddAndGetCustomProperty()
{
    auto ir = std::make_shared<gtirb::IR>();
    ir.push_back(gtirb::LoadModule("/foo/bar"));
    
    auto module = ir[0];
    auto globalRegion = module->getChildren<gtirb::RegionGlobal>();

    if(globalRegion != nullptr)
    {
        globalRegion->setLocalProperty("specialInteger", int(21));
        globalRegion->setLocalProperty("specialVector", std::vector<int>{2, 1, 1, 2});
    }

    // Do some other work...

    if(globalRegion != nullptr)
    {
        const auto siProperty = globalRegion->getLocalProperty("specialInteger");
        const auto svProperty = globalRegion->getLocalProperty("specialVector");

        if(siProperty.has_value() == true)
        {
            const auto specialInteger = std::any_cast<int>(siProperty);
            std::cout << "Special Integer: " << specialInteger << std::endl;
        }

        if(svProperty.has_value() == true)
        {
            const auto specialVector = std::any_cast<std::vector<int>>(svProperty);

            for(const auto i : specialVector)
            {
                std::cout << "Special Vector: " << i << std::endl;
            }
        }
    }
}

// -------------------------------------------------------------------------------------------------
// Example

void IgnoreUserTypes()
{
    auto ir = std::make_shared<gtirb::IR>();
    ir.push_back(gtirb::LoadModule("/foo/bar"));
    
    auto module = ir[0];

    // Add a bunch of user types
    module->push_back(std::make_unique<Foo>());
    // ...
    module->push_back(std::make_unique<Bar>());
    // ...

    // Supports non-member begin and end.
    auto allChildren = std::vector<gtirb::Node*>(std::begin(*module), std::end(*module));

    // Supports regular member begin and end.
    auto allChildrenAgain = std::vector<gtirb::Node*>(module->begin(), module->end());

    // Has a special tempalted member begin and end to filter out object types.
    auto justSymbols = std::vector<gtirb::Symbol*>(module->begin<Symbol>(), module->end<Symbol>());
    auto justFoos = std::vector<Foo*>(module->begin<Foo>(), module->end<Foo>());
    auto justBars = std::vector<Bar*>(module->begin<Bar>(), module->end<Bar>());
}

// -------------------------------------------------------------------------------------------------
// Example

void UseFATTypes(ModuleIR* mir);
{
    auto ir = std::make_shared<gtirb::IR>();
    ir.push_back(std::make_unique<gtirb::Module>());
    auto gtirbModule = ir[0];

    // Allow GTIRB to hold pointers into the existing data structures.
    gtirbModule->setLocalProperty("ModuleIR", mir);
    gtirbModule->setLocalProperty("ASTDataManager", mir->get_ast_manager());

    //...

    auto symbolContainer = gtirbModule->getChildren<Symbols>();
    if(symbolContainer.size() == 1)
    {
        auto allSymbols = symbolContainer->getChildren<Symbol>();
        for(auto s : allSymbols)
        {
            // Allow GTIRB to hold FAT Types.
            auto globalRegion = mir->get_global_region();
            fSymbol_LGrip fsym_lgrip = globalRegion->lookup_symbol(s->getEA());
            s->setLocalProperty("fSymbol", fsym_lgrip);
        }
    }
}

// -------------------------------------------------------------------------------------------------
// Example

void ConvertToFat(gsl::not_null<Node*> x, FATOBJ_STORE_ID store_id)
{
    auto ir = dynamic_cast<gtirb::IR*>(x);
    auto symbol = dynamic_cast<gtirb::Symbol*>(x);
    auto module = dynamic_cast<gtirb::Module*>(x);
    // ...

    if(ir != nullptr)
    {
        AnalysisEnvironment_LGrip f = new AnalysisEnvironment_LGrip(store_id)

        // Wire things up as necessary. 
        // f->(...)

        x->setLocalProperty("AnalysisEnvironment", f);
    }
    else if(symbol != nullptr)
    {
        fSymbol_LGrip f(store_id);
 
        // Wire things up as necessary. 
        // f->(...)

        symbol->setLocalProperty("fSymbol", f);
    }
    else if(module != nullptr)
    {
        ModuleIR_LGrip f(store_id);
        
        // Wire things up as necessary. 
        // f->(...)

        symbol->setLocalProperty("ModuleIR", f);
    }
}

AnalysisEnvironment_LGrip GTIRB2AnalysisEnvironment(gtirb::IR* ir, FATOBJ_STORE_ID store_id);
{
    ir->traverseDepthFirst([store_id](gsl::not_null<Node*> x){ ConvertToFat(x, store_id);});
    return std::any_cast<AnalysisEnvironment_LGrip>(f->getLocalProperty("AnalysisEnvironment"));
}

// -------------------------------------------------------------------------------------------------
// Example

void BuildAndUseTable()
{
    auto ir = std::make_shared<gtirb::IR>();
    ir.push_back(gtirb::LoadModule("/foo/bar"));
    
    auto module = ir[0];

    using SymbolTable = gtirb::TableStore<std::weak_ptr<Symbol>, std::string, std::any>>;
    auto symbolTable = std::make_shared<SymbolTable>("Symbol Table");
    module->addTable(std::move(symbolTable));

    // Build out a table.

    const auto symbolContainer = module->getChildren<Symbols>();
    if(symbolContainer.size() == 1)
    {
        const auto allSymbols = symbolContainer->getChildren<Symbol>();
        for(auto s : allSymbols)
        {
            auto globalRegion = mir->get_global_region();
            fSymbol_LGrip fsym_lgrip = globalRegion->lookup_symbol(s->getEA());

            // The table is stored on the module, so "getTableStore" will walk up until it finds it.
            auto symbolTable = dynamic_cast<SymbolTable*>(s->getTableStore("Symbol Table"));
            symbolTable->setTableData(s->shared_from_this(), "fSymbol", fsym_lgrip);
            symbolTable->setTableData(s->shared_from_this(), "globalRegion", globalRegion);
        }
    }

    // Work with the table.
    // Note the IR itself is not being used.

    auto allFsymbols = symbolTable->getTableColumn("fSymbol");
    for(auto& nodeSymbolPair : allFsymbols)
    {
        auto gtirSymbol = nodeSymbolPair.first.lock();
        auto fSymbol = std::any_cast<fSymbol_LGrip>(nodeSymbolPair.second);

        // ...
    }
}
