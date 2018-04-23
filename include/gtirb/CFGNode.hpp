#pragma once

#include <cstdint>
#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>

namespace gtirb
{
    class CFGNodeInfo;

    ///
    /// \class CFGNode
    /// \author John E. Farrier
    ///
    /// A control flow graph node.
    ///
    /// This can live under either a CFG directly or under another CFGNode.
    ///
    class GTIRB_GTIRB_EXPORT_API CFGNode final : public Node
    {
    public:
        ///
        /// \enum gtirb::CFGNode::Kind
        ///
        enum class Kind
        {
            Unknown,
            ActualIn,
            ActualOut,
            Call,
            ControlPoint,
            ControlTarget, /// not used in x86
            Goto,
            Break, /// not used in x86
            Label, /// not used in x86
            Decl,  /// not used in x86
            VarDecl,
            Empty, /// not used in x86
            EndCall,
            Enter,
            Exit,
            Exp,
            FormalIn,
            FormalOut,
            Return,
            Switch,
            Preend,
            EndReturn, /// not used in x86
            Body,
            Indirect,
            UserDefined /// The last slot in the enumeration so users can add their own types
                        /// using this as an offset.
        };

        ///
        /// Default Constructor.
        ///
        CFGNode();

        ///
        /// Defaulted trivial destructor.
        ///
        virtual ~CFGNode() = default;

        void setEA(EA x);
        EA getEA() const;

        void setKind(CFGNode::Kind x);
        CFGNode::Kind getKind() const;

        ///
        /// Add an existing CFGNode as a new successor.
        ///
        /// \param  x   A non-owned pointer to a CFGNode to add as a successor.  If it has already
        /// been added, it will be added again.
        /// \param  isExecutable    True if the edge to the successor is executable.
        ///
        /// \throws std::bad_weak_ptr if 'x' is not owned by the GTIR (a shared_ptr).
        ///
        void addSuccessor(CFGNode* x, bool isExecutable = false);

        ///
        /// Add a new CFGNode as a new successor.
        ///
        /// \param  x   An owned pointer to a CFGNode to add as a successor.
        /// \param  isExecutable    True if the edge to the successor is executable.
        ///
        void addSuccessor(std::unique_ptr<CFGNode>&& x, bool isExecutable = false);

        ///
        /// Sets the successor at the given index to a new pointer.
        ///
        /// \param  index   The index of the existing successor to replace.  Special case: If the
        /// index is one past the end of the list of successors, it will be appended to the end.
        /// \param  x   A non-owned pointer to a CFGNode to add as a successor.  If it has already
        /// been added, it will be added again.
        /// \param  isExecutable    True if the edge to the successor is executable.
        ///
        /// \throws std::out_of_range if the index is out of range.
        /// \throws std::bad_weak_ptr if 'x' is not owned by the GTIR (a shared_ptr).
        ///
        void setSuccessor(size_t index, CFGNode* x, bool isExecutable = false);

        ///
        /// Sets the successor at the given index to a new pointer.
        ///
        /// \param  index   The index of the existing successor to replace. Special case: If the
        /// index is one past the end of the list of successors, it will be appended to the end.
        /// \param  x   An owned pointer to a CFGNode to add as a successor.
        /// \param  isExecutable    True if the edge to the successor is executable.
        ///
        /// \throws std::out_of_range
        ///
        void setSuccessor(size_t index, std::unique_ptr<CFGNode>&& x, bool isExecutable = false);

        ///
        /// Get the successor at the given index.
        ///
        /// \param x    The index of the successor to get.
        ///
        /// \return     The pointer to the successor at the given index, or nullptr.  The boolean
        /// flag indicates if the forward edge is executable.
        ///
        std::pair<CFGNode*, bool> getSuccessor(size_t x) const;

        ///
        /// Gets if there are any successors or if the list is empty.
        ///
        bool getSuccessorsEmpty() const;

        ///
        /// Gets the total number of successors.
        ///
        size_t getSuccessorSize() const;

        ///
        /// Removes the successor at the given index.
        ///
        /// The CFGNode will be removed from the list of successors, but not deallocated.
        ///
        /// \param  x   The index of the successor to remove.
        ///
        /// \return true on success.
        ///
        void removeSuccessor(size_t x);

        ///
        /// Removes the successors with the given address.
        ///
        /// The CFGNode will be removed from the list of successors, but not deallocated.
        ///
        /// \param  x   The address of the successor to remove.
        ///
        /// \return true on success.
        ///
        /// \throws std::bad_weak_ptr if 'x' is not owned by the GTIR (a shared_ptr).
        ///
        void removeSuccessor(const CFGNode* const x);

        ///
        /// Removes the successors with the given address.
        ///
        /// The CFGNode will be removed from the list of successors, but not deallocated.
        ///
        /// \param  x   The address of the successor to remove.
        /// \param  isExecutable   The executable edge test to apply when deciding if to remove a
        /// CFGNode.
        ///
        /// \return true on success.
        ///
        /// \throws std::bad_weak_ptr if 'x' is not owned by the GTIR (a shared_ptr).
        ///
        void removeSuccessor(const CFGNode* const x, bool isExecutable);

        ///
        /// Add an existing CFGNode as a new predecessor.
        ///
        /// \param  x   A non-owned pointer to a CFGNode to add as a predecessor.  If it has already
        /// been added, it will be added again.
        /// \param  isExecutable    True if the edge to the predecessor is executable.
        ///
        /// \throws std::bad_weak_ptr if 'x' is not owned by the GTIR (a shared_ptr).
        ///
        void addPredecessor(CFGNode* x, bool isExecutable = false);

        ///
        /// Add a new CFGNode as a new predecessor.
        ///
        /// \param  x   An owned pointer to a CFGNode to add as a predecessor.
        /// \param  isExecutable    True if the edge to the predecessor is executable.
        ///
        void addPredecessor(std::unique_ptr<CFGNode>&& x, bool isExecutable = false);

        ///
        /// Sets the predecessor at the given index to a new pointer.
        ///
        /// \param  index   The index of the existing predecessor to replace. Special case: If the
        /// index is one past the end of the list of successors, it will be appended to the end.
        /// \param  x   A non-owned pointer to a CFGNode to add as a predecessor.  If it has already
        /// been added, it will be added again.
        /// \param  isExecutable    True if the edge to the predecessor is executable.
        ///
        /// \throws std::out_of_range if the index is out of range.
        /// \throws std::bad_weak_ptr if 'x' is not owned by the GTIR (a shared_ptr).
        ///
        void setPredecessor(size_t index, CFGNode* x, bool isExecutable = false);

        ///
        /// Sets the predecessor at the given index to a new pointer.
        ///
        /// \param  index   The index of the existing predecessor to replace.  Special case: If the
        /// index is one past the end of the list of successors, it will be appended to the end.
        /// \param  x   An owned pointer to a CFGNode to add as a predecessor.
        /// \param  isExecutable    True if the edge to the predecessor is executable.
        ///
        /// \throws std::out_of_range
        ///
        void setPredecessor(size_t index, std::unique_ptr<CFGNode>&& x, bool isExecutable = false);

        ///
        /// Get the predecessor at the given index.
        ///
        /// \param x    The index of the predecessor to get.
        ///
        /// \return     The pointer to the predecessor at the given index, or nullptr.  The boolean
        /// flag indicates if the forward edge is executable.
        ///
        std::pair<CFGNode*, bool> getPredecessor(size_t x) const;

        ///
        /// Gets if there are any predecessors or if the list is empty.
        ///
        bool getPredecessorsEmpty() const;

        ///
        /// Gets the total number of predecessors.
        ///
        size_t getPredecessorSize() const;

        ///
        /// Removes the predecessor at the given index.
        ///
        /// The CFGNode will be removed from the list of predecessors, but not deallocated.
        ///
        /// \param  x   The index of the predecessor to remove.
        ///
        /// \return true on success.
        ///
        void removePredecessor(size_t x);

        ///
        /// Removes the predecessors with the given address.
        ///
        /// The CFGNode will be removed from the list of predecessors, but not deallocated.
        ///
        /// \param  x   The address of the predecessor to remove.
        ///
        /// \return true on success.
        ///
        /// \throws std::bad_weak_ptr if 'x' is not owned by the GTIR (a shared_ptr).
        ///
        void removePredecessor(const CFGNode* const x);

        ///
        /// Removes the predecessors with the given address.
        ///
        /// The CFGNode will be removed from the list of predecessors, but not deallocated.
        ///
        /// \param  x   The address of the predecessor to remove.
        /// \param  isExecutable   The executable edge test to apply when deciding if to remove a
        /// CFGNode.
        ///
        /// \return true on success.
        ///
        /// \throws std::bad_weak_ptr if 'x' is not owned by the GTIR (a shared_ptr).
        ///
        void removePredecessor(const CFGNode* const x, bool isExecutable);

        ///
        /// Get a pointer to a base class for CFGNodeInfo, if one was added as a child (via
        /// Node::push_back).
        ///
        /// \return     A pointer to the CFGNodeInfo child or nullptr.
        ///
        CFGNodeInfo* getCFGNodeInfo() const;

        ///
        /// The bytes for the loaded instruction associated with this CFGNode.
        ///
        /// It is suggested that this point to either loaded data elsewhere already in memory or a
        /// user-owned std::vector's internal data.
        ///
        /// \param x    A non-owning pointer to storage for the loaded instruction bytes.  It is
        /// owned (and thus has a lifetime) outside the control
        /// of the CFGNode.
        ///
        void setLoadedInstructionBytes(uint8_t* x);

        ///
        /// The bytes for the loaded instruction associated with this CFGNode.
        ///
        /// It is suggested that this point to either loaded data elsewhere already in memory or a
        /// user-owned std::vector's internal data.
        ///
        /// \return A non-owning pointer to storage for the loaded instruction bytes.  It is owned
        /// (and thus has a lifetime) outside the control
        /// of the CFGNode.
        ///
        uint8_t* getLoadedInstructionBytes() const;

    protected:
        ///
        /// Add an existing CFGNode as a new CFGNode.
        ///
        /// Provides uniform implementation for successors and predecessors
        ///
        /// \param  x   A non-owned pointer to a CFGNode to add as a CFGNode.  If it has already
        /// been added, it will be added again.
        /// \param  isExecutable    True if the edge to the CFGNode is executable.
        ///
        /// \throws std::bad_weak_ptr if 'x' is not owned by the GTIR (a shared_ptr).
        ///
        void add(std::vector<std::pair<std::weak_ptr<CFGNode>, bool>>& vec, CFGNode* x,
                 bool isExecutable = false);

        ///
        /// Add a new CFGNode as a new CFGNode.
        ///
        /// Provides uniform implementation for successors and predecessors
        ///
        /// \param  x   An owned pointer to a CFGNode to add as a CFGNode.
        /// \param  isExecutable    True if the edge to the CFGNode is executable.
        ///
        void add(std::vector<std::pair<std::weak_ptr<CFGNode>, bool>>& vec,
                 std::unique_ptr<CFGNode>&& x, bool isExecutable = false);

        ///
        /// Sets the CFGNode at the given index to a new pointer.
        ///
        /// Provides uniform implementation for successors and predecessors
        ///
        /// \param  index   The index of the existing CFGNode to replace. Special case: If the
        /// index is one past the end of the list of successors, it will be appended to the end.
        /// \param  x   A non-owned pointer to a CFGNode to add as a CFGNode.  If it has already
        /// been added, it will be added again.
        /// \param  isExecutable    True if the edge to the CFGNode is executable.
        ///
        /// \throws std::out_of_range if the index is out of range.
        /// \throws std::bad_weak_ptr if 'x' is not owned by the GTIR (a shared_ptr).
        ///
        void set(std::vector<std::pair<std::weak_ptr<CFGNode>, bool>>& vec, size_t index,
                 CFGNode* x, bool isExecutable = false);

        ///
        /// Sets the CFGNode at the given index to a new pointer.
        ///
        /// Provides uniform implementation for successors and predecessors
        ///
        /// \param  index   The index of the existing CFGNode to replace.  Special case: If the
        /// index is one past the end of the list of successors, it will be appended to the end.
        /// \param  x   An owned pointer to a CFGNode to add as a CFGNode.
        /// \param  isExecutable    True if the edge to the CFGNode is executable.
        ///
        /// \throws std::out_of_range
        ///
        void set(std::vector<std::pair<std::weak_ptr<CFGNode>, bool>>& vec, size_t index,
                 std::unique_ptr<CFGNode>&& x, bool isExecutable = false);

        ///
        /// Get the CFGNode at the given index.
        ///
        /// Provides uniform implementation for successors and predecessors
        ///
        /// \param x    The index of the CFGNode to get.
        ///
        /// \return     The pointer to the CFGNode at the given index, or nullptr.  The boolean
        /// flag indicates if the forward edge is executable.
        ///
        std::pair<CFGNode*, bool> get(
            const std::vector<std::pair<std::weak_ptr<CFGNode>, bool>>& vec, size_t x) const;

        ///
        /// Removes the CFGNode at the given index.
        ///
        /// Provides uniform implementation for successors and predecessors
        ///
        /// The CFGNode will be removed from the list of CFGNodes, but not deallocated.
        ///
        /// \param  x   The index of the CFGNode to remove.
        ///
        /// \return true on success.
        ///
        void remove(std::vector<std::pair<std::weak_ptr<CFGNode>, bool>>& vec, size_t x);

        ///
        /// Removes the CFGNodes with the given address.
        ///
        /// Provides uniform implementation for successors and predecessors
        ///
        /// The CFGNode will be removed from the list of CFGNodes, but not deallocated.
        ///
        /// \param  x   The address of the CFGNode to remove.
        ///
        /// \return true on success.
        ///
        /// \throws std::bad_weak_ptr if 'x' is not owned by the GTIR (a shared_ptr).
        ///
        void remove(std::vector<std::pair<std::weak_ptr<CFGNode>, bool>>& vec,
                    const CFGNode* const x);

        ///
        /// Removes the CFGNode with the given address.
        ///
        /// Provides uniform implementation for successors and predecessors
        ///
        /// The CFGNode will be removed from the list of CFGNodes, but not deallocated.
        ///
        /// \param  x   The address of the CFGNode to remove.
        /// \param  isExecutable   The executable edge test to apply when deciding if to remove a
        /// CFGNode.
        ///
        /// \return true on success.
        ///
        /// \throws std::bad_weak_ptr if 'x' is not owned by the GTIR (a shared_ptr).
        ///
        void remove(std::vector<std::pair<std::weak_ptr<CFGNode>, bool>>& vec,
                    const CFGNode* const x, bool isExecutable);

    private:
        EA ea;
        CFGNode::Kind kind;

        // Boolean if the edge "isExecutable"
        std::vector<std::pair<std::weak_ptr<CFGNode>, bool>> successors;

        // Boolean if the backward edge "isExecutable"
        std::vector<std::pair<std::weak_ptr<CFGNode>, bool>> predecessors;

        uint8_t* loadedInstructionBytes{nullptr};
    };
}
