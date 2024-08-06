using System;
using System.Collections.Generic;

namespace Phantom.Results;

/// <summary>
/// Hierarchy node for scoped-and-tagged parser matches.
/// </summary>
public class ScopeNode
{
    /// <summary>
    /// Type of node
    /// </summary>
    public ScopeNodeType NodeType { get; set; }
    
    /// <summary>
    /// ParserMatch that represents a tagged token that exists within its scope.
    /// <c>null</c> if this is a scope change.
    /// </summary>
    public ParserMatch? DataMatch { get; set; }
    
    /// <summary>
    /// ParserMatch that opened the current scope.
    /// <c>null</c> if root level, or is not a scope change.
    /// <p/>
    /// If a node has a OpeningMatch, but no <see cref="ClosingMatch"/>,
    /// this indicates a mismatch in scoping tokens: more opening
    /// tokens than than closing tokens.
    /// </summary>
    public ParserMatch? OpeningMatch { get; set; }

    /// <summary>
    /// ParserMatch that closed the current scope.
    /// <c>null</c> if root level, or is not a scope change.
    /// <p/>
    /// If a node has a ClosingMatch, but no <see cref="OpeningMatch"/>,
    /// this indicates a mismatch in scoping tokens: more closing
    /// tokens than opening tokens.
    /// </summary>
    public ParserMatch? ClosingMatch { get; set; }

    /// <summary>
    /// Nodes within this scope
    /// </summary>
    public List<ScopeNode> Children { get; } = new();

    /// <summary>
    /// Parent node of this scope.
    /// <c>null</c> if root level
    /// </summary>
    public ScopeNode? Parent { get; set; }

    /// <summary>
    /// Add a data node as a child to this node
    /// </summary>
    public void AddDataFrom(ParserMatch match)
    {
        Children.Add(new ScopeNode
        {
            NodeType = ScopeNodeType.Data,
            DataMatch = match,
            OpeningMatch = null,
            ClosingMatch = null,
            Parent = this
        });
    }

    /// <summary>
    /// Open a new scope, with this node as parent.
    /// <see cref="OpeningMatch"/> of the new node will be '<paramref name="match"/>';
    /// <see cref="ClosingMatch"/> of the new node will be <c>null</c>.
    /// </summary>
    /// <returns>The new scope node</returns>
    public ScopeNode OpenScope(ParserMatch match)
    {
        var newScope = new ScopeNode
        {
            NodeType = ScopeNodeType.ScopeChange,
            DataMatch = null,
            OpeningMatch = match,
            ClosingMatch = null,
            Parent = this
        };

        Children.Add(newScope);
        return newScope;
    }
    
    /// <summary>
    /// Close this scope, and return parent.
    /// <see cref="ClosingMatch"/> of this node will be set to '<paramref name="match"/>'.
    /// </summary>
    /// <returns>The parent scope node, or <c>null</c></returns>
    public ScopeNode? CloseScope(ParserMatch match)
    {
        ClosingMatch = match;
        return Parent;
    }

    /// <inheritdoc />
    public override string ToString()
    {
        return NodeType switch
        {
            ScopeNodeType.Root => $"Root ({OpeningMatch}, {ClosingMatch}, {Children.Count} children) ",
            ScopeNodeType.Data => $"Data ({DataMatch}) ",
            ScopeNodeType.ScopeChange => $"Scope ({OpeningMatch}, {ClosingMatch}, {Children.Count} children) ",
            _ => throw new ArgumentOutOfRangeException()
        };
    }
}

/// <summary>
/// Type of node in a <see cref="ScopeNode"/> instance
/// </summary>
public enum ScopeNodeType
{
    /// <summary>
    /// Node is the root of the scoped node tree
    /// </summary>
    Root = 0,
    
    /// <summary>
    /// Node contains data inside a scope
    /// </summary>
    Data = 1,
    
    /// <summary>
    /// Node is a scope container
    /// </summary>
    ScopeChange = 2
}