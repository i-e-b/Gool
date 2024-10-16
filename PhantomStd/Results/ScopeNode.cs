using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using Gool.Parsers;

namespace Gool.Results;



/// <summary>
/// A <see cref="ScopeNode{T}"/> with no user data.
/// <p/>
/// Hierarchy node for scoped-and-tagged parser matches.
/// This is derived from a <see cref="ParserMatch"/> tree, but
/// the structure comes from <see cref="IParser.Tag"/> and <see cref="IParser.Scope"/>
/// rather that the structure of parsers.
/// </summary>
public class ScopeNode : ScopeNode<None>
{
}

/// <summary>
/// Hierarchy node for scoped-and-tagged parser matches.
/// This is derived from a <see cref="ParserMatch"/> tree, but
/// the structure comes from <see cref="IParser.Tag"/> and <see cref="IParser.Scope"/>
/// rather that the structure of parsers.
/// </summary>
/// <typeparam name="T">Type of user data</typeparam>
[SuppressMessage("ReSharper", "UnusedAutoPropertyAccessor.Global")]
public class ScopeNode<T>
{
    /// <summary>
    /// Type of node
    /// </summary>
    public ScopeNodeType NodeType { get; private set; }

    /// <summary>
    /// ParserMatch that represents a tagged token that exists within its scope.
    /// <c>null</c> if this is a scope change.
    /// </summary>
    public ParserMatch? DataMatch { get; private set; }

    /// <summary>
    /// Consumer-supplied context data.
    /// The parser system does not supply or use this, it is for tracking consumer processes.
    /// </summary>
    public T? UserData { get; set; }

    /// <summary>
    /// ParserMatch that opened the current scope.
    /// <c>null</c> if root level, or is not a scope change.
    /// <p/>
    /// If a node has a OpeningMatch, but no <see cref="ClosingMatch"/>,
    /// this indicates a mismatch in scoping tokens: more opening
    /// tokens than than closing tokens.
    /// </summary>
    public ParserMatch? OpeningMatch { get; private set; }

    /// <summary>
    /// ParserMatch that closed the current scope.
    /// <c>null</c> if root level, or is not a scope change.
    /// <p/>
    /// If a node has a ClosingMatch, but no <see cref="OpeningMatch"/>,
    /// this indicates a mismatch in scoping tokens: more closing
    /// tokens than opening tokens.
    /// </summary>
    public ParserMatch? ClosingMatch { get; private set; }

    /// <summary>
    /// Next node in this scope, if any
    /// </summary>
    public ScopeNode<T>? NextNode { get; private set; }

    /// <summary>
    /// Previous node in this scope, if any
    /// </summary>
    public ScopeNode<T>? PrevNode { get; private set; }

    /// <summary>
    /// Nodes within this scope
    /// </summary>
    public List<ScopeNode<T>> Children { get; } = new();

    /// <summary>
    /// Parent node of this scope.
    /// <c>null</c> if root level
    /// </summary>
    public ScopeNode<T>? Parent { get; set; }
    
    /// <summary>
    /// Walk the tree, and remove <see cref="ScopeNodeType.Data"/> nodes tagged with <paramref name="generalTag"/>;
    /// If they have a neighboring peer node with the same value and one of the <paramref name="specialTags"/>.
    /// </summary>
    /// <param name="generalTag">Tag to prune</param>
    /// <param name="specialTags">Tags that specialise the general tag</param>
    public void Specialise(string generalTag, params string[] specialTags)
    {
        var nodesToPrune = new List<ScopeNode<T>>();
        DepthFirstWalk(n =>
        {
            if (n.NodeType != ScopeNodeType.Data) return;
            if (n.DataMatch?.Tag != generalTag) return;
            
            // Ok, we have a match on the general tag.
            var value = n.DataMatch.Value;

            // If a neighbor have the same value, and one of the specialised tags, remove the general tag
            if (n.PrevNode?.NodeType == ScopeNodeType.Data
                && n.PrevNode?.DataMatch?.Value == value
                && specialTags.Contains(n.PrevNode?.DataMatch?.Tag))
            {
                nodesToPrune.Add(n);
            }
            else if (n.NextNode?.NodeType == ScopeNodeType.Data
                     && n.NextNode?.DataMatch?.Value == value
                     && specialTags.Contains(n.NextNode?.DataMatch?.Tag))
            {
                nodesToPrune.Add(n);
            }
        });

        foreach (var node in nodesToPrune)
        {
            node.PruneNode();
        }
    }

    /// <summary>
    /// Remove this node and its children from the tree, stitching peers together.
    /// </summary>
    private void PruneNode()
    {
        // Remove from peer links
        if (PrevNode is not null) PrevNode.NextNode = NextNode;
        if (NextNode is not null) NextNode.PrevNode = PrevNode;
        
        // Remove from parent
        if (Parent is not null) Parent.RemoveChild(this);
    }

    /// <summary>
    /// Remove a single node from this node's child list
    /// </summary>
    private void RemoveChild(ScopeNode<T> node)
    {
        Children.Remove(node);
    }

    /// <inheritdoc />
    public override string ToString()
    {
        return NodeType switch
        {
            ScopeNodeType.Root => $"Root ({OpeningMatch}, {ClosingMatch}, {Children.Count} children) ",
            ScopeNodeType.Data => DataMatch?.Tag is null ? $"Data ({DataMatch}) " : $"Data {DataMatch?.Tag} ({DataMatch}) ",
            ScopeNodeType.ScopeChange => $"Scope ({OpeningMatch}, {ClosingMatch}, {Children.Count} children) ",
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    private void Link(ScopeNode<T> newChild)
    {
        if (Children.Count > 0)
        {
            var endChild = Children[^1];
            endChild.NextNode = newChild;
            newChild.PrevNode = endChild;
        }

        Children.Add(newChild);
    }

    /// <summary>
    /// Perform an action on every node of the tree, visiting nodes breadth-first.
    /// See also <see cref="DepthFirstWalk"/>
    /// </summary>
    public void BreadthFirstWalk(Action<ScopeNode<T>> action)
    {
        action(this);
        var nextSet = new Queue<ScopeNode<T>>(Children);

        while (nextSet.Count > 0)
        {
            var node = nextSet.Dequeue();
            action(node);
            
            foreach (var child in node.Children) nextSet.Enqueue(child);
        }
    }

    /// <summary>
    /// Perform an action on every node of the tree, visiting nodes depth-first.
    /// See also <see cref="BreadthFirstWalk"/>
    /// </summary>
    public void DepthFirstWalk(Action<ScopeNode<T>> action)
    {
        DepthFirstWalkRec(this, action);
    }

    /// <summary>
    /// Perform an action on every node of the tree with the matching tag, visiting nodes depth-first.
    /// See also <see cref="DepthFirstWalk"/>
    /// </summary>
    public void DepthFirstVisitTags(string tag, Action<ScopeNode<T>> action)
    {
        DepthFirstWalkRec(this, n =>
        {
            if (n.Tag == tag) action(n);
        });
    }

    /// <summary>
    /// Read the tag from any match on this node
    /// </summary>
    public string? Tag => DataMatch?.Tag ?? OpeningMatch?.Tag ?? ClosingMatch?.Tag;

    /// <summary>
    /// Read the content of this node, from any match.
    /// Returns empty string if no values found.
    /// </summary>
    public string Value => DataMatch?.Value ?? OpeningMatch?.Value ?? ClosingMatch?.Value ?? "";

    /// <summary>
    /// Find the parser match for this node, from any match.
    /// Returns <c>null</c> if no values found.
    /// </summary>
    public ParserMatch? AnyMatch => DataMatch ?? OpeningMatch ?? ClosingMatch;

    private static void DepthFirstWalkRec(ScopeNode<T> node, Action<ScopeNode<T>> action)
    {
        action(node);
        foreach (var child in node.Children)
        {
            DepthFirstWalkRec(child, action);
        }
    }
    
    

    /// <summary>
    /// Add a data node as a child to this node
    /// </summary>
    internal void AddDataFrom(ParserMatch match)
    {
        Link(new ScopeNode<T>
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
    internal ScopeNode<T> OpenScope(ParserMatch match)
    {
        var newScope = new ScopeNode<T>
        {
            NodeType = ScopeNodeType.ScopeChange,
            DataMatch = null,
            OpeningMatch = match,
            ClosingMatch = null,
            Parent = this
        };

        Link(newScope);
        return newScope;
    }

    /// <summary>
    /// Create a new root node
    /// </summary>
    private static ScopeNode<T> RootNode()
    {
        return new ScopeNode<T>
        {
            NodeType = ScopeNodeType.Root,
            Parent = null
        };
    }
    
    /// <summary>
    /// Return all parser matches where the parser has been given a tag value.
    /// Matches that have a non-zero 'scope' value will build the hierarchy.
    /// <p/>
    /// Breadth-first scopes are good for building data structure trees
    /// </summary>
    public static ScopeNode<T> FromMatch(ParserMatch root)
    {
        var points = ParserMatch.DepthFirstWalk(root, m => !m.Empty && (m.Tag is not null || m.Scope != ScopeType.None));

        return BuildScope(points);
    }

    private static ScopeNode<T> BuildScope(IEnumerable<ParserMatch> points)
    {
        var scopeEnds = new Stack<int>(); // right-edges of single-sided scopes
        var root = RootNode();
        var cursor = (ScopeNode<T>?)root;
        foreach (var match in points)
        {
            // Exit ranged scope if we've got to the end of it
            while (scopeEnds.Count > 0 && match.Offset > scopeEnds.Peek())
            {
                scopeEnds.Pop();
                cursor = cursor?.Parent;
            }
            if (cursor is null) break; // this will happen if there are too many scope closes

            switch (match.Scope)
            {
                case ScopeType.None:
                    cursor.AddDataFrom(match);
                    break;
                case ScopeType.Pivot:
                    // We add this as normal data, then fix-up later
                    cursor.AddDataFrom(match);
                    break;
                case ScopeType.OpenScope:
                    cursor = cursor.OpenScope(match);
                    break;
                case ScopeType.CloseScope:
                    cursor.ClosingMatch = match;
                    cursor = cursor.Parent;
                    break;
                case ScopeType.Enclosed:
                    // Push a new scope, and exit it after the children of this node
                    cursor = cursor.OpenScope(match);
                    cursor.ClosingMatch = match;
                    scopeEnds.Push(match.Right);
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        return PivotNodes(root);
    }

    private static ScopeNode<T> PivotNodes(ScopeNode<T> node)
    {
        ScopeNode<T>? lastPivot = null;
        var        prePivot  = new List<ScopeNode<T>>();

        for (var index = 0; index < node.Children.Count; index++)
        {
            var child = PivotNodes(node.Children[index]);
            node.Children[index] = child;

            if (child.DataMatch?.Scope == ScopeType.Pivot)
            {
                // Change pivot node's scope type
                lastPivot = child;
                lastPivot.NodeType = ScopeNodeType.ScopeChange;
                lastPivot.OpeningMatch = lastPivot.DataMatch;

                // Move peers to be children
                foreach (var p in prePivot)
                {
                    lastPivot.Children.Add(p);
                    node.Children.Remove(p);
                }

                prePivot.Clear();
            }
            else if (lastPivot is not null)
            {
                lastPivot.Children.Add(child);
                node.Children.Remove(child);
                index--;
            }
            else
            {
                prePivot.Add(child);
            }
        }

        prePivot.Clear();

        return node;
    }

    /// <summary>
    /// Find the first scope node, breadth-first, that has the given tag.
    /// </summary>
    public ScopeNode<T>? FirstByTag(string tagName)
    {
        if (Tag == tagName) return this;
        foreach (var child in Children)
        {
            var found = child.FirstByTag(tagName);
            if (found is not null) return found;
        }

        return null;
    }

    /// <summary>
    /// Find all direct child nodes with the given tag.
    /// </summary>
    public IEnumerable<ScopeNode<T>> ChildrenByTag(string tagName)
    {
        var result = new List<ScopeNode<T>>();

        foreach (var child in Children)
        {
            if (child.Tag == tagName) result.Add(child);
        }

        return result;
    }
}

/// <summary>
/// Type of node in a <see cref="ScopeNode{T}"/> instance
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