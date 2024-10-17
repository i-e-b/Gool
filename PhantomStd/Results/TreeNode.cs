using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;

namespace Gool.Results;

/// <summary>
/// A <see cref="TreeNode{T}"/> with no user data.
/// <p/>
/// Hierarchy derived from a <see cref="ParserMatch"/> tree.
/// This can be manipulated and transformed, using visitor functions.
/// </summary>
[SuppressMessage("ReSharper", "UnusedType.Global")]
public class TreeNode : TreeNode<None>
{
}

/// <summary>
/// Hierarchy derived from a <see cref="ParserMatch"/> tree.
/// This can be manipulated and transformed, using visitor functions.
/// </summary>
/// <typeparam name="T">Type of user data</typeparam>
[SuppressMessage("ReSharper", "UnusedAutoPropertyAccessor.Global")]
[SuppressMessage("ReSharper", "UnusedMember.Global")]
public class TreeNode<T>
{
    /// <summary>
    /// Parser that created this match
    /// </summary>
    public ParserMatch Source { get; private set; } = ParserMatch.NullMatch("Unknown source in TreeNode");

    /// <summary>
    /// Nodes below this match
    /// </summary>
    public List<TreeNode<T>> Children { get; } = new();

    /// <summary>
    /// Parent node. Will be null for root.
    /// </summary>
    public TreeNode<T>? Parent { get; set; }

    /// <summary>
    /// Consumer-supplied context data.
    /// The parser system does not supply or use this, it is for tracking consumer processes.
    /// </summary>
    public T? UserData { get; set; }

    /// <summary>
    /// Build a tree from a parser match
    /// </summary>
    /// <param name="match">Top parser match to process</param>
    /// <param name="prune">If true, empty nodes will be removed</param>
    public static TreeNode<T>? FromParserMatch(ParserMatch match, bool prune)
    {
        var tree = BasicTreeFromMatch(match, null);

        if (prune) tree = PruneTree(tree);

        return PivotTree(tree);
    }

    /// <summary>
    /// Build a tree node from a single string value
    /// </summary>
    public static TreeNode<T> FromString(string match, string? tag = null)
    {
        return new TreeNode<T>
        {
            Source = new ParserMatch(match, tag)
        };
    }

    /// <summary>
    /// Apply a transform operation to all nodes in this tree.
    /// This is repeated until no more transform are made.
    /// <p/>
    /// <b>Important</b>: if your transform always returns new nodes, this method
    /// will continue forever.
    /// </summary>
    /// <param name="node">Root node of tree</param>
    /// <param name="transform">
    /// <p>Function that takes an existing node, and returns its reduced form.</p>
    /// <p>If no reduction is possible, return the given node</p>
    /// <p>If the node should be entirely removed, return <c>null</c></p>
    /// </param>
    public static TreeNode<T>? TransformTree(TreeNode<T>? node, Func<TreeNode<T>, TreeNode<T>?> transform)
    {
        if (node is null) return null;

        var cursor = node;
        var tries  = 2;
        while (tries > 0)
        {
            var changes = false;
            cursor = ApplyTransformRec(cursor, transform, ref changes);
            if (!changes) tries--;
        }

        return cursor;
    }

    /// <summary>
    /// Search the tree node and all children, returning any nodes that match the predicate
    /// </summary>
    public IEnumerable<TreeNode<T>> FindBy(Func<TreeNode<T>, bool> predicate)
    {
        var everything = new List<TreeNode<T>>();
        ListBottomUp(this, everything, predicate);
        return everything;
    }

    #region Inner workings


    /// <summary>
    /// Search the tree node and all children, starting at the bottom-most nodes
    /// </summary>
    private static void ListBottomUp(TreeNode<T>? node, List<TreeNode<T>> target, Func<TreeNode<T>, bool> predicate)
    {
        if (node is null) return;

        // Go down to children first (assuming most trees work bottom-up)
        foreach (var child in node.Children)
        {
            ListBottomUp(child, target, predicate);
        }

        // Now handle this node
        if (predicate(node)) target.Add(node);
    }

    /// <summary>
    /// If a node has no meta-data, and one or zero children;
    /// then remove that node, attaching children to their grandparent.
    /// </summary>
    private static TreeNode<T>? ApplyTransformRec(TreeNode<T>? node, Func<TreeNode<T>, TreeNode<T>?> transform, ref bool changes)
    {
        if (node is null) return null;

        // Go down to children first (assuming most trees work bottom-up)
        for (var index = 0; index < node.Children.Count; index++)
        {
            bool transformChanges = false;

            var before = node.Children[index];
            var after  = ApplyTransformRec(before, transform, ref transformChanges);

            if (transformChanges || after != before || !Equals(after.UserData, before.UserData)) changes = true;

            if (after is null)
            {
                node.Children.RemoveAt(index);
                index--;
                continue;
            }

            node.Children[index] = after;
        }

        // Now handle this node
        var newNode = transform(node);
        if (newNode is null)
        {
            changes = true;
            return null;
        }

        if (node != newNode || !Equals(node.UserData, newNode.UserData))
        {
            changes = true;
            return newNode;
        }

        return node;
    }

    /// <summary>
    /// If a node has no meta-data, and one or zero children;
    /// then remove that node, attaching children to their grandparent.
    /// </summary>
    private static TreeNode<T>? PruneTree(TreeNode<T>? node)
    {
        if (node is null) return null;

        for (var index = 0; index < node.Children.Count; index++)
        {
            var child = PruneTree(node.Children[index]);
            if (child is null)
            {
                node.Children.RemoveAt(index);
                index--;
                continue;
            }

            node.Children[index] = child;

            if (!child.Source.HasMetaData()) // if this node is not interesting on its own,
            {
                if (child.Children.Count == 1) // pull single child up
                {
                    node.Children[index] = child.Children[0];
                }
                else if (child.Children.Count < 1) // remove this node
                {
                    node.Children.RemoveAt(index);
                    index--;
                }
            }
        }

        return node;
    }

    /// <summary>
    /// Build a tree from a parser match
    /// </summary>
    private static TreeNode<T>? BasicTreeFromMatch(ParserMatch match, TreeNode<T>? parent)
    {
        // Recurse, skipping any non-meta nodes
        if (!match.HasMetaData() && !match.HasChildren) return null;

        var cursor = new TreeNode<T> { Source = match, Parent = parent};

        foreach (var child in match.Children())
        {
            var node = BasicTreeFromMatch(child, cursor);
            if (node is not null) cursor.Children.Add(node);
        }

        if (!cursor.Source.HasMetaData()) // always keep nodes with meta-data
        {
            if (cursor.Children.Count < 0) return null; // prune empty results

            if (cursor.Children.Count == 1) // shorten empty lines
            {
                cursor.Children[0].Parent = parent;
                return cursor.Children[0];
            }
        }

        return cursor;
    }

    /// <summary>
    /// Push non-pivot peers of pivot nodes down.
    /// <p/>
    /// The new parent is the last pivot node in order,
    /// or the first pivot if non-pivot nodes come first.
    /// </summary>
    private static TreeNode<T>? PivotTree(TreeNode<T>? node)
    {
        if (node is null) return null;

        TreeNode<T>? lastPivot = null;
        TreeNode<T>? prePivot  = null;
        for (var index = 0; index < node.Children.Count; index++)
        {
            var child = PivotTree(node.Children[index]);
            if (child is null)
            {
                node.Children.RemoveAt(index);
                index--;
                continue;
            }

            node.Children[index] = child;

            if (child.Source.Scope == ScopeType.Pivot)
            {
                lastPivot = child;
                if (prePivot is not null)
                {
                    prePivot.Parent = lastPivot;
                    lastPivot.Children.Add(prePivot);
                    node.Children.Remove(prePivot);
                }

                prePivot = null;
            }
            else if (lastPivot is not null)
            {
                child.Parent = lastPivot;
                lastPivot.Children.Add(child);
                node.Children.Remove(child);
                lastPivot = null;
                index--;
            }
            else
            {
                prePivot = child;
            }
        }

        return node;
    }

    /// <summary>
    /// Output a diagnostic string
    /// </summary>
    public override string ToString() => Source.ToString();

    #endregion Inner workings
}